{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-
 - Copyright (C) 2009-2010 Vincent Hanquez <vincent@snarc.org>
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU Lesser General Public License as published by
 - the Free Software Foundation; version 2.1 only.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -}


{- |
   Module      : Git
   Copyright   : Copyright (C) 2009-2010 Vincent Hanquez
   License     : GNU LGPL, version 2.1
   Maintainer  : Vincent Hanquez <vincent@snarc.org>
   Stability   : alpha
   Portabily   : haven't tested

This module provide Git functionality exec'ing the git binary.

give simple access to commit, tree, tag, blob objects.
-}


module Lib.Git (
	runGit,
	makeConfig,
	revlist, revparse, taglist, treelist,
	hasDiff,
	commit,
	add, rm,
	checkout,
	getObjType, getObjsType,
	catBlob, catTag, catCommit, catTree,
	resolveFilePath,
	Object(..),
	Commitent(..),
	Person(..),
	Config(..), ID,
	toID, objToID
	)
	where

import System.Process
import System.Exit
import Data.Maybe
import qualified Data.List
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad.Reader
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)
import Data.Char (ord, chr)

type ID = String
type CommitID = ID
type BlobID = ID
type TreeID = ID
type TagID = ID

data Object =
	  Commit CommitID
	| Blob BlobID
	| Tree TreeID
	| Tag TagID
	deriving (Show)

data Person = Person {
	personName :: String,
	personEmail :: String
} deriving (Show)

data Commitent = Commitent {
	ceParents :: [CommitID],
	ceTree :: TreeID,
	ceAuthor :: Person,
	ceAuthorTime :: String,
	ceCommitter :: Person,
	ceCommitterTime :: String,
	ceCommitMsg :: String
} deriving (Show)

type Perms = (Int, Int, Int, Int, Int, Int) -- find something better
type Treeent = [ (Perms, Object, FilePath) ]

data Config = Config { configCwd :: FilePath, configGitPath :: Maybe FilePath }

newtype GitCtx a = GitCtx (ReaderT Config IO a)
	deriving (Monad, MonadIO, MonadReader Config)

-- read a string as an ID
toID :: String -> ID
toID = id

objToID :: Object -> ID
objToID (Commit gitid) = gitid
objToID (Tree gitid) = gitid
objToID (Blob gitid) = gitid
objToID (Tag gitid) = gitid

split :: Char -> String -> [String]
split _     []       = [""]
split delim (c:cs)
	| c == delim = "" : rest
	| otherwise  = (c : head rest) : tail rest
	where
		rest = split delim cs

spanSubList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanSubList _ xs@[]      = (xs, xs)
spanSubList p xs@(x:xs')
	| p xs           = let (ys,zs) = spanSubList p xs' in (x:ys,zs)
	| otherwise      = ([],xs)

breakSubList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakSubList p = spanSubList (not . p)

permsOfString :: String -> Perms
permsOfString s =
	case map (read . replicate 1) $ s of
		[ a, b, c, d, e, f ] -> (a, b, c, d, e, f)
		_                    -> (0, 0, 0, 0, 0, 0)

-- revision can be specified as CommitID | TagID

-- just exec with stdin/stdout/stderr as pipes
execProcWithPipes :: FilePath -> String -> [String] -> [(String, String)]
                  -> IO (Handle, Handle, Handle, ProcessHandle)
execProcWithPipes mcwd command args menv = do
	(Just inh, Just outh, Just errh, pid) <- createProcess (proc command args)
		{ std_in = CreatePipe,
		  std_out = CreatePipe,
		  std_err = CreatePipe,
		  cwd = Just mcwd,
		  env = Just menv }
	return (inh, outh, errh, pid)

-- same as readProcessWithExitCode but having a configurable cwd and env,
-- and using bytestring as input/ouput/error
readProc :: FilePath -> String -> [String] -> [(String, String)] -> String
         -> IO (ExitCode, String, String)
readProc mcwd command args menv input = do
	(inh, outh, errh, pid) <- execProcWithPipes mcwd command args menv

	outMVar <- newEmptyMVar

	out <- hGetContents outh
	_ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()

	err <- hGetContents errh
	_ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

	when (length input > 0) $ do hPutStr inh input; hFlush inh
	hClose inh

	takeMVar outMVar
	takeMVar outMVar
	hClose outh
	hClose errh
	
	ex <- waitForProcess pid
 	return (ex, out, err)

type GitFailure = (Int, String, String, String, [String])

-- execute a git command returning either an exit code with the stderr bytestring
-- or the stdout bytestring
gitExec :: String -> [String] -> [(String, String)]
        -> GitCtx (Either GitFailure String)
gitExec cmd opts menv = do
	cfg <- ask
	let args = cmd : opts
	let gitpath = maybe "git" id (configGitPath cfg)
	(ec, out, err) <- liftIO $ readProc (configCwd cfg) gitpath args menv ""
	case ec of
		ExitSuccess   -> return $ Right out
		ExitFailure i -> return $ Left (i, out, err, (configCwd cfg), cmd : opts)

gitError :: GitFailure -> String -> b
gitError (exitval, stdout, stderr, mcwd, cmd) msg =
	error $ concat [ "git error ", "[cwd: ", mcwd,
		"][exec: ", concat cmd, "][exit: ", show exitval, "][msg: ", msg, "] ",
		 "stdout: ", stdout, " stderr: ", stderr ] 

runGit :: Config -> GitCtx t -> IO t
runGit config (GitCtx a) = runReaderT a config

{- initialize a git context. just a path for now could take limit afterwards -}
makeConfig :: FilePath -> Maybe FilePath -> Config
makeConfig path gitpath = Config { configCwd = path, configGitPath = gitpath }

{- revlist return a commit list in reverse chronological order l -}
revlist :: Maybe Int -> Maybe CommitID -> [ FilePath ] -> GitCtx [ CommitID ]
revlist lim topcommit paths = do
	let commitid = maybe "HEAD" id topcommit
	let opt_max = maybe [] (\x -> [ "max-count=" ++ show x ]) lim
	let opts = opt_max ++ [ commitid, "--" ] ++ paths
	o <- gitExec "rev-list" opts []
	case o of
		Right out -> return $ lines out
		Left err  -> gitError err "rev-list"

{- parse a tag/branch-name/commit into a commit if it exists -}
revparse :: String -> GitCtx (Maybe CommitID)
revparse commitid = do
	o <- gitExec "rev-parse" [ commitid ] []
	case o of
		Right out -> return $ Just (head $ lines out)
		Left err  -> gitError err "rev-parse"

{- return a list of tags in this repository -}
taglist :: GitCtx [ TagID ]
taglist = do
	o <- gitExec "tag" [] []
	case o of
		Right out -> return $ lines out
		Left _    -> return []

{- return tree list -}
treelist :: Maybe CommitID -> GitCtx Treeent
treelist commitid = do
	let treeent_of_line line =
		-- parse persmission SP type SP SHA1 SP filename
		case split '\t' line of
			[ o, filename ]    ->
				case split ' ' o of
					[ perms, ty, sha1 ] ->
						let object = fromJust $ objOfString ty sha1 in
						Just (permsOfString perms, object, filename)
					_                   -> Nothing
			_                  -> Nothing
	let comm = maybe "HEAD" id commitid
	let opts = [ comm ]
	o <- gitExec "ls-tree" opts []
	case o of
		Right out -> return $ (catMaybes $ map treeent_of_line $ lines $ out)
		Left err  -> gitError err "ls-tree"

{- add filepath to repository -}
add :: [ FilePath ] -> GitCtx ()
add paths = do
	let opts = [ "--" ] ++ paths
	o <- gitExec "add" opts []
	case o of
		Right _  -> return ()
		Left err -> gitError err "add"

{- rm filepath from repository -}
rm :: [ FilePath ] -> GitCtx ()
rm paths = do
	let opts = [ "--" ] ++ paths
	o <- gitExec "rm" opts []
	case o of
		Right _  -> return ()
		Left err -> gitError err "rm"

{- commit change to the repository with optional filepaths -}
commit :: [ FilePath ] -> String -> String -> String -> GitCtx ()
commit rsrcs author author_email logmsg = do
	let authopts = [ "--author=", author ++ " <" ++ author_email ++ ">" ]
	let msgopts = [ "-m", logmsg ]
	let opts = authopts ++ msgopts ++ [ "--" ] ++ rsrcs
	o <- gitExec "commit" opts []
	case o of
		Right _  -> return ()
		Left err -> gitError err "commit"

{- checkout the index to some commit id creating potentially a branch -}
checkout :: Maybe CommitID -> Maybe String -> GitCtx ()
checkout rev branch = do
	let bopt = maybe [] (\b -> [ "-b", b ]) branch
	let copt = maybe [] (\c -> [ c ]) rev
	_ <- gitExec "checkout" (bopt ++ copt) []
	return ()

{- does the repository has modification in its index -}
hasDiff :: GitCtx Bool
hasDiff = do
	o <- gitExec "diff" [ "--exit-code" ] []
	case o of
		Left (1, _, _, _, _) -> return True
		Right _           -> return False
		Left err          -> gitError err "hasdiff"

objOfString :: String -> ID -> Maybe Object
objOfString s gitid =
	case s of
		"blob"   -> Just $ Blob gitid
		"tree"   -> Just $ Tree gitid
		"commit" -> Just $ Commit gitid
		"tag"    -> Just $ Tag gitid
		_        -> Nothing

{- return object type -}
getObjType :: ID -> GitCtx (Maybe Object)
getObjType s = do
	let object_of o = objOfString (head $ lines o) s
	o <- gitExec "cat-file" [ "-t", s ] []
	case o of
		Right out -> return $ object_of out
		Left err  -> gitError err ("cat-file -t " ++ s)

{- return types of list of objects -}
getObjsType :: [ID] -> GitCtx [Maybe Object]
getObjsType ids = forM ids $ getObjType

{- cat an object with type specified -}
catType :: String -> ID -> GitCtx String
catType ty obj = do
	o <- gitExec "cat-file" [ ty, obj ] []
	case o of
		Right out -> return out
		Left err  -> gitError err "object doesn't exists or wrong type"

-- cat specific objects type
catBlob :: BlobID -> GitCtx String
catTag :: TagID -> GitCtx String

catBlob = catType "blob"
catTag = catType "tag"

{- perms SP file \0 sha1 -}
catTree :: TreeID -> GitCtx Treeent
catTree treeid = do
	let treebin_of (c, bs) =
		let (perms, l1) = break (== ' ') bs in
		let (file, left) = break (== '\0') (drop 1 l1) in
		let sha1 = hexalise $ take 20 $ drop 1 left in
		let bs' = drop 21 left in
		let nt = (permsOfString perms, sha1, file) in
		(nt : c, bs')
	out <- catType "tree" treeid 
	let (trees, _) = runParseString treebin_of ([], out)
	ents <- forM trees $ \(perms, sha1, file) -> do
		obj <- getObjType sha1
		return (perms, fromJust obj, file)
	return $ ents

{- FIXME time : 1253463017 +0100 -}
catCommit :: CommitID -> GitCtx Commitent
catCommit commitid = do
	out <- catType "commit" commitid 
	let (fullhdr, commitMsg) = breakSubList ((==) "\n\n" . take 2) out
	let centinit = Commitent {
			ceParents = [],
			ceTree = "",
			ceAuthor = Person { personName = "", personEmail = "" },
			ceAuthorTime = "",
			ceCommitter = Person { personName = "", personEmail = "" },
			ceCommitterTime = "",
			ceCommitMsg = drop 2 commitMsg
		}
	let id_of_string bs =
		let (name, left) = breakSubList ((==) " <" . take 2) bs in
		let (email, left') = breakSubList ((==) "> " . take 2) (drop 2 left) in
		let time = drop 2 left' in
		(Person { personName = name, personEmail = email }, time)
	let author_of_line c bs =
		let (author, authorTime) = id_of_string bs in
		c { ceAuthor = author, ceAuthorTime = authorTime }
	let committer_of_line c bs =
		let (committer, committerTime) = id_of_string bs in
		c { ceCommitter = committer, ceCommitterTime = committerTime }
	let hdr_of_string (c, (fline:left)) =
		let (cat, line) = break (== ' ') fline in
		let c' =
			case cat of
				"tree"      -> c { ceTree = drop 1 line }
				"parent"    -> c { ceParents = drop 1 line : ceParents c }
				"author"    -> author_of_line c (drop 1 line)
				"committer" -> committer_of_line c (drop 1 line)
				_           -> c
			in
		(c', left)
	let (cent, _) = runParseLines hdr_of_string (centinit, lines fullhdr)
	return cent

resolveFilePath :: Commitent -> FilePath -> GitCtx [ (FilePath, Object) ]
resolveFilePath commitent filepath = do
	let treeid = ceTree commitent
	t <- catTree treeid
	resolveFilePathTree t filepath

resolveFilePathTree :: Treeent -> FilePath -> GitCtx [ (FilePath, Object) ]
resolveFilePathTree tree filepath = do
	case break (== '/') filepath of
		("", path)  -> resolveFilePathTree tree (tail path)
		(ent, "")   -> do
			let obj = objOfTreepath tree ent
			case obj of
				Just o   -> return [ (ent, o) ]
				Nothing  -> error ("missing last ent " ++ ent)
		(ent, path) ->
			let obj = objOfTreepath tree ent in
			case obj of
				Just (Tree treeid) -> do
					childtree <- catTree treeid
					ret <- resolveFilePathTree childtree (tail path)
					return $ ((ent, fromJust obj) : ret)
				Just (Blob _)  ->
					return [ (ent, fromJust obj) ]
				Just _         ->
					error ("assertion failed: expecting tree or blob")
				Nothing        ->
					error ("missing ent " ++ ent)

objOfTreepath :: Treeent -> String -> Maybe Object
objOfTreepath treeent path =
	case Data.List.find (\(_, _, p) -> p == path) treeent of
		Nothing          -> Nothing
		Just (_, obj, _) -> Just obj

hexalise :: String -> String
hexalise s =
	concatMap (\b -> let c = ord b in [ hex $ c `div` 16, hex $ c `mod` 16 ]) s
	where hex i
		| i >= 0 && i <= 9   = chr $ ord '0' + i
		| i >= 10 && i <= 15 = chr $ ord 'a' + i - 10
		| otherwise          = '\0'

runParseString :: ((t, String) -> (t, String)) -> (t, String) -> (t, String)
runParseString f (c, s) =
	if null s
	then (c, s)
	else runParseString f $ f (c, s)

runParseLines :: ((t, [a]) -> (t, [a])) -> (t, [a]) -> (t, [a])
runParseLines f (c, l) =
	if null l
	then (c, l)
	else runParseLines f $ f (c, l)
