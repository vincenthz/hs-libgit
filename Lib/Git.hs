{- |
   Module      : Git
   Copyright   : Copyright (C) 2009-2010 Vincent Hanquez
   License     : BSD3
   Maintainer  : Vincent Hanquez <vincent@snarc.org>
   Stability   : alpha
   Portabily   : haven't tested

This module provide Git functionality exec'ing the git binary.

give simple access to commit, tree, tag, blob objects.
-}


module Lib.Git
	( module Lib.Git.Type
	, module Lib.Git.Tree
	, module Lib.Git.Index
	, module Lib.Git.Lowlevel
	, taglist
	, initDB
	, add
	, rm
	, commit
	, checkout
	, hasDiff
	, resolveFilePath
	) where

import Data.Maybe
import qualified Data.List
import Lib.Git.Type
import Lib.Git.Tree
import Lib.Git.Index
import Lib.Git.Lowlevel

{- return a list of tags in this repository -}
taglist :: GitCtx [ TagID ]
taglist = do
	o <- gitExec "tag" [] []
	case o of
		Right out -> return $ lines out
		Left _    -> return []

{- initialize a new repository database -}
initDB :: GitCtx ()
initDB = do
	o <- gitExec "init-db" ["--bare"] []
	case o of
		Right _  -> return ()
		Left err -> gitError err "init-db"

{- add filepath to repository -}
add :: [ FilePath ] -> GitCtx ()
add paths = do
	let opts = "--" : paths
	o <- gitExec "add" opts []
	case o of
		Right _  -> return ()
		Left err -> gitError err "add"

{- rm filepath from repository -}
rm :: [ FilePath ] -> GitCtx ()
rm paths = do
	let opts = "--" : paths
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
	let copt = maybeToList rev -- [] (: []) rev
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

resolveFilePath :: Commitent -> FilePath -> GitCtx [ (FilePath, Object) ]
resolveFilePath commitent filepath = do
	let treeid = ceTree commitent
	t <- catTree treeid
	resolveFilePathTree t filepath

resolveFilePathTree :: Treeent -> FilePath -> GitCtx [ (FilePath, Object) ]
resolveFilePathTree tree filepath =
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
					return ((ent, fromJust obj) : ret)
				Just (Blob _)  ->
					return [ (ent, fromJust obj) ]
				Just _         ->
					error "assertion failed: expecting tree or blob"
				Nothing        ->
					error ("missing ent " ++ ent)

objOfTreepath :: Treeent -> String -> Maybe Object
objOfTreepath treeent path =
	case Data.List.find (\(_, _, p) -> p == path) treeent of
		Nothing          -> Nothing
		Just (_, obj, _) -> Just obj
