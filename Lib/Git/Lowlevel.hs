module Lib.Git.Lowlevel
	(
	  revlist
	, revparse
	, catBlob
	, catTag
	, catTree
	, catCommit
	) where

import Lib.Git.Type
import Lib.Git.Tree
import Data.Char (chr, ord)
import Control.Monad
import Data.Maybe

spanSubList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanSubList _ xs@[]      = (xs, xs)
spanSubList p xs@(x:xs')
	| p xs           = let (ys,zs) = spanSubList p xs' in (x:ys,zs)
	| otherwise      = ([],xs)

breakSubList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakSubList p = spanSubList (not . p)

-- revision can be specified as CommitID | TagID

{- revlist return a commit list in reverse chronological order l -}
revlist :: Maybe Int -> Maybe CommitID -> [ FilePath ] -> GitCtx [ CommitID ]
revlist lim topcommit paths = do
	let commitid = fromMaybe "HEAD" topcommit
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
getObjsType = mapM getObjType

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
	forM trees $ \(perms, sha1, file) -> do
		obj <- getObjType sha1
		return (perms, fromJust obj, file)

{- FIXME time : 1253463017 +0100 -}
catCommit :: CommitID -> GitCtx Commitent
catCommit commitid = do
	out <- catType "commit" commitid 
	let (fullhdr, commitMsg) = breakSubList ((==) "\n\n" . take 2) out
	let centinit = Commitent
		{ ceParents = []
		, ceTree = ""
		, ceAuthor = Person { personName = "", personEmail = "" }
		, ceAuthorTime = ""
		, ceCommitter = Person { personName = "", personEmail = "" }
		, ceCommitterTime = ""
		, ceCommitMsg = drop 2 commitMsg
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
	let hdr_of_string (c, fline:left) =
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
