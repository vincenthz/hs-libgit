module Lib.Git.Tree
	( treeList
	, Perms
	, permsOfString
	, Treeent
	) where

import Lib.Git.Type
import Data.Maybe

type Perms = (Int, Int, Int, Int, Int, Int) -- find something better
type Treeent = [ (Perms, Object, FilePath) ]

permsOfString :: String -> Perms
permsOfString s =
	case map (read . replicate 1) s of
		[ a, b, c, d, e, f ] -> (a, b, c, d, e, f)
		_                    -> (0, 0, 0, 0, 0, 0)

split :: Char -> String -> [String]
split _     []       = [""]
split delim (c:cs)
	| c == delim = "" : rest
	| otherwise  = (c : head rest) : tail rest
	where
		rest = split delim cs

{- return tree list -}
treeList :: Maybe CommitID -> GitCtx Treeent
treeList commitid = do
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
	let comm = fromMaybe "HEAD" commitid
	let opts = [ comm ]
	o <- gitExec "ls-tree" opts []
	case o of
		Right out -> return (mapMaybe treeent_of_line $ lines out)
		Left err  -> gitError err "ls-tree"
