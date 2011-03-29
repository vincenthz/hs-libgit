module Lib.Git.Index
	(
	  indexUpdate
	, indexCheckoutAll
	, indexCheckout
	, indexList
	) where

import Lib.Git.Type

{-| update index with the list of file -}
indexUpdate :: [FilePath] -> GitCtx ()
indexUpdate files = do
	let args = [ "--" ] ++ files
	o <- gitExec "update-index" args []
	case o of
		Right _  -> return ()
		Left err -> gitError err "update-index"

indexCheckoutAll :: Bool -> GitCtx ()
indexCheckoutAll force = do
	let args = (if force then [ "-f" ] else []) ++ [ "-a" ]
	o <- gitExec "checkout-index" args []
	case o of
		Right _  -> return ()
		Left err -> gitError err "checkout-index"

indexCheckout :: [FilePath] -> Bool -> GitCtx ()
indexCheckout paths force =  do
	let args =
		(if force then [ "-f" ] else []) ++
		[ "--" ] ++ paths
	o <- gitExec "checkout-index" args []
	case o of
		Right _  -> return ()
		Left err -> gitError err "checkout-index"

indexList :: GitCtx (FilePath)
indexList = do
	return []
