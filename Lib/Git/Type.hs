{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Lib.Git.Type
	( runGit
	, GitFailure
	, gitExec
	, gitError
	, GitCtx
	, makeConfig
	, Object(..)
	, Config(..)
	, Commitent(..)
	, Person(..)
	, ID
	, CommitID
	, BlobID
	, TreeID
	, TagID
	, toID
	, objToID
	, objOfString
	) where

import System.Process
import System.Exit
import Data.Maybe
import qualified Control.Exception as C
import Control.Concurrent
import Control.Monad.Reader
import System.IO (Handle, hFlush, hClose, hGetContents, hPutStr)

-- | any ID (git SHA1 string)
type ID = String

-- | a commit ID
type CommitID = ID

-- | a blob ID
type BlobID = ID

-- | a tree ID
type TreeID = ID

-- | a tag ID
type TagID = ID

-- | Tagged ID of all possible types
data Object =
	  Commit CommitID
	| Blob BlobID
	| Tree TreeID
	| Tag TagID
	deriving (Show)

type GitFailure = (Int, String, String, String, [String])

{-| Represent a repository -}
data Config = Config
	{ configCwd     :: FilePath       -- ^ Path to the repository .git
	, configGitPath :: Maybe FilePath -- ^ Optional path to the git executable (otherwise resolved from $PATH)
	} deriving (Show)

newtype GitCtx a = GitCtx (ReaderT Config IO a)
	deriving (Monad, MonadIO, MonadReader Config)

-- | Commit object author/commiter representation
data Person = Person
	{ personName  :: String
	, personEmail :: String
	} deriving (Show)

-- | Commit entity representation
data Commitent = Commitent
	{ ceParents       :: [CommitID]
	, ceTree          :: TreeID
	, ceAuthor        :: Person
	, ceAuthorTime    :: String
	, ceCommitter     :: Person
	, ceCommitterTime :: String
	, ceCommitMsg     :: String
	} deriving (Show)

-- read a string as an ID
toID :: String -> ID
toID = id

objToID :: Object -> ID
objToID (Commit gitid) = gitid
objToID (Tree gitid)   = gitid
objToID (Blob gitid)   = gitid
objToID (Tag gitid)    = gitid

objOfString :: String -> ID -> Maybe Object
objOfString s gitid =
	case s of
		"blob"   -> Just $ Blob gitid
		"tree"   -> Just $ Tree gitid
		"commit" -> Just $ Commit gitid
		"tag"    -> Just $ Tag gitid
		_        -> Nothing

{-| Run a git context from a config and returns the result
 -}
runGit :: Config -> GitCtx t -> IO t
runGit config (GitCtx a) = runReaderT a config

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

-- | internal function to execute a git command
gitExec :: String -> [String] -> [(String, String)]
        -> GitCtx (Either GitFailure String)
gitExec cmd opts menv = do
	cfg <- ask
	let args = cmd : opts
	let gitpath = fromMaybe "git" (configGitPath cfg)
	(ec, out, err) <- liftIO $ readProc (configCwd cfg) gitpath args menv ""
	case ec of
		ExitSuccess   -> return $ Right out
		ExitFailure i -> return $ Left (i, out, err, configCwd cfg, cmd : opts)

-- | internal function to call on failure to make a friendly error message
gitError :: GitFailure -> String -> b
gitError (exitval, stdout, stderr, mcwd, cmd) msg =
	error $ concat [ "git error ", "[cwd: ", mcwd,
		"][exec: ", concat cmd, "][exit: ", show exitval, "][msg: ", msg, "] ",
		 "stdout: ", stdout, " stderr: ", stderr ] 

-- same as readProcessWithExitCode but having a configurable cwd and env,
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

{- initialize a git context. just a path for now could take limit afterwards -}
makeConfig :: FilePath -> Maybe FilePath -> Config
makeConfig path gitpath = Config { configCwd = path, configGitPath = gitpath }
