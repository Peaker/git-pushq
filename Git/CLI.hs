{-# OPTIONS -Wall #-}
module Git.CLI
    ( GitCommandFailed(..), onFail, ignoreError
    , fetch
    , BranchLocation(..)
    , RepoPath
    , Remote
    , Branch
    , CommitID
    , RefSpec
    , listBranches
    , commitsBetween
    , branchCheckout
    , branchNewCheckout
    , branchNew
    , branchDelete
    , branchDeleteRemote
    , branchRename
    , commit
    , rebaseOnto
    , revParse
    ) where

import Control.Exception (Exception, throwIO, catch)
import Control.Monad (void)
import Data.List (isSuffixOf)
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.Process

type Remote = String
type RepoPath = FilePath
type Branch = String
type RefSpec = String
type CommitID = String

data GitCommandFailed = GitCommandFailed
    { gitCommand :: String
    , gitCommandArgs :: [String]
    , gitCommandExitCode :: Int
    , gitCommandStdout :: String
    , gitCommandStderr :: String
    } deriving (Typeable)
instance Show GitCommandFailed where
    show (GitCommandFailed cmd args errCode out err) =
        concat
        [ "git ", cmd, " ", show args, " failed with error code:"
        , show errCode, ".\n\n"
        , if null out then "" else "git output was:\n" ++ out
        , if null err then "" else "git stderr was:\n" ++ err
        ]
instance Exception GitCommandFailed

infixl 1 `onFail`
onFail :: IO a -> IO a -> IO a
act `onFail` handler = act `catch` \GitCommandFailed {} -> handler

ignoreError :: IO () -> IO ()
ignoreError = (`onFail` return ())

git :: RepoPath -> String -> [String] -> IO String
git repoPath cmd args =
    do  (exitCode, out, err) <- readCreateProcessWithExitCode process stdin
        case exitCode of
            ExitSuccess -> return out
            ExitFailure i -> throwIO (GitCommandFailed cmd args i out err)
    where
        stdin = ""
        process = CreateProcess
            { cmdspec = RawCommand "git" (cmd:args)
            , cwd = Just repoPath
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = True
            , create_group = False
            , delegate_ctlc = False
            }

fetch :: RepoPath -> Remote -> IO ()
fetch repoPath remoteName =
    do  _ <- git repoPath "remote" ["prune", remoteName]
        _ <- git repoPath "fetch" [remoteName]
        return ()

data BranchLocation = LocalBranch | RemoteBranch | AllBranches

listBranches :: RepoPath -> BranchLocation -> IO [Branch]
listBranches repoPath loc =
    map (drop 2) . lines <$> git repoPath "branch" opts
    where
        opts = case loc of
            LocalBranch -> []
            RemoteBranch -> ["-r"]
            AllBranches -> ["-a"]

-- | equivalent to git log src..dest (exclusive src/inclusive dest)
-- only hashes
commitsBetween :: RepoPath -> RefSpec -> RefSpec -> IO [CommitID]
commitsBetween repoPath src dest =
    lines <$> git repoPath "log" ["--pretty=format:%h", src ++ ".." ++ dest]

branchCheckout :: RepoPath -> Branch -> IO ()
branchCheckout repoPath name =
    void $ git repoPath "checkout" [name, "--"]

branchNewCheckout :: RepoPath -> Branch -> RefSpec -> IO ()
branchNewCheckout repoPath name pos =
    void $ git repoPath "checkout" ["-b", name, pos, "--"]

branchNew :: RepoPath -> Branch -> RefSpec -> IO ()
branchNew repoPath name pos = void $ git repoPath "branch" [name, pos]

branchDelete :: RepoPath -> Branch -> IO ()
branchDelete repoPath name =
    void $ git repoPath "branch" ["-D", name]

branchDeleteRemote :: RepoPath -> Remote -> Branch -> IO ()
branchDeleteRemote repoPath remoteName name =
    void $ git repoPath "push" [remoteName, "--delete", name]

branchRename :: RepoPath -> Branch -> Branch -> IO ()
branchRename repoPath old new =
    void $ git repoPath "branch" ["-M", old, new]

stripNewline :: String -> String
stripNewline xs
    | "\n" `isSuffixOf` xs = init xs
    | otherwise = xs

commit :: RepoPath -> String -> IO ()
commit repoPath msg =
    void $ git repoPath "commit" ["-m", msg, "--allow-empty"]

rebaseOnto :: RepoPath -> RefSpec -> RefSpec -> IO ()
rebaseOnto repoPath newBase oldBase = void $ git repoPath "rebase" ["--onto", newBase, oldBase]

revParse :: RepoPath -> RefSpec -> IO CommitID
revParse repoPath refSpec = stripNewline <$> git repoPath "rev-parse" [refSpec]
