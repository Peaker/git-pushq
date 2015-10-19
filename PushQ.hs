{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
import           Control.Concurrent
import           Control.Exception (catch, throwIO, SomeException(..))
import           Control.Monad
import           Data.List (stripPrefix, sort)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Time.Clock (NominalDiffTime)
import           GHC.Stack (whoCreated)
import qualified Git
import           Opts (parseOpts, Opts(..))

-- TODO: configurable remote name
remoteName :: Git.Remote
remoteName = "origin"

filterBranchesByPrefix ::
    Git.RepoPath -> String -> Git.BranchLocation -> IO [(String, Git.Branch)]
filterBranchesByPrefix repoPath prefix branchPos =
    do  refs <- Git.listBranches repoPath branchPos
        return (mapMaybe pair refs)
    where
        pair name = (, name) <$> stripPrefix prefix name

testingBranchSplitName :: String -> Maybe (Int, String)
testingBranchSplitName str =
    case break (=='/') str of
    (_, "") -> Nothing
    (numStr, '/':name) ->
        case reads numStr of
        [(num, "")] -> Just (num, name)
        _ -> Nothing
    _ -> error "break returns non-empty snd that doesn't match the break predicate?"

getTestingBranches :: Git.RepoPath -> IO [(Int, String, Git.Branch)]
getTestingBranches repoPath =
    do  branches <-
            map extractPrefix
            <$> filterBranchesByPrefix repoPath "testing/" Git.LocalBranch
        return $ sort branches
    where
        extractPrefix (fullName, branch) =
            (num, name, branch)
            where
                (num, name) = fromMaybe err $ testingBranchSplitName fullName
        err = error $ "/testing/* branch format should be <num>-<name>"

reject :: Git.RepoPath -> String -> Git.RefSpec -> String -> IO ()
reject repoPath name refSpec msg =
    do  putStrLn $ "rejecting " ++ name ++ ":\n" ++ msg
        newRejectBranch `catch` \failure ->
            putStrLn $ "Failed to reject branch, git output:\n" ++ Git.gitCommandStderr failure
    where
        newRejectBranch =
            do  Git.branchNewCheckout repoPath ("rejected/" ++ name) refSpec
                Git.commit repoPath msg

-- | Rebase an orig/<name> branch on top of the given point into testing/<name>
-- Returns the new result commit (tip of rebased)
attemptRebase :: Git.RepoPath -> (String, Git.Branch) -> String -> IO (Maybe Git.CommitID)
attemptRebase repoPath (name, testingBranch) base =
    do  Git.branchCheckout repoPath base
        Git.ignoreError (Git.branchDelete repoPath testingBranch)
        Git.branchNewCheckout repoPath testingBranch origBranch
        (Just testingBranch <$ Git.rebase repoPath base)
            `Git.onFail`
            do  Git.branchCheckout repoPath origBranch
                Git.branchDelete repoPath testingBranch
                baseCommitId <- Git.revParse repoPath base
                reject repoPath name "HEAD" $ concat
                    [ "Rejected due to rebase failure (on "
                    , base, "[", baseCommitId, "])" ]
                Git.branchCheckout repoPath base
                Git.branchDelete repoPath origBranch
                return Nothing
    where
        origBranch = "orig/" ++ name

-- | Rebase all testing branches to be fast-forwards of one another
rebaseTestingBranches :: Git.RepoPath -> IO Git.Branch
rebaseTestingBranches repoPath =
    do  branches <- getTestingBranches repoPath
        (rebased, attempted, res) <- loop 0 0 "master" branches
        putStrLn $ concat
            [ show (length branches), " testing branches exists (successfully rebased "
            , show rebased, " of ", show attempted, ")"]
        return res
    where
        loop :: Int -> Int -> Git.RefSpec -> [(Int, String, Git.RefSpec)] -> IO (Int, Int, Git.RefSpec)
        loop rebased attempted prev [] = return (rebased, attempted, prev)
        loop rebased attempted prev ((_num, name, branch):nexts) =
            do  divergence <- Git.commitsBetween repoPath branch prev
                (onRebased, onAttempted, newPos) <-
                    if null divergence
                    then return (id, id, branch)
                    else maybe (id, (+1), prev) ((,,) (+1) (+1))
                         <$> attemptRebase repoPath (name, branch) prev
                loop (onRebased rebased) (onAttempted attempted) newPos nexts

poll :: Git.RepoPath -> IO ()
poll repoPath =
    do  Git.fetch repoPath remoteName
        proposed <-
            filterBranchesByPrefix repoPath (remoteName <> "/q/") Git.RemoteBranch
        base <- rebaseTestingBranches repoPath
        let nextId = maybe 1 fst $ testingBranchSplitName base
        unless (null proposed) $
            do  putStrLn $ "Rebasing proposed branches on top of " ++ show base ++ ":"
                mapM_ (putStrLn . fst) proposed
        loop nextId base proposed
    where
        loop _ _ [] = return ()
        loop nextId prev ((name, branch):nexts) =
            do  q <- Git.revParse repoPath branch
                Git.branchDeleteRemote repoPath remoteName ("q/" ++ name)
                success <-
                    True <$ Git.branchNewCheckout repoPath ("orig/" ++ name) q
                    `Git.onFail`
                    False <$
                    ( reject repoPath name q $ concat
                      ["Rejected, test of ", show name, " already in progress!"]
                    )
                when success $
                    do  let newTestingBranch = "testing/" ++ show nextId ++ "/" ++ name
                        newBase <-
                            fromMaybe prev
                            <$> attemptRebase repoPath (name, newTestingBranch) prev
                        loop (nextId+1) newBase nexts

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . truncate . (1000000 *)

run :: Opts -> IO ()
run (Opts interval repoPath) =
    forever $
    do  poll repoPath
        sleep interval

main :: IO ()
main =
    do  opts <- parseOpts
        run opts
    `catch` \e@SomeException {} ->
        do  mapM_ putStrLn =<< whoCreated e
            throwIO e
