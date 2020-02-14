#!/usr/bin/env cabal
{- cabal:
    build-depends:
      base, text, containers,
      typed-process, stm, async,
      directory, temporary, filepath
-}

import Control.Monad
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import System.Process.Typed
import System.Directory
import System.IO
import System.IO.Temp
import System.FilePath
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Concurrent.Async

cores = 6
nofibRepeats = 2
perfEvents = []
ghcUtilsPath = ""

nofibDeps = [ "random", "old-time", "parallel"]

data Commit = Commit { commitName :: String
                     , commitRef :: String
                     }
            deriving (Eq, Ord, Show)

commitWorkDir :: Commit -> FilePath
commitWorkDir c = "ghc-" <> commitName c

commitResultDir :: Commit -> FilePath
commitResultDir c = "results" </> commitName c

withCommitResultFile :: Commit -> FilePath -> (Handle -> IO a) -> IO a
withCommitResultFile c fname action = do
    createDirectoryIfMissing True dir
    withFile (dir </> fname) WriteMode action
  where
    dir = commitResultDir c

commitGhcPath :: Commit -> FilePath
commitGhcPath commit =
  commitWorkDir commit </> "_build" </> "stage1" </> "bin" </> "ghc"

readCommits :: IO [Commit]
readCommits =
    mapMaybe parse . lines <$> readFile "branches.txt"
  where
    parse line =
      case words line of
        [] -> Nothing
        [commit] -> Just $ Commit commit commit
        [name, commit] -> Just $ Commit name commit

data Parallelism = Exclusive
                 | WantsCores Int

data Task = Task { taskName :: String
                 , taskDeps :: [Task]
                 , taskRun  :: Commit -> IO ()
                 , taskParallelism :: Parallelism
                 }

task :: String -> (Commit -> IO ()) -> Task
task name run =
  Task { taskName = name
       , taskDeps = []
       , taskRun  = run
       , taskParallelism = Exclusive
       }

parallel :: Int -> Task -> Task
parallel n task = task { taskParallelism = WantsCores n }

requires :: [Task] -> Task -> Task
requires deps task = task { taskDeps = taskDeps task ++ deps }

logMsg :: String -> IO ()
logMsg = putStrLn

checkout :: Task
checkout = parallel 1 $ task "checkout" $ \commit -> do
  let wdir = commitWorkDir commit
      git args = runInCommitDir commit $ proc "git" args
  exists <- doesDirectoryExist (commitWorkDir commit)
  unless exists $ do
    logMsg $ concat [ "Checking out "
                    , commitName commit
                    , " ("
                    , commitRef commit
                    , ") at " <> wdir
                    ]
    runProcess_
      $ setWorkingDir "ghc"
      $ proc "git" ["worktree", "add", "../"<>wdir, commitRef commit]

  git ["checkout", commitRef commit]
  git ["--no-pager", "show"]
  git ["submodule", "update", "--init"]

  createDirectoryIfMissing True (wdir </> "_build")
  let settingsTemplate = wdir </> "_build" </> "hadrian.settings"
  writeFile settingsTemplate . fromMaybe "" =<< readIfExists "hadrian.settings"

  runInCommitDir commit $ proc "./boot" []
  runInCommitDir commit $ proc "./configure" []

build :: Task
build = parallel cores $ requires [checkout] $ task "build" run
  where
    run commit = withCommitResultFile commit "build.log" $ \hdl -> do
      runInCommitDir commit
        $ setStderr (useHandleOpen hdl)
        $ setStdout (useHandleOpen hdl)
        $ proc ("hadrian" </> "build.cabal.sh") ["-j" <> show cores]

test :: Task
test = parallel cores $ requires [build] $ task "test" run
  where
    run commit = withCommitResultFile commit "test.log" $ \hdl -> do
      runInCommitDir commit
        $ setStderr (useHandleOpen hdl)
        $ setStdout (useHandleOpen hdl)
        $ proc ("hadrian" </> "build.cabal.sh")
        $ [ "-j" <> show cores
          , "--summary-metrics=" <> commitResultDir commit </> "test-metrics"
          , "test"
          ]

nofibBoot :: Task
nofibBoot = parallel 1 $ requires [build] $ task "boot-nofib" run
  where
    run commit = do
      withCommitResultFile commit "build-nofib-deps.log" $ \hdl -> do
        runInCommitDir commit
          $ setStderr (useHandleOpen hdl)
          $ setStdout (useHandleOpen hdl)
          $ proc "cabal"
          $ ["v1-install", "-w", commitGhcPath commit, "--allow-newer"]
          ++ nofibDeps

nofibRun :: Task
nofibRun = requires [nofibBoot] $ task "run-nofib" run
  where
    run commit = do
      withCommitResultFile commit "run-nofib.log" $ \hdl -> do
        runProcess_
          $ setWorkingDir (commitWorkDir commit </> "nofib")
          $ setStderr (useHandleOpen hdl)
          $ setStdout (useHandleOpen hdl)
          $ proc "cabal"
          $ [ "new-run", "nofib-run", "--"
            , "--compiler=" <> commitGhcPath commit
            , "--compiler-args=-fproc-alignment=64"
            , "--times=" <> show nofibRepeats
            , "--perf", "--perf-args", "-e " <> intercalate "," perfEvents
            ]

buildPackage :: String   -- ^ package name
             -> FilePath -- ^ source directory
             -> [String] -- ^ @ghc_perf.py@ arguments
             -> [String] -- ^ compiler arguments
             -> Int      -- ^ number of repetitions
             -> Task
buildPackage name srcPath ghcPerfArgs hcArgs repeats =
    requires [build] $ task name' run
  where
    name' = "build-" <> name
    run commit = do
      withCommitResultFile commit (name'<>".log") $ \hdl -> do
        withTempDirectory (commitWorkDir commit) name' $ \tmpDir -> do
          runProcess_
            $ setWorkingDir (commitWorkDir commit </> srcPath)
            $ setStderr (useHandleOpen hdl)
            $ setStdout (useHandleOpen hdl)
            $ proc (ghcUtilsPath <> "ghc_perf.py")
            $ [ "-n", name'
              , "-o", name'<>".json"
              ] ++ ghcPerfArgs ++
              [ commitGhcPath commit
              , "-isrc", "-Iinclude", "-fforce-recomp"
              , "-odir", tmpDir
              , "-hidir", tmpDir
              ] ++ hcArgs

runInCommitDir :: Commit -> ProcessConfig stdin stdout stderr -> IO ()
runInCommitDir commit =
  runProcess_ . setWorkingDir (commitWorkDir commit)

testNofib :: Task
testNofib = requires [nofibBoot, build] $ task "test-nofib" $ \commit -> do
  runInCommitDir commit $ proc "cabal"
    [ "v1-install", "-w", commitGhcPath commit, "" ]

readIfExists :: FilePath -> IO (Maybe String)
readIfExists fname = do
  exists <- doesFileExist fname
  case exists of
    True -> Just <$> readFile fname
    False -> return Nothing

data ExclSem = ExclSem Int TSem

newExclSem :: Int -> IO ExclSem
newExclSem n =
  atomically $ fmap (ExclSem n) $ newTSem (fromIntegral n)

withExclusive :: ExclSem -> IO a -> IO a
withExclusive (ExclSem n sem) = bracket_ take release
  where
    take = atomically $ replicateM n $ waitTSem sem
    release = atomically $ signalTSemN (fromIntegral n) sem

withNonexclusive :: ExclSem -> Int -> IO a -> IO a
withNonexclusive (ExclSem n sem) m action
  | n < m = fail "uh oh"
  | otherwise = bracket_ take release action
  where
    take = atomically $ replicateM m $ waitTSem sem
    release = atomically $ signalTSemN (fromIntegral m) sem

runTasks :: Int -> [Commit] -> [Task] -> IO ()
runTasks cores commits tasks = do
  sem <- newExclSem cores
  asyncs <- M.fromList <$> sequence
    [ do a <- async $ mapM_ (runTask sem commit) tasks
         return (commit, a)
    | commit <- commits
    ]
  excs <- mapM waitCatch asyncs
  forM_ (M.toList excs) $ \(commit, result) ->
    putStrLn $ commitName commit ++ ": " ++ show result

runTask :: ExclSem -> Commit -> Task -> IO ()
runTask sem commit task = withSem $ do
    done <- checkStamp task commit
    unless done $ do
      mapM_ assertStamp (taskDeps task)
      taskRun task commit
      mkStamp task commit
  where
    assertStamp task' = do
      exists <- checkStamp task' commit
      unless exists $ fail
        $ commitName commit <> ": "
          <> taskName task <> " needs " <> taskName task'

    withSem = case taskParallelism task of
                Exclusive -> withExclusive sem
                WantsCores n -> withNonexclusive sem n


stampPath :: Task -> Commit -> FilePath
stampPath task commit =
  commitWorkDir commit </> ".stamp-" <> taskName task

mkStamp :: Task -> Commit -> IO ()
mkStamp task commit = writeFile (stampPath task commit) ""

checkStamp :: Task -> Commit -> IO Bool
checkStamp task commit = doesFileExist (stampPath task commit)

main :: IO ()
main = do
  exists <- doesDirectoryExist "ghc"
  unless exists $ fail "ghc/ should be a GHC checkout"
  commits <- readCommits
  let tasks = [test]
  runTasks 17 commits tasks

