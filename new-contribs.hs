#!/usr/bin/env runghc

import Data.Tuple
import Data.List
import System.Process
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import System.Environment

args = []

main = do
    mapM_ getNew [1..10]

getCommitAuthors :: [String] -> IO (M.Map String Int)
getCommitAuthors args = do
    authors <- lines <$> readProcess "git" (["log", "--format=%aN <%aE>"]++args) ""
    return $ M.fromListWith (+) [ (a, 1) | a <- authors ]

getNew :: Int -> IO (M.Map String Int)
getNew year' = do
    let year = show year'++" year ago"
    let nextYear = show (year'-1)++" year ago"
    after <- getCommitAuthors (args++["--after="++year, "--before="++nextYear])
    before <- getCommitAuthors (args++["--before="++year])
    let new = after `M.difference` before
    putStrLn $ unlines [ show n ++ "\t" ++ name | (n, name) <- sort $ map swap $ M.toList new ]
    putStrLn $ unwords [ show year, show $ M.size before, show $ M.size new ]
    return new
