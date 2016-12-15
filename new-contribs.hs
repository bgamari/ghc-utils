import System.Process
import qualified Data.Set as S
import System.Environment

args = []

main = do
    mapM_ getNew [1..10]

getNew :: Int -> IO (S.Set String)
getNew year' = do
    let year = show year'++" year ago"
    let nextYear = show (year'-1)++" year ago"
    after <- S.fromList . lines <$> readProcess "git" (args++["log", "--format=%aN", "--after="++year, "--before="++nextYear]) ""
    before <- S.fromList . lines <$> readProcess "git" (args++["log", "--format=%aN", "--before="++year]) ""
    let new = after `S.difference` before
    --print new
    putStrLn $ unwords [ show year, show $ S.size before, show $ S.size new ]
    return new
