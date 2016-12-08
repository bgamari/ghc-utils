{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.Foldable
import Data.Maybe
import System.Process
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map.Strict as M

type Commit = TL.Text
type Sha = TL.Text

parseLines :: TL.Text -> [(Commit, TL.Text)]
parseLines = map f . TL.lines
  where f = TL.span (not . isSpace)

resolveCommits :: [Commit] -> IO (M.Map Commit Sha)
resolveCommits commits =
    M.fromList . flip zip commits . map TL.pack . lines <$> readProcess "git" ("rev-parse" : map TL.unpack commits) ""

main :: IO ()
main = do
    commits <- M.fromList . parseLines <$> TL.getContents
    shas <- resolveCommits $ M.keys commits
    (_, Just hout, _, _) <- createProcess (proc "git" ["rev-list", "--topo-order", "--all"]){ std_out = CreatePipe }
    sorted <- TL.hGetContents hout
    let sorted_shas = mapMaybe (\sha -> fmap (sha,) $ M.lookup sha shas) $ TL.lines sorted
    mapM_ (\(sha,commit) -> TL.putStrLn $ TL.intercalate "\t" [sha, commit, commits M.! commit]) sorted_shas
