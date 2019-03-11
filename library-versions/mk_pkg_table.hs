#!/usr/bin/env runghc

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)

parseVer :: String -> Version
parseVer s = case [ v | (v,"") <- readP_to_S parseVersion s ] of
    (v:_) -> v
    []    -> error ("parseVer: " ++ show s)

newtype GhcRelease = GhcRelease { getGhcRelease :: String }
                   deriving (Show, Eq, Ord)

newtype PackageName = PackageName { getPackageName :: String }
                    deriving (Show, Eq, Ord)

data Visibility = HiddenByDefault | VisibleByDefault
                deriving (Eq, Ord, Show)

parseVersions :: String -> M.Map GhcRelease (M.Map PackageName (Visibility, Version))
parseVersions input = M.unionsWith (<>) $ fmap parseLine ls
  where
    ls = map words $ normalizeLines $ lines input

    parseLine :: [String] -> M.Map GhcRelease (M.Map PackageName (Visibility, Version))
    parseLine (gv:pvs) = M.singleton (GhcRelease gv)
        $ M.fromList [ (PackageName pn, (vis, ver))
                     | pnv <- pvs
                     , let (pn, pv) = fmap tail $ break (=='/') pnv
                           ver = parseVer $ filter (/= '*') pv
                           vis = if last pv == '*' then HiddenByDefault else VisibleByDefault
                     ]
    
    normalizeLines = filter (not . null) . map normLine
      where normLine = dropWhile isSpace . fst . span (/='#')


main :: IO ()
main = do
    vs <- parseVersions <$> getContents

    let allpns :: [PackageName]
        allpns  = S.toAscList $ foldMap M.keysSet vs
    let allgvs :: [GhcRelease]
        allgvs  = S.toDescList $ M.keysSet vs

    let hdr = "<tr><th>" ++ cells ++ "</th></tr>"
          where cells = intercalate "</th> <th>" $ (" " : [ "<b>" <> getGhcRelease v <> "</b>" | v <- allgvs ]) ++ [""]
    putStrLn "<table>"
    putStrLn hdr

    forM_ allpns $ \pn -> do
        let makeRow :: Eq a => [a] -> [(Int, a)]
            makeRow [] = []
            makeRow (x:xs) =
              let (ys, ys') = span (==x) xs
              in (1+length ys, x) : makeRow ys'

        let row :: [(Int, Maybe (Visibility, Version))]
            row = makeRow [ M.lookup gv vs >>= M.lookup pn | gv <- allgvs ]
        let tmp = intercalate " " $ do
                (n, v) <- row
                let text = case v of
                      Nothing -> "<i>none</i>"
                      Just (vis, pver) ->
                        let vis' = case vis of
                                     VisibleByDefault -> ""
                                     HiddenByDefault -> "*"
                        in showVersion pver <> vis'
                    cell = case n of
                      1 -> "<td>" ++ text ++ "</td>"
                      _ -> "<td colspan=\"" ++ show n  ++ "\">" ++ text ++ "</td>"
                return cell

        putStrLn $ "<tr><td><tt>" <> getPackageName pn <> "</tt></td> " <> tmp <> " </tr>"

    -- repeat header
    putStrLn hdr
    putStrLn "</table>"

    return ()

