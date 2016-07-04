{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Numeric
import Data.List (sortBy, transpose, intercalate)
import Data.Monoid
import qualified Options.Applicative as O
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S
import TickyReport

delta :: Real a => a -> a -> Double
delta a b
  | a == b    = 0
  | otherwise = (b' - a') / a'
  where
    a' = realToFrac a
    b' = realToFrac b

tabulateDeltas :: (Ord id, Real metric)
               => M.Map id a -> M.Map id a -> (a -> metric) -> [(Double, id)]
tabulateDeltas as bs = \f -> map (\k->(delta (f $ as M.! k) (f $ bs M.! k), k))
                    $ S.toList keys
    where keys = M.keysSet as `S.intersection` M.keysSet bs

deltasTable :: (Ord id, Real metric, Show metric)
            => M.Map id a -> M.Map id a
            -> (String, a -> metric)                       -- ^ Metric to examine change in
            -> [(String, id -> a -> a -> String)]  -- ^ Other columns
            -> [Row]
deltasTable as bs (metricName, metric) cols = header ++ dataRows
  where
    header = [ Cells $ ["% change", metricName++" A", metricName++" B"]++map fst cols
             , HeaderSep
             ]
    dataRows =
        map formatRow
        $ sortBy (flip compare)
        $ tabulateDeltas as bs metric
    formatRow (delta, k) =
        Cells $ [ delta', showMetric as, showMetric bs ]
                ++ map (\(_,f) -> f k (as M.! k) (bs M.! k)) cols
      where
        delta'
          | delta == 0 = "-"
          | otherwise  = showSigned' (showFFloat (Just 1)) (delta*100)<>"%"
        showMetric xs = show $ metric $ xs M.! k
        showSigned' :: Real a => (a -> ShowS) -> a -> String
        showSigned' showIt x = sign <> showIt x ""
          where sign = if x > 0 then "+" else "-"

data Row = HeaderSep
         | Cells [String]
         deriving (Show)

formatTable :: [Row] -> String
formatTable [] = ""
formatTable unpaddedRows = unlines $ map formatRow rows
  where
    -- Surround with spaces
    rows = map (\case Cells cells -> Cells $ map (\s->" "++s++" ") cells
                      HeaderSep   -> HeaderSep
               ) unpaddedRows

    cellWidths :: Row -> [Int]
    cellWidths (Cells cells) = map length cells
    cellWidths HeaderSep     = replicate nCols 0

    colWidths = map maximum $ transpose $ map cellWidths rows
    nCols = maximum $ map (\case Cells cells -> length cells
                                 HeaderSep   -> 0
                          ) rows

    formatRow :: Row -> String
    formatRow HeaderSep =
      formatRow $ Cells $ map (\width -> replicate width '-') colWidths
    formatRow (Cells cells) =
      colSep ++ intercalate colSep (zipWith pad colWidths cells) ++ colSep

    pad :: Int -> String -> String
    pad n s = take n $ s ++ repeat ' '


    colSep = "|"

args :: O.Parser (FilePath, FilePath)
args =
    (,) <$> tickyProfile <*> tickyProfile
  where
    tickyProfile = O.argument O.str (O.metavar "FILE" <> O.help "ticky profile output")

main :: IO ()
main = do
    (fa, fb) <- O.execParser $ O.info (O.helper <*> args) mempty
    a <- parseReport <$> T.readFile fa
    b <- parseReport <$> T.readFile fb
    let tabulate :: [TickyFrame] -> M.Map (String, String) TickyFrame
        tabulate = M.fromList . map (\frame -> ((moduleName $ stgnDefiningModule $ stgName frame, stgnName $ stgName frame), frame))
        a' = tabulate $ frames a
        b' = tabulate $ frames b
        table = deltasTable a' b' ("alloc", alloc) [ ("name", \k a b->pprStgName $ stgName a) ]
    putStrLn $ formatTable table
