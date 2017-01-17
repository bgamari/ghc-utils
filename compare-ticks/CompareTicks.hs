{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Numeric
import Data.Function (on)
import Data.List (sortBy, transpose, intercalate)
import Data.Monoid
import qualified Options.Applicative as O
import qualified Data.Text.IO as T
import qualified Data.Map as M
import TickyReport

relDelta :: (Real a, Fractional b) => a -> a -> b
relDelta a b
  | a == b    = 0
  | otherwise = (b' - a') / a'
  where
    a' = realToFrac a
    b' = realToFrac b

data DeltaType = Relative | Absolute
data FieldChoice = Alloc | Allocd | Entries deriving Show

calculateDeltas :: (Ord id, Real metric)
                => DeltaType
                -> (a -> metric)
                -> M.Map id a -> M.Map id a -> M.Map id (Double, a, a)
calculateDeltas deltaType measure =
  M.intersectionWith $ \x y -> let !delta = combine x y in (delta, x, y)
  where
    combine = case deltaType of
        Relative -> relDelta `on` measure
        Absolute -> \x y -> realToFrac (measure y - measure x)

tabulateDeltas :: (Ord id, Real metric)
                => DeltaType
                -> (a -> metric)
                -> M.Map id a -> M.Map id a -> [(id, Double, a, a)]
tabulateDeltas deltaType metric as bs =
    -- Sort by delta from greatest to smallest
    sortBy (flip compare `on` (\(_id, delta, _a, _b) -> delta)) 

    -- get rid of nesting we don't need
  . map ( \ (k, (delta, a, b)) -> (k, delta, a, b) )
  . M.toList
  $ calculateDeltas deltaType metric as bs

calculateMissing :: Ord id
                 => (a -> metric)
                 -> M.Map id a -> M.Map id a -> M.Map id (metric, a)
calculateMissing measure xs ys = fmap (\x -> (measure x, x)) $ M.difference xs ys

tabulateMissing :: (Ord id, Ord metric)
                => (a -> metric)
                -> M.Map id a -> M.Map id a -> [(id, a)]
tabulateMissing metric as bs =
  map ( \ (k, (_value, a)) -> (k, a) )
  . sortBy (flip compare `on` (\(_id, (value, _a)) -> value))
  . M.toList
  $ calculateMissing metric as bs

missingTable :: (Ord id, Ord metric, Show metric)
             => String
             -> M.Map id a -> M.Map id a
             -> (String, a -> metric)
             -> [(String, id -> a -> String)]
             -> [Row]
missingTable name as bs (metricName, metric) cols = header ++ dataRows
  where
    header = [ Cells $ [ metricName++" " ++ name ] ++ map fst cols
             , HeaderSep
             ]

    dataRows =
        map formatRow
        $ tabulateMissing metric as bs

    formatRow (k, a) =
        Cells $ [ show (metric a) ]
                ++ map (\(_,f) -> f k a) cols

deltasTable :: (Ord id, Real metric, Show metric)
            => DeltaType                           -- ^ relative or absolute changes
            -> M.Map id a -> M.Map id a
            -> (String, a -> metric)               -- ^ metric to examine change in
            -> [(String, id -> a -> a -> String)]  -- ^ other columns to include
            -> [Row]
deltasTable deltaType as bs (metricName, metric) cols = header ++ dataRows
  where
    header = [ Cells $ [ case deltaType of
                           Relative -> "% change"
                           Absolute -> "Change"
                       , metricName++" A"
                       , metricName++" B"
                       ] ++ map fst cols
             , HeaderSep
             ]

    dataRows =
        map formatRow
        $ tabulateDeltas deltaType metric as bs

    formatRow (k, delta, a, b) =
        Cells $ [ delta', show (metric a), show (metric b) ]
                ++ map (\(_,f) -> f k a b) cols
      where
        delta'
          | delta == 0 = "-"
          | otherwise  =
            case deltaType of
              Relative -> showSigned' (showFFloat (Just 1)) (delta*100)<>"%"
              Absolute -> showSigned' (showFFloat (Just 1)) delta

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

args :: O.Parser (FieldChoice, DeltaType, FilePath, FilePath)
args =
    (,,,) <$> fieldChoice <*> deltaType <*> tickyProfile <*> tickyProfile
  where
    fieldChoice = O.option (p =<< O.str) ( O.long "field" <> O.short 'f' <> O.metavar "FIELD"
                                <> O.value Alloc <> O.help "Select alloc, allocd, or entries"
                                <> O.completeWith ["alloc", "allocd", "entries"]
                                <> O.showDefaultWith (const "alloc") )
      where p "alloc" = pure Alloc
            p "allocd" = pure Allocd
            p "entries" = pure Entries
            p _ = fail "FIELD must be alloc, allocd, or entries"
    deltaType = O.flag Absolute Relative (O.long "relative" <> O.short 'r' <> O.help "relative changes")
    tickyProfile = O.argument O.str (O.metavar "FILE" <> O.help "ticky profile output")

chosenFieldName :: FieldChoice -> String
chosenFieldName Alloc = "alloc"
chosenFieldName Allocd = "allocd"
chosenFieldName Entries = "entries"

chosenFieldSelector :: FieldChoice -> TickyFrame -> Integer
chosenFieldSelector Alloc = alloc
chosenFieldSelector Allocd = allocd
chosenFieldSelector Entries = entries

main :: IO ()
main = do
    (chosenField, deltaType, fa, fb) <- O.execParser $ O.info (O.helper <*> args) mempty
    a <- parseReport <$> T.readFile fa
    b <- parseReport <$> T.readFile fb
    let tabulate :: [TickyFrame] -> M.Map (String, String) TickyFrame
        tabulate = M.fromList . map (\frame -> ((moduleName $ stgnDefiningModule $ stgName frame, stgnName $ stgName frame), frame))
        a' = tabulate $ frames a
        b' = tabulate $ frames b
        table = deltasTable deltaType a' b' (chosenFieldName chosenField, chosenFieldSelector chosenField)
                   [ ("name", \_k a'' _b -> pprStgName $ stgName a'') ]
        tableA = missingTable "A" a' b' (chosenFieldName chosenField, chosenFieldSelector chosenField)
                   [ ("name", \_k a'' -> pprStgName $ stgName a'') ]
        tableB = missingTable "B" b' a' (chosenFieldName chosenField, chosenFieldSelector chosenField)
                   [ ("name", \_k b'' -> pprStgName $ stgName b'') ]
    putStrLn $ formatTable table
    putStrLn $ formatTable tableA
    putStrLn $ formatTable tableB
