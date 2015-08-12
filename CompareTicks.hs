{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Numeric
import Text.Trifecta
import Data.List (sortBy, transpose, intercalate)
import Data.Char (isSpace)
import Data.Monoid
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S

data TickyReport = TickyReport { frames :: [TickyFrame] }
                   deriving (Show)

data TickyFrame = TickyFrame { entries   :: Integer
                             , alloc     :: Integer
                             , allocd    :: Integer
                             , arguments :: String
                             , stgName   :: StgName
                             }
                  deriving (Show)


-- | The name of a module
data ModuleName = ModName { modulePackage :: String
                          , moduleName    :: String
                          }
                deriving (Show)

pprModuleName :: ModuleName -> String
pprModuleName (ModName pkg name) = pkg<>":"<>name

-- | An STG name
--
-- An exported name like @ghc-7.11:CmdLineParser.runCmdLine{v rWL}@ is,
--
-- @
-- StgName { definingModule = Just $ ModName (Just "ghc-7.11") "CmdLineParser"
--         , name = "runCmdLine"
--         , signature = "v rWL"
--         , parent = Nothing
-- @
--
-- A non-exported name like @sat_s5EP{v} (ghc-7.11:CmdLineParser) in rXu@ is,
--
-- @
-- StgName { definingModule = Nothing
--         , name = "sat_s5EP"
--         , signature = "v"
--         , parent = "rXu"
--         }
-- @
data StgName = StgName { stgnExported       :: Bool
                       , stgnDefiningModule :: ModuleName
                       , _stgnName          :: String
                       , stgnSignature      :: String
                       , stgnParent         :: Maybe String
                       }
             | Top
             deriving (Show)

stgnName :: StgName -> String
stgnName (StgName {_stgnName=n}) = n
stgnName Top = "Top"

pprStgName :: StgName -> String
pprStgName Top = "Top"
pprStgName s@(StgName exported mod name sig parent) =
    name<>" ("<>pprModuleName mod<>")"

parseReport :: T.Text -> TickyReport
parseReport s =
    let ls = T.lines s
        _:tableLines = dropWhile (\l -> not $ "---------------------" `T.isPrefixOf` l) ls
        parseIt s = case parseString parseFrame mempty (T.unpack s) of
          Success a -> a
          Failure doc -> error $ show doc
    in TickyReport $ map parseIt
                   $ filter (not . T.null)
                   $ takeWhile (\l -> not $ "*****************" `T.isPrefixOf` l)
                   $ tableLines

spacesThen :: Parser a -> Parser a
spacesThen parser = skipMany space *> parser

named :: String -> Parser a -> Parser a
named n p = p <?> n

parseFrame :: Parser TickyFrame
parseFrame =
    TickyFrame
     <$> spacesThen integer
     <*> spacesThen integer
     <*> spacesThen integer
     <*> spacesThen (do n <- integer
                        count (fromIntegral n) anyChar)
     <*> spacesThen ((text "TOP" *> pure Top) <|> try nonExportedName <|> exportedName)
     <*  eof
  where
    funcName = many $ alphaNum <|> oneOf "$=<>[]()+-,.#*|/_'"
    sig = named "signature" $ braces $ many $ noneOf "}"

    -- e.g. ghc-7.11:CmdLineParser.runCmdLine{v rWL}
    exportedName :: Parser StgName
    exportedName = named "exported name" $ do
        mod <- parseModuleName
        _name <- funcName
        _sig <- sig
        return $ StgName True mod _name _sig Nothing

    -- e.g. sat_sbMg{v} (main@main:Main) in sbMh
    nonExportedName = named "non-exported name" $ do
        --_name <- funcName
        _name <- many $ noneOf "{"
        _sig <- sig
        spaces
        mod <- parens parseModuleName
        parent <- optional $ text "in" *> spaces *> funcName
        return $ StgName False mod _name _sig parent

type QualifiedName = (ModuleName, String)

startsWith :: Parser a -> Parser a -> Parser [a]
startsWith first others = (:) <$> first <*> many others

parseModuleName :: Parser ModuleName
parseModuleName = named "module name" $ do
    package <- packageName
    optional $ char '@' >> packageName  -- what is this?
    char ':'
    name <- startsWith upper $ alphaNum <|> oneOf "_."
    return $ ModName package name
  where packageName = many $ alphaNum <|> oneOf "-_."

delta :: Real a => a -> a -> Double
delta a b = (b' - a') / a'
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
        Cells $ [ showSigned' (showFFloat (Just 1)) (delta*100)<>"%", showMetric as, showMetric bs ]
                ++ map (\(_,f) -> f k (as M.! k) (bs M.! k)) cols
      where
        showMetric xs = show $ metric $ xs M.! k
        showSigned' :: Real a => (a -> ShowS) -> a -> String
        showSigned' showIt x = sign <> showIt x ""
          where sign = if x > 0 then "+" else "-"

test = do
    a <- parseReport <$> T.readFile "ghc-master/ticks"
    b <- parseReport <$> T.readFile "ghc-compare/ticks"
    let tabulate = M.fromList . map (\frame -> (stgnName $ stgName frame, frame))
        a' = tabulate $ frames a
        b' = tabulate $ frames b
    return $ deltasTable a' b' ("alloc", alloc)
             [ ("name", \k a b->pprStgName $ stgName a)
             ]


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
