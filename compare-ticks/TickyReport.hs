{-# LANGUAGE OverloadedStrings #-}

module TickyReport where

import Text.Trifecta
import Control.Monad (void)
import Control.Applicative
import Data.Monoid
import qualified Data.Text as T

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
--         }
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
                       , _stgnDefiningModule :: ModuleName
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

stgnDefiningModule :: StgName -> ModuleName
stgnDefiningModule Top = ModName "" ""
stgnDefiningModule s@(StgName _ mod _ _ _) = mod

parseReport :: T.Text -> TickyReport
parseReport s =
    let ls = T.lines s
        tableLines = case dropWhile (\l -> not $ "---------------------" `T.isPrefixOf` l) ls of
                       _:xs -> xs
                       _    -> error "parseReport: Parse error: Failed to find beginning of report"
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
    funcName = many $ alphaNum <|> oneOf "$=<>[]()+-,.#*|/_'!@"
    sig = named "signature" $ braces $ many $ noneOf "}"

    -- e.g. ghc-7.11:CmdLineParser.runCmdLine{v rWL}
    exportedName :: Parser StgName
    exportedName = named "exported name" $ do
        mod <- parseModuleName
        _name <- funcName
        _sig <- sig
        spaces
        optional $ text "(fun)" <|> text "(fun,se)"
        return $ StgName True mod _name _sig Nothing

    -- e.g. sat_sbMg{v} (main@main:Main) in sbMh
    nonExportedName = named "non-exported name" $ do
        --_name <- funcName
        _name <- many $ noneOf "{"
        _sig <- sig
        spaces
        mod <- parens parseModuleName
        spaces
        optional $ text "(fun)" <|> text "(fun,se)"
        spaces
        parent <- optional $ text "in" *> spaces *> funcName
        return $ StgName False mod _name _sig parent

type QualifiedName = (ModuleName, String)

startsWith :: Parser a -> Parser a -> Parser [a]
startsWith first others = (:) <$> first <*> many others

parseModuleName :: Parser ModuleName
parseModuleName = named "module name" $ do
    package <- packageName
    optional $ char '@' >> packageName  -- what is this?
    void (text "::") <|> void (char ':') -- not sure what :: is all about
    name <- startsWith upper $ alphaNum <|> oneOf "_."
    return $ ModName package name
  where packageName = many $ alphaNum <|> oneOf "-_."
