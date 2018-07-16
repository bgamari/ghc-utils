#! /usr/bin/env runghc

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Monoid
import System.Environment
import System.Exit
import System.IO

-- | These are packages which are sometimes installed in the global package
-- database (e.g. during @validate@) but aren't actually boot packages.
nonBootPackages :: [String]
nonBootPackages = [ "parallel" ]

main :: IO ()
main = do
    fns <- getArgs

    when (null fns) $ do
        hPutStrLn stderr "usage: ./pack_pkg_list.hs <files with 'ghc-pkg list --global' output>..."
        exitFailure

    forM_ fns $ \fn -> do
        c <- (words . map normWS) <$> readFile fn

        unless (not (null c)
                && (("package.conf.d:" `isSuffixOf` head c) || ("package.conf.d" `isSuffixOf` head c))
                && not (any ("package.conf.d" `isInfixOf`) (tail c))) $
            fail ("invalid file " ++ show fn)

        let c' = sort (map parsePkgName $ tail c)
            Just ghcver = lookup "ghc" (map fst c')

        let ln = ghcver <> "\t" <> unwords [ n<>"/"<>v<>(if h then "*" else "")
                                           | ((n,v),h) <- c'
                                           , not $ n `elem` nonBootPackages
                                           ]
        putStrLn ln

    return ()
  where
    normWS c | isSpace c = ' '
             | otherwise = c

    parsePkgName n | "(" `isPrefixOf` n
                   , ")" `isSuffixOf` n = (splitPkg (init (tail n)), True)
                   | otherwise = (splitPkg n, False)

    splitPkg pn | head n == '-'  = (reverse $ tail n,reverse v)
                | otherwise = error "splitPkg: internal error"
      where
        (v,n) = break (=='-') . reverse $ pn
