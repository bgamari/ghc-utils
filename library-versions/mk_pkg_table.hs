#!/usr/bin/env runghc

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)

parseVer :: String -> Either Version String
parseVer "HEAD" = Right "HEAD"
parseVer s = case [ v | (v,"") <- readP_to_S parseVersion s ] of
    (v:_) -> Left v
    []    -> error ("parseVer: " ++ show s)

showVer :: Either Version String -> String
showVer (Right s) = s
showVer (Left v) = showVersion v

main :: IO ()
main = do
    ls <- map words . normalizeLines . lines <$> getContents

    let entries = sort [ ((parseVer gv,pn),pv) | gv:pnvs <- ls, pnv <- pnvs, let (pn,pv) = fmap tail $ break (=='/') pnv ]
        allpns  = sort $ nub [ pn | ((_,pn),_) <- entries ]
        allgvs  = reverse $ sort $ nub [ gv | ((gv,_),_) <- entries ]

    let hdr = intercalate "||" $ ("" : " " : [ "=  '''" <> showVer v <> "'''  =" | v <- allgvs ]) ++ [""]

    putStrLn $ hdr

    forM_ allpns $ \pn -> do
        let pghcvers = [ (showVer gv,pv) | ((gv,pn'),pv) <- entries, pn' == pn ]

        let tmp = intercalate "||" $ do
                (v,v_next) <- zip (map showVer allgvs) (tail (map showVer allgvs) ++ [""])
                let pver      = lookup v pghcvers
                    pver_next = lookup v_next pghcvers
                return $case pver of
                    _ | v_next /= "" && pver == pver_next -> "" -- collapse
                    Nothing -> "  ''none''  "
                    Just pver' -> "  " <> pver' <> "  "

        -- print (pn, pghcvers)
        putStrLn $ "||=`" <> pn <> "` =||" <> tmp <> "||"

    -- repeat header
    putStrLn $ hdr

    return ()
  where
    normalizeLines = filter (not . null) . map normLine
    normLine = dropWhile isSpace . fst . span (/='#')
