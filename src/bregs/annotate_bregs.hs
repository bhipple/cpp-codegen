#!/usr/bin/runhaskell
-- Utility to update BREG number comments on #includes

{-# LANGUAGE OverloadedStrings #-}
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B

includeToHeader :: B.ByteString -> B.ByteString
includeToHeader ln = acc
    where parts = B.words ln
          header = parts !! 1
          acc = B.tail . B.init $ header

formatFile :: FilePath -> IO ()
formatFile f = do
    contents <- B.readFile f
    let lns = B.lines contents
    let includes = filter (B.isPrefixOf "#include <bbit/") lns
    let headers = map includeToHeader includes
    mapM_ B.putStrLn headers

main = do
    args <- getArgs
    mapM_ formatFile args
