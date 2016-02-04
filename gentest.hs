#!/usr/bin/env runhaskell
import System.Environment (getArgs)
import System.FilePath.Posix
import Data.List (intersperse, intercalate, isPrefixOf)

testName :: FilePath -> FilePath
testName f = takeBaseName f ++ ".t.cpp"

commentLine :: FilePath -> String
commentLine f = prefix ++ ws ++ tag
    where prefix = "// " ++ testName f
          tag = "-*-C++-*-"
          spaceCt = 79 - length prefix - length tag
          ws = replicate spaceCt ' '

centeredTag :: String -> String
centeredTag t = intercalate "\n" [separator, tagLn, separator]
    where separator = "// " ++ replicate 76 '='
          spaceCt = 40 - (length t `div` 2) - 2
          tagLn = "//" ++ replicate spaceCt ' ' ++ t

namespaces :: [String] -> [String]
namespaces lns = filter (/= "BloombergLP") namespaces
    where nsLines = filter (isPrefixOf "namespace ") lns
          nsParts = map words nsLines
          notAnon = filter (\p -> length p == 3) nsParts
          namespaces = map (!! 1) notAnon

genTest :: FilePath -> [String] -> String
genTest src namespaces = intercalate "\n" res
    where nsUsings = map (\n -> "using namespace BloombergLP::" ++ n) namespaces
          res = [commentLine src,
                 "#include <" ++ takeBaseName src ++ ".h>\n",
                 "// Application Includes\n",
                 "// System Includes",
                 "#include <gtest/gtest.h>\n",
                 "using namespace BloombergLP;"
                ] ++ nsUsings ++
                [ "", centeredTag "Tests", "\n"]

genFile :: FilePath -> IO ()
genFile f = do
    contents <- readFile f
    let ns = namespaces (lines contents)
    let testContents = genTest f ns
    let testName = dropExtension f ++ ".t.cpp"
    writeFile testName testContents
    putStrLn $ "Generated file " ++ testName ++ ":\n"
    putStrLn testContents

main = do
    args <- getArgs
    mapM_ genFile args
