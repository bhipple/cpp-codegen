{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Turtle
import Codegen
import Data.Text

type Namespace = Text

data Command = Test FilePath
             | Cpp Namespace FilePath

parser :: Parser Command
parser = subcommand "test" "Generate a Google Test skeleton for file"
                    (Test <$> argPath "cppFile" "Cpp|h file which needs test file")
    <|> parseCpp

parseCpp :: Parser Command
parseCpp = subcommand "cpp" "Generate a cpp|h pair"
            (Cpp <$> argText "namespace" "cpp|h namespace"
                 <*> argPath "fname" "cpp|h filename")

main = do
    cmd <- options "Scripts for generating cpp skeletons" parser
    genFile cmd

genFile :: Command -> IO ()
genFile (Test cpp) = genTestFile . unpack . format fp $ cpp
genFile (Cpp ns file) = genCppHPair ns file
