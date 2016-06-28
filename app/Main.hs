{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Turtle
import Codegen
import Data.Text

type Namespace = Text

data Command = Test FilePath
             | Cpp Namespace FilePath

parser :: Parser Command
parser =
      Test <$> (subcommand "test" "Generate a Google Test skeleton for file"
                (argPath "cppFile" "File to generate test file for"))
  <|> undefined
  -- <|> Cpp <$> (subcommand "cpp" "Generate a cpp|h pair"
                --(argText "namespace" "cpp|h namespace" <*> argPath "fname" "cpp|h Filename"))

main = do
    cmd <- options "Scripts for generating cpp skeletons" parser
    genFile cmd

genFile :: Command -> IO ()
genFile (Test cpp) = genTestFile . unpack . format fp $ cpp
genFile (Cpp ns fp) = undefined
