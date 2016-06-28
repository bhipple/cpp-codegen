{-# LANGUAGE OverloadedStrings #-}
module Gencpp ( genpair ) where

import System.Environment (getArgs)
import System.FilePath.Posix
import Data.Text hiding (head, drop)
import Turtle

main = do
    args <- getArgs
    let namespace = head args
    let files = drop 1 args
    --mapM_ (genpair namespace) files
    putStrLn "Done"

genpair :: Text -> Text -> IO ()
genpair ns fn = undefined
