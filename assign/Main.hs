module Main where

import           Assign
import           Data.Text
import           System.Environment
import           System.Process

main :: IO ()
main = do
  (inputFile:keysfile:outputFile:[]) <- getArgs
  assignKeys inputFile keysfile outputFile
