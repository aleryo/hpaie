module Main where

import           Data.Text
import           Run
import           System.Environment
import           System.Process

main :: IO ()
main = do
  (inputFile:outputFile:[]) <- getArgs
  comptaAnalytique inputFile outputFile
  callProcess "hledger" [ "-f", outputFile, "bal" ]
