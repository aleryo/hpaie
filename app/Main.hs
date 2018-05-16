module Main where

import           Data.Text
import           Run
import           System.Environment
import           System.Process

main :: IO ()
main = do
  (inputFile:outputFile:keys) <- getArgs
  comptaAnalytique inputFile outputFile (fmap pack keys)
  callProcess "hledger" [ "-f", outputFile, "bal" ]
