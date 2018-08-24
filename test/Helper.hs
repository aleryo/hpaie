module Helper where

import           Data.Text        as Text
import           Data.Text.IO     as Text
import           System.Directory (doesFileExist)
import           Test.Hspec
import           Test.HUnit

fileShouldExist :: FilePath -> Expectation
fileShouldExist fp = do
  exist <- doesFileExist fp
  if exist
    then pure ()
    else assertFailure $ "file " <> fp <> " does not exist"

fileContains :: FilePath -> Text -> Expectation
fileContains fp expected = do
  content <- Text.readFile fp
  content `shouldBe` expected
