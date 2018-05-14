module AppSpec where

import           Data.Maybe
import           Data.Monoid
import           Data.Text        as Text
import           Data.Text.IO     as Text
import           Run
import           System.Directory (doesFileExist)
import           Test.Hspec
import           Test.HUnit

sample1CSV = Text.unlines $ fmap (intercalate ",")
  [ [ "Date", "compte", "libelle", "sens", "montant" ]
  , [ "2018-05-14", "612000:KPMG", "Frais tenu de comptes", "D", "120,00" ]
  ]

sample1Ledger :: Text
sample1Ledger = Text.unlines
  [ "2018/05/14 Frais tenu de comptes"
  , "    612000:KPMG               120.00"
  , "    801000:Arnaud              60.00"
  , "    802000:Fred                60.00"
  ]


spec :: Spec
spec = before (Text.writeFile "sample1.csv" sample1CSV ) $ describe "Application" $ do
  describe "lit un fichier CSV et produit un fichier ledger" $ do
    it "utilise une clé de répartition équitable" $ do
      comptaAnalytique "sample1.csv" "sample1.ledger" [ "Arnaud", "Fred" ]

      fileShouldExist "sample1.ledger"
      "sample1.ledger" `fileContains` sample1Ledger

    it "parse le fichier CSV" $ do
      parsed <- parseCSV "sample1.csv"
      parsed `shouldBe` [ Entry (fromJust $ isoDate "2018-05-14") "612000:KPMG" "Frais tenu de comptes" Debit (EUR 12000) ]

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
