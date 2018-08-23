{-# LANGUAGE OverloadedStrings #-}
module AssignSpec where

import           Assign
import           Data.Text    as Text
import           Data.Text.IO as Text
import           Helper
import           Test.Hspec

-- raw input from KPMG export
sample1CSV :: Text
sample1CSV = Text.unlines $ fmap (intercalate "\t")
  [ [ "Compte", "Journal",  "Date",  "Piece",  "Libelle",  "RefLibelle",  "Reference", "Debit", "Credit", "Solde"]
  , [ "10100000" , "BQ", "13/02/2018", "40", "CLOTURE COMPTE CAPITAL", "Capital", "VIRT", "0" ,"4000" , "-4000"]
  , [ "40110000",  "ACH",  "31/10/2017","",   "Mois Octobre 2017", "Fournisseurs",  "",  "0", "24,71", "-24,71" ]
  ]


sampleOut :: Text
sampleOut = Text.unlines $ fmap (intercalate "\t")
  [ [ "Date", "compte", "libelle", "sens",  "montant",  "keys" ]
  , [ "13/02/2018", "10100000:Capital" , "CLOTURE COMPTE CAPITAL", "C",  "4000,00", "ALL" ]
  , [ "31/10/2017", "40110000:Fournisseurs", "Mois Octobre 2017", "C", "24,71", "ALL" ]
  ]

spec :: Spec
spec = before (Text.writeFile "rawSample.tsv" sample1CSV ) $ describe "Assignment" $ do

  describe "read raw TSV export and rules file and generate " $ do

    it "utilise une clé de répartition équitable" $ do
      assignKeys "rawsample.tsv" "rules" "sample1.tsv"

      fileShouldExist "sample1.tsv"
      "sample1.tsv" `fileContains` sampleOut
