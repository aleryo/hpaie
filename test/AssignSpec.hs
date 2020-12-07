{-# LANGUAGE OverloadedStrings #-}
module AssignSpec where

import           Assign
import           Data.Maybe
import           Data.Text    as Text
import           Data.Text.IO as Text
import           Date
import           Entry
import           Helper
import           Montant
import           RawEntry
import           Rules
import           Test.Hspec

-- raw input from KPMG export
sample1CSV :: Text
sample1CSV = Text.unlines $ fmap (intercalate "\t")
  [ [ "Compte", "Journal",  "Date",  "Piece",  "Libelle",  "RefLibelle",  "Reference", "Debit", "Credit", "Solde"]
  , [ "10100000" , "BQ", "13/02/2018", "40", "CLOTURE COMPTE CAPITAL", "Capital", "VIRT", "0" ,"4000" , "-4000"]
  , [ "40110000",  "ACH",  "31/10/2017","",   "Mois Octobre 2017", "Fournisseurs",  "",  "0", "24,71", "-24,71" ]
  ]

sample1CSVSemi :: Text
sample1CSVSemi = Text.unlines $ fmap (intercalate ";")
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

sampleRules :: Rules
sampleRules = Rules [Rule "\".*\"" "ALL"]

spec :: Spec
spec = before (Text.writeFile "rawSample.csv" sample1CSVSemi >> Text.writeFile "rawSample.tsv" sample1CSV >> Text.writeFile "rules" (renderRules sampleRules)) $ describe "Assignment" $ do

  it "can generate assigned entries from rules and raw entries" $ do
    assignKeys "rawsample.tsv" (Just "rules") "sample1.tsv" '\t'

    fileShouldExist "sample1.tsv"
    "sample1.tsv" `fileContains` sampleOut

  it "can generate assigned entries from rules and raw entries with semicolon delimiter" $ do
    assignKeys "rawsample.csv" (Just "rules") "sample1.tsv" ';'

    fileShouldExist "sample1.tsv"
    "sample1.tsv" `fileContains` sampleOut

  it "can generate assigned entries without rules file and raw entries" $ do
    assignKeys "rawsample.tsv" Nothing "sample1.tsv" '\t'

    fileShouldExist "sample1.tsv"
    "sample1.tsv" `fileContains` sampleOut

  it "can parse raw entries from TSV" $ do
    parsed <- parseRawInput "rawSample.tsv" '\t'
    parsed `shouldBe` [ RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000)
                      , RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329) ]

  describe "Rules" $ do

    it "can parse rules file" $ do
      parsed <- parseRulesFile "rules"
      parsed `shouldBe` Rules [Rule ".*" "ALL" ]

    it "can parse simple mapping rule" $ do
      parseRules "\"foo.*bar\" -> 801000:Arnaud"
        `shouldBe` Right (Rules [ Rule "foo.*bar" "801000:Arnaud" ])
      parseRules "\"foo.*bar\" -> 801000:Arnaud\n"
        `shouldBe` Right (Rules [ Rule "foo.*bar" "801000:Arnaud" ])
      parseRules "\"foo.*bar\" -> 801000:Arnaud\n"
        `shouldBe` Right (Rules [ Rule "foo.*bar" "801000:Arnaud" ])

    it "can parse several mapping rules" $ do
      parseRules "\".*baz\" -> 802000:Bernard\n\"foo.*bar\" -> 801000:Arnaud"
        `shouldBe` Right (Rules [Rule ".*baz" "802000:Bernard", Rule "foo.*bar" "801000:Arnaud" ])

    describe "Assignment" $ do

      it "assign rule to rawentry" $ do
        assignToEntries
          (Rules [Rule "CLOTURE.*" "ALL", Rule ".*Octobre 2017" "801000:Arnaud" ])
          [ RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000)
          , RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329) ]
          `shouldBe`
          [ (RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000), "ALL")
          , (RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329), "801000:Arnaud")
          ]

      it "assign rule to rawentry based on piece" $ do
        assignToEntries
          (Rules [Rule ".*ARNAUD" "801000:Arnaud" ])
          [ RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000)
          , RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "ARNAUD" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329) ]
          `shouldBe`
          [ (RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000), "ALL")
          , (RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "ARNAUD" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329), "801000:Arnaud")
          ]

      it "assign rule to rawentry based on piece" $ do
        assignToEntries
          (Rules [Rule ".*ARNAUD" "801000:Arnaud" ])
          [ RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000)
          , RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "ARNAUD" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329) ]
          `shouldBe`
          [ (RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000), "ALL")
          , (RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "ARNAUD" "Mois Octobre 2017" "Fournisseurs" "" 0 2471 (-2329), "801000:Arnaud")
          ]

      it "assigns ALL to rawentry based on reference" $ do
        assignToEntries
          (Rules [Rule ".*ARNAUD" "801000:Arnaud" ])
          [ RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000)
          , RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "Fournisseurs" "ARNAUD" 0 2471 (-2329) ]
          `shouldBe`
          [ (RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000), "ALL")
          , (RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "Fournisseurs" "ARNAUD" 0 2471 (-2329), "801000:Arnaud")
          ]

      it "assigns ALL to rawentry based on refLibelle" $ do
        assignToEntries
          (Rules [Rule ".*ARNAUD" "801000:Arnaud" ])
          [ RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000)
          , RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "ARNAUD" "" 0 2471 (-2329) ]
          `shouldBe`
          [ (RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000), "ALL")
          , (RawEntry "40110000" "ACH" (fromJust $ isoDate "2017-10-31") "" "Mois Octobre 2017" "ARNAUD" "" 0 2471 (-2329), "801000:Arnaud")
          ]

    it "transform RawEntry to Entry" $ do
      generateEntry (RawEntry "10100000" "BQ" (fromJust $ isoDate "2018-02-13") "40" "CLOTURE COMPTE CAPITAL" "Capital" "VIRT" 0 400000 (-400000), "ALL")
      `shouldBe` Entry (fromJust $ isoDate "2018-02-13") "10100000:Capital" "CLOTURE COMPTE CAPITAL" Credit 400000 ["ALL"]
