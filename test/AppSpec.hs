module AppSpec where

import           Data.Maybe
import           Data.Text    as Text
import           Data.Text.IO as Text
import           Date
import           Entry
import           Helper
import           Montant
import           Run
import           Test.Hspec
import           Transaction

sample1CSV :: Text
sample1CSV = Text.unlines $ fmap (intercalate "\t")
  [ [ "Date", "compte", "libelle", "sens", "montant", "keys" ]
  , [ "14/05/2018", "612000:Fournisseur-KPMG", "Frais tenu de comptes", "D", "120,00", "" ]
  ]

sample1Ledger :: Text
sample1Ledger = Text.unlines
  [ "2018-05-14 Frais tenu de comptes"
  , "    612000:Fournisseur-KPMG                            120.00"
  , "    801000:Arnaud                                      -60.00"
  , "    802000:Anna                                        -60.00"
  ]


spec :: Spec
spec = before (Text.writeFile "sample1.csv" sample1CSV ) $ describe "Application" $ do

  describe "lit un fichier CSV et produit un fichier ledger" $ do

    it "utilise une clé de répartition équitable" $ do
      comptaAnalytique "sample1.csv" "sample1.ledger"

      fileShouldExist "sample1.ledger"
      "sample1.ledger" `fileContains` sample1Ledger

    it "parse le fichier CSV" $ do
      parsed <- parseCSV "sample1.csv"
      parsed `shouldBe` [ Entry (fromJust $ isoDate "2018-05-14") "612000:Fournisseur-KPMG" "Frais tenu de comptes" Debit (12000) ["801000:Arnaud", "802000:Anna" ] ]

    it "répartit une Entry équitablement quand il génère une transaction" $
      generateTransaction
        (Entry (fromJust $ isoDate "2018-05-14")
               "612000:Fournisseur-KPMG" "Frais tenu de comptes"
               Debit (12000)
               [ "801000:Arnaud", "802000:Fred" ])
      `shouldBe`
      Transaction (fromJust $ isoDate "2018-05-14") "Frais tenu de comptes"
      [ Posting "612000:Fournisseur-KPMG"   Debit  (12000)
      , Posting "801000:Arnaud" Credit (6000)
      , Posting "802000:Fred"   Credit (6000)
      ]

    it "répartit le reste d'une division non entière quand il génère une transaction" $
      generateTransaction
      (Entry (fromJust $ isoDate "2018-05-14") "612000:KPMG" "Frais tenu de comptes" Debit (13000) ["801000:Arnaud", "802000:Fred", "803000:Bernard" ])
      `shouldBe`
      Transaction (fromJust $ isoDate "2018-05-14") "Frais tenu de comptes"
      [ Posting "612000:KPMG"   Debit  (13000)
      , Posting "801000:Arnaud" Credit (4334)
      , Posting "802000:Fred"   Credit (4333)
      , Posting "803000:Bernard"   Credit (4333)
      ]

    it "formate une transaction au format ledger" $ do
      render (Transaction (fromJust $ isoDate "2018-05-14") "Frais tenu de comptes"
               [ Posting "612000:Fournisseur-KPMG"   Debit  (12000)
               , Posting "801000:Arnaud"  Credit (6000)
               , Posting "802000:Anna"    Credit (6000)
               ]) <> "\n"
        `shouldBe` sample1Ledger

    it "transforme une liste d'Entry en ledger" $ do
      generateLedger "sample2.ledger" [ Entry (fromJust $ isoDate "2018-05-14") "612000:Fournisseur-KPMG" "Frais tenu de comptes" Debit (12000) [ "801000:Arnaud", "802000:Anna" ] ]

      "sample2.ledger" `fileContains` sample1Ledger
