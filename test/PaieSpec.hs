module PaieSpec where

import Test.Hspec
import Data.Text
import Data.Time.Calendar

type Date = Day
type Compte  = Text
type Libelle = Text
type Montant = Double

data Ecriture = Debit  Compte Montant
              | Credit Compte Montant
              deriving (Eq, Show)

data Transaction = Tx Libelle Date [ Ecriture ]
                 deriving (Eq, Show)


date :: Integer -> Int -> Int -> Date
date = fromGregorian

paie :: Date -> Montant -> [ Transaction ]
paie jourDePaie montantNet =
  [ Tx "Charges patronales" jourDePaie
    [ Debit  "100" montantNet
    , Credit "200" montantNet
    ]
  ]


cotisationMaladie :: Date -> Montant -> Transaction
cotisationMaladie date montant =
  Tx "Maladie" date [ Debit  "645100:URSSAF" cotisation
                    , Credit "431100:URSSAF" cotisation
                    ]
  where cotisation = 0.13 * montant


spec :: Spec
spec = describe "Générateur de paie" $ do

  it "génère la paie"  $ do
    paie (date 2018 02 12) 1000 `shouldBe`
      [ Tx "Charges patronales" (date 2018 02 12)
        [ Debit  "100" 1000.00
        , Credit "200" 1000.00
        ]
      ]

  describe "Charges patronales" $ do

    it "calcul les charges maladies" $ do
     cotisationMaladie (date 2018 02 12) 1000 `shouldBe`
      Tx "Maladie" (date 2018 02 12)
        [ Debit  "645100:URSSAF" 130.00
        , Credit "431100:URSSAF" 130.00
        ]
