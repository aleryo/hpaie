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

spec :: Spec
spec = describe "Generateur de paie" $ do

  it "génere la paie"  $ do
    paie (date 2018 02 12) 1000 `shouldBe`
      [ Tx "Charges patronales" (date 2018 02 12)
        [ Debit  "100" 1000.00
        , Credit "200" 1000.00
        ]
      ]
