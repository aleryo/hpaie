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

cotisationSecuDeplafonnee :: Date -> Montant -> Transaction
cotisationSecuDeplafonnee date montant =
  Tx "Sécurité sociale déplafonnée" date [ Debit  "421100:TOTO"   cotisation
                                         , Credit "431100:URSSAF" cotisation
                                         ]
  where cotisation = 0.004 * montant

spec :: Spec
spec = describe "Générateur de paie" $ do
  let
    aujourd'hui = date 2018 02 12

  it "génère la paie"  $ do
    paie (date 2018 02 12) 1000 `shouldBe`
      [ Tx "Charges patronales" (date 2018 02 12)
        [ Debit  "100" 1000.00
        , Credit "200" 1000.00
        ]
      ]

  describe "Charges patronales" $ do

    it "calcule les charges maladies" $ do
     cotisationMaladie aujourd'hui 1000 `shouldBe`
      Tx "Maladie" aujourd'hui
        [ Debit  "645100:URSSAF" 130.00
        , Credit "431100:URSSAF" 130.00
        ]

  describe "Charges salariales" $ do

    it "calcule la sécurité sociale déplafonnée" $ do
      cotisationSecuDeplafonnee aujourd'hui 1000 `shouldBe`
        Tx "Sécurité sociale déplafonnée" aujourd'hui
          [ Debit  "421100:TOTO"   4.00
          , Credit "431100:URSSAF" 4.00
          ]
