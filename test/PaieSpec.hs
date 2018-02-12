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


arrondi :: Montant -> Montant
arrondi m = (fromIntegral $ round $ m * 100) / 100

salaireBrut :: Montant -> Montant
salaireBrut net = secant 0.001 (\ b -> salaireNet b - net) net (2 * net)
  where
    salaireNet brut = brut * (1 - 0.004)
    secant epsilon f guess1 guess0 = let
      newGuess = guess1 - f guess1 * (guess1 - guess0) / (f guess1 - f guess0)
      err =  abs (newGuess - guess1)
      in if (err < epsilon)
         then newGuess
         else secant epsilon f newGuess guess1 

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

  it "calcule le brut" $ do
    arrondi (salaireBrut 1000) `shouldBe` 1004.02

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
