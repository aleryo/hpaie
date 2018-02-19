module PaieSpec where

import Test.Hspec
import Data.Text
import Data.Time.Calendar
import Data.Ratio

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
  where cotisation = fromRational tauxMaladiePatronal * montant

tauxMaladiePatronal :: Rational
tauxMaladiePatronal = 13 % 100

tauxSecuDeplafonnée :: Rational
tauxSecuDeplafonnée = 4 % 1000

trancheA :: Montant
trancheA = 3311

tauxSecuPlafonnée :: Rational
tauxSecuPlafonnée = 69 % 1000

tauxComplémentaireTrancheA :: Rational
tauxComplémentaireTrancheA = 403 % 10000

cotisationSalariale :: Libelle ->  Date -> Montant -> Transaction
cotisationSalariale libelle date cotisation = 
  Tx libelle date [ Debit  "421100:TOTO"   cotisation
                  , Credit "431100:URSSAF" cotisation
                  ]

cotisationSecuDeplafonnee' montant = arrondi $ fromRational tauxSecuDeplafonnée * montant

cotisationSecuDeplafonnee :: Date -> Montant -> Transaction
cotisationSecuDeplafonnee date montant =
  cotisationSalariale "Sécurité sociale déplafonnée" date cotisation
  where cotisation = cotisationSecuDeplafonnee' montant

cotisationSecuPlafonneeSalariale' montant = arrondi $ fromRational tauxSecuPlafonnée * min montant trancheA

cotisationSecuPlafonneeSalariale :: Date -> Montant -> Transaction
cotisationSecuPlafonneeSalariale date montant =
  cotisationSalariale "Sécurité sociale plafonnée" date cotisation
  where cotisation = cotisationSecuPlafonneeSalariale'  montant

complementaireTrancheA' montant = arrondi $ fromRational tauxComplémentaireTrancheA * min montant trancheA

complementaireTrancheA :: Date -> Montant -> Transaction
complementaireTrancheA date montant =
  cotisationSalariale "Complémentaire tranche A" date cotisation
  where cotisation = complementaireTrancheA' montant

arrondi :: Montant -> Montant
arrondi m = (fromIntegral $ round $ m * 100) / 100

salaireBrut :: Montant -> Montant
salaireBrut net = secant 0.001 (\ b -> salaireNet b - net) net (2 * net)
  where
    salaireNet brut = brut - cotisationsSalariales brut

    cotisationsSalariales brut =
      cotisationSecuPlafonneeSalariale' brut +
      cotisationSecuDeplafonnee' brut +
      complementaireTrancheA' brut
      
    
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
    arrondi (salaireBrut 1000) `shouldBe` 1127.78

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

    it "calcule la sécurité sociale plafonnée" $ do
      cotisationSecuPlafonneeSalariale aujourd'hui 1000 `shouldBe`
        Tx "Sécurité sociale plafonnée" aujourd'hui
          [ Debit  "421100:TOTO"   69.00
          , Credit "431100:URSSAF" 69.00
          ]

      let montant = arrondi $ trancheA * fromRational tauxSecuPlafonnée
      
      cotisationSecuPlafonneeSalariale aujourd'hui (trancheA + 100) `shouldBe`
        Tx "Sécurité sociale plafonnée" aujourd'hui
          [ Debit  "421100:TOTO"   montant
          , Credit "431100:URSSAF" montant
          ]

    it "calcule la complémentaire tranche A" $ do
      complementaireTrancheA aujourd'hui 1000 `shouldBe`
        Tx "Complémentaire tranche A" aujourd'hui
          [ Debit  "421100:TOTO"   40.30
          , Credit "431100:URSSAF" 40.30
          ]

      let montant = arrondi $ trancheA * fromRational tauxComplémentaireTrancheA

      complementaireTrancheA aujourd'hui (trancheA + 100) `shouldBe`
        Tx "Complémentaire tranche A" aujourd'hui
          [ Debit  "421100:TOTO"   montant
          , Credit "431100:URSSAF" montant
          ]

