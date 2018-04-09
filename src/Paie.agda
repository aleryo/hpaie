module Paie where

open import Data.Nat hiding (_+_)
open import Data.Integer
open import Data.Vec
open import Relation.Binary.PropositionalEquality
open import Function

record Posting : Set where
  field
    tx_date : ℤ
    tx_amount : ℤ
    tx_libelle : ℤ

reductionFun : Posting -> ℤ -> ℤ
reductionFun p acc =
  acc + (Posting.tx_amount p)

balanceOf : ∀ {n} -> Vec Posting n -> ℤ
balanceOf postings =
   foldr (λ _ -> ℤ) reductionFun (+ 0) postings

sample : Vec Posting 2
sample = record { tx_date = + 1; tx_amount = + 2; tx_libelle = + 3} ∷ record { tx_date = + 1; tx_amount = + 4; tx_libelle = + 3} ∷ []

test1 : balanceOf sample ≡ + 6
test1 = refl

--data Transaction : Set where
--   Tx : (postings : List Posting)
--     → { prf : balanceOf postings ≡ + 0 } -> Transaction
