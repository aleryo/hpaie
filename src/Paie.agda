module Paie where

open import Data.Nat.Base as ℕ
  using (ℕ) renaming (_+_ to _ℕ+_)
open import Data.Integer renaming (_+_ to _ℤ+_)
open import Data.Vec
open import Relation.Binary.PropositionalEquality

record Posting : Set where
  field
    tx_date : ℤ
    tx_amount : ℤ
    tx_libelle : ℤ

balanceOf : ∀ {n} . Vec Posting n -> ℤ
balanceOf postings =
  let z : ℤ
      z = + 0
  in foldr (λ acc p → (Posting.tx_amount p) ℤ+ acc) z postings

-- data Transaction : Set where
--   Tx : ∀ {n} . (postings : Vec Posting n)
--     → { prf : balanceOf postings ≡ 0 } -> Transaction
