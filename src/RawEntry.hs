{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module RawEntry where

import           Data.Csv
import           Data.Text
import           Data.Time.Calendar (Day (..))
import           GHC.Generics
import           Montant

data RawEntry (cur :: Currency) =
  RawEntry { compte     :: Text
           , journal    :: Text
           , date       :: Day
           , piece      :: Text
           , libelle    :: Text
           , refLibelle :: Text
           , reference  :: Text
           , debit      :: Montant cur
           , credit     :: Montant cur
           , solde      :: Montant cur
           }
  deriving (Eq,Show,Generic)


instance FromNamedRecord (RawEntry a) where
  parseNamedRecord r = RawEntry
                       <$> r .: "Compte"
                       <*> r .: "Journal"
                       <*> r .: "Date"
                       <*> r .: "Piece"
                       <*> r .: "Libelle"
                       <*> r .: "RefLibelle"
                       <*> r .: "Reference"
                       <*> r .: "Debit"
                       <*> r .: "Credit"
                       <*> r .: "Solde"
