{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Entry where

import           Data.Csv
import           Data.Text          as Text
import           Data.Time.Calendar (Day (..))
import           Date               ()
import           GHC.Generics
import           Montant

data Entry (cur :: Currency) = Entry
    { date    :: Day
    , compte  :: Text
    , libelle :: Text
    , sens    :: Sens
    , montant :: Montant cur
    , keys    :: Keys
    }
    deriving (Eq, Show, Generic)

instance FromNamedRecord (Entry a) where
  parseNamedRecord r = Entry <$> r .: "Date" <*> r .: "compte" <*> r .: "libelle" <*> r .: "sens" <*> r .: "montant" <*> r .: "keys"

instance ToNamedRecord (Entry a) where
  toNamedRecord Entry{..} =
    namedRecord [ "Date" .= date
                , "compte" .= compte
                , "libelle" .= libelle
                , "sens" .= sens
                , "montant" .= montant
                , "keys" .= keys
                ]

instance DefaultOrdered (Entry a) where
  headerOrder _ = header [ "Date", "compte", "libelle", "sens", "montant", "keys" ]

type Keys = [ Text ]

arnaud,anna :: Text
arnaud  = "801000:Arnaud"
anna    = "802000:Anna"

instance FromField Keys where
  parseField "A"        = pure [ arnaud ]
  parseField "Arnaud"   = pure [ arnaud ]
  parseField "Anna"     = pure [ anna ]
  parseField _          = pure [ arnaud, anna ]

instance ToField Keys where
  toField ["801000:Arnaud"]  = "Arnaud"
  toField ["802000:Anna"]    = "Anna"
  toField _                  = "ALL"
