{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Entry where

import           Data.Csv
import           Data.Text          as Text
import           Data.Time.Calendar (Day (..))
import           Date               ()
import           GHC.Generics
import           Montant

data Entry (cur :: Currency) =
  Entry { date    :: Day
        , compte  :: Text
        , libelle :: Text
        , sens    :: Sens
        , montant :: Montant cur
        , keys    :: Keys
        }
  deriving (Eq,Show,Generic)

instance FromNamedRecord (Entry a) where
  parseNamedRecord r = Entry <$> r .: "Date" <*> r .: "compte" <*> r .: "libelle" <*> r .: "sens" <*> r .: "montant" <*> r .: "keys"

type Keys = [ Text ]

arnaud,bernard,fred :: Text
arnaud  = "801000:Arnaud"
bernard = "802000:Bernard"
fred    = "803000:Fred"

instance FromField Keys where
  parseField "A"       = pure [ arnaud ]
  parseField "Arnaud"  = pure [ arnaud ]
  parseField "B"       = pure [ bernard ]
  parseField "Bernard" = pure [ bernard ]
  parseField "F"       = pure [ fred ]
  parseField "Fred"    = pure [ fred ]
  parseField _         = pure [ arnaud, bernard, fred ]
