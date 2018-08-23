{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Entry where

import           Data.Csv
import           Data.Monoid
import           Data.Text          as Text
import           Data.Text.Encoding
import           Data.Time.Calendar (Day (..))
import           Data.Time.Format
import           GHC.Generics
import           Text.Parsec

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

instance FromField Day where
  parseField bs =
    case ddmmYYYY (decodeUtf8 bs) of
      Nothing -> fail $ "cannot parse " <> show bs <> " as a date"
      Just d  -> pure d

ddmmYYYY :: Text -> Maybe Day
ddmmYYYY = parseTimeM True defaultTimeLocale "%d/%m/%Y" . Text.unpack

isoDate :: Text -> Maybe Day
isoDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . Text.unpack

data Sens = Debit | Credit
  deriving (Eq,Show,Generic)

invert :: Sens -> Sens
invert Debit  = Credit
invert Credit = Debit

instance FromField Sens where
  parseField "D" = pure Debit
  parseField "C" = pure Credit
  parseField s   = fail $ "cannot parse " <> show s <> " as a CSV Field"

data Currency = EUR

newtype Montant (currency :: Currency) = Montant Integer
  deriving (Eq,Ord,Show,Generic,Num,Enum,Real,Integral)

instance FromField (Montant a) where
  parseField bs =
    either (fail . show) (pure . Montant . fromIntegral) $ parse decimal "" (unpack $ decodeUtf8 bs)
    where
      decimal :: Parsec String () Int
      decimal = do
        intPart <- read <$> (spaces *> digits <* comma)
        decPart <- read <$> (digits <* spaces)
        pure $ (intPart * 100 + decPart)

      digits = many1 digit
      comma = char ','


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
