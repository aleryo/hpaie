{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Date where

import           Data.Csv
import           Data.Text          as Text
import           Data.Text.Encoding
import           Data.Time.Calendar (Day (..))
import           Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import           Data.Time.Format.ISO8601 (iso8601ParseM)

instance FromField Day where
  parseField bs =
    case ddmmYYYY (decodeUtf8 bs) of
      Nothing -> fail $ "cannot parse " <> show bs <> " as a date"
      Just d  -> pure d

instance ToField Day where
  toField = encodeUtf8 . Text.pack . formatTime defaultTimeLocale "%d/%m/%Y"

ddmmYYYY :: Text -> Maybe Day
ddmmYYYY = parseTimeM True defaultTimeLocale "%d/%m/%Y" . Text.unpack

isoDate :: Text -> Maybe Day
isoDate = iso8601ParseM . Text.unpack
