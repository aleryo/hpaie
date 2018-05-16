{-# LANGUAGE RecordWildCards #-}
module Run where

import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.Csv
import           Data.Monoid
import           Data.Text            as Text
import           Data.Text.Encoding
import           Data.Text.IO         as Text
import           Data.Time.Calendar   (Day (..))
import           Data.Time.Format
import qualified Data.Vector          as V
import           GHC.Generics
import           System.FilePath
import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Token


comptaAnalytique :: FilePath -> FilePath -> [ Text ] -> IO ()
comptaAnalytique _input output _keys = do
  Text.writeFile output (Text.unlines
                         [ "2018/05/14 Frais tenu de comptes"
                         , "    612000:KPMG               120.00"
                         , "    801000:Arnaud             -60.00"
                         , "    802000:Fred               -60.00"
                         ])


parseCSV :: FilePath -> IO [ Entry ]
parseCSV fp = do
  csv <- LBS.readFile fp
  case decodeByNameWith options csv  of
    Left err     -> throwIO $ userError err
    Right (_, v) -> pure $ V.toList v
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

generateLedger :: FilePath -> [ Text ] -> [ Entry ] -> IO ()
generateLedger fp repartition entries = pure ()

data Entry = Entry { date    :: Day
                   , compte  :: Text
                   , libelle :: Text
                   , sens    :: Sens
                   , montant :: Montant
                   }
  deriving (Eq,Show,Generic)

instance FromNamedRecord Entry where
  parseNamedRecord r = Entry <$> r .: "Date" <*> r .: "compte" <*> r .: "libelle" <*> r .: "sens" <*> r .: "montant"

instance FromField Day where
  parseField bs =
    case isoDate (decodeUtf8 bs) of
      Nothing -> fail $ "cannot parse " <> show bs <> " as a date"
      Just d  -> pure d

isoDate :: Text -> Maybe Day
isoDate = parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . Text.unpack

data Sens = Debit | Credit
  deriving (Eq,Show,Generic)

instance FromField Sens where
  parseField "D" = pure Debit
  parseField "C" = pure Credit
  parseField s   = fail $ "cannot parse " <> show s <> " as a CSV Field"

data Montant = EUR Integer
  deriving (Eq,Show,Generic)

instance FromField Montant where
  parseField bs =
    either (fail . show) (pure . EUR) $ parse decimal "" (unpack $ decodeUtf8 bs)
    where

      decimal = do
        intPart <- read <$> (spaces *> digits)
        comma
        decPart <- read <$> (digits <* spaces)
        pure $ (intPart * 100 + decPart)

      digits = many1 digit
      spaces = many space
      comma = char ','

generateTransaction :: [ Text ] -> Entry -> Transaction
generateTransaction keys Entry{..} =
  Transaction date libelle []

data Transaction = Transaction { txDate     :: Day
                               , txLabel    :: Text
                               , txPostings :: [ Posting ]
                               }
  deriving (Eq, Show, Generic)

data Posting = Posting { postAccount :: Text
                       , postAmount  :: Montant
                       }
  deriving (Eq, Show, Generic)
