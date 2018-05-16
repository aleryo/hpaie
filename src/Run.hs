{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Run where

import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc hiding ((<>), space)
import Data.Ratio
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
import Text.Printf
import System.IO

comptaAnalytique :: FilePath -> FilePath -> [ Text ] -> IO ()
comptaAnalytique input output keys =
  parseCSV input >>= generateLedger output keys


parseCSV :: FilePath -> IO [ Entry cur ]
parseCSV fp = do
  csv <- LBS.readFile fp
  case decodeByNameWith options csv  of
    Left err     -> throwIO $ userError err
    Right (_, v) -> pure $ V.toList v
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

generateLedger :: FilePath -> [ Text ] -> [ Entry cur ] -> IO ()
generateLedger fp repartition entries =
  let txs = fmap (generateTransaction repartition) entries
  in withFile fp WriteMode $ \ h -> hPutDoc h (vcat (fmap pretty txs) <> hardline)

data Entry (cur :: Currency) =
  Entry { date    :: Day
        , compte  :: Text
        , libelle :: Text
        , sens    :: Sens
        , montant :: Montant cur
        }
  deriving (Eq,Show,Generic)

instance FromNamedRecord (Entry a) where
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

invert :: Sens -> Sens
invert Debit = Credit
invert Credit = Debit

instance FromField Sens where
  parseField "D" = pure Debit
  parseField "C" = pure Credit
  parseField s   = fail $ "cannot parse " <> show s <> " as a CSV Field"

data Currency = EUR

newtype Montant (currency :: Currency) = Montant Rational
  deriving (Eq,Show,Generic,Num,Fractional)


instance FromField (Montant a) where
  parseField bs =
    either (fail . show) (pure . Montant . fromIntegral) $ parse decimal "" (unpack $ decodeUtf8 bs)
    where

      decimal = do
        intPart <- read <$> (spaces *> digits)
        comma
        decPart <- read <$> (digits <* spaces)
        pure $ (intPart * 100 + decPart)

      digits = many1 digit
      spaces = many space
      comma = char ','

generateTransaction :: [ Text ] -> Entry cur -> Transaction
generateTransaction keys Entry{..} =
  Transaction date libelle (basePosting:distributedPostings)
  where
    basePosting = Posting compte sens montant
    distributedAmount = montant / fromIntegral (Prelude.length keys)
    distributedPostings = fmap (\ k -> Posting k (invert sens) distributedAmount) keys

render :: Transaction -> Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

data Transaction = Transaction { txDate     :: Day
                               , txLabel    :: Text
                               , txPostings :: [ Posting ]
                               }
  deriving (Eq, Show, Generic)

data Posting where
  Posting :: forall cur . { postAccount :: Text
                          , postSens    :: Sens
                          , postAmount  :: Montant cur
                          } -> Posting

instance Show Posting where
  show Posting{..} = "Posting {" <>
                     "postAccount = " <> show postAccount <>
                     ", postSens = " <> show postSens <>
                     ", postAmount = " <> show postAmount <>
                     "}"


instance Eq Posting where
  posting1 == posting2 =
    postAccount posting1 == postAccount posting2 &&
    postSens posting1 == postSens posting2 &&
    case posting1 of
      Posting _ _ (Montant v) -> case posting2 of
                                   Posting _ _ (Montant v') -> v == v'

instance Pretty Transaction where
  pretty Transaction{..} =
    vsep [ pretty txDate <+> pretty txLabel,  postings ]
    where
      postings = indent 4 $ vcat $ fmap pretty txPostings

instance Pretty Posting where
  pretty Posting{..} = fill 20 (pretty postAccount) <+> minus <> pretty postAmount
    where
      minus = case postSens of
                Debit -> ""
                Credit -> "-"

instance Pretty (Montant a) where
  pretty (Montant m) = pretty $ (printf "%.2f" (fromRational $ m / 100 :: Double) :: String)

instance Pretty Day where
  pretty day = pretty $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) day
