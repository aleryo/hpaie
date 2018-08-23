{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Run where

import           Control.Exception
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Csv
import           Data.Monoid
import           Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             hiding (space, (<>))
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Time.Calendar                    (Day (..))
import           Data.Time.Format
import qualified Data.Vector                           as V
import           Entry
import           GHC.Generics
import           System.IO
import           Text.Printf

comptaAnalytique :: FilePath -> FilePath -> IO ()
comptaAnalytique input output =
  parseCSV input >>= generateLedger output


parseCSV :: FilePath -> IO [ Entry cur ]
parseCSV fp = do
  csv <- LBS.readFile fp
  case decodeByNameWith options csv  of
    Left err     -> throwIO $ userError err
    Right (_, v) -> pure $ V.toList v
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral (0x09 :: Int)}

generateLedger :: FilePath -> [ Entry cur ] -> IO ()
generateLedger fp entries =
  let txs = fmap generateTransaction entries
  in withFile fp WriteMode $ \ h -> hPutDoc h (vcat (fmap pretty txs) <> hardline)

generateTransaction :: Entry cur -> Transaction
generateTransaction Entry{..} =
  Transaction date libelle (basePosting:distributedPostings)
  where
    basePosting = Posting compte sens montant
    (distributedAmount, remaining) = montant `divMod` fromIntegral (Prelude.length keys)
    distributedPostings =
      case fmap (\ k -> Posting k (invert sens) distributedAmount) keys of
        (Posting a s (Montant m):ps) -> Posting a s (addRemaining m):ps
        []                           -> []
    addRemaining v = case remaining of
                       Montant m' -> Montant $ v + m'

render :: Transaction -> Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

data Transaction = Transaction { txDate     :: Day
                               , txLabel    :: Text
                               , txPostings :: [ Posting ]
                               }
  deriving (Eq, Show, Generic)

data Posting where
  Posting :: { postAccount :: Text, postSens :: Sens, postAmount ::Montant cur}  ->  Posting

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
  pretty Posting{..} = fill 50 (pretty postAccount) <+> minus <> pretty postAmount
    where
      minus = case postSens of
                Debit  -> ""
                Credit -> "-"

instance Pretty (Montant a) where
  pretty (Montant m) = pretty $ (printf "%.2f" (fromIntegral m / 100 :: Double) :: String)

instance Pretty Day where
  pretty day = pretty $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) day
