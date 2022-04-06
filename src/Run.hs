{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Run where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import qualified Data.Vector as V
import Entry
import Montant
import Prettyprinter hiding (space, (<>))
import Prettyprinter.Render.Text
import System.IO
import Transaction

comptaAnalytique :: FilePath -> FilePath -> Char -> IO ()
comptaAnalytique input output delimiter =
    parseCSV delimiter input >>= generateLedger output

parseCSV :: Char -> FilePath -> IO [Entry cur]
parseCSV delimiter fp = do
    csv <- LBS.readFile fp
    case decodeByNameWith options csv of
        Left err -> throwIO $ userError err
        Right (_, v) -> pure $ V.toList v
  where
    options = defaultDecodeOptions{decDelimiter = fromIntegral (fromEnum delimiter)}

generateLedger :: FilePath -> [Entry cur] -> IO ()
generateLedger fp entries =
    let txs = fmap generateTransaction entries
     in withFile fp WriteMode $ \h -> hPutDoc h (vcat (fmap pretty txs) <> hardline)

generateTransaction :: Entry cur -> Transaction
generateTransaction Entry{..} =
    Transaction date libelle (basePosting : distributedPostings)
  where
    basePosting = Posting compte sens montant
    (distributedAmount, remaining) = montant `divMod` fromIntegral (Prelude.length keys)
    distributedPostings =
        case fmap (\k -> Posting k (invert sens) distributedAmount) keys of
            (Posting a s (Montant m) : ps) -> Posting a s (addRemaining m) : ps
            [] -> []
    addRemaining v = case remaining of
        Montant m' -> Montant $ v + m'
