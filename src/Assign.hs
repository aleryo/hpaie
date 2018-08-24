{-# LANGUAGE RecordWildCards #-}
-- | Provides logic to assign analytic distribution keys to entries
module Assign where

import           Control.Exception
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Char            (ord)
import           Data.Csv
import           Data.Text            (Text)
import           Data.Text.Encoding
import qualified Data.Vector          as V
import           Entry                (Entry (Entry))
import           Montant
import           RawEntry
import           Rules

-- | Transform an input tab-separated file containing accounting `Entry`
-- into another TSV file containing the same entries with keys assigned.
--
-- The analytical keys assignment is controlled by another file which
-- contains a list of rules of the form @<regex> -> <key>@. The process
-- simply tries to apply rules using `Entry`'s `libelle` field and use
-- the first that match.
assignKeys :: FilePath -> FilePath -> FilePath -> IO ()
assignKeys rawInputTsv rulesFile outputTsv = do
  inp <- parseRawInput rawInputTsv
  rules <- parseRulesFile rulesFile
  generateAssignedEntries outputTsv inp rules


parseRawInput :: FilePath -> IO [ RawEntry cur ]
parseRawInput rawInputFile = do
  csv <- LBS.readFile rawInputFile
  case decodeByNameWith options csv  of
    Left err     -> throwIO $ userError err
    Right (_, v) -> pure $ V.toList v
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral (0x09 :: Int)}

parseRulesFile :: FilePath -> IO Rules
parseRulesFile rulesFile = do
  txt <- decodeUtf8 <$> BS.readFile rulesFile
  either (throwIO . userError . show) pure $ parseRules txt

generateEntry :: (RawEntry cur, Text) -> Entry cur
generateEntry (RawEntry{..},a) =
  Entry date (compte <> ":" <> refLibelle) libelle s m [a]
  where
    s = if debit > 0 then Debit else Credit
    m = debit + credit

generateAssignedEntries :: FilePath -> [ RawEntry cur ] -> Rules -> IO ()
generateAssignedEntries outputTsv rawEntries rules =
  LBS.writeFile outputTsv (encodeDefaultOrderedByNameWith opts entries)
  where
    entries = fmap generateEntry $ assignToEntries rules rawEntries
    opts = defaultEncodeOptions { encDelimiter = fromIntegral (ord '\t')
                                , encUseCrLf = False
                                }
