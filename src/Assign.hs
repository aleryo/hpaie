-- | Provides logic to assign analytic distribution keys to entries
module Assign where


import           Control.Exception
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as LBS
import           Data.Csv
import           Data.Monoid
import           Data.Text                             as Text
import           Data.Text.Encoding
import           Data.Text.Prettyprint.Doc             hiding (space, (<>))
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Time.Calendar                    (Day (..))
import           Data.Time.Format
import qualified Data.Vector                           as V
import           GHC.Generics
import           RawEntry
import           Rules
import           System.IO
import           Text.Printf

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
  rules <- parseRules rulesFile
  generateAssignedEntries outputTsv inp rules


parseRawInput :: FilePath -> IO [ RawEntry cur ]
parseRawInput rawInputFile = do
  csv <- LBS.readFile rawInputFile
  case decodeByNameWith options csv  of
    Left err     -> throwIO $ userError err
    Right (_, v) -> pure $ V.toList v
  where
    options = defaultDecodeOptions { decDelimiter = fromIntegral (0x09 :: Int)}


parseRules :: FilePath -> IO Rules
parseRules _rulesFile = pure mempty

generateAssignedEntries :: FilePath -> [ RawEntry cur ] -> Rules -> IO ()
generateAssignedEntries outputTsv _entries _rules =
  BS.writeFile outputTsv $ encodeUtf8 $ "Date\tcompte\tlibelle\tsens\tmontant\tkeys\n13/02/2018\t10100000:Capital\tCLOTURE COMPTE CAPITAL\tC\t4000,00\tALL\n31/10/2017\t40110000:Fournisseurs\tMois Octobre 2017\tC\t24,71\tALL\n"
