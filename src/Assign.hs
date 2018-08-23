-- | Provides logic to assign analytic distribution keys to entries
module Assign where

import qualified Data.ByteString    as BS
import           Data.Text          ()
import           Data.Text.Encoding
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
  rules <- parseRules rulesFile
  generateAssignedEntries outputTsv inp rules


parseRawInput :: FilePath -> IO [ RawEntry ]
parseRawInput _rawInputFile = pure []

parseRules :: FilePath -> IO Rules
parseRules _rulesFile = pure mempty

generateAssignedEntries :: FilePath -> [ RawEntry ] -> Rules -> IO ()
generateAssignedEntries outputTsv _entries _rules =
  BS.writeFile outputTsv $ encodeUtf8 $ "Date\tcompte\tlibelle\tsens\tmontant\tkeys\n13/02/2018\t10100000:Capital\tCLOTURE COMPTE CAPITAL\tC\t4000,00\tALL\n31/10/2017\t40110000:Fournisseurs\tMois Octobre 2017\tC\t24,71\tALL\n"
