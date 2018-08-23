-- | Provides logic to assign analytic distribution keys to entries
module Assign where


-- | Transform an input tab-separated file containing accounting `Entry`
-- into another TSV file containing the same entries with keys assigned.
--
-- The analytical keys assignment is controlled by another file which
-- contains a list of rules of the form @<regex> -> <key>@. The process
-- simply tries to apply rules using `Entry`'s `libelle` field and use
-- the first that match.
assignKeys :: FilePath -> FilePath -> FilePath -> IO ()
assignKeys = undefined
