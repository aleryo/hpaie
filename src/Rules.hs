{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Rules where

import           Data.Text                             (Text, pack, unpack)
import           Data.Text.Prettyprint.Doc             hiding (space, (<>))
import           Data.Text.Prettyprint.Doc.Render.Text
import           RawEntry
import           Text.Parsec
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text                  ()

newtype Rules = Rules [ Rule ]
  deriving (Eq, Show, Semigroup, Monoid)

data Rule = Rule { regex :: Text, target :: Text }
  deriving (Eq, Show)

assignToEntries :: Rules -> [ RawEntry cur ] -> [ (RawEntry cur, Text) ]
assignToEntries rules = fmap (assignToEntry rules)

assignToEntry :: Rules -> RawEntry cur -> (RawEntry cur, Text)
assignToEntry (Rules rules) e@RawEntry{piece, libelle, reference, refLibelle} = (e, foldr match' "ALL" rules)
  where
    match' :: Rule -> Text -> Text
    match' (Rule re tg) cur  =
      if libelle =~ re || piece =~re || reference =~ re || refLibelle =~ re
      then tg
      else cur


renderRules :: Rules -> Text
renderRules = renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Pretty Rules where
  pretty (Rules rls)  = vsep $ fmap pretty rls

instance Pretty Rule where
  pretty (Rule re tg) = pretty re <> " -> " <> pretty tg

parseRules :: Text -> Either ParseError Rules
parseRules = parse rulesParser "" . unpack
  where
    rulesParser :: Parsec String () Rules
    rulesParser = multipleRules

    multipleRules = Rules <$> sepEndBy singleRule newline

    singleRule :: Parsec String () Rule
    singleRule = do
      re <- pack <$> quoted (many1 $ noneOf "\"")
      spaces >> string "->" >> spaces
      tg <- pack <$> many1 (noneOf "\n\r")
      pure $ Rule re tg

    quoted = between (char '"') (char '"')
