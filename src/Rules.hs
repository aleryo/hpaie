module Rules where

import           Data.Monoid
import           Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             hiding (space, (<>))
import           Data.Text.Prettyprint.Doc.Render.Text
import           Text.Parsec

data Rules = Rule { regex :: Text, target :: Text }
           | Rules [ Rules ]
  deriving (Eq, Show)

instance Monoid Rules where
  mempty = NoRule
  mappend = undefined

renderRules :: Rules -> Text
renderRules = renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Pretty Rules where
  pretty (Rule re tg) = pretty re <> " -> " <> pretty tg
  pretty (Rules rls)  = vsep $ fmap pretty rls

parseRules :: Text -> Either ParseError Rules
parseRules = parse rulesParser "" . unpack
  where
    rulesParser :: Parsec String () Rules
    rulesParser = multipleRules

    multipleRules = Rules <$> sepBy singleRule newline

    singleRule :: Parsec String () Rules
    singleRule = do
      re <- pack <$> quoted (many1 $ noneOf "\"")
      spaces >> string "->" >> spaces
      tg <- pack <$> many1 (noneOf "\n\r")
      pure $ Rule re tg

    quoted = between (char '"') (char '"')
