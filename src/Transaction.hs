{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Transaction where

import Data.Text as Text
import Data.Time.Calendar (Day (..))
import Data.Time.Format.ISO8601(iso8601Show)
import GHC.Generics
import Montant
import Prettyprinter hiding (space, (<>))
import Prettyprinter.Render.Text
import Text.Printf

render :: Transaction -> Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

data Transaction = Transaction
    { txDate :: Day
    , txLabel :: Text
    , txPostings :: [Posting]
    }
    deriving (Eq, Show, Generic)

data Posting where
    Posting :: {postAccount :: Text, postSens :: Sens, postAmount :: Montant cur} -> Posting

instance Show Posting where
    show Posting{..} =
        "Posting {"
            <> "postAccount = "
            <> show postAccount
            <> ", postSens = "
            <> show postSens
            <> ", postAmount = "
            <> show postAmount
            <> "}"

instance Eq Posting where
    posting1 == posting2 =
        postAccount posting1 == postAccount posting2
            && postSens posting1 == postSens posting2
            && case posting1 of
                Posting _ _ (Montant v) -> case posting2 of
                    Posting _ _ (Montant v') -> v == v'

instance Pretty Transaction where
    pretty Transaction{..} =
        vsep [pretty txDate <+> pretty txLabel, postings]
      where
        postings = indent 4 $ vcat $ fmap pretty txPostings

instance Pretty Posting where
    pretty Posting{..} = fill 50 (pretty postAccount) <+> space <> minus <> pretty postAmount
      where
        space = " " -- there must be at least 2 spaces between label and amount
        minus = case postSens of
            Debit -> ""
            Credit -> "-"

instance Pretty (Montant a) where
    pretty (Montant m) = pretty $ (printf "%.2f" (fromIntegral m / 100 :: Double) :: String)

instance Pretty Day where
    pretty day = pretty $ iso8601Show day
