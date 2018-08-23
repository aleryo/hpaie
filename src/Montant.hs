{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Montant where

import           Data.Csv
import           Data.Maybe
import           Data.Text          as Text
import           Data.Text.Encoding
import           Date               ()
import           GHC.Generics
import           Text.Parsec

data Currency = EUR

newtype Montant (currency :: Currency) = Montant Integer
  deriving (Eq,Ord,Show,Generic,Num,Enum,Real,Integral)

instance FromField (Montant a) where
  parseField bs =
    either (fail . show) (pure . Montant . fromIntegral) $ parse decimal "" (unpack $ decodeUtf8 bs)
    where
      decimal :: Parsec String () Int
      decimal = do
        sign <- option 1 (char '-' *> spaces >> pure (-1))
        intPart <- read <$> (spaces *> digits)
        decPart <- optionMaybe $ read <$> (comma *> digits <* spaces)
        pure $ (sign * intPart * 100 + fromMaybe 0 decPart)

      digits = many1 digit
      comma = char ','
