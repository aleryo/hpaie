module Rules where

data Rules = NoRule

instance Monoid Rules where
  mempty = NoRule
  mappend = undefined
