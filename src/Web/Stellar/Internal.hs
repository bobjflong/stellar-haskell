{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Internal where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text

data APIMoney = ExtractedText {
  innerMoney :: Text
} deriving (Show, Eq)

emptyAPIMoney :: APIMoney
emptyAPIMoney = ExtractedText ""

instance FromJSON APIMoney where
  parseJSON (Object o) = do
    ExtractedText <$> (o .: "value")
  parseJSON (String s) = do
    return $ ExtractedText s
  parseJSON _ = mzero

data APICurrency = ExtractedCurrency {
  innerCurrency :: Text
} deriving (Show, Eq)

defaultAPICurrency :: APICurrency
defaultAPICurrency = ExtractedCurrency ""

instance FromJSON APICurrency where
  parseJSON (Object o) = ExtractedCurrency <$> (o .: "currency")
  parseJSON _ = return defaultAPICurrency
