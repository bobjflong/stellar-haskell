{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Internal where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Fixed
import           Data.Text
import           Debug.Trace

data APIMoney = ExtractedText {
  innerText :: Text
} deriving (Show, Eq)

emptyAPIMoney :: APIMoney
emptyAPIMoney = ExtractedText ""

instance FromJSON APIMoney where
  parseJSON (Object o) = do
    ExtractedText <$> (o .: "value")
  parseJSON (String s) = do
    return $ ExtractedText s

data APICurrency = ExtractedCurrency {
  innerCurrency :: Text
} deriving (Show, Eq)

defaultAPICurrency = ExtractedCurrency "STR"

instance FromJSON APICurrency where
  parseJSON (Object o) = ExtractedCurrency <$> (o .: "currency")
  parseJSON _ = return defaultAPICurrency
