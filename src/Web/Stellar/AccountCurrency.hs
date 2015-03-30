{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.AccountCurrency where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Web.Stellar.Request

data AccountCurrency = AccountCurrency {
  receive_currencies :: [Text],
  send_currencies    :: [Text]
} deriving (Generic, Show)

$(makeLensesFor [("receive_currencies", "receive"), ("send_currencies", "send")] ''AccountCurrency)

instance FromJSON AccountCurrency

data AccountCurrencyData = AccountCurrencyData {
  innerCurrency :: AccountCurrency
} deriving (Show)

instance FromJSON AccountCurrencyData where
  parseJSON (Object v) = do
    AccountCurrencyData <$> (v .: "result")
  parseJSON _ = mzero

fetchCurrencies :: StellarEndpoint -> AccountID -> IO (Maybe AccountCurrency)
fetchCurrencies endpoint (AccountID aid) = do
  currencyData <- fetchCurrencyData
  return $ fmap innerCurrency currencyData
  where fetchCurrencyData :: IO (Maybe AccountCurrencyData)
        fetchCurrencyData = fmap decode $ makeRequest endpoint request
        request = simpleRequest & method .~ "account_currencies" & accountId .~ aid
