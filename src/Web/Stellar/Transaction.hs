{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Transaction (
    fetchTransactions,
    fetchTransactionsWithMarker,
    Transaction(..),
    transactionAccount,
    amount,
    destination,
    signingPubKey,
    transactionType,
    transactionSignature,
    date,
    hash,
    currency
  ) where

import           Control.Applicative
import           Control.Lens         hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed
import           Data.Monoid
import           Data.Text
import           Debug.Trace
import           Lens.Family          hiding ((.~), (^.))
import           Web.Stellar.Internal
import           Web.Stellar.Request
import           Web.Stellar.Types

data Transaction = Transaction {
  _transactionAccount   :: Text,
  _amountData           :: Text,
  _destination          :: Maybe Text,
  _signingPubKey        :: Text,
  _transactionType      :: Text,
  _transactionSignature :: Text,
  _date                 :: Int,
  _hash                 :: Text,
  _currency             :: Text
} deriving (Eq, Show)

$(makeLenses ''Transaction)

amount :: Lens' Transaction (Maybe Money)
amount = moneyLens amountData

instance FromJSON Transaction where
  parseJSON (Object v) = do
    Transaction <$> ((v .: "tx") >>= (.: "Account"))
    <*> fmap (extractInner innerText) ((v .: "tx") >>=
          (\o -> o .:? "Amount" .!= (Just emptyAPIMoney)))
    <*> ((v .: "tx") >>= (.:? "Destination"))
    <*> ((v .: "tx") >>= (.: "SigningPubKey"))
    <*> ((v .: "tx") >>= (.: "TransactionType"))
    <*> ((v .: "tx") >>= (.: "TxnSignature"))
    <*> ((v .: "tx") >>= (.: "date"))
    <*> ((v .: "tx") >>= (.: "hash"))
    <*> fmap (extractInner innerCurrency) ((v .: "tx") >>=
          (\o -> o .:? "Amount" .!= (Just defaultAPICurrency)))
    where extractInner _ Nothing = mempty
          extractInner f (Just x) = f x

data TransactionList = TransactionList {
  innerTransactions :: [Transaction]
}

instance FromJSON TransactionList where
  parseJSON (Object v) = do
    TransactionList <$> ((v .: "result") >>= (.: "transactions"))

data TransactionRequest = TransactionRequest {
  account        :: Text,
  ledgerIndexMin :: Int,
  ledgerIndexMax :: Int,
  marker         :: Text
}

instance ToJSON TransactionRequest where
  toJSON transactionRequest = object [
    "method" .= ("account_tx" :: Text),
    "params" .= [object [
      "account" .= (account transactionRequest),
      "ledger_index_min" .= (ledgerIndexMin transactionRequest),
      "ledger_index_max" .= (ledgerIndexMax transactionRequest),
      "marker" .= (marker transactionRequest)]]]

fetchTransactions :: StellarEndpoint -> Text -> Int -> Int -> IO (Maybe [Transaction])
fetchTransactions endpoint acc min max = fetchTransactionsWithMarker endpoint acc min max ""

fetchTransactionsWithMarker :: StellarEndpoint -> Text -> Int -> Int -> Text -> IO (Maybe [Transaction])
fetchTransactionsWithMarker endpoint acc min max marker = do
  r <- fetchTransactionData
  return $ fmap innerTransactions r
  where fetchTransactionData = fmap decode $ makeRequest endpoint $ TransactionRequest acc min max marker

