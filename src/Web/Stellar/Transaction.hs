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
import           Control.Monad
import           Data.Aeson
import           Data.Monoid
import           Data.Text
import           Web.Stellar.Internal
import           Web.Stellar.Request
import           Web.Stellar.Types
import Debug.Trace
data Transaction = Transaction {
  _transactionAccount   :: Text,
  _destination          :: Maybe Text,
  _signingPubKey        :: Text,
  _transactionType      :: Text,
  _transactionSignature :: Text,
  _date                 :: Int,
  _hash                 :: Text,
  _amountData           :: Text,
  _currency             :: Text
} deriving (Eq, Show)

$(makeLenses ''Transaction)

amount :: Lens' Transaction (Maybe Money)
amount = moneyLens amountData

instance FromJSON Transaction where
  parseJSON (Object v) = do
    Transaction <$> (tx >>= (.: "Account"))
    <*> (tx >>= (.:? "Destination"))
    <*> (tx >>= (.: "SigningPubKey"))
    <*> (tx >>= (.: "TransactionType"))
    <*> (tx >>= (.: "TxnSignature"))
    <*> (tx >>= (.: "date"))
    <*> (tx >>= (.: "hash"))
    <*> fmap (extract innerMoney) (withDefault "Amount" emptyAPIMoney)
    <*> fmap (extract innerCurrency) (withDefault "Amount" defaultAPICurrency)
    where extract _ Nothing = mempty
          extract f (Just x) = f x
          withDefault k d = (tx >>= (\o -> o .:? k .!= (Just d)))
          tx = (v .: "tx")
  parseJSON _ = mzero

data TransactionList = TransactionList {
  innerTransactions :: [Transaction]
}

instance FromJSON TransactionList where
  parseJSON (Object v) = do
    TransactionList <$> ((v .: "result") >>= (.: "transactions"))
  parseJSON _ = mzero

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
fetchTransactions endpoint acc fetchMin fetchMax = fetchTransactionsWithMarker endpoint acc fetchMin fetchMax ""

fetchTransactionsWithMarker :: StellarEndpoint -> Text -> Int -> Int -> Text -> IO (Maybe [Transaction])
fetchTransactionsWithMarker endpoint acc fetchMin fetchMax fetchMarker = do
  r <- fetchTransactionData
  d <- rawRequestData 
  putStrLn $ show d
  return $ fmap innerTransactions r
  where fetchTransactionData = fmap decode rawRequestData
        rawRequestData = makeRequest endpoint $ TransactionRequest acc fetchMin fetchMax fetchMarker


