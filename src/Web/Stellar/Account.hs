{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Account (
    fetchAccount,
    Account(..),
    flags,
    ownerCount,
    previousTxnID,
    previousTxnLgrSeq,
    stellarSequence,
    stellarIndex,
    balance
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Text
import           Web.Stellar.Request
import           Web.Stellar.Types

-- | Provides a lens interface onto a Stellar account
--
-- >>> account ^. balance
-- Just 135013248.000000000000
--
-- >>> account ^. flags
-- 1048576
--
-- >>> account ^. ownerCount
-- 0
--
-- >>> account ^. previousTxnID
-- "A584CEF24F24DAA16E99C2858B62A978975C448181FBF34B21B5873AFBA6A8AB
--
-- >>> account ^. previousTxnLgrSeq
-- 1056876
--
-- >>> account ^. stellarSequence
-- 4229
--
-- >>> account ^. stellarIndex
-- "6047FB9C7976F2D0554618F5ABFF423E7136205BAF19E92BE9D295E549442C45"
data Account = Account {
  _balanceData       :: Text,
  _flags             :: !Int,
  _ownerCount        :: !Int,
  _previousTxnID     :: Text,
  _previousTxnLgrSeq :: !Int,
  _stellarSequence   :: !Int,
  _stellarIndex      :: Text
} deriving (Show)

$(makeLenses ''Account)

balance :: Lens' Account (Maybe Money)
balance = moneyLens balanceData

instance FromJSON Account where
  parseJSON (Object v) = do
    Account <$> v .: "Balance"
    <*> v .: "Flags"
    <*> v .: "OwnerCount"
    <*> v .: "PreviousTxnID"
    <*> v .: "PreviousTxnLgrSeq"
    <*> v .: "Sequence"
    <*> v .: "index"
  parseJSON _ = mzero

data AccountData = AccountData {
  innerAccount :: Account
} deriving (Show)

instance FromJSON AccountData where
  parseJSON (Object v) = do
    AccountData <$> ((v .: "result") >>= (.: "account_data"))
  parseJSON _ = mzero

data AccountInfoRequest = AccountInfoRequest {
  account :: Text
}

instance ToJSON AccountInfoRequest where
  toJSON accountInfoRequest = object [
    "method" .= ("account_info" :: Text),
    "params" .= [object ["account" .= (account accountInfoRequest)]]]

-- | Fetch account information for an Account ID
--
-- >>> r <- fetchAccount "https://test.stellar.org:9002" "abcdef..."
fetchAccount :: StellarEndpoint -> Text -> IO (Maybe Account)
fetchAccount endpoint accountId = do
  accountData <- fetchTestAccountData
  return $ fmap innerAccount accountData
  where fetchTestAccountData :: IO (Maybe AccountData)
        fetchTestAccountData = fmap decode $ makeRequest endpoint $ AccountInfoRequest accountId
