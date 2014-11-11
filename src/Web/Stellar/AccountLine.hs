{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.AccountLine (
    fetchAccountLines,
    Money,
    AccountLine(..),
    otherAccount,
    balance,
    currency,
    limit,
    limitPeer
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Fixed
import           Data.Text
import           Debug.Trace
import           Lens.Family         hiding ((.~), (^.))
import           Web.Stellar.Request

-- | Provides a lens interface onto a Stellar Trust Line
--
-- >>> acc ^. otherAccount
-- "ghj4kXtHfQcCaLQwpLJ11q2hq6248R7k9C"
--
-- >>> acc ^. balance
-- Just 360.000000000000
--
-- >>> acc ^. currency
-- "BTC"
-- 
-- >>> acc ^. limit
-- Just 0.000000000000
--
-- >>> acc ^. limitPeer
-- Just 0.000000000000
data AccountLine = AccountLine {
  _otherAccount  :: Text,
  _balanceData   :: Text,
  _currency      :: Text,
  _limitData     :: Text,
  _limitPeerData :: Text
} deriving (Show)

$(makeLenses ''AccountLine)

instance FromJSON AccountLine where
  parseJSON (Object v) = do
    AccountLine <$> v .: "account"
    <*> v .: "balance"
    <*> v .: "currency"
    <*> v .: "limit"
    <*> v .: "limit_peer"

type Money = Fixed E12

moneyLens :: (Lens' AccountLine Text) -> Lens' AccountLine (Maybe Money)
moneyLens l = lens g s
  where g v = textToFixed $ v ^. l
        s v (Just x) = l .~ ((pack.show) x) $ v
        s v _ = l .~ "" $ v

limit :: Lens' AccountLine (Maybe Money)
limit = moneyLens limitData

limitPeer :: Lens' AccountLine (Maybe Money)
limitPeer = moneyLens limitPeerData

balance :: Lens' AccountLine (Maybe Money)
balance = moneyLens balanceData

data AccountLineData = AccountLineData {
  innerLines :: [AccountLine]
}

instance FromJSON AccountLineData where
  parseJSON (Object v) = do
    AccountLineData <$> ((v .: "result") >>= (.: "lines"))

data AccountLineRequest = AccountLineRequest {
  account :: Text
}

instance ToJSON AccountLineRequest where
  toJSON accountLineRequest = object [
    "method" .= ("account_lines" :: Text),
    "params" .= [object ["account" .= (account accountLineRequest)]]]

-- | Fetch trust lines for an Account ID
--
-- >>> r <- fetchAccountLines "https://test.stellar.org:9002" "abcdef..."
fetchAccountLines :: StellarEndpoint -> Text -> IO (Maybe [AccountLine])
fetchAccountLines endpoint accountId = do
  accountData <- fetchAccountLineData
  return $ fmap innerLines accountData
  where fetchAccountLineData :: IO (Maybe AccountLineData)
        fetchAccountLineData = fmap decode $ makeRequest endpoint $ AccountLineRequest accountId

textToFixed :: Text -> Maybe Money
textToFixed t = case ((reads $ unpack t) :: [(Double, String)]) of
  [(a,"")] -> Just $ realToFrac a
  _ -> Nothing

