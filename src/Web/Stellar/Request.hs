{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Request (
  PingResponse,
  StellarEndpoint,
  pingStellar,
  PingStatus(..),
  makeRequest
) where

import           Control.Applicative
import           Control.Lens         hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text
import           Debug.Trace
import           GHC.Generics
import           Network.Wreq

requestOptions :: Options
requestOptions = defaults & setContentType & setAccept
  where setContentType = header "Content-Type" .~ ["application/json"]
        setAccept = header "Accept" .~ ["application/json"]

data PingRequest = PingRequest {
  method :: String
} deriving Generic

$(makeLensesFor [("method", "rpcMethod")] ''PingRequest)

instance ToJSON PingRequest

defaultPingRequest :: PingRequest
defaultPingRequest = PingRequest { method = "ping" }

type StellarEndpoint = Text

data PingResponse = PingResponse Text deriving (Show)
instance FromJSON PingResponse where
  parseJSON (Object v) = PingResponse <$> ((v .: "result") >>= (.: "status"))

data PingStatus = PingSuccess | PingFailure deriving (Show)

toPingStatus :: PingResponse -> PingStatus
toPingStatus (PingResponse x) = if x == "success" then PingSuccess else PingFailure
toPingStatus _ = PingFailure

-- | Ping to test the connection to stellard
--
-- >>> :set -XOverloadedStrings
-- >>> pingStellar "https://test.stellar.org:9002"
-- Just PingSuccess
pingStellar :: StellarEndpoint -> IO (Maybe PingStatus)
pingStellar = flip pingStellar' defaultPingRequest
  where pingStellar' :: StellarEndpoint -> PingRequest -> IO (Maybe PingStatus)
        pingStellar' endpoint ping = do
          r <- post (unpack endpoint) (toJSON defaultPingRequest)
          return $ fmap toPingStatus $ decode (r ^. responseBody)

makeRequest :: (ToJSON a) => StellarEndpoint -> a -> IO (LBS.ByteString)
makeRequest x v = do
  r <- post (unpack x) (toJSON v)
  return $ r ^. responseBody


