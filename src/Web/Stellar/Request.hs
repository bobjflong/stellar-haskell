{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Request (
  PingResponse,
  StellarEndpoint,
  pingStellar,
  defaultPingRequest
) where

import           Control.Applicative
import           Control.Lens         hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text
import           GHC.Generics
import           Network.Wreq
import Debug.Trace

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

-- | Ping to test the connection to stellard 
--
-- >>> :set -XOverloadedStrings
-- >>> pingStellar "https://test.stellar.org:9002" defaultPingRequest
-- Just (PingResponse "success") 
pingStellar :: StellarEndpoint -> PingRequest -> IO (Maybe PingResponse)
pingStellar endpoint ping = do
  r <- post (unpack endpoint) (toJSON defaultPingRequest)
  return $ decode (r ^. responseBody)

