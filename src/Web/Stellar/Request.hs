{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web.Stellar.Request (
  module Web.Stellar.Types,
  PingResponse,
  pingStellar,
  PingStatus,
  PingSuccess(..),
  PingFailure(..),
  makeRequest
) where

import           Control.Applicative
import qualified Control.Exception    as E
import           Control.Lens         hiding ((.=))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text
import           GHC.Generics
import           Network.Wreq
import Web.Stellar.Types
import Data.Maybe

data PingRequest = PingRequest {
  method :: Text
} deriving Generic

instance ToJSON PingRequest

defaultPingRequest :: PingRequest
defaultPingRequest = PingRequest { Web.Stellar.Request.method = "ping" }

data PingResponse = PingResponse Text deriving (Show)
instance FromJSON PingResponse where
  parseJSON (Object v) = PingResponse <$> ((v .: "result") >>= (.: "status"))
  parseJSON _ = mzero

data PingFailure = PingFailure deriving (Eq, Show)
data PingSuccess = PingSuccess deriving (Eq, Show)

type PingStatus = Either PingFailure PingSuccess

pingFailure :: PingStatus
pingFailure = Left PingFailure

pingSuccess :: PingStatus
pingSuccess = Right PingSuccess

toPingStatus :: PingResponse -> PingStatus
toPingStatus (PingResponse x) = if x == "success" then pingSuccess else pingFailure

-- | Ping to test the connection to stellard
--
-- >>> :set -XOverloadedStrings
-- >>> pingStellar "https://test.stellar.org:9002"
-- Just PingSuccess
pingStellar :: StellarEndpoint -> IO PingStatus
pingStellar e = (pingStellar' e defaultPingRequest) `E.catch` pingHandler
  where pingHandler = (\(_ :: E.SomeException) -> return $ pingFailure)
        pingStellar' :: StellarEndpoint -> PingRequest -> IO PingStatus
        pingStellar' endpoint ping = do
          r <- makeRequest endpoint ping
          return $ fromMaybe pingFailure (fmap toPingStatus (decode r))

makeRequest :: (ToJSON a) => StellarEndpoint -> a -> IO (LBS.ByteString)
makeRequest (Endpoint x) v = do
  r <- post (unpack x) (toJSON v)
  return $ r ^. responseBody


