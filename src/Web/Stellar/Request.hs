{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web.Stellar.Request (
  PingResponse,
  pingStellar,
  PingStatus(..),
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
import qualified Web.Stellar.Types    as T

data PingRequest = PingRequest {
  method :: Text
} deriving Generic

instance ToJSON PingRequest

defaultPingRequest :: PingRequest
defaultPingRequest = PingRequest { method = "ping" }

data PingResponse = PingResponse Text deriving (Show)
instance FromJSON PingResponse where
  parseJSON (Object v) = PingResponse <$> ((v .: "result") >>= (.: "status"))
  parseJSON _ = mzero

data PingStatus = PingSuccess | PingFailure deriving (Show)

toPingStatus :: PingResponse -> PingStatus
toPingStatus (PingResponse x) = if x == "success" then PingSuccess else PingFailure

-- | Ping to test the connection to stellard
--
-- >>> :set -XOverloadedStrings
-- >>> pingStellar "https://test.stellar.org:9002"
-- Just PingSuccess
pingStellar :: T.StellarEndpoint -> IO (Maybe PingStatus)
pingStellar e = (pingStellar' e defaultPingRequest) `E.catch` (\(_ :: E.SomeException) -> return $ Just PingFailure)
  where pingStellar' :: T.StellarEndpoint -> PingRequest -> IO (Maybe PingStatus)
        pingStellar' endpoint ping = do
          r <- makeRequest endpoint ping
          return $ fmap toPingStatus $ decode r

makeRequest :: (ToJSON a) => T.StellarEndpoint -> a -> IO (LBS.ByteString)
makeRequest (T.Endpoint x) v = do
  r <- post (unpack x) (toJSON v)
  return $ r ^. responseBody


