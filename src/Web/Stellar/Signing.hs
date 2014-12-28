{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Signing (
    SignRequest(..),
    defaultSignRequest,
    signSecret,
    txJSON,
    signRequest,
    blob,
    makeSignedRequest
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Text
import           Prelude             hiding (sequence)
import           Web.Stellar.Request
import           Web.Stellar.Types

data SignRequest = SignRequest {
  _signSecret :: Text,
  _txJSON     :: Value
} deriving (Eq, Show)

defaultSignRequest :: SignRequest
defaultSignRequest = SignRequest "" Null

$(makeLenses ''SignRequest)

instance ToJSON SignRequest where
  toJSON p = object [
               "method" .= ("sign" :: Text),
               "params" .= [object [
                 "secret" .= (p ^. signSecret),
                 "tx_json" .= (p ^. txJSON)
               ]]
             ]

data SignResponse = SignResponse {
  _blob :: Text
} deriving (Eq, Show)

$(makeLenses ''SignResponse)

instance FromJSON SignResponse where
  parseJSON (Object v) = SignResponse <$> ((v .: "result") >>= (.: "tx_blob"))
  parseJSON _ = mzero

instance ToJSON SignResponse where
  toJSON p = object [
               "method" .= ("submit" :: Text),
               "params" .= [object [
                 "tx_blob" .= (p ^. blob)
               ]]
             ]

signRequest :: StellarEndpoint -> SignRequest -> IO (Maybe SignResponse)
signRequest e p = makeRequest e p >>= (return.decode)

-- todo: duplication ^
makeSignedRequest :: StellarEndpoint -> SignResponse -> IO (Maybe SubmissionResponse)
makeSignedRequest e p = makeRequest e p >>= (return.decode)
