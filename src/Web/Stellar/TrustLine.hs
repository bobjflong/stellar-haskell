{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.TrustLine where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Text
import           Web.Stellar.Request
import           Web.Stellar.Types

data TrustSetParams = TrustSetParams {
  _paymentAmount :: !APIAmount,
  _secret :: !Text,
  _account :: !Text,
  _flags :: !Int
} deriving (Eq, Show)

$(makeLenses ''TrustSetParams)

instance ToJSON TrustSetParams where
  toJSON p = object [
                "method" .= ("submit" :: Text),
                "params" .= [object [
                  "secret" .= (p ^. secret),
                  "flags" .= (p ^. flags),
                  "tx_json" .= object [
                    "TransactionType" .= ("TrustSet" :: Text),
                    "Account" .= (p ^. account),
                    "LimitAmount" .= (p ^. paymentAmount)
                  ]
                ]]
              ]

defaultTrustSetParams :: TrustSetParams
defaultTrustSetParams = TrustSetParams (WithCurrency "" "" 0) "" "" 0

setTrust :: StellarEndpoint -> TrustSetParams -> IO (Maybe SubmissionResponse)
setTrust e p = makeRequest e p >>= (return.decode)
