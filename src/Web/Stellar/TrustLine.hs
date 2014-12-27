{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.TrustLine where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Monoid
import           Data.Text
import           Prelude             hiding (sequence)
import           Web.Stellar.Request
import           Web.Stellar.Types

data TrustSetParams = TrustSetParams {
  _paymentAmount :: APIAmount,
  _secret        :: Text,
  _account       :: Text,
  _flags         :: !Int,
  _sequence      :: !Int
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
                    "LimitAmount" .= (p ^. paymentAmount),
                    "Sequence" .= (p ^. sequence)
                  ]
                ]]
              ]

defaultTrustSetParams :: TrustSetParams
defaultTrustSetParams = TrustSetParams defaultMoney mempty mempty 0 0
  where defaultMoney = WithCurrency (CurrencyCode mempty) (Issuer mempty) 0

setTrust :: StellarEndpoint -> TrustSetParams -> IO (Maybe SubmissionResponse)
setTrust e p = makeRequest e p >>= (return.decode)
