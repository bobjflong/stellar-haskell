{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Web.Stellar.TrustLine where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Monoid
import           Data.Text
import           Prelude             hiding (sequence)
import           Web.Stellar.Request
import qualified Web.Stellar.Signing as S

data TrustSetParams = TrustSetParams {
  _paymentAmount :: APIAmount,
  _secret        :: Secret,
  _account       :: AccountID,
  _flags         :: !Flags,
  _sequence      :: !Sequence
} deriving (Eq, Show)

$(makeLenses ''TrustSetParams)

instance ToJSON TrustSetParams where
  toJSON p = object [
                "method" .= ("submit" :: Text),
                "params" .= [object [
                  "secret" .= (p ^. secret),
                  "tx_json" .= (txJSON p)
                ]]
              ]

txJSON :: TrustSetParams -> Value
txJSON p = object [
             "Flags" .= (p ^. flags),
             "TransactionType" .= ("TrustSet" :: Text),
             "Account" .= (p ^. account),
             "LimitAmount" .= (p ^. paymentAmount),
             "Sequence" .= (p ^. sequence)
           ]

instance S.SignableRequest TrustSetParams where
  txJSONToSign = txJSON
  secretToUse (flip (^.) secret -> Secret s) = s

defaultTrustSetParams :: TrustSetParams
defaultTrustSetParams = TrustSetParams defaultMoney mempty mempty (Flags 0) (Sequence 0)
  where defaultMoney = WithCurrency (CurrencyCode mempty) (Issuer mempty) 0

setTrust :: StellarEndpoint -> TrustSetParams -> IO (Maybe SubmissionResponse)
setTrust e p = makeRequest e p >>= (return.decode)
