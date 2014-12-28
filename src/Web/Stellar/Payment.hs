{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Payment (
    APIAmount(..),
    xaymentParams, 
    PaymentParams(..),
    defaultPaymentParams,
    makePayment,
    paymentAmount,
    secret,
    sequence,
    fromAccount,
    toAccount,
    toSignRequest
  ) where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Monoid
import           Data.Text
import           Prelude             hiding (sequence)
import           Web.Stellar.Request
import qualified Web.Stellar.Signing as S
import           Web.Stellar.Types

-- | Describes an API request to send a payment
--
-- >>> defaultPaymentParams & paymentAmount .~ (WithMicroStellars 1) &
--                            secret .~ "..." &
--                            fromAccount .~ "..." &
--                            toAccount .~ "..."
data PaymentParams = PaymentParams {
  _paymentAmount :: !APIAmount,
  _secret        :: Text,
  _fromAccount   :: Text,
  _toAccount     :: Text,
  _sequence      :: !Int
} deriving (Eq, Show)

$(makeLenses ''PaymentParams)

instance ToJSON PaymentParams where
  toJSON p = object [
               "method" .= ("submit" :: Text),
               "params" .= [object [
                 "secret" .= (p ^. secret),
                 "tx_json" .= (txJSON p)
               ]]
             ]

txJSON :: PaymentParams -> Value
txJSON p = object [
             "TransactionType" .= ("Payment" :: Text),
             "Account" .= (p ^. fromAccount),
             "Destination" .= (p ^. toAccount),
             "Amount" .= (p ^. paymentAmount),
             "Sequence" .= (p ^. sequence)
           ]

-- | Default payment parameters
defaultPaymentParams :: PaymentParams
defaultPaymentParams = PaymentParams (WithMicroStellars 0) mempty mempty mempty 0

-- | Create a request to sign payment parameters
toSignRequest :: PaymentParams -> S.SignRequest
toSignRequest p = S.defaultSignRequest & S.signSecret .~ (p ^. secret) &
                                         S.txJSON .~ (txJSON p)

-- | Make a payment by passing an endpoint and some PaymentParams
--
-- >>> r <- makePayment "https://test.stellar.org:9002" defaultPaymentParams
makePayment :: StellarEndpoint -> PaymentParams -> IO (Maybe SubmissionResponse)
makePayment e p = makeRequest e p >>= (return.decode)

xaymentParams = defaultPaymentParams & paymentAmount .~ (WithMicroStellars 1) &
                                       secret .~ "s3q5ZGX2ScQK2rJ4JATp7rND6X5npG3De8jMbB7tuvm2HAVHcCN" &
                                       fromAccount .~ "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb" &
                                       toAccount .~ "gHJPw9kW8v4BsUyDnBR8ZHWo8aEkhUMeAq" &
                                       Web.Stellar.Payment.sequence .~ 123
