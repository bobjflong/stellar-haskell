{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Payment (
    APIAmount(..),
    PaymentParams(..),
    defaultPaymentParams,
    makePayment,
    paymentAmount,
    secret,
    fromAccount,
    toAccount
  ) where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Fixed
import           Data.Text
import           Web.Stellar.Request
import           Web.Stellar.Types

-- | Describes an API request to send a payment
--
-- >>> defaultPaymentParams & paymentAmount .~ (WithMicroStellars 1) & 
--                            secret .~ "..." &
--                            fromAccount .~ "..." &
--                            toAccount .~ "..."
data PaymentParams = PaymentParams {
  _paymentAmount :: !APIAmount,
  _secret :: Text,
  _fromAccount :: Text,
  _toAccount :: Text
} deriving (Eq, Show)

$(makeLenses ''PaymentParams)

instance ToJSON PaymentParams where
  toJSON p = object [
                "method" .= ("submit" :: Text),
                "params" .= [object [
                  "secret" .= (p ^. secret),
                  "tx_json" .= object [
                    "TransactionType" .= ("Payment" :: Text),
                    "Account" .= (p ^. fromAccount),
                    "Destination" .= (p ^. toAccount),
                    "Amount" .= (p ^. paymentAmount)
                  ]
               ]]
             ]

-- | Default payment parameters
defaultPaymentParams :: PaymentParams
defaultPaymentParams = PaymentParams (WithMicroStellars 0) "" "" ""

-- | Make a payment by passing an endpoint and some PaymentParams
--
-- >>> r <- makePayment "https://test.stellar.org:9002" defaultPaymentParams
makePayment :: StellarEndpoint -> PaymentParams -> IO (Maybe SubmissionResponse)
makePayment e p = do
  paymentData <- makeRequest e p
  return $ decode paymentData 
