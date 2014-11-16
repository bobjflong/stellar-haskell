{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Payment (
    APIAmount(..),
    PaymentParams(..),
    defaultPaymentParams,
    PaymentStatus(..),
    PaymentResponse(..),
    makePayment,
    paymentAmount,
    secret,
    fromAccount,
    toAccount,
    status,
    errorMessage
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Fixed
import           Data.Text
import           Web.Stellar.Request
import           Web.Stellar.Types

-- | An API representation of money to be sent
-- Can either describe the currency; issuer; value - or be an amount in microstellars
data APIAmount = WithCurrency Text Text Money
                 | WithMicroStellars Money
                 deriving (Eq, Show)

instance ToJSON APIAmount where
  toJSON (WithCurrency c i a) = object [
      "currency" .= c,
      "value" .= (showFixed True a),
      "issuer" .= i
    ]
  toJSON (WithMicroStellars s) = String $ pack $ showFixed True s

-- | Describes an API request to send a payment
--
-- >>> defaultPaymentParams & paymentAmount .~ (WithMicroStellars 1) & 
--                            secret .~ "..." &
--                            fromAccount .~ "..." &
--                            toAccount .~ "..."
data PaymentParams = PaymentParams {
  _paymentAmount :: APIAmount,
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

-- | Represents the returned status code after payment
data PaymentStatus = PaymentSuccess | PaymentError deriving (Eq, Show)

-- | Represents an entire response after payment
--
-- >>> result ^. status
-- PaymentSuccess
--
-- >>> result ^. errorMessage
-- Nothing
data PaymentResponse = PaymentResponse {
  _statusData :: Text,
  _errorMessage :: Maybe Text
} deriving (Eq, Show)

$(makeLenses ''PaymentResponse)

status :: Lens' PaymentResponse PaymentStatus
status = lens getStatus setStatus
  where getStatus p = if ((p ^. statusData) == ok) then PaymentSuccess else PaymentError
        setStatus p PaymentSuccess = statusData .~ ok $ p
        setStatus p PaymentError = statusData .~ notOk $ p
        ok = "success"
        notOk = "error"

instance FromJSON PaymentResponse where
  parseJSON (Object v) = do
    PaymentResponse <$> (r >>= (.: "status")) <*> (r >>= (.:? "error_message"))
    where r = (v .: "result")
  parseJSON _ = mzero

-- | Make a payment by passing an endpoint and some PaymentParams
--
-- >>> r <- makePayment "https://test.stellar.org:9002" defaultPaymentParams
makePayment :: StellarEndpoint -> PaymentParams -> IO (Maybe PaymentResponse)
makePayment e p = do
  paymentData <- makeRequest e p
  putStrLn $ show $ paymentData
  return $ decode paymentData 
