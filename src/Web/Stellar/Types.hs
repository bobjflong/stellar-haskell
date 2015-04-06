{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Types (
    AccountID(..),
    Money,
    StellarEndpoint(..),
    moneyLens,
    CurrencyCode(..),
    Issuer(..),
    simpleRequest,
    method,
    accountId,
    SubmissionResponse,
    SubmissionStatus(..),
    errorMessage,
    status,
    APIAmount(..),
    Flags(..),
    Secret(..),
    Sequence(..)
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Monoid
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Fixed
import           Data.Text
import           GHC.Generics        (Generic)

newtype AccountID = AccountID Text deriving (Eq, Show, Generic)

instance ToJSON AccountID

instance Monoid AccountID where
  mempty = AccountID mempty
  mappend (AccountID m) (AccountID m') = AccountID (mappend m m')

newtype Secret = Secret Text deriving (Eq, Show, Generic)

instance Monoid Secret where
  mempty = Secret mempty
  mappend (Secret m) (Secret m') = Secret (mappend m m')

instance ToJSON Secret

newtype Sequence = Sequence Int deriving (Eq, Show, Generic)
instance ToJSON Sequence

newtype StellarEndpoint = Endpoint Text deriving (Eq, Show)

type Money = Fixed E12

newtype Flags = Flags Int deriving (Eq, Show, Generic)
instance ToJSON Flags

moneyLens :: (Lens' a Text) -> Lens' a (Maybe Money)
moneyLens l = lens g s
  where g v = textToFixed $ v ^. l
        s v (Just x) = l .~ (pack $ showFixed True x) $ v
        s v _ = l .~ "" $ v

textToFixed :: Text -> Maybe Money
textToFixed t = case ((reads $ unpack t) :: [(Money, String)]) of
  [(a,"")] -> Just a
  _ -> Nothing


newtype Issuer = Issuer Text deriving (Eq, Show)
instance ToJSON Issuer where toJSON (Issuer t) = String t

newtype CurrencyCode = CurrencyCode Text deriving (Eq, Show)
instance ToJSON CurrencyCode where toJSON (CurrencyCode t) = String t
-- | An API representation of money to be sent
-- Can either describe the currency; issuer; value - or be an amount in microstellars
data APIAmount = WithCurrency CurrencyCode Issuer Money
                 | WithMicroStellars Money
                 deriving (Eq, Show)

instance ToJSON APIAmount where
  toJSON (WithCurrency c i a) = object [
      "currency" .= c,
      "value" .= (showFixed True a),
      "issuer" .= i
    ]
  toJSON (WithMicroStellars s) = String $ pack $ showFixed True s

data SimpleRequest = SimpleRequest {
  _method    :: Text,
  _accountId :: Text
}

$(makeLenses ''SimpleRequest)

instance ToJSON SimpleRequest where
  toJSON req = object [
    "method" .= (req ^. method),
    "params" .= [object ["account" .= (req ^. accountId)]]]

simpleRequest :: SimpleRequest
simpleRequest = SimpleRequest "" ""

-- | Represents the returned status code after submission
data SubmissionStatus = SubmissionSuccess | SubmissionError deriving (Eq, Show)

-- | Represents an entire response after payment
--
-- >>> result ^. status
-- SubmissionSuccess
--
-- >>> result ^. errorMessage
-- Nothing
data SubmissionResponse = SubmissionResponse {
  _status       :: !SubmissionStatus,
  _errorMessage :: Maybe Text
} deriving (Eq, Show)

$(makeLenses ''SubmissionResponse)

instance FromJSON SubmissionResponse where
  parseJSON (Object v) = do
    code <- engineCode
    stellarStatusResult <- stellarStatus
    case code of
      -- Select between the engine_message or the error_message heuristically using the
      -- engine_status_code
      0 ->
        case stellarStatusResult of
          "success" -> SubmissionResponse SubmissionSuccess <$> givenErrorMessage
          _ -> SubmissionResponse SubmissionError <$> givenErrorMessage
      _ -> SubmissionResponse SubmissionError <$> engineMessage
    where r = (v .: "result")
          stellarStatus = r >>= (.: "status") :: Parser String
          givenErrorMessage = r >>= (.:? "error_message")
          engineCode = r >>= (\o -> o .:? "engine_result_code" .!= 0) :: Parser Int
          engineMessage = r >>= (.:? "engine_result_message")
  parseJSON _ = mzero
