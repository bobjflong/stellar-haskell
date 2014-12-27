{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Offer where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Monoid
import           Data.Text
import           Prelude             hiding (sequence)
import           Web.Stellar.Request
import           Web.Stellar.Types

data OfferCreateParams = OfferCreateParams {
  _account   :: Text,
  _takerGets :: APIAmount,
  _takerPays :: APIAmount,
  _sequence  :: !Int,
  _secret    :: Text
} deriving (Eq, Show)

$(makeLenses ''OfferCreateParams)

instance ToJSON OfferCreateParams where
  toJSON p = object [
               "method" .= ("submit" :: Text),
               "params" .= [object [
                 "secret" .= (p ^. secret),
                 "tx_json" .= object [
                   "TransactionType" .= ("OfferCreate" :: Text),
                   "Account" .= (p ^. account),
                   "TakerGets" .= (p ^. takerGets),
                   "TakerPays" .= (p ^. takerPays),
                   "Sequence" .= (p ^. sequence)
                 ]
               ]]
            ]

-- | Default offer parameters
defaultOfferParams :: OfferCreateParams
defaultOfferParams = OfferCreateParams mempty defaultMoney defaultMoney 0 mempty
  where defaultMoney = WithCurrency (CurrencyCode mempty) (Issuer mempty) 0

offerCreate :: StellarEndpoint -> OfferCreateParams -> IO (Maybe SubmissionResponse)
offerCreate e p = makeRequest e p >>= (return.decode)
