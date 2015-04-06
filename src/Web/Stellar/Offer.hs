{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Web.Stellar.Offer where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Monoid
import           Data.Text
import           Prelude             hiding (sequence)
import           Web.Stellar.Request
import qualified Web.Stellar.Signing as S

data OfferCreateParams = OfferCreateParams {
  _account   :: AccountID,
  _takerGets :: APIAmount,
  _takerPays :: APIAmount,
  _sequence  :: !Sequence,
  _secret    :: Secret
} deriving (Eq, Show)

$(makeLenses ''OfferCreateParams)

instance ToJSON OfferCreateParams where
  toJSON p = object [
               "method" .= ("submit" :: Text),
               "params" .= [object [
                 "secret" .= (p ^. secret),
                 "tx_json" .= (txJSON p)
               ]]
            ]

txJSON :: OfferCreateParams -> Value
txJSON p = object [
             "TransactionType" .= ("OfferCreate" :: Text),
             "Account" .= (p ^. account),
             "TakerGets" .= (p ^. takerGets),
             "TakerPays" .= (p ^. takerPays),
             "Sequence" .= (p ^. sequence)
           ]

-- | Default offer parameters
defaultOfferParams :: OfferCreateParams
defaultOfferParams = OfferCreateParams mempty defaultMoney defaultMoney (Sequence 0) mempty
  where defaultMoney = WithCurrency (CurrencyCode mempty) (Issuer mempty) 0

instance S.SignableRequest OfferCreateParams where
  txJSONToSign = txJSON
  secretToUse (flip (^.) secret -> Secret s) = s

offerCreate :: StellarEndpoint -> OfferCreateParams -> IO (Maybe SubmissionResponse)
offerCreate e p = makeRequest e p >>= (return.decode)
