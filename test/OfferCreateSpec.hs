{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module OfferCreateSpec (spec) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad       hiding (sequence)
import           Data.Aeson
import           Data.Fixed
import           Data.Maybe
import           Data.Text
import           Prelude             hiding (sequence)
import           System.IO.Unsafe
import           Test.Hspec
import           Web.Stellar.Offer
import           Web.Stellar.Signing
import           Web.Stellar.Types

spec :: Spec
spec = do
  describe "Offer signing/encoding" $ do
    it "correctly signs offer create params" $ do
      result <- signRequest (Endpoint "http://localhost:5005") (toSignRequest offerCreateParams1)
      (show result) `shouldBe` offerCreate1SignsResult
    it "correctly encodes offer create params" $ do
      (encode offerCreateParams1) `shouldBe` offerCreateParams1Encoded

gets = WithCurrency (CurrencyCode "USD") (Issuer "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb") 1
pays = WithCurrency (CurrencyCode "BTC") (Issuer "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb") 1
offerCreateParams1 = defaultOfferParams & takerGets .~ gets &
                                          takerPays .~ pays &
                                          sequence .~ 123 &
                                          account .~ "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb" &
                                          secret .~ "s3q5ZGX2ScQK2rJ4JATp7rND6X5npG3De8jMbB7tuvm2HAVHcCN"

offerCreateParams1Encoded = "{\"params\":[{\"secret\":\"s3q5ZGX2ScQK2rJ4JATp7rND6X5npG3De8jMbB7tuvm2HAVHcCN\",\"tx_json\":{\"TransactionType\":\"OfferCreate\",\"TakerPays\":{\"value\":\"1\",\"currency\":\"BTC\",\"issuer\":\"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb\"},\"Account\":\"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb\",\"TakerGets\":{\"value\":\"1\",\"currency\":\"USD\",\"issuer\":\"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb\"},\"Sequence\":123}}],\"method\":\"submit\"}"

offerCreate1SignsResult = "Just (SignResponse {_blob = \"1200072280000000240000007B64D4838D7EA4C68000000000000000000000000000425443000000000037B1B26BE3C91C55D51586C3F0E5C4B03E9CEA7F65D4838D7EA4C68000000000000000000000000000555344000000000037B1B26BE3C91C55D51586C3F0E5C4B03E9CEA7F68400000000000000A7320BE3900393891A2A2244E28A82C43BA94CA94DD6BFE36D523576A22BFF86055D474402A62573F0AD813B22A4B71E34EF98379686E1272326CEFA9A520CD682BED9C9821E73286B5C19D46808619FC333B76C0CA5101BA797CBA984929D15C90550407811437B1B26BE3C91C55D51586C3F0E5C4B03E9CEA7F\"})"
