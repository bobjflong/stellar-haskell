{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PaymentSpec (spec) where

import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Maybe
import           Data.Text
import           Prelude             hiding (sequence)
import           Test.Hspec
import           Web.Stellar.Payment
import           Web.Stellar.Types

spec = do
  describe "Payment encoding/decoding" $ do
    it "correctly encodes payment params" $
      (encode paymentParams1) `shouldBe` paymentEncodeResult
    it "correctly decodes payment results" $
      ((fromJust ((decode result1) :: Maybe SubmissionResponse)) ^. status) `shouldBe` SubmissionSuccess

paymentEncodeResult = "{\"params\":\
                        \[{\"secret\":\"2secret\"\
                         \,\"tx_json\":\
                             \{\"TransactionType\":\"Payment\"\
                             \,\"Amount\":\
                                 \{\"value\":\"1\"\
                                 \,\"currency\":\"USD\"\
                                 \,\"issuer\":\"abc\"}\
                                 \,\"Destination\":\"luckyduck\"\
                                 \,\"Account\":\"moneybags\"\
                                 \,\"Sequence\":4}\
                          \}]\
                       \,\"method\":\"submit\"}"

amount1 = WithCurrency (CurrencyCode "USD") (Issuer "abc") 1
paymentParams1 = defaultPaymentParams & paymentAmount .~ amount1 &
                 secret .~ "2secret" &
                 fromAccount .~ "moneybags" &
                 toAccount .~ "luckyduck" &
                 sequence .~ 4

result1 = "{\"result\":\
            \{\"engine_result\":\"tesSUCCESS\"\
            \,\"engine_result_code\":0\
            \,\"engine_result_message\":\"The transaction was applied.\"\
            \,\"status\":\"success\"\
            \,\"tx_blob\":\"abc\"\
            \,\"tx_json\":\
              \{\"Account\":\"abc\"\
              \,\"Amount\":\
                \{\"currency\":\"USD\"\
                \,\"issuer\":\"abc\"\
                \,\"value\":\"1\"}\
              \,\"Destination\":\"abc\"\
              \,\"Fee\":\"10\"\
              \,\"Flags\":2147483648\
              \,\"Sequence\":2967\
              \,\"SigningPubKey\":\"abc\"\
              \,\"TransactionType\":\"Payment\"\
              \,\"TxnSignature\":\"abc\"\
              \,\"hash\":\"abc\"\
            \}}}\r"
