{-# LANGUAGE OverloadedStrings #-}

import           System.IO.Unsafe
import           Test.Hspec
import           Web.Stellar.Request
import           Web.Stellar.Types

main :: IO ()
main = hspec $ do
  describe "pingStellar" $ do
    it "returns Right PingSuccess when everything is ok" $ do
      res <- pingStellar (Endpoint "http://localhost:5005")
      res `shouldBe` (Right PingSuccess)
    it "returns Left PingFailure when everything is not ok" $ do
      res <- pingStellar (Endpoint "https://google.com")
      res `shouldBe` (Left PingFailure)
