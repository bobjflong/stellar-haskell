{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module TrustLineSpec (spec) where

import           Data.Text
import           Prelude               hiding (sequence)
import           Test.Hspec
import           Test.HUnit
import           Web.Stellar.Signing
import           Web.Stellar.TrustLine
import           Web.Stellar.Types

spec =  do
  describe "Trust set signing" $ do
    it "should work" $ do
      signing <- signRequest (Endpoint "http://localhost:5005") (toSignRequest trustSetParams1)
      (show signing) `shouldBe` signResult

trustSetParams1 = TrustSetParams (WithMicroStellars 1) "s3q5ZGX2ScQK2rJ4JATp7rND6X5npG3De8jMbB7tuvm2HAVHcCN" "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb" 0 1
signResult = "Just (SignResponse {_blob = \"1200142200000000240000000163400000000000000168400000000000000A7320BE3900393891A2A2244E28A82C43BA94CA94DD6BFE36D523576A22BFF86055D474401D7FC2B242E7F8EE962B2F97785909CCBF7684DFDD4C338C5ACE7FD62E119ABC16E832A627686AFFC2900409A7622BF81F391FB852536CD9876EF9E160601701811437B1B26BE3C91C55D51586C3F0E5C4B03E9CEA7F\"})"
