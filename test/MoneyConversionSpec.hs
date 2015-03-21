{-# LANGUAGE OverloadedStrings #-}

module MoneyConversionSpec (spec) where

import           Control.Lens            hiding (elements)
import           Data.Text
import           Test.Hspec
import           Test.QuickCheck
import           Web.Stellar.AccountLine
import           Web.Stellar.Types

spec = do
  describe "MoneyLens laws" $ do
    it "sets what it gets" $
      property $ \v -> v == (balance .~ (v ^. balance) $ v)
    it "gets back what it puts in" $
      property $ \m a -> Just m == (balance .~ (Just m) $ a) ^. balance
    it "sets twice safely" $
      property $ \m a ->
                  (balance .~ (Just m) $ a) == (balance .~ (Just m) $ (balance .~ (Just m) $ a))
