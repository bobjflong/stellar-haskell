{-# LANGUAGE OverloadedStrings #-}

module Submission where

import           Control.Lens               hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe
import           Data.Text
import           Prelude                    hiding (sequence)
import           System.IO.Unsafe
import           Test.Hspec
import           Test.HUnit
import           Web.Stellar.Types

main = hspec $ do
  describe "engine errors" $ do
    it "should report as an error" $ do
      (sequenceError ^. status) `shouldBe` SubmissionError
    it "should give a correct error message" $ do
      (fromJust $ sequenceError ^. errorMessage) `shouldBe` "This sequence number has already past."
  describe "stellar errors" $ do
    it "should report as an error" $ do
      (stellarError ^. status) `shouldBe` SubmissionError
  describe "normal operation" $ do
    it "should be ok" $ do
      (submissionOK ^. status) `shouldBe` SubmissionSuccess

sequenceError :: SubmissionResponse
sequenceError = (fromJust.decode.BLC.pack) (unsafePerformIO $ readFile "test/fixtures/submission_sequence_error.json")

submissionOK :: SubmissionResponse
submissionOK = (fromJust.decode.BLC.pack) (unsafePerformIO $ readFile "test/fixtures/no_error.json")

stellarError :: SubmissionResponse
stellarError = (fromJust.decode.BLC.pack) (unsafePerformIO $ readFile "test/fixtures/stellar_error.json")

