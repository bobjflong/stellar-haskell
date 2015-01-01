{-# LANGUAGE OverloadedStrings #-}

module SubmissionErrors (

  ) where

import           Control.Applicative
import           Control.Lens               hiding ((.=))
import           Control.Monad              hiding (sequence)
import           Data.Aeson
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Maybe
import           Data.Maybe
import           Data.Text
import           Prelude                    hiding (sequence)
import           System.IO.Unsafe
import           Test.HUnit
import           Web.Stellar.Types

sequenceError :: SubmissionResponse
sequenceError = (fromJust.decode.BLC.pack) (unsafePerformIO $ readFile "test/fixtures/submission_sequence_error.json")

submissionOK :: SubmissionResponse
submissionOK = (fromJust.decode.BLC.pack) (unsafePerformIO $ readFile "test/fixtures/no_error.json")

stellarError :: SubmissionResponse
stellarError = (fromJust.decode.BLC.pack) (unsafePerformIO $ readFile "test/fixtures/stellar_error.json")

sequenceErrorNotSuccess = TestCase (
                            assertEqual
                            "Engine errors should not be considered successful even though status = success in the json"
                            SubmissionError
                            (sequenceError ^. status)
                          )

sequenceErrorMessage = TestCase (
                         assertEqual
                         "Engine messages should show up in the errorMessage field"
                         "This sequence number has already past."
                         (fromJust $ sequenceError ^. errorMessage)
                       )

checkSuccess = TestCase (
                 assertEqual
                 "Error handling should not interfere with successes"
                 SubmissionSuccess
                 (submissionOK ^. status)
               )

checkStellarError = TestCase (
                      assertEqual
                      "Stellar errors should cause a SubmissionError"
                      SubmissionError
                      (stellarError ^. status)
                    )

tests = TestList [
          TestLabel "sequenceError should not be successful" sequenceErrorNotSuccess,
          TestLabel "engine messages should present as errorMessage" sequenceErrorMessage,
          TestLabel "error handling should not interfere with successes" checkSuccess,
          TestLabel "Stellar errors should cause a SubmissionError" checkStellarError
        ]

main = runTestTT tests
