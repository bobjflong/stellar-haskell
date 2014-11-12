{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Types (
    Money,
    moneyLens,
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Fixed
import           Data.Text
import           Debug.Trace

type Money = Fixed E12

moneyLens :: (Lens' a Text) -> Lens' a (Maybe Money)
moneyLens l = lens g s
  where g v = textToFixed $ v ^. l
        s v (Just x) = l .~ (pack $ showFixed True x) $ v
        s v _ = l .~ "" $ v

textToFixed :: Text -> Maybe Money
textToFixed t = case ((reads $ unpack t) :: [(Money, String)]) of
  [(a,"")] -> Just a
  _ -> Nothing
