{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Types (
    Money,
    moneyLens,
    simpleRequest,
    method,
    accountId
  ) where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Fixed
import           Data.Text

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

data SimpleRequest = SimpleRequest {
  _method    :: Text,
  _accountId :: Text
}

$(makeLenses ''SimpleRequest)

instance ToJSON SimpleRequest where
  toJSON req = object [
    "method" .= (req ^. method),
    "params" .= [object ["account" .= (req ^. accountId)]]]

simpleRequest :: SimpleRequest
simpleRequest = SimpleRequest "" ""
