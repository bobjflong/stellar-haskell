{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module BasicProperties where

import           Control.Lens            hiding (elements)
import           Data.Text
import           Debug.Trace
import           Test.QuickCheck
import           Web.Stellar.AccountLine
import           Web.Stellar.Types

genNumericText :: Gen Text
genNumericText = fmap pack $ listOf $ elements ['1'..'9']

genSafeText :: Gen Text
genSafeText = fmap pack $ listOf $ elements ['a'..'z']

{-
 - Properties to check moneyLens
 - Which converts between Text and Money (Fixed E12)
-}
instance Arbitrary AccountLine where
  arbitrary = do acc <- genSafeText
                 bal <- genNumericText
                 currency <- genSafeText
                 limit <- genNumericText
                 limitPeer <- genNumericText
                 return $ AccountLine acc bal currency limit limitPeer

settingWhatYouGetInAccountLine :: (Lens' AccountLine (Maybe Money)) -> AccountLine -> Bool
settingWhatYouGetInAccountLine l v = v == (l .~ (v ^. l) $ v)

getBackWhatYouPutInAccountLine :: (Lens' AccountLine (Maybe Money)) -> Money -> AccountLine -> Bool
getBackWhatYouPutInAccountLine l m a = Just m == (l .~ (Just m) $ a) ^. l

settingTwiceAccountLine :: (Lens' AccountLine (Maybe Money)) -> Money -> AccountLine -> Bool
settingTwiceAccountLine l m a = (l .~ (Just m) $ a) == (l .~ (Just m) $ (l .~ (Just m) $ a))

accountLineProperties = do
  quickCheck (settingWhatYouGetInAccountLine limit)
  >> quickCheck (settingWhatYouGetInAccountLine balance)
  >> quickCheck (settingWhatYouGetInAccountLine limitPeer)
  >> quickCheck (getBackWhatYouPutInAccountLine limit)
  >> quickCheck (getBackWhatYouPutInAccountLine balance)
  >> quickCheck (getBackWhatYouPutInAccountLine limitPeer)
  >> quickCheck (settingTwiceAccountLine balance)
  >> quickCheck (settingTwiceAccountLine limit)
  >> quickCheck (settingTwiceAccountLine limitPeer)

main = do
  accountLineProperties
