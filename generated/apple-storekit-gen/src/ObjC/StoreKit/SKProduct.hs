{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProduct@.
module ObjC.StoreKit.SKProduct
  ( SKProduct
  , IsSKProduct(..)
  , localizedDescription
  , localizedTitle
  , price
  , priceLocale
  , productIdentifier
  , isDownloadable
  , downloadable
  , isFamilyShareable
  , contentLengths
  , downloadContentLengths
  , contentVersion
  , downloadContentVersion
  , subscriptionPeriod
  , introductoryPrice
  , subscriptionGroupIdentifier
  , discounts
  , contentLengthsSelector
  , contentVersionSelector
  , discountsSelector
  , downloadContentLengthsSelector
  , downloadContentVersionSelector
  , downloadableSelector
  , introductoryPriceSelector
  , isDownloadableSelector
  , isFamilyShareableSelector
  , localizedDescriptionSelector
  , localizedTitleSelector
  , priceLocaleSelector
  , priceSelector
  , productIdentifierSelector
  , subscriptionGroupIdentifierSelector
  , subscriptionPeriodSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- localizedDescription@
localizedDescription :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
localizedDescription skProduct =
  sendMessage skProduct localizedDescriptionSelector

-- | @- localizedTitle@
localizedTitle :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
localizedTitle skProduct =
  sendMessage skProduct localizedTitleSelector

-- | @- price@
price :: IsSKProduct skProduct => skProduct -> IO (Id NSDecimalNumber)
price skProduct =
  sendMessage skProduct priceSelector

-- | @- priceLocale@
priceLocale :: IsSKProduct skProduct => skProduct -> IO (Id NSLocale)
priceLocale skProduct =
  sendMessage skProduct priceLocaleSelector

-- | @- productIdentifier@
productIdentifier :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
productIdentifier skProduct =
  sendMessage skProduct productIdentifierSelector

-- | @- isDownloadable@
isDownloadable :: IsSKProduct skProduct => skProduct -> IO Bool
isDownloadable skProduct =
  sendMessage skProduct isDownloadableSelector

-- | @- downloadable@
downloadable :: IsSKProduct skProduct => skProduct -> IO Bool
downloadable skProduct =
  sendMessage skProduct downloadableSelector

-- | @- isFamilyShareable@
isFamilyShareable :: IsSKProduct skProduct => skProduct -> IO Bool
isFamilyShareable skProduct =
  sendMessage skProduct isFamilyShareableSelector

-- | @- contentLengths@
contentLengths :: IsSKProduct skProduct => skProduct -> IO (Id NSArray)
contentLengths skProduct =
  sendMessage skProduct contentLengthsSelector

-- | @- downloadContentLengths@
downloadContentLengths :: IsSKProduct skProduct => skProduct -> IO (Id NSArray)
downloadContentLengths skProduct =
  sendMessage skProduct downloadContentLengthsSelector

-- | @- contentVersion@
contentVersion :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
contentVersion skProduct =
  sendMessage skProduct contentVersionSelector

-- | @- downloadContentVersion@
downloadContentVersion :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
downloadContentVersion skProduct =
  sendMessage skProduct downloadContentVersionSelector

-- | @- subscriptionPeriod@
subscriptionPeriod :: IsSKProduct skProduct => skProduct -> IO (Id SKProductSubscriptionPeriod)
subscriptionPeriod skProduct =
  sendMessage skProduct subscriptionPeriodSelector

-- | @- introductoryPrice@
introductoryPrice :: IsSKProduct skProduct => skProduct -> IO (Id SKProductDiscount)
introductoryPrice skProduct =
  sendMessage skProduct introductoryPriceSelector

-- | @- subscriptionGroupIdentifier@
subscriptionGroupIdentifier :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
subscriptionGroupIdentifier skProduct =
  sendMessage skProduct subscriptionGroupIdentifierSelector

-- | @- discounts@
discounts :: IsSKProduct skProduct => skProduct -> IO (Id NSArray)
discounts skProduct =
  sendMessage skProduct discountsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector '[] (Id NSString)
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @price@
priceSelector :: Selector '[] (Id NSDecimalNumber)
priceSelector = mkSelector "price"

-- | @Selector@ for @priceLocale@
priceLocaleSelector :: Selector '[] (Id NSLocale)
priceLocaleSelector = mkSelector "priceLocale"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector '[] (Id NSString)
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @isDownloadable@
isDownloadableSelector :: Selector '[] Bool
isDownloadableSelector = mkSelector "isDownloadable"

-- | @Selector@ for @downloadable@
downloadableSelector :: Selector '[] Bool
downloadableSelector = mkSelector "downloadable"

-- | @Selector@ for @isFamilyShareable@
isFamilyShareableSelector :: Selector '[] Bool
isFamilyShareableSelector = mkSelector "isFamilyShareable"

-- | @Selector@ for @contentLengths@
contentLengthsSelector :: Selector '[] (Id NSArray)
contentLengthsSelector = mkSelector "contentLengths"

-- | @Selector@ for @downloadContentLengths@
downloadContentLengthsSelector :: Selector '[] (Id NSArray)
downloadContentLengthsSelector = mkSelector "downloadContentLengths"

-- | @Selector@ for @contentVersion@
contentVersionSelector :: Selector '[] (Id NSString)
contentVersionSelector = mkSelector "contentVersion"

-- | @Selector@ for @downloadContentVersion@
downloadContentVersionSelector :: Selector '[] (Id NSString)
downloadContentVersionSelector = mkSelector "downloadContentVersion"

-- | @Selector@ for @subscriptionPeriod@
subscriptionPeriodSelector :: Selector '[] (Id SKProductSubscriptionPeriod)
subscriptionPeriodSelector = mkSelector "subscriptionPeriod"

-- | @Selector@ for @introductoryPrice@
introductoryPriceSelector :: Selector '[] (Id SKProductDiscount)
introductoryPriceSelector = mkSelector "introductoryPrice"

-- | @Selector@ for @subscriptionGroupIdentifier@
subscriptionGroupIdentifierSelector :: Selector '[] (Id NSString)
subscriptionGroupIdentifierSelector = mkSelector "subscriptionGroupIdentifier"

-- | @Selector@ for @discounts@
discountsSelector :: Selector '[] (Id NSArray)
discountsSelector = mkSelector "discounts"

