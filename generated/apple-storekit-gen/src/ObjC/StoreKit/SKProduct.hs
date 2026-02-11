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
  , localizedDescriptionSelector
  , localizedTitleSelector
  , priceSelector
  , priceLocaleSelector
  , productIdentifierSelector
  , isDownloadableSelector
  , downloadableSelector
  , isFamilyShareableSelector
  , contentLengthsSelector
  , downloadContentLengthsSelector
  , contentVersionSelector
  , downloadContentVersionSelector
  , subscriptionPeriodSelector
  , introductoryPriceSelector
  , subscriptionGroupIdentifierSelector
  , discountsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- localizedDescription@
localizedDescription :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
localizedDescription skProduct  =
    sendMsg skProduct (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedTitle@
localizedTitle :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
localizedTitle skProduct  =
    sendMsg skProduct (mkSelector "localizedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- price@
price :: IsSKProduct skProduct => skProduct -> IO (Id NSDecimalNumber)
price skProduct  =
    sendMsg skProduct (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- priceLocale@
priceLocale :: IsSKProduct skProduct => skProduct -> IO (Id NSLocale)
priceLocale skProduct  =
    sendMsg skProduct (mkSelector "priceLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- productIdentifier@
productIdentifier :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
productIdentifier skProduct  =
    sendMsg skProduct (mkSelector "productIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isDownloadable@
isDownloadable :: IsSKProduct skProduct => skProduct -> IO Bool
isDownloadable skProduct  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skProduct (mkSelector "isDownloadable") retCULong []

-- | @- downloadable@
downloadable :: IsSKProduct skProduct => skProduct -> IO Bool
downloadable skProduct  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skProduct (mkSelector "downloadable") retCULong []

-- | @- isFamilyShareable@
isFamilyShareable :: IsSKProduct skProduct => skProduct -> IO Bool
isFamilyShareable skProduct  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skProduct (mkSelector "isFamilyShareable") retCULong []

-- | @- contentLengths@
contentLengths :: IsSKProduct skProduct => skProduct -> IO (Id NSArray)
contentLengths skProduct  =
    sendMsg skProduct (mkSelector "contentLengths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- downloadContentLengths@
downloadContentLengths :: IsSKProduct skProduct => skProduct -> IO (Id NSArray)
downloadContentLengths skProduct  =
    sendMsg skProduct (mkSelector "downloadContentLengths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- contentVersion@
contentVersion :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
contentVersion skProduct  =
    sendMsg skProduct (mkSelector "contentVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- downloadContentVersion@
downloadContentVersion :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
downloadContentVersion skProduct  =
    sendMsg skProduct (mkSelector "downloadContentVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subscriptionPeriod@
subscriptionPeriod :: IsSKProduct skProduct => skProduct -> IO (Id SKProductSubscriptionPeriod)
subscriptionPeriod skProduct  =
    sendMsg skProduct (mkSelector "subscriptionPeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- introductoryPrice@
introductoryPrice :: IsSKProduct skProduct => skProduct -> IO (Id SKProductDiscount)
introductoryPrice skProduct  =
    sendMsg skProduct (mkSelector "introductoryPrice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subscriptionGroupIdentifier@
subscriptionGroupIdentifier :: IsSKProduct skProduct => skProduct -> IO (Id NSString)
subscriptionGroupIdentifier skProduct  =
    sendMsg skProduct (mkSelector "subscriptionGroupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- discounts@
discounts :: IsSKProduct skProduct => skProduct -> IO (Id NSArray)
discounts skProduct  =
    sendMsg skProduct (mkSelector "discounts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @localizedTitle@
localizedTitleSelector :: Selector
localizedTitleSelector = mkSelector "localizedTitle"

-- | @Selector@ for @price@
priceSelector :: Selector
priceSelector = mkSelector "price"

-- | @Selector@ for @priceLocale@
priceLocaleSelector :: Selector
priceLocaleSelector = mkSelector "priceLocale"

-- | @Selector@ for @productIdentifier@
productIdentifierSelector :: Selector
productIdentifierSelector = mkSelector "productIdentifier"

-- | @Selector@ for @isDownloadable@
isDownloadableSelector :: Selector
isDownloadableSelector = mkSelector "isDownloadable"

-- | @Selector@ for @downloadable@
downloadableSelector :: Selector
downloadableSelector = mkSelector "downloadable"

-- | @Selector@ for @isFamilyShareable@
isFamilyShareableSelector :: Selector
isFamilyShareableSelector = mkSelector "isFamilyShareable"

-- | @Selector@ for @contentLengths@
contentLengthsSelector :: Selector
contentLengthsSelector = mkSelector "contentLengths"

-- | @Selector@ for @downloadContentLengths@
downloadContentLengthsSelector :: Selector
downloadContentLengthsSelector = mkSelector "downloadContentLengths"

-- | @Selector@ for @contentVersion@
contentVersionSelector :: Selector
contentVersionSelector = mkSelector "contentVersion"

-- | @Selector@ for @downloadContentVersion@
downloadContentVersionSelector :: Selector
downloadContentVersionSelector = mkSelector "downloadContentVersion"

-- | @Selector@ for @subscriptionPeriod@
subscriptionPeriodSelector :: Selector
subscriptionPeriodSelector = mkSelector "subscriptionPeriod"

-- | @Selector@ for @introductoryPrice@
introductoryPriceSelector :: Selector
introductoryPriceSelector = mkSelector "introductoryPrice"

-- | @Selector@ for @subscriptionGroupIdentifier@
subscriptionGroupIdentifierSelector :: Selector
subscriptionGroupIdentifierSelector = mkSelector "subscriptionGroupIdentifier"

-- | @Selector@ for @discounts@
discountsSelector :: Selector
discountsSelector = mkSelector "discounts"

