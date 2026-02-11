{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductDiscount@.
module ObjC.StoreKit.SKProductDiscount
  ( SKProductDiscount
  , IsSKProductDiscount(..)
  , price
  , priceLocale
  , identifier
  , subscriptionPeriod
  , numberOfPeriods
  , paymentMode
  , type_
  , priceSelector
  , priceLocaleSelector
  , identifierSelector
  , subscriptionPeriodSelector
  , numberOfPeriodsSelector
  , paymentModeSelector
  , typeSelector

  -- * Enum types
  , SKProductDiscountPaymentMode(SKProductDiscountPaymentMode)
  , pattern SKProductDiscountPaymentModePayAsYouGo
  , pattern SKProductDiscountPaymentModePayUpFront
  , pattern SKProductDiscountPaymentModeFreeTrial
  , SKProductDiscountType(SKProductDiscountType)
  , pattern SKProductDiscountTypeIntroductory
  , pattern SKProductDiscountTypeSubscription

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
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- price@
price :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id NSDecimalNumber)
price skProductDiscount  =
  sendMsg skProductDiscount (mkSelector "price") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- priceLocale@
priceLocale :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id NSLocale)
priceLocale skProductDiscount  =
  sendMsg skProductDiscount (mkSelector "priceLocale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id NSString)
identifier skProductDiscount  =
  sendMsg skProductDiscount (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subscriptionPeriod@
subscriptionPeriod :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id SKProductSubscriptionPeriod)
subscriptionPeriod skProductDiscount  =
  sendMsg skProductDiscount (mkSelector "subscriptionPeriod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfPeriods@
numberOfPeriods :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO CULong
numberOfPeriods skProductDiscount  =
  sendMsg skProductDiscount (mkSelector "numberOfPeriods") retCULong []

-- | @- paymentMode@
paymentMode :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO SKProductDiscountPaymentMode
paymentMode skProductDiscount  =
  fmap (coerce :: CULong -> SKProductDiscountPaymentMode) $ sendMsg skProductDiscount (mkSelector "paymentMode") retCULong []

-- | @- type@
type_ :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO SKProductDiscountType
type_ skProductDiscount  =
  fmap (coerce :: CULong -> SKProductDiscountType) $ sendMsg skProductDiscount (mkSelector "type") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @price@
priceSelector :: Selector
priceSelector = mkSelector "price"

-- | @Selector@ for @priceLocale@
priceLocaleSelector :: Selector
priceLocaleSelector = mkSelector "priceLocale"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @subscriptionPeriod@
subscriptionPeriodSelector :: Selector
subscriptionPeriodSelector = mkSelector "subscriptionPeriod"

-- | @Selector@ for @numberOfPeriods@
numberOfPeriodsSelector :: Selector
numberOfPeriodsSelector = mkSelector "numberOfPeriods"

-- | @Selector@ for @paymentMode@
paymentModeSelector :: Selector
paymentModeSelector = mkSelector "paymentMode"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

