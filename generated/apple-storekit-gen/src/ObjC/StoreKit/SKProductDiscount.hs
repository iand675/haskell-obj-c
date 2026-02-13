{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , identifierSelector
  , numberOfPeriodsSelector
  , paymentModeSelector
  , priceLocaleSelector
  , priceSelector
  , subscriptionPeriodSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- price@
price :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id NSDecimalNumber)
price skProductDiscount =
  sendMessage skProductDiscount priceSelector

-- | @- priceLocale@
priceLocale :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id NSLocale)
priceLocale skProductDiscount =
  sendMessage skProductDiscount priceLocaleSelector

-- | @- identifier@
identifier :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id NSString)
identifier skProductDiscount =
  sendMessage skProductDiscount identifierSelector

-- | @- subscriptionPeriod@
subscriptionPeriod :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO (Id SKProductSubscriptionPeriod)
subscriptionPeriod skProductDiscount =
  sendMessage skProductDiscount subscriptionPeriodSelector

-- | @- numberOfPeriods@
numberOfPeriods :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO CULong
numberOfPeriods skProductDiscount =
  sendMessage skProductDiscount numberOfPeriodsSelector

-- | @- paymentMode@
paymentMode :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO SKProductDiscountPaymentMode
paymentMode skProductDiscount =
  sendMessage skProductDiscount paymentModeSelector

-- | @- type@
type_ :: IsSKProductDiscount skProductDiscount => skProductDiscount -> IO SKProductDiscountType
type_ skProductDiscount =
  sendMessage skProductDiscount typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @price@
priceSelector :: Selector '[] (Id NSDecimalNumber)
priceSelector = mkSelector "price"

-- | @Selector@ for @priceLocale@
priceLocaleSelector :: Selector '[] (Id NSLocale)
priceLocaleSelector = mkSelector "priceLocale"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @subscriptionPeriod@
subscriptionPeriodSelector :: Selector '[] (Id SKProductSubscriptionPeriod)
subscriptionPeriodSelector = mkSelector "subscriptionPeriod"

-- | @Selector@ for @numberOfPeriods@
numberOfPeriodsSelector :: Selector '[] CULong
numberOfPeriodsSelector = mkSelector "numberOfPeriods"

-- | @Selector@ for @paymentMode@
paymentModeSelector :: Selector '[] SKProductDiscountPaymentMode
paymentModeSelector = mkSelector "paymentMode"

-- | @Selector@ for @type@
typeSelector :: Selector '[] SKProductDiscountType
typeSelector = mkSelector "type"

