{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductSubscriptionPeriod@.
module ObjC.StoreKit.SKProductSubscriptionPeriod
  ( SKProductSubscriptionPeriod
  , IsSKProductSubscriptionPeriod(..)
  , numberOfUnits
  , unit
  , numberOfUnitsSelector
  , unitSelector

  -- * Enum types
  , SKProductPeriodUnit(SKProductPeriodUnit)
  , pattern SKProductPeriodUnitDay
  , pattern SKProductPeriodUnitWeek
  , pattern SKProductPeriodUnitMonth
  , pattern SKProductPeriodUnitYear

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

-- | @- numberOfUnits@
numberOfUnits :: IsSKProductSubscriptionPeriod skProductSubscriptionPeriod => skProductSubscriptionPeriod -> IO CULong
numberOfUnits skProductSubscriptionPeriod =
  sendMessage skProductSubscriptionPeriod numberOfUnitsSelector

-- | @- unit@
unit :: IsSKProductSubscriptionPeriod skProductSubscriptionPeriod => skProductSubscriptionPeriod -> IO SKProductPeriodUnit
unit skProductSubscriptionPeriod =
  sendMessage skProductSubscriptionPeriod unitSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfUnits@
numberOfUnitsSelector :: Selector '[] CULong
numberOfUnitsSelector = mkSelector "numberOfUnits"

-- | @Selector@ for @unit@
unitSelector :: Selector '[] SKProductPeriodUnit
unitSelector = mkSelector "unit"

