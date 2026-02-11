{-# LANGUAGE PatternSynonyms #-}
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

-- | @- numberOfUnits@
numberOfUnits :: IsSKProductSubscriptionPeriod skProductSubscriptionPeriod => skProductSubscriptionPeriod -> IO CULong
numberOfUnits skProductSubscriptionPeriod  =
  sendMsg skProductSubscriptionPeriod (mkSelector "numberOfUnits") retCULong []

-- | @- unit@
unit :: IsSKProductSubscriptionPeriod skProductSubscriptionPeriod => skProductSubscriptionPeriod -> IO SKProductPeriodUnit
unit skProductSubscriptionPeriod  =
  fmap (coerce :: CULong -> SKProductPeriodUnit) $ sendMsg skProductSubscriptionPeriod (mkSelector "unit") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfUnits@
numberOfUnitsSelector :: Selector
numberOfUnitsSelector = mkSelector "numberOfUnits"

-- | @Selector@ for @unit@
unitSelector :: Selector
unitSelector = mkSelector "unit"

