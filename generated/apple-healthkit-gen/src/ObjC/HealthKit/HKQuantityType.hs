{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantityType
--
-- Represents types of HKQuantitySamples.
--
-- Generated bindings for @HKQuantityType@.
module ObjC.HealthKit.HKQuantityType
  ( HKQuantityType
  , IsHKQuantityType(..)
  , isCompatibleWithUnit
  , aggregationStyle
  , aggregationStyleSelector
  , isCompatibleWithUnitSelector

  -- * Enum types
  , HKQuantityAggregationStyle(HKQuantityAggregationStyle)
  , pattern HKQuantityAggregationStyleCumulative
  , pattern HKQuantityAggregationStyleDiscreteArithmetic
  , pattern HKQuantityAggregationStyleDiscrete
  , pattern HKQuantityAggregationStyleDiscreteTemporallyWeighted
  , pattern HKQuantityAggregationStyleDiscreteEquivalentContinuousLevel

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | isCompatibleWithUnit:
--
-- Returns YES if the type of HKQuantitySample represented by the receiver can be created with quantities                 of the given unit.
--
-- ObjC selector: @- isCompatibleWithUnit:@
isCompatibleWithUnit :: (IsHKQuantityType hkQuantityType, IsHKUnit unit) => hkQuantityType -> unit -> IO Bool
isCompatibleWithUnit hkQuantityType unit =
  sendMessage hkQuantityType isCompatibleWithUnitSelector (toHKUnit unit)

-- | @- aggregationStyle@
aggregationStyle :: IsHKQuantityType hkQuantityType => hkQuantityType -> IO HKQuantityAggregationStyle
aggregationStyle hkQuantityType =
  sendMessage hkQuantityType aggregationStyleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isCompatibleWithUnit:@
isCompatibleWithUnitSelector :: Selector '[Id HKUnit] Bool
isCompatibleWithUnitSelector = mkSelector "isCompatibleWithUnit:"

-- | @Selector@ for @aggregationStyle@
aggregationStyleSelector :: Selector '[] HKQuantityAggregationStyle
aggregationStyleSelector = mkSelector "aggregationStyle"

