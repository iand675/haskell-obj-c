{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQuantity
--
-- The HKQuantity class provides an encapsulation of a quantity value and the unit of measurement.
--
-- Generated bindings for @HKQuantity@.
module ObjC.HealthKit.HKQuantity
  ( HKQuantity
  , IsHKQuantity(..)
  , init_
  , quantityWithUnit_doubleValue
  , isCompatibleWithUnit
  , doubleValueForUnit
  , compare_
  , compareSelector
  , doubleValueForUnitSelector
  , initSelector
  , isCompatibleWithUnitSelector
  , quantityWithUnit_doubleValueSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKQuantity hkQuantity => hkQuantity -> IO (Id HKQuantity)
init_ hkQuantity =
  sendOwnedMessage hkQuantity initSelector

-- | quantityWithUnit:doubleValue:
--
-- Returns a new object representing a quantity measurement with the given unit.
--
-- ObjC selector: @+ quantityWithUnit:doubleValue:@
quantityWithUnit_doubleValue :: IsHKUnit unit => unit -> CDouble -> IO (Id HKQuantity)
quantityWithUnit_doubleValue unit value =
  do
    cls' <- getRequiredClass "HKQuantity"
    sendClassMessage cls' quantityWithUnit_doubleValueSelector (toHKUnit unit) value

-- | isCompatibleWithUnit:
--
-- Returns yes if the receiver's value can be converted to a value of the given unit.
--
-- ObjC selector: @- isCompatibleWithUnit:@
isCompatibleWithUnit :: (IsHKQuantity hkQuantity, IsHKUnit unit) => hkQuantity -> unit -> IO Bool
isCompatibleWithUnit hkQuantity unit =
  sendMessage hkQuantity isCompatibleWithUnitSelector (toHKUnit unit)

-- | doubleValueForUnit:
--
-- Returns the quantity value converted to the given unit.
--
-- Throws an exception if the receiver's value cannot be converted to one of the requested unit.
--
-- ObjC selector: @- doubleValueForUnit:@
doubleValueForUnit :: (IsHKQuantity hkQuantity, IsHKUnit unit) => hkQuantity -> unit -> IO CDouble
doubleValueForUnit hkQuantity unit =
  sendMessage hkQuantity doubleValueForUnitSelector (toHKUnit unit)

-- | compare:
--
-- Returns an NSComparisonResult value that indicates whether the receiver is greater than, equal to, or                 less than a given quantity.
--
-- Throws an exception if the unit of the given quantity is not compatible with the receiver's unit.
--
-- ObjC selector: @- compare:@
compare_ :: (IsHKQuantity hkQuantity, IsHKQuantity quantity) => hkQuantity -> quantity -> IO NSComparisonResult
compare_ hkQuantity quantity =
  sendMessage hkQuantity compareSelector (toHKQuantity quantity)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKQuantity)
initSelector = mkSelector "init"

-- | @Selector@ for @quantityWithUnit:doubleValue:@
quantityWithUnit_doubleValueSelector :: Selector '[Id HKUnit, CDouble] (Id HKQuantity)
quantityWithUnit_doubleValueSelector = mkSelector "quantityWithUnit:doubleValue:"

-- | @Selector@ for @isCompatibleWithUnit:@
isCompatibleWithUnitSelector :: Selector '[Id HKUnit] Bool
isCompatibleWithUnitSelector = mkSelector "isCompatibleWithUnit:"

-- | @Selector@ for @doubleValueForUnit:@
doubleValueForUnitSelector :: Selector '[Id HKUnit] CDouble
doubleValueForUnitSelector = mkSelector "doubleValueForUnit:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id HKQuantity] NSComparisonResult
compareSelector = mkSelector "compare:"

