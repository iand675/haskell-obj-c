{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKDiscreteQuantitySample
--
-- An HKQuantitySample subclass representing a quantity measurement with                discrete aggregation style.
--
-- Generated bindings for @HKDiscreteQuantitySample@.
module ObjC.HealthKit.HKDiscreteQuantitySample
  ( HKDiscreteQuantitySample
  , IsHKDiscreteQuantitySample(..)
  , minimumQuantity
  , averageQuantity
  , maximumQuantity
  , mostRecentQuantity
  , mostRecentQuantityDateInterval
  , averageQuantitySelector
  , maximumQuantitySelector
  , minimumQuantitySelector
  , mostRecentQuantityDateIntervalSelector
  , mostRecentQuantitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | minimumQuantity
--
-- The minimum of the receiver's quantities
--
-- ObjC selector: @- minimumQuantity@
minimumQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
minimumQuantity hkDiscreteQuantitySample =
  sendMessage hkDiscreteQuantitySample minimumQuantitySelector

-- | averageQuantity
--
-- The average of the receiver's quantities
--
-- ObjC selector: @- averageQuantity@
averageQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
averageQuantity hkDiscreteQuantitySample =
  sendMessage hkDiscreteQuantitySample averageQuantitySelector

-- | maximumQuantity
--
-- The maximum of the receiver's quantities
--
-- ObjC selector: @- maximumQuantity@
maximumQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
maximumQuantity hkDiscreteQuantitySample =
  sendMessage hkDiscreteQuantitySample maximumQuantitySelector

-- | mostRecentQuantity
--
-- The receiver's quantity with most recent date interval
--
-- ObjC selector: @- mostRecentQuantity@
mostRecentQuantity :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id HKQuantity)
mostRecentQuantity hkDiscreteQuantitySample =
  sendMessage hkDiscreteQuantitySample mostRecentQuantitySelector

-- | mostRecentQuantityDateInterval
--
-- The date interval for the receiver's most recent quantity
--
-- ObjC selector: @- mostRecentQuantityDateInterval@
mostRecentQuantityDateInterval :: IsHKDiscreteQuantitySample hkDiscreteQuantitySample => hkDiscreteQuantitySample -> IO (Id NSDateInterval)
mostRecentQuantityDateInterval hkDiscreteQuantitySample =
  sendMessage hkDiscreteQuantitySample mostRecentQuantityDateIntervalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumQuantity@
minimumQuantitySelector :: Selector '[] (Id HKQuantity)
minimumQuantitySelector = mkSelector "minimumQuantity"

-- | @Selector@ for @averageQuantity@
averageQuantitySelector :: Selector '[] (Id HKQuantity)
averageQuantitySelector = mkSelector "averageQuantity"

-- | @Selector@ for @maximumQuantity@
maximumQuantitySelector :: Selector '[] (Id HKQuantity)
maximumQuantitySelector = mkSelector "maximumQuantity"

-- | @Selector@ for @mostRecentQuantity@
mostRecentQuantitySelector :: Selector '[] (Id HKQuantity)
mostRecentQuantitySelector = mkSelector "mostRecentQuantity"

-- | @Selector@ for @mostRecentQuantityDateInterval@
mostRecentQuantityDateIntervalSelector :: Selector '[] (Id NSDateInterval)
mostRecentQuantityDateIntervalSelector = mkSelector "mostRecentQuantityDateInterval"

