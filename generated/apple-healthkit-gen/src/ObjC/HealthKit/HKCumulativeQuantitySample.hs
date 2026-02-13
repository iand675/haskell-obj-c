{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKCumulativeQuantitySample
--
-- An HKQuantitySample subclass representing a quantity measurement with cumulative aggregation style.
--
-- Generated bindings for @HKCumulativeQuantitySample@.
module ObjC.HealthKit.HKCumulativeQuantitySample
  ( HKCumulativeQuantitySample
  , IsHKCumulativeQuantitySample(..)
  , sumQuantity
  , sumQuantitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sumQuantity
--
-- The sum of quantities represented by the receiver.
--
-- ObjC selector: @- sumQuantity@
sumQuantity :: IsHKCumulativeQuantitySample hkCumulativeQuantitySample => hkCumulativeQuantitySample -> IO (Id HKQuantity)
sumQuantity hkCumulativeQuantitySample =
  sendMessage hkCumulativeQuantitySample sumQuantitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sumQuantity@
sumQuantitySelector :: Selector '[] (Id HKQuantity)
sumQuantitySelector = mkSelector "sumQuantity"

