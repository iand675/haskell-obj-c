{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent
  ( MTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent
  , IsMTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent(..)
  , ranges
  , setRanges
  , rangesSelector
  , setRangesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- ranges@
ranges :: IsMTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent => mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent -> IO (Id NSArray)
ranges mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent rangesSelector

-- | @- setRanges:@
setRanges :: (IsMTRElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent, IsNSArray value) => mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent -> value -> IO ()
setRanges mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementPeriodRangesEvent setRangesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ranges@
rangesSelector :: Selector '[] (Id NSArray)
rangesSelector = mkSelector "ranges"

-- | @Selector@ for @setRanges:@
setRangesSelector :: Selector '[Id NSArray] ()
setRangesSelector = mkSelector "setRanges:"

