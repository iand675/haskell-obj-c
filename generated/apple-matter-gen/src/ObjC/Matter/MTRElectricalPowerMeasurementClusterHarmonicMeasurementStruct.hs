{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct
  ( MTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct
  , IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct(..)
  , order
  , setOrder
  , measurement
  , setMeasurement
  , measurementSelector
  , orderSelector
  , setMeasurementSelector
  , setOrderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- order@
order :: IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> IO (Id NSNumber)
order mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct =
  sendMessage mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct orderSelector

-- | @- setOrder:@
setOrder :: (IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> value -> IO ()
setOrder mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct setOrderSelector (toNSNumber value)

-- | @- measurement@
measurement :: IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> IO (Id NSNumber)
measurement mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct =
  sendMessage mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct measurementSelector

-- | @- setMeasurement:@
setMeasurement :: (IsMTRElectricalPowerMeasurementClusterHarmonicMeasurementStruct mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct -> value -> IO ()
setMeasurement mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterHarmonicMeasurementStruct setMeasurementSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @order@
orderSelector :: Selector '[] (Id NSNumber)
orderSelector = mkSelector "order"

-- | @Selector@ for @setOrder:@
setOrderSelector :: Selector '[Id NSNumber] ()
setOrderSelector = mkSelector "setOrder:"

-- | @Selector@ for @measurement@
measurementSelector :: Selector '[] (Id NSNumber)
measurementSelector = mkSelector "measurement"

-- | @Selector@ for @setMeasurement:@
setMeasurementSelector :: Selector '[Id NSNumber] ()
setMeasurementSelector = mkSelector "setMeasurement:"

