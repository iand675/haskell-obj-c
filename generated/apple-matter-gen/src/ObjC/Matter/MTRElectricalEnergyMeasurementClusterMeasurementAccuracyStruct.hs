{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct
  ( MTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct
  , IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct(..)
  , measurementType
  , setMeasurementType
  , measured
  , setMeasured
  , minMeasuredValue
  , setMinMeasuredValue
  , maxMeasuredValue
  , setMaxMeasuredValue
  , accuracyRanges
  , setAccuracyRanges
  , accuracyRangesSelector
  , maxMeasuredValueSelector
  , measuredSelector
  , measurementTypeSelector
  , minMeasuredValueSelector
  , setAccuracyRangesSelector
  , setMaxMeasuredValueSelector
  , setMeasuredSelector
  , setMeasurementTypeSelector
  , setMinMeasuredValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- measurementType@
measurementType :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measurementType mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct measurementTypeSelector

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasurementType mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct setMeasurementTypeSelector (toNSNumber value)

-- | @- measured@
measured :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measured mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct measuredSelector

-- | @- setMeasured:@
setMeasured :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasured mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct setMeasuredSelector (toNSNumber value)

-- | @- minMeasuredValue@
minMeasuredValue :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
minMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct minMeasuredValueSelector

-- | @- setMinMeasuredValue:@
setMinMeasuredValue :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMinMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct setMinMeasuredValueSelector (toNSNumber value)

-- | @- maxMeasuredValue@
maxMeasuredValue :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
maxMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct maxMeasuredValueSelector

-- | @- setMaxMeasuredValue:@
setMaxMeasuredValue :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMaxMeasuredValue mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct setMaxMeasuredValueSelector (toNSNumber value)

-- | @- accuracyRanges@
accuracyRanges :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSArray)
accuracyRanges mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct accuracyRangesSelector

-- | @- setAccuracyRanges:@
setAccuracyRanges :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct, IsNSArray value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setAccuracyRanges mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyStruct setAccuracyRangesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @measurementType@
measurementTypeSelector :: Selector '[] (Id NSNumber)
measurementTypeSelector = mkSelector "measurementType"

-- | @Selector@ for @setMeasurementType:@
setMeasurementTypeSelector :: Selector '[Id NSNumber] ()
setMeasurementTypeSelector = mkSelector "setMeasurementType:"

-- | @Selector@ for @measured@
measuredSelector :: Selector '[] (Id NSNumber)
measuredSelector = mkSelector "measured"

-- | @Selector@ for @setMeasured:@
setMeasuredSelector :: Selector '[Id NSNumber] ()
setMeasuredSelector = mkSelector "setMeasured:"

-- | @Selector@ for @minMeasuredValue@
minMeasuredValueSelector :: Selector '[] (Id NSNumber)
minMeasuredValueSelector = mkSelector "minMeasuredValue"

-- | @Selector@ for @setMinMeasuredValue:@
setMinMeasuredValueSelector :: Selector '[Id NSNumber] ()
setMinMeasuredValueSelector = mkSelector "setMinMeasuredValue:"

-- | @Selector@ for @maxMeasuredValue@
maxMeasuredValueSelector :: Selector '[] (Id NSNumber)
maxMeasuredValueSelector = mkSelector "maxMeasuredValue"

-- | @Selector@ for @setMaxMeasuredValue:@
setMaxMeasuredValueSelector :: Selector '[Id NSNumber] ()
setMaxMeasuredValueSelector = mkSelector "setMaxMeasuredValue:"

-- | @Selector@ for @accuracyRanges@
accuracyRangesSelector :: Selector '[] (Id NSArray)
accuracyRangesSelector = mkSelector "accuracyRanges"

-- | @Selector@ for @setAccuracyRanges:@
setAccuracyRangesSelector :: Selector '[Id NSArray] ()
setAccuracyRangesSelector = mkSelector "setAccuracyRanges:"

