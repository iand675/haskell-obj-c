{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct
  ( MTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct
  , IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct(..)
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
measurementType :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measurementType mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct measurementTypeSelector

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasurementType mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct setMeasurementTypeSelector (toNSNumber value)

-- | @- measured@
measured :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
measured mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct measuredSelector

-- | @- setMeasured:@
setMeasured :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMeasured mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct setMeasuredSelector (toNSNumber value)

-- | @- minMeasuredValue@
minMeasuredValue :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
minMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct minMeasuredValueSelector

-- | @- setMinMeasuredValue:@
setMinMeasuredValue :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMinMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct setMinMeasuredValueSelector (toNSNumber value)

-- | @- maxMeasuredValue@
maxMeasuredValue :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSNumber)
maxMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct maxMeasuredValueSelector

-- | @- setMaxMeasuredValue:@
setMaxMeasuredValue :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setMaxMeasuredValue mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct setMaxMeasuredValueSelector (toNSNumber value)

-- | @- accuracyRanges@
accuracyRanges :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> IO (Id NSArray)
accuracyRanges mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct accuracyRangesSelector

-- | @- setAccuracyRanges:@
setAccuracyRanges :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct, IsNSArray value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct -> value -> IO ()
setAccuracyRanges mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyStruct setAccuracyRangesSelector (toNSArray value)

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

