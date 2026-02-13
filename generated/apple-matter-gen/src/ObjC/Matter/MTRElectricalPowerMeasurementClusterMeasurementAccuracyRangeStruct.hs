{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct
  ( MTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct
  , IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct(..)
  , rangeMin
  , setRangeMin
  , rangeMax
  , setRangeMax
  , percentMax
  , setPercentMax
  , percentMin
  , setPercentMin
  , percentTypical
  , setPercentTypical
  , fixedMax
  , setFixedMax
  , fixedMin
  , setFixedMin
  , fixedTypical
  , setFixedTypical
  , fixedMaxSelector
  , fixedMinSelector
  , fixedTypicalSelector
  , percentMaxSelector
  , percentMinSelector
  , percentTypicalSelector
  , rangeMaxSelector
  , rangeMinSelector
  , setFixedMaxSelector
  , setFixedMinSelector
  , setFixedTypicalSelector
  , setPercentMaxSelector
  , setPercentMinSelector
  , setPercentTypicalSelector
  , setRangeMaxSelector
  , setRangeMinSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- rangeMin@
rangeMin :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct rangeMinSelector

-- | @- setRangeMin:@
setRangeMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setRangeMinSelector (toNSNumber value)

-- | @- rangeMax@
rangeMax :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct rangeMaxSelector

-- | @- setRangeMax:@
setRangeMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setRangeMaxSelector (toNSNumber value)

-- | @- percentMax@
percentMax :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct percentMaxSelector

-- | @- setPercentMax:@
setPercentMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setPercentMaxSelector (toNSNumber value)

-- | @- percentMin@
percentMin :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct percentMinSelector

-- | @- setPercentMin:@
setPercentMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setPercentMinSelector (toNSNumber value)

-- | @- percentTypical@
percentTypical :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct percentTypicalSelector

-- | @- setPercentTypical:@
setPercentTypical :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setPercentTypicalSelector (toNSNumber value)

-- | @- fixedMax@
fixedMax :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct fixedMaxSelector

-- | @- setFixedMax:@
setFixedMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMax mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setFixedMaxSelector (toNSNumber value)

-- | @- fixedMin@
fixedMin :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct fixedMinSelector

-- | @- setFixedMin:@
setFixedMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMin mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setFixedMinSelector (toNSNumber value)

-- | @- fixedTypical@
fixedTypical :: IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct fixedTypicalSelector

-- | @- setFixedTypical:@
setFixedTypical :: (IsMTRElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedTypical mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementAccuracyRangeStruct setFixedTypicalSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rangeMin@
rangeMinSelector :: Selector '[] (Id NSNumber)
rangeMinSelector = mkSelector "rangeMin"

-- | @Selector@ for @setRangeMin:@
setRangeMinSelector :: Selector '[Id NSNumber] ()
setRangeMinSelector = mkSelector "setRangeMin:"

-- | @Selector@ for @rangeMax@
rangeMaxSelector :: Selector '[] (Id NSNumber)
rangeMaxSelector = mkSelector "rangeMax"

-- | @Selector@ for @setRangeMax:@
setRangeMaxSelector :: Selector '[Id NSNumber] ()
setRangeMaxSelector = mkSelector "setRangeMax:"

-- | @Selector@ for @percentMax@
percentMaxSelector :: Selector '[] (Id NSNumber)
percentMaxSelector = mkSelector "percentMax"

-- | @Selector@ for @setPercentMax:@
setPercentMaxSelector :: Selector '[Id NSNumber] ()
setPercentMaxSelector = mkSelector "setPercentMax:"

-- | @Selector@ for @percentMin@
percentMinSelector :: Selector '[] (Id NSNumber)
percentMinSelector = mkSelector "percentMin"

-- | @Selector@ for @setPercentMin:@
setPercentMinSelector :: Selector '[Id NSNumber] ()
setPercentMinSelector = mkSelector "setPercentMin:"

-- | @Selector@ for @percentTypical@
percentTypicalSelector :: Selector '[] (Id NSNumber)
percentTypicalSelector = mkSelector "percentTypical"

-- | @Selector@ for @setPercentTypical:@
setPercentTypicalSelector :: Selector '[Id NSNumber] ()
setPercentTypicalSelector = mkSelector "setPercentTypical:"

-- | @Selector@ for @fixedMax@
fixedMaxSelector :: Selector '[] (Id NSNumber)
fixedMaxSelector = mkSelector "fixedMax"

-- | @Selector@ for @setFixedMax:@
setFixedMaxSelector :: Selector '[Id NSNumber] ()
setFixedMaxSelector = mkSelector "setFixedMax:"

-- | @Selector@ for @fixedMin@
fixedMinSelector :: Selector '[] (Id NSNumber)
fixedMinSelector = mkSelector "fixedMin"

-- | @Selector@ for @setFixedMin:@
setFixedMinSelector :: Selector '[Id NSNumber] ()
setFixedMinSelector = mkSelector "setFixedMin:"

-- | @Selector@ for @fixedTypical@
fixedTypicalSelector :: Selector '[] (Id NSNumber)
fixedTypicalSelector = mkSelector "fixedTypical"

-- | @Selector@ for @setFixedTypical:@
setFixedTypicalSelector :: Selector '[Id NSNumber] ()
setFixedTypicalSelector = mkSelector "setFixedTypical:"

