{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct
  ( MTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct
  , IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct(..)
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
rangeMin :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct rangeMinSelector

-- | @- setRangeMin:@
setRangeMin :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setRangeMinSelector (toNSNumber value)

-- | @- rangeMax@
rangeMax :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct rangeMaxSelector

-- | @- setRangeMax:@
setRangeMax :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setRangeMaxSelector (toNSNumber value)

-- | @- percentMax@
percentMax :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct percentMaxSelector

-- | @- setPercentMax:@
setPercentMax :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setPercentMaxSelector (toNSNumber value)

-- | @- percentMin@
percentMin :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct percentMinSelector

-- | @- setPercentMin:@
setPercentMin :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setPercentMinSelector (toNSNumber value)

-- | @- percentTypical@
percentTypical :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct percentTypicalSelector

-- | @- setPercentTypical:@
setPercentTypical :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setPercentTypicalSelector (toNSNumber value)

-- | @- fixedMax@
fixedMax :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct fixedMaxSelector

-- | @- setFixedMax:@
setFixedMax :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMax mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setFixedMaxSelector (toNSNumber value)

-- | @- fixedMin@
fixedMin :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct fixedMinSelector

-- | @- setFixedMin:@
setFixedMin :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMin mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setFixedMinSelector (toNSNumber value)

-- | @- fixedTypical@
fixedTypical :: IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct fixedTypicalSelector

-- | @- setFixedTypical:@
setFixedTypical :: (IsMTRElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedTypical mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterMeasurementAccuracyRangeStruct setFixedTypicalSelector (toNSNumber value)

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

