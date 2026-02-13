{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeMeasurementAccuracyRangeStruct@.
module ObjC.Matter.MTRDataTypeMeasurementAccuracyRangeStruct
  ( MTRDataTypeMeasurementAccuracyRangeStruct
  , IsMTRDataTypeMeasurementAccuracyRangeStruct(..)
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
rangeMin :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMin mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct rangeMinSelector

-- | @- setRangeMin:@
setRangeMin :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMin mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setRangeMinSelector (toNSNumber value)

-- | @- rangeMax@
rangeMax :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
rangeMax mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct rangeMaxSelector

-- | @- setRangeMax:@
setRangeMax :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setRangeMax mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setRangeMaxSelector (toNSNumber value)

-- | @- percentMax@
percentMax :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMax mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct percentMaxSelector

-- | @- setPercentMax:@
setPercentMax :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMax mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setPercentMaxSelector (toNSNumber value)

-- | @- percentMin@
percentMin :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentMin mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct percentMinSelector

-- | @- setPercentMin:@
setPercentMin :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentMin mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setPercentMinSelector (toNSNumber value)

-- | @- percentTypical@
percentTypical :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
percentTypical mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct percentTypicalSelector

-- | @- setPercentTypical:@
setPercentTypical :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setPercentTypical mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setPercentTypicalSelector (toNSNumber value)

-- | @- fixedMax@
fixedMax :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMax mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct fixedMaxSelector

-- | @- setFixedMax:@
setFixedMax :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMax mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setFixedMaxSelector (toNSNumber value)

-- | @- fixedMin@
fixedMin :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedMin mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct fixedMinSelector

-- | @- setFixedMin:@
setFixedMin :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedMin mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setFixedMinSelector (toNSNumber value)

-- | @- fixedTypical@
fixedTypical :: IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct => mtrDataTypeMeasurementAccuracyRangeStruct -> IO (Id NSNumber)
fixedTypical mtrDataTypeMeasurementAccuracyRangeStruct =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct fixedTypicalSelector

-- | @- setFixedTypical:@
setFixedTypical :: (IsMTRDataTypeMeasurementAccuracyRangeStruct mtrDataTypeMeasurementAccuracyRangeStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyRangeStruct -> value -> IO ()
setFixedTypical mtrDataTypeMeasurementAccuracyRangeStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyRangeStruct setFixedTypicalSelector (toNSNumber value)

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

