{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeMeasurementAccuracyStruct@.
module ObjC.Matter.MTRDataTypeMeasurementAccuracyStruct
  ( MTRDataTypeMeasurementAccuracyStruct
  , IsMTRDataTypeMeasurementAccuracyStruct(..)
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
measurementType :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
measurementType mtrDataTypeMeasurementAccuracyStruct =
  sendMessage mtrDataTypeMeasurementAccuracyStruct measurementTypeSelector

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMeasurementType mtrDataTypeMeasurementAccuracyStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyStruct setMeasurementTypeSelector (toNSNumber value)

-- | @- measured@
measured :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
measured mtrDataTypeMeasurementAccuracyStruct =
  sendMessage mtrDataTypeMeasurementAccuracyStruct measuredSelector

-- | @- setMeasured:@
setMeasured :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMeasured mtrDataTypeMeasurementAccuracyStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyStruct setMeasuredSelector (toNSNumber value)

-- | @- minMeasuredValue@
minMeasuredValue :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
minMeasuredValue mtrDataTypeMeasurementAccuracyStruct =
  sendMessage mtrDataTypeMeasurementAccuracyStruct minMeasuredValueSelector

-- | @- setMinMeasuredValue:@
setMinMeasuredValue :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMinMeasuredValue mtrDataTypeMeasurementAccuracyStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyStruct setMinMeasuredValueSelector (toNSNumber value)

-- | @- maxMeasuredValue@
maxMeasuredValue :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSNumber)
maxMeasuredValue mtrDataTypeMeasurementAccuracyStruct =
  sendMessage mtrDataTypeMeasurementAccuracyStruct maxMeasuredValueSelector

-- | @- setMaxMeasuredValue:@
setMaxMeasuredValue :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSNumber value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setMaxMeasuredValue mtrDataTypeMeasurementAccuracyStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyStruct setMaxMeasuredValueSelector (toNSNumber value)

-- | @- accuracyRanges@
accuracyRanges :: IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct => mtrDataTypeMeasurementAccuracyStruct -> IO (Id NSArray)
accuracyRanges mtrDataTypeMeasurementAccuracyStruct =
  sendMessage mtrDataTypeMeasurementAccuracyStruct accuracyRangesSelector

-- | @- setAccuracyRanges:@
setAccuracyRanges :: (IsMTRDataTypeMeasurementAccuracyStruct mtrDataTypeMeasurementAccuracyStruct, IsNSArray value) => mtrDataTypeMeasurementAccuracyStruct -> value -> IO ()
setAccuracyRanges mtrDataTypeMeasurementAccuracyStruct value =
  sendMessage mtrDataTypeMeasurementAccuracyStruct setAccuracyRangesSelector (toNSArray value)

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

