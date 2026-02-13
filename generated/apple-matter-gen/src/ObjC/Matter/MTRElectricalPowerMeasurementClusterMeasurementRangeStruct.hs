{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalPowerMeasurementClusterMeasurementRangeStruct@.
module ObjC.Matter.MTRElectricalPowerMeasurementClusterMeasurementRangeStruct
  ( MTRElectricalPowerMeasurementClusterMeasurementRangeStruct
  , IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct(..)
  , measurementType
  , setMeasurementType
  , min_
  , setMin
  , max_
  , setMax
  , startTimestamp
  , setStartTimestamp
  , endTimestamp
  , setEndTimestamp
  , minTimestamp
  , setMinTimestamp
  , maxTimestamp
  , setMaxTimestamp
  , startSystime
  , setStartSystime
  , endSystime
  , setEndSystime
  , minSystime
  , setMinSystime
  , maxSystime
  , setMaxSystime
  , endSystimeSelector
  , endTimestampSelector
  , maxSelector
  , maxSystimeSelector
  , maxTimestampSelector
  , measurementTypeSelector
  , minSelector
  , minSystimeSelector
  , minTimestampSelector
  , setEndSystimeSelector
  , setEndTimestampSelector
  , setMaxSelector
  , setMaxSystimeSelector
  , setMaxTimestampSelector
  , setMeasurementTypeSelector
  , setMinSelector
  , setMinSystimeSelector
  , setMinTimestampSelector
  , setStartSystimeSelector
  , setStartTimestampSelector
  , startSystimeSelector
  , startTimestampSelector


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
measurementType :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
measurementType mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct measurementTypeSelector

-- | @- setMeasurementType:@
setMeasurementType :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMeasurementType mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMeasurementTypeSelector (toNSNumber value)

-- | @- min@
min_ :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
min_ mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct minSelector

-- | @- setMin:@
setMin :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMin mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMinSelector (toNSNumber value)

-- | @- max@
max_ :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
max_ mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct maxSelector

-- | @- setMax:@
setMax :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMax mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMaxSelector (toNSNumber value)

-- | @- startTimestamp@
startTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
startTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct startTimestampSelector

-- | @- setStartTimestamp:@
setStartTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setStartTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setStartTimestampSelector (toNSNumber value)

-- | @- endTimestamp@
endTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
endTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct endTimestampSelector

-- | @- setEndTimestamp:@
setEndTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setEndTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setEndTimestampSelector (toNSNumber value)

-- | @- minTimestamp@
minTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
minTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct minTimestampSelector

-- | @- setMinTimestamp:@
setMinTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMinTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMinTimestampSelector (toNSNumber value)

-- | @- maxTimestamp@
maxTimestamp :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
maxTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct maxTimestampSelector

-- | @- setMaxTimestamp:@
setMaxTimestamp :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMaxTimestamp mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMaxTimestampSelector (toNSNumber value)

-- | @- startSystime@
startSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
startSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct startSystimeSelector

-- | @- setStartSystime:@
setStartSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setStartSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setStartSystimeSelector (toNSNumber value)

-- | @- endSystime@
endSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
endSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct endSystimeSelector

-- | @- setEndSystime:@
setEndSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setEndSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setEndSystimeSelector (toNSNumber value)

-- | @- minSystime@
minSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
minSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct minSystimeSelector

-- | @- setMinSystime:@
setMinSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMinSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMinSystimeSelector (toNSNumber value)

-- | @- maxSystime@
maxSystime :: IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> IO (Id NSNumber)
maxSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct maxSystimeSelector

-- | @- setMaxSystime:@
setMaxSystime :: (IsMTRElectricalPowerMeasurementClusterMeasurementRangeStruct mtrElectricalPowerMeasurementClusterMeasurementRangeStruct, IsNSNumber value) => mtrElectricalPowerMeasurementClusterMeasurementRangeStruct -> value -> IO ()
setMaxSystime mtrElectricalPowerMeasurementClusterMeasurementRangeStruct value =
  sendMessage mtrElectricalPowerMeasurementClusterMeasurementRangeStruct setMaxSystimeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @measurementType@
measurementTypeSelector :: Selector '[] (Id NSNumber)
measurementTypeSelector = mkSelector "measurementType"

-- | @Selector@ for @setMeasurementType:@
setMeasurementTypeSelector :: Selector '[Id NSNumber] ()
setMeasurementTypeSelector = mkSelector "setMeasurementType:"

-- | @Selector@ for @min@
minSelector :: Selector '[] (Id NSNumber)
minSelector = mkSelector "min"

-- | @Selector@ for @setMin:@
setMinSelector :: Selector '[Id NSNumber] ()
setMinSelector = mkSelector "setMin:"

-- | @Selector@ for @max@
maxSelector :: Selector '[] (Id NSNumber)
maxSelector = mkSelector "max"

-- | @Selector@ for @setMax:@
setMaxSelector :: Selector '[Id NSNumber] ()
setMaxSelector = mkSelector "setMax:"

-- | @Selector@ for @startTimestamp@
startTimestampSelector :: Selector '[] (Id NSNumber)
startTimestampSelector = mkSelector "startTimestamp"

-- | @Selector@ for @setStartTimestamp:@
setStartTimestampSelector :: Selector '[Id NSNumber] ()
setStartTimestampSelector = mkSelector "setStartTimestamp:"

-- | @Selector@ for @endTimestamp@
endTimestampSelector :: Selector '[] (Id NSNumber)
endTimestampSelector = mkSelector "endTimestamp"

-- | @Selector@ for @setEndTimestamp:@
setEndTimestampSelector :: Selector '[Id NSNumber] ()
setEndTimestampSelector = mkSelector "setEndTimestamp:"

-- | @Selector@ for @minTimestamp@
minTimestampSelector :: Selector '[] (Id NSNumber)
minTimestampSelector = mkSelector "minTimestamp"

-- | @Selector@ for @setMinTimestamp:@
setMinTimestampSelector :: Selector '[Id NSNumber] ()
setMinTimestampSelector = mkSelector "setMinTimestamp:"

-- | @Selector@ for @maxTimestamp@
maxTimestampSelector :: Selector '[] (Id NSNumber)
maxTimestampSelector = mkSelector "maxTimestamp"

-- | @Selector@ for @setMaxTimestamp:@
setMaxTimestampSelector :: Selector '[Id NSNumber] ()
setMaxTimestampSelector = mkSelector "setMaxTimestamp:"

-- | @Selector@ for @startSystime@
startSystimeSelector :: Selector '[] (Id NSNumber)
startSystimeSelector = mkSelector "startSystime"

-- | @Selector@ for @setStartSystime:@
setStartSystimeSelector :: Selector '[Id NSNumber] ()
setStartSystimeSelector = mkSelector "setStartSystime:"

-- | @Selector@ for @endSystime@
endSystimeSelector :: Selector '[] (Id NSNumber)
endSystimeSelector = mkSelector "endSystime"

-- | @Selector@ for @setEndSystime:@
setEndSystimeSelector :: Selector '[Id NSNumber] ()
setEndSystimeSelector = mkSelector "setEndSystime:"

-- | @Selector@ for @minSystime@
minSystimeSelector :: Selector '[] (Id NSNumber)
minSystimeSelector = mkSelector "minSystime"

-- | @Selector@ for @setMinSystime:@
setMinSystimeSelector :: Selector '[Id NSNumber] ()
setMinSystimeSelector = mkSelector "setMinSystime:"

-- | @Selector@ for @maxSystime@
maxSystimeSelector :: Selector '[] (Id NSNumber)
maxSystimeSelector = mkSelector "maxSystime"

-- | @Selector@ for @setMaxSystime:@
setMaxSystimeSelector :: Selector '[Id NSNumber] ()
setMaxSystimeSelector = mkSelector "setMaxSystime:"

