{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct@.
module ObjC.Matter.MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct
  ( MTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct
  , IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct(..)
  , energy
  , setEnergy
  , startTimestamp
  , setStartTimestamp
  , endTimestamp
  , setEndTimestamp
  , startSystime
  , setStartSystime
  , endSystime
  , setEndSystime
  , apparentEnergy
  , setApparentEnergy
  , reactiveEnergy
  , setReactiveEnergy
  , apparentEnergySelector
  , endSystimeSelector
  , endTimestampSelector
  , energySelector
  , reactiveEnergySelector
  , setApparentEnergySelector
  , setEndSystimeSelector
  , setEndTimestampSelector
  , setEnergySelector
  , setReactiveEnergySelector
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

-- | @- energy@
energy :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
energy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct energySelector

-- | @- setEnergy:@
setEnergy :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setEnergySelector (toNSNumber value)

-- | @- startTimestamp@
startTimestamp :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
startTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct startTimestampSelector

-- | @- setStartTimestamp:@
setStartTimestamp :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setStartTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setStartTimestampSelector (toNSNumber value)

-- | @- endTimestamp@
endTimestamp :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
endTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct endTimestampSelector

-- | @- setEndTimestamp:@
setEndTimestamp :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setEndTimestamp mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setEndTimestampSelector (toNSNumber value)

-- | @- startSystime@
startSystime :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
startSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct startSystimeSelector

-- | @- setStartSystime:@
setStartSystime :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setStartSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setStartSystimeSelector (toNSNumber value)

-- | @- endSystime@
endSystime :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
endSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct endSystimeSelector

-- | @- setEndSystime:@
setEndSystime :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setEndSystime mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setEndSystimeSelector (toNSNumber value)

-- | @- apparentEnergy@
apparentEnergy :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
apparentEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct apparentEnergySelector

-- | @- setApparentEnergy:@
setApparentEnergy :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setApparentEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setApparentEnergySelector (toNSNumber value)

-- | @- reactiveEnergy@
reactiveEnergy :: IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> IO (Id NSNumber)
reactiveEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct reactiveEnergySelector

-- | @- setReactiveEnergy:@
setReactiveEnergy :: (IsMTRElectricalEnergyMeasurementClusterEnergyMeasurementStruct mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct, IsNSNumber value) => mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct -> value -> IO ()
setReactiveEnergy mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct value =
  sendMessage mtrElectricalEnergyMeasurementClusterEnergyMeasurementStruct setReactiveEnergySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @energy@
energySelector :: Selector '[] (Id NSNumber)
energySelector = mkSelector "energy"

-- | @Selector@ for @setEnergy:@
setEnergySelector :: Selector '[Id NSNumber] ()
setEnergySelector = mkSelector "setEnergy:"

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

-- | @Selector@ for @apparentEnergy@
apparentEnergySelector :: Selector '[] (Id NSNumber)
apparentEnergySelector = mkSelector "apparentEnergy"

-- | @Selector@ for @setApparentEnergy:@
setApparentEnergySelector :: Selector '[Id NSNumber] ()
setApparentEnergySelector = mkSelector "setApparentEnergy:"

-- | @Selector@ for @reactiveEnergy@
reactiveEnergySelector :: Selector '[] (Id NSNumber)
reactiveEnergySelector = mkSelector "reactiveEnergy"

-- | @Selector@ for @setReactiveEnergy:@
setReactiveEnergySelector :: Selector '[Id NSNumber] ()
setReactiveEnergySelector = mkSelector "setReactiveEnergy:"

