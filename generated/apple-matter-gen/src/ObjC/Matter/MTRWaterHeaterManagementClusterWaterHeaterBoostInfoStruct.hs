{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct@.
module ObjC.Matter.MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct
  ( MTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct
  , IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct(..)
  , duration
  , setDuration
  , oneShot
  , setOneShot
  , emergencyBoost
  , setEmergencyBoost
  , temporarySetpoint
  , setTemporarySetpoint
  , targetPercentage
  , setTargetPercentage
  , targetReheat
  , setTargetReheat
  , durationSelector
  , emergencyBoostSelector
  , oneShotSelector
  , setDurationSelector
  , setEmergencyBoostSelector
  , setOneShotSelector
  , setTargetPercentageSelector
  , setTargetReheatSelector
  , setTemporarySetpointSelector
  , targetPercentageSelector
  , targetReheatSelector
  , temporarySetpointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- duration@
duration :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
duration mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setDuration mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct setDurationSelector (toNSNumber value)

-- | @- oneShot@
oneShot :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
oneShot mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct oneShotSelector

-- | @- setOneShot:@
setOneShot :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setOneShot mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct setOneShotSelector (toNSNumber value)

-- | @- emergencyBoost@
emergencyBoost :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
emergencyBoost mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct emergencyBoostSelector

-- | @- setEmergencyBoost:@
setEmergencyBoost :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setEmergencyBoost mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct setEmergencyBoostSelector (toNSNumber value)

-- | @- temporarySetpoint@
temporarySetpoint :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
temporarySetpoint mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct temporarySetpointSelector

-- | @- setTemporarySetpoint:@
setTemporarySetpoint :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setTemporarySetpoint mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct setTemporarySetpointSelector (toNSNumber value)

-- | @- targetPercentage@
targetPercentage :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
targetPercentage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct targetPercentageSelector

-- | @- setTargetPercentage:@
setTargetPercentage :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setTargetPercentage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct setTargetPercentageSelector (toNSNumber value)

-- | @- targetReheat@
targetReheat :: IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> IO (Id NSNumber)
targetReheat mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct targetReheatSelector

-- | @- setTargetReheat:@
setTargetReheat :: (IsMTRWaterHeaterManagementClusterWaterHeaterBoostInfoStruct mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct, IsNSNumber value) => mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct -> value -> IO ()
setTargetReheat mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct value =
  sendMessage mtrWaterHeaterManagementClusterWaterHeaterBoostInfoStruct setTargetReheatSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @oneShot@
oneShotSelector :: Selector '[] (Id NSNumber)
oneShotSelector = mkSelector "oneShot"

-- | @Selector@ for @setOneShot:@
setOneShotSelector :: Selector '[Id NSNumber] ()
setOneShotSelector = mkSelector "setOneShot:"

-- | @Selector@ for @emergencyBoost@
emergencyBoostSelector :: Selector '[] (Id NSNumber)
emergencyBoostSelector = mkSelector "emergencyBoost"

-- | @Selector@ for @setEmergencyBoost:@
setEmergencyBoostSelector :: Selector '[Id NSNumber] ()
setEmergencyBoostSelector = mkSelector "setEmergencyBoost:"

-- | @Selector@ for @temporarySetpoint@
temporarySetpointSelector :: Selector '[] (Id NSNumber)
temporarySetpointSelector = mkSelector "temporarySetpoint"

-- | @Selector@ for @setTemporarySetpoint:@
setTemporarySetpointSelector :: Selector '[Id NSNumber] ()
setTemporarySetpointSelector = mkSelector "setTemporarySetpoint:"

-- | @Selector@ for @targetPercentage@
targetPercentageSelector :: Selector '[] (Id NSNumber)
targetPercentageSelector = mkSelector "targetPercentage"

-- | @Selector@ for @setTargetPercentage:@
setTargetPercentageSelector :: Selector '[Id NSNumber] ()
setTargetPercentageSelector = mkSelector "setTargetPercentage:"

-- | @Selector@ for @targetReheat@
targetReheatSelector :: Selector '[] (Id NSNumber)
targetReheatSelector = mkSelector "targetReheat"

-- | @Selector@ for @setTargetReheat:@
setTargetReheatSelector :: Selector '[Id NSNumber] ()
setTargetReheatSelector = mkSelector "setTargetReheat:"

