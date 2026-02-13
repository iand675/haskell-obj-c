{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct
  ( MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct
  , IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct(..)
  , triggerType
  , setTriggerType
  , motionZones
  , setMotionZones
  , motionSensitivity
  , setMotionSensitivity
  , motionTimeControl
  , setMotionTimeControl
  , maxPreRollLen
  , setMaxPreRollLen
  , maxPreRollLenSelector
  , motionSensitivitySelector
  , motionTimeControlSelector
  , motionZonesSelector
  , setMaxPreRollLenSelector
  , setMotionSensitivitySelector
  , setMotionTimeControlSelector
  , setMotionZonesSelector
  , setTriggerTypeSelector
  , triggerTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- triggerType@
triggerType :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSNumber)
triggerType mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct triggerTypeSelector

-- | @- setTriggerType:@
setTriggerType :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setTriggerType mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct setTriggerTypeSelector (toNSNumber value)

-- | @- motionZones@
motionZones :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSArray)
motionZones mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct motionZonesSelector

-- | @- setMotionZones:@
setMotionZones :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSArray value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMotionZones mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct setMotionZonesSelector (toNSArray value)

-- | @- motionSensitivity@
motionSensitivity :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSNumber)
motionSensitivity mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct motionSensitivitySelector

-- | @- setMotionSensitivity:@
setMotionSensitivity :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMotionSensitivity mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct setMotionSensitivitySelector (toNSNumber value)

-- | @- motionTimeControl@
motionTimeControl :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct)
motionTimeControl mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct motionTimeControlSelector

-- | @- setMotionTimeControl:@
setMotionTimeControl :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMotionTimeControl mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct setMotionTimeControlSelector (toMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value)

-- | @- maxPreRollLen@
maxPreRollLen :: IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> IO (Id NSNumber)
maxPreRollLen mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct maxPreRollLenSelector

-- | @- setMaxPreRollLen:@
setMaxPreRollLen :: (IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct -> value -> IO ()
setMaxPreRollLen mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportTriggerOptionsStruct setMaxPreRollLenSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @triggerType@
triggerTypeSelector :: Selector '[] (Id NSNumber)
triggerTypeSelector = mkSelector "triggerType"

-- | @Selector@ for @setTriggerType:@
setTriggerTypeSelector :: Selector '[Id NSNumber] ()
setTriggerTypeSelector = mkSelector "setTriggerType:"

-- | @Selector@ for @motionZones@
motionZonesSelector :: Selector '[] (Id NSArray)
motionZonesSelector = mkSelector "motionZones"

-- | @Selector@ for @setMotionZones:@
setMotionZonesSelector :: Selector '[Id NSArray] ()
setMotionZonesSelector = mkSelector "setMotionZones:"

-- | @Selector@ for @motionSensitivity@
motionSensitivitySelector :: Selector '[] (Id NSNumber)
motionSensitivitySelector = mkSelector "motionSensitivity"

-- | @Selector@ for @setMotionSensitivity:@
setMotionSensitivitySelector :: Selector '[Id NSNumber] ()
setMotionSensitivitySelector = mkSelector "setMotionSensitivity:"

-- | @Selector@ for @motionTimeControl@
motionTimeControlSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct)
motionTimeControlSelector = mkSelector "motionTimeControl"

-- | @Selector@ for @setMotionTimeControl:@
setMotionTimeControlSelector :: Selector '[Id MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct] ()
setMotionTimeControlSelector = mkSelector "setMotionTimeControl:"

-- | @Selector@ for @maxPreRollLen@
maxPreRollLenSelector :: Selector '[] (Id NSNumber)
maxPreRollLenSelector = mkSelector "maxPreRollLen"

-- | @Selector@ for @setMaxPreRollLen:@
setMaxPreRollLenSelector :: Selector '[Id NSNumber] ()
setMaxPreRollLenSelector = mkSelector "setMaxPreRollLen:"

