{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator Alarm    Attributes and commands for configuring the Refrigerator alarm.
--
-- Generated bindings for @MTRClusterRefrigeratorAlarm@.
module ObjC.Matter.MTRClusterRefrigeratorAlarm
  ( MTRClusterRefrigeratorAlarm
  , IsMTRClusterRefrigeratorAlarm(..)
  , readAttributeMaskWithParams
  , readAttributeStateWithParams
  , readAttributeSupportedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaskWithParamsSelector
  , readAttributeStateWithParamsSelector
  , readAttributeSupportedWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMaskWithParams:@
readAttributeMaskWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeMaskWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeMaskWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStateWithParams:@
readAttributeStateWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeStateWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedWithParams:@
readAttributeSupportedWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeSupportedWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeSupportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRReadParams params) => mtrClusterRefrigeratorAlarm -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRefrigeratorAlarm params =
  sendMessage mtrClusterRefrigeratorAlarm readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm => mtrClusterRefrigeratorAlarm -> IO (Id MTRClusterRefrigeratorAlarm)
init_ mtrClusterRefrigeratorAlarm =
  sendOwnedMessage mtrClusterRefrigeratorAlarm initSelector

-- | @+ new@
new :: IO (Id MTRClusterRefrigeratorAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRefrigeratorAlarm"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRefrigeratorAlarm mtrClusterRefrigeratorAlarm, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRefrigeratorAlarm -> device -> endpointID -> queue -> IO (Id MTRClusterRefrigeratorAlarm)
initWithDevice_endpointID_queue mtrClusterRefrigeratorAlarm device endpointID queue =
  sendOwnedMessage mtrClusterRefrigeratorAlarm initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMaskWithParams:@
readAttributeMaskWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaskWithParamsSelector = mkSelector "readAttributeMaskWithParams:"

-- | @Selector@ for @readAttributeStateWithParams:@
readAttributeStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStateWithParamsSelector = mkSelector "readAttributeStateWithParams:"

-- | @Selector@ for @readAttributeSupportedWithParams:@
readAttributeSupportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedWithParamsSelector = mkSelector "readAttributeSupportedWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterRefrigeratorAlarm)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterRefrigeratorAlarm)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterRefrigeratorAlarm)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

