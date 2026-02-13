{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Switch    This cluster exposes interactions with a switch device, for the purpose of using those interactions by other devices.Two types of switch devices are supported: latching switch (e.g. rocker switch) and momentary switch (e.g. push button), distinguished with their feature flags.Interactions with the switch device are exposed as attributes (for the latching switch) and as events (for both types of switches). An interested party MAY subscribe to these attributes/events and thus be informed of the interactions, and can perform actions based on this, for example by sending commands to perform an action such as controlling a light or a window shade.
--
-- Generated bindings for @MTRClusterSwitch@.
module ObjC.Matter.MTRClusterSwitch
  ( MTRClusterSwitch
  , IsMTRClusterSwitch(..)
  , readAttributeNumberOfPositionsWithParams
  , readAttributeCurrentPositionWithParams
  , readAttributeMultiPressMaxWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentPositionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMultiPressMaxWithParamsSelector
  , readAttributeNumberOfPositionsWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeNumberOfPositionsWithParams:@
readAttributeNumberOfPositionsWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeNumberOfPositionsWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeNumberOfPositionsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionWithParams:@
readAttributeCurrentPositionWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeCurrentPositionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMultiPressMaxWithParams:@
readAttributeMultiPressMaxWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeMultiPressMaxWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeMultiPressMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRReadParams params) => mtrClusterSwitch -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSwitch params =
  sendMessage mtrClusterSwitch readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterSwitch mtrClusterSwitch => mtrClusterSwitch -> IO (Id MTRClusterSwitch)
init_ mtrClusterSwitch =
  sendOwnedMessage mtrClusterSwitch initSelector

-- | @+ new@
new :: IO (Id MTRClusterSwitch)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSwitch"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRDevice device, IsNSObject queue) => mtrClusterSwitch -> device -> CUShort -> queue -> IO (Id MTRClusterSwitch)
initWithDevice_endpoint_queue mtrClusterSwitch device endpoint queue =
  sendOwnedMessage mtrClusterSwitch initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSwitch mtrClusterSwitch, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSwitch -> device -> endpointID -> queue -> IO (Id MTRClusterSwitch)
initWithDevice_endpointID_queue mtrClusterSwitch device endpointID queue =
  sendOwnedMessage mtrClusterSwitch initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeNumberOfPositionsWithParams:@
readAttributeNumberOfPositionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNumberOfPositionsWithParamsSelector = mkSelector "readAttributeNumberOfPositionsWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionWithParams:@
readAttributeCurrentPositionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionWithParamsSelector = mkSelector "readAttributeCurrentPositionWithParams:"

-- | @Selector@ for @readAttributeMultiPressMaxWithParams:@
readAttributeMultiPressMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMultiPressMaxWithParamsSelector = mkSelector "readAttributeMultiPressMaxWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterSwitch)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterSwitch)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterSwitch)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterSwitch)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

