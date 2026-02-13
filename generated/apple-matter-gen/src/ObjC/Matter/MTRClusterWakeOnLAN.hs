{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Wake on LAN    This cluster provides an interface for managing low power mode on a device that supports the Wake On LAN protocol.
--
-- Generated bindings for @MTRClusterWakeOnLAN@.
module ObjC.Matter.MTRClusterWakeOnLAN
  ( MTRClusterWakeOnLAN
  , IsMTRClusterWakeOnLAN(..)
  , readAttributeMACAddressWithParams
  , readAttributeLinkLocalAddressWithParams
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
  , readAttributeLinkLocalAddressWithParamsSelector
  , readAttributeMACAddressWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMACAddressWithParams:@
readAttributeMACAddressWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeMACAddressWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeMACAddressWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLinkLocalAddressWithParams:@
readAttributeLinkLocalAddressWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeLinkLocalAddressWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeLinkLocalAddressWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRReadParams params) => mtrClusterWakeOnLAN -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWakeOnLAN params =
  sendMessage mtrClusterWakeOnLAN readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN => mtrClusterWakeOnLAN -> IO (Id MTRClusterWakeOnLAN)
init_ mtrClusterWakeOnLAN =
  sendOwnedMessage mtrClusterWakeOnLAN initSelector

-- | @+ new@
new :: IO (Id MTRClusterWakeOnLAN)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWakeOnLAN"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWakeOnLAN mtrClusterWakeOnLAN, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWakeOnLAN -> device -> endpointID -> queue -> IO (Id MTRClusterWakeOnLAN)
initWithDevice_endpointID_queue mtrClusterWakeOnLAN device endpointID queue =
  sendOwnedMessage mtrClusterWakeOnLAN initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMACAddressWithParams:@
readAttributeMACAddressWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMACAddressWithParamsSelector = mkSelector "readAttributeMACAddressWithParams:"

-- | @Selector@ for @readAttributeLinkLocalAddressWithParams:@
readAttributeLinkLocalAddressWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLinkLocalAddressWithParamsSelector = mkSelector "readAttributeLinkLocalAddressWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterWakeOnLAN)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWakeOnLAN)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWakeOnLAN)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

