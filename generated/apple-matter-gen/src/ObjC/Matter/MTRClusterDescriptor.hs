{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Descriptor    The Descriptor Cluster is meant to replace the support from the Zigbee Device Object (ZDO) for describing a node, its endpoints and clusters.
--
-- Generated bindings for @MTRClusterDescriptor@.
module ObjC.Matter.MTRClusterDescriptor
  ( MTRClusterDescriptor
  , IsMTRClusterDescriptor(..)
  , readAttributeDeviceTypeListWithParams
  , readAttributeServerListWithParams
  , readAttributeClientListWithParams
  , readAttributePartsListWithParams
  , readAttributeTagListWithParams
  , readAttributeEndpointUniqueIDWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributeDeviceListWithParams
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClientListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDeviceListWithParamsSelector
  , readAttributeDeviceTypeListWithParamsSelector
  , readAttributeEndpointUniqueIDWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePartsListWithParamsSelector
  , readAttributeServerListWithParamsSelector
  , readAttributeTagListWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeDeviceTypeListWithParams:@
readAttributeDeviceTypeListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeDeviceTypeListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeDeviceTypeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeServerListWithParams:@
readAttributeServerListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeServerListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeServerListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClientListWithParams:@
readAttributeClientListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeClientListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeClientListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePartsListWithParams:@
readAttributePartsListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributePartsListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributePartsListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTagListWithParams:@
readAttributeTagListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeTagListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeTagListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEndpointUniqueIDWithParams:@
readAttributeEndpointUniqueIDWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeEndpointUniqueIDWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeEndpointUniqueIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterDescriptor mtrClusterDescriptor => mtrClusterDescriptor -> IO (Id MTRClusterDescriptor)
init_ mtrClusterDescriptor =
  sendOwnedMessage mtrClusterDescriptor initSelector

-- | @+ new@
new :: IO (Id MTRClusterDescriptor)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRDevice device, IsNSObject queue) => mtrClusterDescriptor -> device -> CUShort -> queue -> IO (Id MTRClusterDescriptor)
initWithDevice_endpoint_queue mtrClusterDescriptor device endpoint queue =
  sendOwnedMessage mtrClusterDescriptor initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- readAttributeDeviceListWithParams:@
readAttributeDeviceListWithParams :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRReadParams params) => mtrClusterDescriptor -> params -> IO (Id NSDictionary)
readAttributeDeviceListWithParams mtrClusterDescriptor params =
  sendMessage mtrClusterDescriptor readAttributeDeviceListWithParamsSelector (toMTRReadParams params)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDescriptor mtrClusterDescriptor, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDescriptor -> device -> endpointID -> queue -> IO (Id MTRClusterDescriptor)
initWithDevice_endpointID_queue mtrClusterDescriptor device endpointID queue =
  sendOwnedMessage mtrClusterDescriptor initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDeviceTypeListWithParams:@
readAttributeDeviceTypeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDeviceTypeListWithParamsSelector = mkSelector "readAttributeDeviceTypeListWithParams:"

-- | @Selector@ for @readAttributeServerListWithParams:@
readAttributeServerListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeServerListWithParamsSelector = mkSelector "readAttributeServerListWithParams:"

-- | @Selector@ for @readAttributeClientListWithParams:@
readAttributeClientListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClientListWithParamsSelector = mkSelector "readAttributeClientListWithParams:"

-- | @Selector@ for @readAttributePartsListWithParams:@
readAttributePartsListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePartsListWithParamsSelector = mkSelector "readAttributePartsListWithParams:"

-- | @Selector@ for @readAttributeTagListWithParams:@
readAttributeTagListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTagListWithParamsSelector = mkSelector "readAttributeTagListWithParams:"

-- | @Selector@ for @readAttributeEndpointUniqueIDWithParams:@
readAttributeEndpointUniqueIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEndpointUniqueIDWithParamsSelector = mkSelector "readAttributeEndpointUniqueIDWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterDescriptor)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeDeviceListWithParams:@
readAttributeDeviceListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDeviceListWithParamsSelector = mkSelector "readAttributeDeviceListWithParams:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterDescriptor)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

