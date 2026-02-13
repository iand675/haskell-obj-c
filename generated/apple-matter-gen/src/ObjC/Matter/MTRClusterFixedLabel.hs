{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Fixed Label    The Fixed Label Cluster provides a feature for the device to tag an endpoint with zero or more read onlylabels.
--
-- Generated bindings for @MTRClusterFixedLabel@.
module ObjC.Matter.MTRClusterFixedLabel
  ( MTRClusterFixedLabel
  , IsMTRClusterFixedLabel(..)
  , readAttributeLabelListWithParams
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
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLabelListWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeLabelListWithParams:@
readAttributeLabelListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeLabelListWithParams mtrClusterFixedLabel params =
  sendMessage mtrClusterFixedLabel readAttributeLabelListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterFixedLabel params =
  sendMessage mtrClusterFixedLabel readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterFixedLabel params =
  sendMessage mtrClusterFixedLabel readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterFixedLabel params =
  sendMessage mtrClusterFixedLabel readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterFixedLabel params =
  sendMessage mtrClusterFixedLabel readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRReadParams params) => mtrClusterFixedLabel -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterFixedLabel params =
  sendMessage mtrClusterFixedLabel readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterFixedLabel mtrClusterFixedLabel => mtrClusterFixedLabel -> IO (Id MTRClusterFixedLabel)
init_ mtrClusterFixedLabel =
  sendOwnedMessage mtrClusterFixedLabel initSelector

-- | @+ new@
new :: IO (Id MTRClusterFixedLabel)
new  =
  do
    cls' <- getRequiredClass "MTRClusterFixedLabel"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRDevice device, IsNSObject queue) => mtrClusterFixedLabel -> device -> CUShort -> queue -> IO (Id MTRClusterFixedLabel)
initWithDevice_endpoint_queue mtrClusterFixedLabel device endpoint queue =
  sendOwnedMessage mtrClusterFixedLabel initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterFixedLabel mtrClusterFixedLabel, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterFixedLabel -> device -> endpointID -> queue -> IO (Id MTRClusterFixedLabel)
initWithDevice_endpointID_queue mtrClusterFixedLabel device endpointID queue =
  sendOwnedMessage mtrClusterFixedLabel initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLabelListWithParams:@
readAttributeLabelListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLabelListWithParamsSelector = mkSelector "readAttributeLabelListWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterFixedLabel)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterFixedLabel)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterFixedLabel)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterFixedLabel)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

