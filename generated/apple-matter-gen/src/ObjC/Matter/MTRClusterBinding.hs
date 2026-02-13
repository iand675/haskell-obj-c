{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Binding    The Binding Cluster is meant to replace the support from the Zigbee Device Object (ZDO) for supporting the binding table.
--
-- Generated bindings for @MTRClusterBinding@.
module ObjC.Matter.MTRClusterBinding
  ( MTRClusterBinding
  , IsMTRClusterBinding(..)
  , readAttributeBindingWithParams
  , writeAttributeBindingWithValue_expectedValueInterval
  , writeAttributeBindingWithValue_expectedValueInterval_params
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
  , readAttributeBindingWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , writeAttributeBindingWithValue_expectedValueIntervalSelector
  , writeAttributeBindingWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeBindingWithParams:@
readAttributeBindingWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeBindingWithParams mtrClusterBinding params =
  sendMessage mtrClusterBinding readAttributeBindingWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBindingWithValue:expectedValueInterval:@
writeAttributeBindingWithValue_expectedValueInterval :: (IsMTRClusterBinding mtrClusterBinding, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinding -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBindingWithValue_expectedValueInterval mtrClusterBinding dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinding writeAttributeBindingWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBindingWithValue:expectedValueInterval:params:@
writeAttributeBindingWithValue_expectedValueInterval_params :: (IsMTRClusterBinding mtrClusterBinding, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinding -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBindingWithValue_expectedValueInterval_params mtrClusterBinding dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinding writeAttributeBindingWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBinding params =
  sendMessage mtrClusterBinding readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBinding params =
  sendMessage mtrClusterBinding readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBinding params =
  sendMessage mtrClusterBinding readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBinding params =
  sendMessage mtrClusterBinding readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBinding mtrClusterBinding, IsMTRReadParams params) => mtrClusterBinding -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBinding params =
  sendMessage mtrClusterBinding readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBinding mtrClusterBinding => mtrClusterBinding -> IO (Id MTRClusterBinding)
init_ mtrClusterBinding =
  sendOwnedMessage mtrClusterBinding initSelector

-- | @+ new@
new :: IO (Id MTRClusterBinding)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBinding"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBinding mtrClusterBinding, IsMTRDevice device, IsNSObject queue) => mtrClusterBinding -> device -> CUShort -> queue -> IO (Id MTRClusterBinding)
initWithDevice_endpoint_queue mtrClusterBinding device endpoint queue =
  sendOwnedMessage mtrClusterBinding initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBinding mtrClusterBinding, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBinding -> device -> endpointID -> queue -> IO (Id MTRClusterBinding)
initWithDevice_endpointID_queue mtrClusterBinding device endpointID queue =
  sendOwnedMessage mtrClusterBinding initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeBindingWithParams:@
readAttributeBindingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBindingWithParamsSelector = mkSelector "readAttributeBindingWithParams:"

-- | @Selector@ for @writeAttributeBindingWithValue:expectedValueInterval:@
writeAttributeBindingWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBindingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBindingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBindingWithValue:expectedValueInterval:params:@
writeAttributeBindingWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBindingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBindingWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterBinding)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBinding)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterBinding)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBinding)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

