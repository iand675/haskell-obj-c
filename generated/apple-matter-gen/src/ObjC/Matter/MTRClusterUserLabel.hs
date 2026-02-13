{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster User Label    The User Label Cluster provides a feature to tag an endpoint with zero or more labels.
--
-- Generated bindings for @MTRClusterUserLabel@.
module ObjC.Matter.MTRClusterUserLabel
  ( MTRClusterUserLabel
  , IsMTRClusterUserLabel(..)
  , readAttributeLabelListWithParams
  , writeAttributeLabelListWithValue_expectedValueInterval
  , writeAttributeLabelListWithValue_expectedValueInterval_params
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
  , writeAttributeLabelListWithValue_expectedValueIntervalSelector
  , writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector


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
readAttributeLabelListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeLabelListWithParams mtrClusterUserLabel params =
  sendMessage mtrClusterUserLabel readAttributeLabelListWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLabelListWithValue:expectedValueInterval:@
writeAttributeLabelListWithValue_expectedValueInterval :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterUserLabel -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLabelListWithValue_expectedValueInterval mtrClusterUserLabel dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterUserLabel writeAttributeLabelListWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLabelListWithValue:expectedValueInterval:params:@
writeAttributeLabelListWithValue_expectedValueInterval_params :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterUserLabel -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLabelListWithValue_expectedValueInterval_params mtrClusterUserLabel dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterUserLabel writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterUserLabel params =
  sendMessage mtrClusterUserLabel readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterUserLabel params =
  sendMessage mtrClusterUserLabel readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterUserLabel params =
  sendMessage mtrClusterUserLabel readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterUserLabel params =
  sendMessage mtrClusterUserLabel readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRReadParams params) => mtrClusterUserLabel -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterUserLabel params =
  sendMessage mtrClusterUserLabel readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterUserLabel mtrClusterUserLabel => mtrClusterUserLabel -> IO (Id MTRClusterUserLabel)
init_ mtrClusterUserLabel =
  sendOwnedMessage mtrClusterUserLabel initSelector

-- | @+ new@
new :: IO (Id MTRClusterUserLabel)
new  =
  do
    cls' <- getRequiredClass "MTRClusterUserLabel"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRDevice device, IsNSObject queue) => mtrClusterUserLabel -> device -> CUShort -> queue -> IO (Id MTRClusterUserLabel)
initWithDevice_endpoint_queue mtrClusterUserLabel device endpoint queue =
  sendOwnedMessage mtrClusterUserLabel initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterUserLabel mtrClusterUserLabel, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterUserLabel -> device -> endpointID -> queue -> IO (Id MTRClusterUserLabel)
initWithDevice_endpointID_queue mtrClusterUserLabel device endpointID queue =
  sendOwnedMessage mtrClusterUserLabel initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLabelListWithParams:@
readAttributeLabelListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLabelListWithParamsSelector = mkSelector "readAttributeLabelListWithParams:"

-- | @Selector@ for @writeAttributeLabelListWithValue:expectedValueInterval:@
writeAttributeLabelListWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLabelListWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLabelListWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLabelListWithValue:expectedValueInterval:params:@
writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLabelListWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLabelListWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterUserLabel)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterUserLabel)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterUserLabel)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterUserLabel)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

