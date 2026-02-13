{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Binary Input (Basic)    An interface for reading the value of a binary measurement and accessing various characteristics of that measurement.
--
-- Generated bindings for @MTRClusterBinaryInputBasic@.
module ObjC.Matter.MTRClusterBinaryInputBasic
  ( MTRClusterBinaryInputBasic
  , IsMTRClusterBinaryInputBasic(..)
  , readAttributeActiveTextWithParams
  , writeAttributeActiveTextWithValue_expectedValueInterval
  , writeAttributeActiveTextWithValue_expectedValueInterval_params
  , readAttributeDescriptionWithParams
  , writeAttributeDescriptionWithValue_expectedValueInterval
  , writeAttributeDescriptionWithValue_expectedValueInterval_params
  , readAttributeInactiveTextWithParams
  , writeAttributeInactiveTextWithValue_expectedValueInterval
  , writeAttributeInactiveTextWithValue_expectedValueInterval_params
  , readAttributeOutOfServiceWithParams
  , writeAttributeOutOfServiceWithValue_expectedValueInterval
  , writeAttributeOutOfServiceWithValue_expectedValueInterval_params
  , readAttributePolarityWithParams
  , readAttributePresentValueWithParams
  , writeAttributePresentValueWithValue_expectedValueInterval
  , writeAttributePresentValueWithValue_expectedValueInterval_params
  , readAttributeReliabilityWithParams
  , writeAttributeReliabilityWithValue_expectedValueInterval
  , writeAttributeReliabilityWithValue_expectedValueInterval_params
  , readAttributeStatusFlagsWithParams
  , readAttributeApplicationTypeWithParams
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
  , readAttributeActiveTextWithParamsSelector
  , readAttributeApplicationTypeWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDescriptionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeInactiveTextWithParamsSelector
  , readAttributeOutOfServiceWithParamsSelector
  , readAttributePolarityWithParamsSelector
  , readAttributePresentValueWithParamsSelector
  , readAttributeReliabilityWithParamsSelector
  , readAttributeStatusFlagsWithParamsSelector
  , writeAttributeActiveTextWithValue_expectedValueIntervalSelector
  , writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector
  , writeAttributeDescriptionWithValue_expectedValueIntervalSelector
  , writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector
  , writeAttributeInactiveTextWithValue_expectedValueIntervalSelector
  , writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector
  , writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector
  , writeAttributePresentValueWithValue_expectedValueIntervalSelector
  , writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector
  , writeAttributeReliabilityWithValue_expectedValueIntervalSelector
  , writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeActiveTextWithParams:@
readAttributeActiveTextWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeActiveTextWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeActiveTextWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeActiveTextWithValue:expectedValueInterval:@
writeAttributeActiveTextWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeActiveTextWithValue_expectedValueInterval mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinaryInputBasic writeAttributeActiveTextWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeActiveTextWithValue:expectedValueInterval:params:@
writeAttributeActiveTextWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeActiveTextWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinaryInputBasic writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeDescriptionWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeDescriptionWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeDescriptionWithValue:expectedValueInterval:@
writeAttributeDescriptionWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDescriptionWithValue_expectedValueInterval mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinaryInputBasic writeAttributeDescriptionWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeDescriptionWithValue:expectedValueInterval:params:@
writeAttributeDescriptionWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDescriptionWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinaryInputBasic writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeInactiveTextWithParams:@
readAttributeInactiveTextWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeInactiveTextWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeInactiveTextWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeInactiveTextWithValue:expectedValueInterval:@
writeAttributeInactiveTextWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeInactiveTextWithValue_expectedValueInterval mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinaryInputBasic writeAttributeInactiveTextWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeInactiveTextWithValue:expectedValueInterval:params:@
writeAttributeInactiveTextWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeInactiveTextWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinaryInputBasic writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOutOfServiceWithParams:@
readAttributeOutOfServiceWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeOutOfServiceWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeOutOfServiceWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOutOfServiceWithValue:expectedValueInterval:@
writeAttributeOutOfServiceWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOutOfServiceWithValue_expectedValueInterval mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinaryInputBasic writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOutOfServiceWithValue:expectedValueInterval:params:@
writeAttributeOutOfServiceWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOutOfServiceWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinaryInputBasic writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributePolarityWithParams:@
readAttributePolarityWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributePolarityWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributePolarityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePresentValueWithParams:@
readAttributePresentValueWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributePresentValueWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributePresentValueWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributePresentValueWithValue:expectedValueInterval:@
writeAttributePresentValueWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePresentValueWithValue_expectedValueInterval mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinaryInputBasic writeAttributePresentValueWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributePresentValueWithValue:expectedValueInterval:params:@
writeAttributePresentValueWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePresentValueWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinaryInputBasic writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeReliabilityWithParams:@
readAttributeReliabilityWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeReliabilityWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeReliabilityWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeReliabilityWithValue:expectedValueInterval:@
writeAttributeReliabilityWithValue_expectedValueInterval :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeReliabilityWithValue_expectedValueInterval mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBinaryInputBasic writeAttributeReliabilityWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeReliabilityWithValue:expectedValueInterval:params:@
writeAttributeReliabilityWithValue_expectedValueInterval_params :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBinaryInputBasic -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeReliabilityWithValue_expectedValueInterval_params mtrClusterBinaryInputBasic dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBinaryInputBasic writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeStatusFlagsWithParams:@
readAttributeStatusFlagsWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeStatusFlagsWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeStatusFlagsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApplicationTypeWithParams:@
readAttributeApplicationTypeWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationTypeWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeApplicationTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRReadParams params) => mtrClusterBinaryInputBasic -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBinaryInputBasic params =
  sendMessage mtrClusterBinaryInputBasic readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic => mtrClusterBinaryInputBasic -> IO (Id MTRClusterBinaryInputBasic)
init_ mtrClusterBinaryInputBasic =
  sendOwnedMessage mtrClusterBinaryInputBasic initSelector

-- | @+ new@
new :: IO (Id MTRClusterBinaryInputBasic)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBinaryInputBasic"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterBinaryInputBasic -> device -> CUShort -> queue -> IO (Id MTRClusterBinaryInputBasic)
initWithDevice_endpoint_queue mtrClusterBinaryInputBasic device endpoint queue =
  sendOwnedMessage mtrClusterBinaryInputBasic initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBinaryInputBasic mtrClusterBinaryInputBasic, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBinaryInputBasic -> device -> endpointID -> queue -> IO (Id MTRClusterBinaryInputBasic)
initWithDevice_endpointID_queue mtrClusterBinaryInputBasic device endpointID queue =
  sendOwnedMessage mtrClusterBinaryInputBasic initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeActiveTextWithParams:@
readAttributeActiveTextWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveTextWithParamsSelector = mkSelector "readAttributeActiveTextWithParams:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:expectedValueInterval:@
writeAttributeActiveTextWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeActiveTextWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeActiveTextWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeActiveTextWithValue:expectedValueInterval:params:@
writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeActiveTextWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeActiveTextWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDescriptionWithParamsSelector = mkSelector "readAttributeDescriptionWithParams:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:expectedValueInterval:@
writeAttributeDescriptionWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeDescriptionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDescriptionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDescriptionWithValue:expectedValueInterval:params:@
writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeDescriptionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDescriptionWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeInactiveTextWithParams:@
readAttributeInactiveTextWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInactiveTextWithParamsSelector = mkSelector "readAttributeInactiveTextWithParams:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:expectedValueInterval:@
writeAttributeInactiveTextWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeInactiveTextWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeInactiveTextWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeInactiveTextWithValue:expectedValueInterval:params:@
writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeInactiveTextWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeInactiveTextWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOutOfServiceWithParams:@
readAttributeOutOfServiceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOutOfServiceWithParamsSelector = mkSelector "readAttributeOutOfServiceWithParams:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:expectedValueInterval:@
writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOutOfServiceWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOutOfServiceWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOutOfServiceWithValue:expectedValueInterval:params:@
writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOutOfServiceWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOutOfServiceWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePolarityWithParams:@
readAttributePolarityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePolarityWithParamsSelector = mkSelector "readAttributePolarityWithParams:"

-- | @Selector@ for @readAttributePresentValueWithParams:@
readAttributePresentValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePresentValueWithParamsSelector = mkSelector "readAttributePresentValueWithParams:"

-- | @Selector@ for @writeAttributePresentValueWithValue:expectedValueInterval:@
writeAttributePresentValueWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributePresentValueWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePresentValueWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePresentValueWithValue:expectedValueInterval:params:@
writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributePresentValueWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePresentValueWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeReliabilityWithParams:@
readAttributeReliabilityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReliabilityWithParamsSelector = mkSelector "readAttributeReliabilityWithParams:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:expectedValueInterval:@
writeAttributeReliabilityWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeReliabilityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeReliabilityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeReliabilityWithValue:expectedValueInterval:params:@
writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeReliabilityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeReliabilityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStatusFlagsWithParams:@
readAttributeStatusFlagsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStatusFlagsWithParamsSelector = mkSelector "readAttributeStatusFlagsWithParams:"

-- | @Selector@ for @readAttributeApplicationTypeWithParams:@
readAttributeApplicationTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApplicationTypeWithParamsSelector = mkSelector "readAttributeApplicationTypeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterBinaryInputBasic)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBinaryInputBasic)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterBinaryInputBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBinaryInputBasic)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

