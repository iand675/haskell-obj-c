{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Identify    Attributes and commands for putting a device into Identification mode (e.g. flashing a light).
--
-- Generated bindings for @MTRClusterIdentify@.
module ObjC.Matter.MTRClusterIdentify
  ( MTRClusterIdentify
  , IsMTRClusterIdentify(..)
  , identifyWithParams_expectedValues_expectedValueInterval_completion
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeIdentifyTimeWithParams
  , writeAttributeIdentifyTimeWithValue_expectedValueInterval
  , writeAttributeIdentifyTimeWithValue_expectedValueInterval_params
  , readAttributeIdentifyTypeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , identifyWithParams_expectedValues_expectedValueInterval_completionHandler
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , identifyWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeIdentifyTimeWithParamsSelector
  , readAttributeIdentifyTypeWithParamsSelector
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector
  , writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- identifyWithParams:expectedValues:expectedValueInterval:completion:@
identifyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterIdentifyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
identifyWithParams_expectedValues_expectedValueInterval_completion mtrClusterIdentify params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterIdentify identifyWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRIdentifyClusterIdentifyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- triggerEffectWithParams:expectedValues:expectedValueInterval:completion:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterTriggerEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
triggerEffectWithParams_expectedValues_expectedValueInterval_completion mtrClusterIdentify params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterIdentify triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRIdentifyClusterTriggerEffectParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeIdentifyTimeWithParams:@
readAttributeIdentifyTimeWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeIdentifyTimeWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeIdentifyTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeIdentifyTimeWithValue:expectedValueInterval:@
writeAttributeIdentifyTimeWithValue_expectedValueInterval :: (IsMTRClusterIdentify mtrClusterIdentify, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIdentifyTimeWithValue_expectedValueInterval mtrClusterIdentify dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterIdentify writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:@
writeAttributeIdentifyTimeWithValue_expectedValueInterval_params :: (IsMTRClusterIdentify mtrClusterIdentify, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterIdentify -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIdentifyTimeWithValue_expectedValueInterval_params mtrClusterIdentify dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterIdentify writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeIdentifyTypeWithParams:@
readAttributeIdentifyTypeWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeIdentifyTypeWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeIdentifyTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRReadParams params) => mtrClusterIdentify -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterIdentify params =
  sendMessage mtrClusterIdentify readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterIdentify mtrClusterIdentify => mtrClusterIdentify -> IO (Id MTRClusterIdentify)
init_ mtrClusterIdentify =
  sendOwnedMessage mtrClusterIdentify initSelector

-- | @+ new@
new :: IO (Id MTRClusterIdentify)
new  =
  do
    cls' <- getRequiredClass "MTRClusterIdentify"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRDevice device, IsNSObject queue) => mtrClusterIdentify -> device -> CUShort -> queue -> IO (Id MTRClusterIdentify)
initWithDevice_endpoint_queue mtrClusterIdentify device endpoint queue =
  sendOwnedMessage mtrClusterIdentify initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- identifyWithParams:expectedValues:expectedValueInterval:completionHandler:@
identifyWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterIdentifyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
identifyWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterIdentify params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterIdentify identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRIdentifyClusterIdentifyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRIdentifyClusterTriggerEffectParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterIdentify -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterIdentify params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterIdentify triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRIdentifyClusterTriggerEffectParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterIdentify mtrClusterIdentify, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterIdentify -> device -> endpointID -> queue -> IO (Id MTRClusterIdentify)
initWithDevice_endpointID_queue mtrClusterIdentify device endpointID queue =
  sendOwnedMessage mtrClusterIdentify initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @identifyWithParams:expectedValues:expectedValueInterval:completion:@
identifyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRIdentifyClusterIdentifyParams, Id NSArray, Id NSNumber, Ptr ()] ()
identifyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "identifyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @triggerEffectWithParams:expectedValues:expectedValueInterval:completion:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRIdentifyClusterTriggerEffectParams, Id NSArray, Id NSNumber, Ptr ()] ()
triggerEffectWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "triggerEffectWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeIdentifyTimeWithParams:@
readAttributeIdentifyTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIdentifyTimeWithParamsSelector = mkSelector "readAttributeIdentifyTimeWithParams:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:expectedValueInterval:@
writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeIdentifyTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIdentifyTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:@
writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeIdentifyTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIdentifyTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeIdentifyTypeWithParams:@
readAttributeIdentifyTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIdentifyTypeWithParamsSelector = mkSelector "readAttributeIdentifyTypeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterIdentify)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterIdentify)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterIdentify)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @identifyWithParams:expectedValues:expectedValueInterval:completionHandler:@
identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRIdentifyClusterIdentifyParams, Id NSArray, Id NSNumber, Ptr ()] ()
identifyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "identifyWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:@
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRIdentifyClusterTriggerEffectParams, Id NSArray, Id NSNumber, Ptr ()] ()
triggerEffectWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "triggerEffectWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterIdentify)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

