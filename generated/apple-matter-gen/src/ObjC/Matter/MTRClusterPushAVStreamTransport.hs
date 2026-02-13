{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Push AV Stream Transport    This cluster implements the upload of Audio and Video streams from the Push AV Stream Transport Cluster using suitable push-based transports.
--
-- Generated bindings for @MTRClusterPushAVStreamTransport@.
module ObjC.Matter.MTRClusterPushAVStreamTransport
  ( MTRClusterPushAVStreamTransport
  , IsMTRClusterPushAVStreamTransport(..)
  , allocatePushTransportWithParams_expectedValues_expectedValueInterval_completion
  , deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completion
  , modifyPushTransportWithParams_expectedValues_expectedValueInterval_completion
  , setTransportStatusWithParams_expectedValues_expectedValueInterval_completion
  , manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completion
  , findTransportWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedFormatsWithParams
  , readAttributeCurrentConnectionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , findTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentConnectionsWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedFormatsWithParamsSelector
  , setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterAllocatePushTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendOwnedMessage mtrClusterPushAVStreamTransport allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRPushAVStreamTransportClusterAllocatePushTransportParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterDeallocatePushTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterPushAVStreamTransport deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRPushAVStreamTransportClusterDeallocatePushTransportParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:@
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterModifyPushTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterPushAVStreamTransport modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRPushAVStreamTransportClusterModifyPushTransportParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:@
setTransportStatusWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterSetTransportStatusParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTransportStatusWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterPushAVStreamTransport setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRPushAVStreamTransportClusterSetTransportStatusParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:@
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterManuallyTriggerTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterPushAVStreamTransport manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRPushAVStreamTransportClusterManuallyTriggerTransportParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- findTransportWithParams:expectedValues:expectedValueInterval:completion:@
findTransportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRPushAVStreamTransportClusterFindTransportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterPushAVStreamTransport -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findTransportWithParams_expectedValues_expectedValueInterval_completion mtrClusterPushAVStreamTransport params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterPushAVStreamTransport findTransportWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRPushAVStreamTransportClusterFindTransportParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedFormatsWithParams:@
readAttributeSupportedFormatsWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeSupportedFormatsWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeSupportedFormatsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentConnectionsWithParams:@
readAttributeCurrentConnectionsWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeCurrentConnectionsWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeCurrentConnectionsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRReadParams params) => mtrClusterPushAVStreamTransport -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPushAVStreamTransport params =
  sendMessage mtrClusterPushAVStreamTransport readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport => mtrClusterPushAVStreamTransport -> IO (Id MTRClusterPushAVStreamTransport)
init_ mtrClusterPushAVStreamTransport =
  sendOwnedMessage mtrClusterPushAVStreamTransport initSelector

-- | @+ new@
new :: IO (Id MTRClusterPushAVStreamTransport)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPushAVStreamTransport"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPushAVStreamTransport mtrClusterPushAVStreamTransport, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPushAVStreamTransport -> device -> endpointID -> queue -> IO (Id MTRClusterPushAVStreamTransport)
initWithDevice_endpointID_queue mtrClusterPushAVStreamTransport device endpointID queue =
  sendOwnedMessage mtrClusterPushAVStreamTransport initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterAllocatePushTransportParams, Id NSArray, Id NSNumber, Ptr ()] ()
allocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "allocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:@
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterDeallocatePushTransportParams, Id NSArray, Id NSNumber, Ptr ()] ()
deallocatePushTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "deallocatePushTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:@
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterModifyPushTransportParams, Id NSArray, Id NSNumber, Ptr ()] ()
modifyPushTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "modifyPushTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:@
setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterSetTransportStatusParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTransportStatusWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTransportStatusWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:@
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterManuallyTriggerTransportParams, Id NSArray, Id NSNumber, Ptr ()] ()
manuallyTriggerTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "manuallyTriggerTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findTransportWithParams:expectedValues:expectedValueInterval:completion:@
findTransportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRPushAVStreamTransportClusterFindTransportParams, Id NSArray, Id NSNumber, Ptr ()] ()
findTransportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findTransportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedFormatsWithParams:@
readAttributeSupportedFormatsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedFormatsWithParamsSelector = mkSelector "readAttributeSupportedFormatsWithParams:"

-- | @Selector@ for @readAttributeCurrentConnectionsWithParams:@
readAttributeCurrentConnectionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentConnectionsWithParamsSelector = mkSelector "readAttributeCurrentConnectionsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterPushAVStreamTransport)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPushAVStreamTransport)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPushAVStreamTransport)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

