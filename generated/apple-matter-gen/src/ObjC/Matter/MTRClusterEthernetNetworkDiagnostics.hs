{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ethernet Network Diagnostics    The Ethernet Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems.
--
-- Generated bindings for @MTRClusterEthernetNetworkDiagnostics@.
module ObjC.Matter.MTRClusterEthernetNetworkDiagnostics
  ( MTRClusterEthernetNetworkDiagnostics
  , IsMTRClusterEthernetNetworkDiagnostics(..)
  , resetCountsWithParams_expectedValues_expectedValueInterval_completion
  , resetCountsWithExpectedValues_expectedValueInterval_completion
  , readAttributePHYRateWithParams
  , readAttributeFullDuplexWithParams
  , readAttributePacketRxCountWithParams
  , readAttributePacketTxCountWithParams
  , readAttributeTxErrCountWithParams
  , readAttributeCollisionCountWithParams
  , readAttributeOverrunCountWithParams
  , readAttributeCarrierDetectWithParams
  , readAttributeTimeSinceResetWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeCarrierDetectWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCollisionCountWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeFullDuplexWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOverrunCountWithParamsSelector
  , readAttributePHYRateWithParamsSelector
  , readAttributePacketRxCountWithParamsSelector
  , readAttributePacketTxCountWithParamsSelector
  , readAttributeTimeSinceResetWithParamsSelector
  , readAttributeTxErrCountWithParamsSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionSelector
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTREthernetNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEthernetNetworkDiagnostics params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEthernetNetworkDiagnostics resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREthernetNetworkDiagnosticsClusterResetCountsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completion mtrClusterEthernetNetworkDiagnostics expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterEthernetNetworkDiagnostics resetCountsWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributePHYRateWithParams:@
readAttributePHYRateWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePHYRateWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributePHYRateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFullDuplexWithParams:@
readAttributeFullDuplexWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFullDuplexWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeFullDuplexWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePacketRxCountWithParams:@
readAttributePacketRxCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketRxCountWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributePacketRxCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePacketTxCountWithParams:@
readAttributePacketTxCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePacketTxCountWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributePacketTxCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxErrCountWithParams:@
readAttributeTxErrCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrCountWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeTxErrCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCollisionCountWithParams:@
readAttributeCollisionCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCollisionCountWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeCollisionCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOverrunCountWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeOverrunCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCarrierDetectWithParams:@
readAttributeCarrierDetectWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeCarrierDetectWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeCarrierDetectWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimeSinceResetWithParams:@
readAttributeTimeSinceResetWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTimeSinceResetWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeTimeSinceResetWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRReadParams params) => mtrClusterEthernetNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEthernetNetworkDiagnostics params =
  sendMessage mtrClusterEthernetNetworkDiagnostics readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics => mtrClusterEthernetNetworkDiagnostics -> IO (Id MTRClusterEthernetNetworkDiagnostics)
init_ mtrClusterEthernetNetworkDiagnostics =
  sendOwnedMessage mtrClusterEthernetNetworkDiagnostics initSelector

-- | @+ new@
new :: IO (Id MTRClusterEthernetNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEthernetNetworkDiagnostics"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterEthernetNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterEthernetNetworkDiagnostics)
initWithDevice_endpoint_queue mtrClusterEthernetNetworkDiagnostics device endpoint queue =
  sendOwnedMessage mtrClusterEthernetNetworkDiagnostics initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTREthernetNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterEthernetNetworkDiagnostics params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterEthernetNetworkDiagnostics resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTREthernetNetworkDiagnosticsClusterResetCountsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEthernetNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandler mtrClusterEthernetNetworkDiagnostics expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterEthernetNetworkDiagnostics resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEthernetNetworkDiagnostics mtrClusterEthernetNetworkDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEthernetNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterEthernetNetworkDiagnostics)
initWithDevice_endpointID_queue mtrClusterEthernetNetworkDiagnostics device endpointID queue =
  sendOwnedMessage mtrClusterEthernetNetworkDiagnostics initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREthernetNetworkDiagnosticsClusterResetCountsParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributePHYRateWithParams:@
readAttributePHYRateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePHYRateWithParamsSelector = mkSelector "readAttributePHYRateWithParams:"

-- | @Selector@ for @readAttributeFullDuplexWithParams:@
readAttributeFullDuplexWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFullDuplexWithParamsSelector = mkSelector "readAttributeFullDuplexWithParams:"

-- | @Selector@ for @readAttributePacketRxCountWithParams:@
readAttributePacketRxCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePacketRxCountWithParamsSelector = mkSelector "readAttributePacketRxCountWithParams:"

-- | @Selector@ for @readAttributePacketTxCountWithParams:@
readAttributePacketTxCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePacketTxCountWithParamsSelector = mkSelector "readAttributePacketTxCountWithParams:"

-- | @Selector@ for @readAttributeTxErrCountWithParams:@
readAttributeTxErrCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxErrCountWithParamsSelector = mkSelector "readAttributeTxErrCountWithParams:"

-- | @Selector@ for @readAttributeCollisionCountWithParams:@
readAttributeCollisionCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCollisionCountWithParamsSelector = mkSelector "readAttributeCollisionCountWithParams:"

-- | @Selector@ for @readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOverrunCountWithParamsSelector = mkSelector "readAttributeOverrunCountWithParams:"

-- | @Selector@ for @readAttributeCarrierDetectWithParams:@
readAttributeCarrierDetectWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCarrierDetectWithParamsSelector = mkSelector "readAttributeCarrierDetectWithParams:"

-- | @Selector@ for @readAttributeTimeSinceResetWithParams:@
readAttributeTimeSinceResetWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimeSinceResetWithParamsSelector = mkSelector "readAttributeTimeSinceResetWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterEthernetNetworkDiagnostics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterEthernetNetworkDiagnostics)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterEthernetNetworkDiagnostics)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTREthernetNetworkDiagnosticsClusterResetCountsParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterEthernetNetworkDiagnostics)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

