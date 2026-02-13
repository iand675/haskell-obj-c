{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Diagnostic Logs    The cluster provides commands for retrieving unstructured diagnostic logs from a Node that may be used to aid in diagnostics.
--
-- Generated bindings for @MTRClusterDiagnosticLogs@.
module ObjC.Matter.MTRClusterDiagnosticLogs
  ( MTRClusterDiagnosticLogs
  , IsMTRClusterDiagnosticLogs(..)
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandler
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
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDiagnosticLogs -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDiagnosticLogs params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDiagnosticLogs retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDiagnosticLogsClusterRetrieveLogsRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDiagnosticLogs params =
  sendMessage mtrClusterDiagnosticLogs readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDiagnosticLogs params =
  sendMessage mtrClusterDiagnosticLogs readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDiagnosticLogs params =
  sendMessage mtrClusterDiagnosticLogs readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDiagnosticLogs params =
  sendMessage mtrClusterDiagnosticLogs readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRReadParams params) => mtrClusterDiagnosticLogs -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDiagnosticLogs params =
  sendMessage mtrClusterDiagnosticLogs readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs => mtrClusterDiagnosticLogs -> IO (Id MTRClusterDiagnosticLogs)
init_ mtrClusterDiagnosticLogs =
  sendOwnedMessage mtrClusterDiagnosticLogs initSelector

-- | @+ new@
new :: IO (Id MTRClusterDiagnosticLogs)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDiagnosticLogs"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDevice device, IsNSObject queue) => mtrClusterDiagnosticLogs -> device -> CUShort -> queue -> IO (Id MTRClusterDiagnosticLogs)
initWithDevice_endpoint_queue mtrClusterDiagnosticLogs device endpoint queue =
  sendOwnedMessage mtrClusterDiagnosticLogs initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDiagnosticLogs -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterDiagnosticLogs params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterDiagnosticLogs retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRDiagnosticLogsClusterRetrieveLogsRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDiagnosticLogs mtrClusterDiagnosticLogs, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDiagnosticLogs -> device -> endpointID -> queue -> IO (Id MTRClusterDiagnosticLogs)
initWithDevice_endpointID_queue mtrClusterDiagnosticLogs device endpointID queue =
  sendOwnedMessage mtrClusterDiagnosticLogs initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDiagnosticLogsClusterRetrieveLogsRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterDiagnosticLogs)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterDiagnosticLogs)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterDiagnosticLogs)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:@
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRDiagnosticLogsClusterRetrieveLogsRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
retrieveLogsRequestWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "retrieveLogsRequestWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterDiagnosticLogs)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

