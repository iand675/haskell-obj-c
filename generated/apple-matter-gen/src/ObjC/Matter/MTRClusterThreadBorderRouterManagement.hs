{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Border Router Management    Manage the Thread network of Thread Border Router
--
-- Generated bindings for @MTRClusterThreadBorderRouterManagement@.
module ObjC.Matter.MTRClusterThreadBorderRouterManagement
  ( MTRClusterThreadBorderRouterManagement
  , IsMTRClusterThreadBorderRouterManagement(..)
  , getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completion
  , getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completion
  , setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeBorderRouterNameWithParams
  , readAttributeBorderAgentIDWithParams
  , readAttributeThreadVersionWithParams
  , readAttributeInterfaceEnabledWithParams
  , readAttributeActiveDatasetTimestampWithParams
  , readAttributePendingDatasetTimestampWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector
  , getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector
  , getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveDatasetTimestampWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBorderAgentIDWithParamsSelector
  , readAttributeBorderRouterNameWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeInterfaceEnabledWithParamsSelector
  , readAttributePendingDatasetTimestampWithParamsSelector
  , readAttributeThreadVersionWithParamsSelector
  , setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadBorderRouterManagement getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadBorderRouterManagement getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadBorderRouterManagement getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadBorderRouterManagement getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadBorderRouterManagement setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadBorderRouterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadBorderRouterManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadBorderRouterManagement setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeBorderRouterNameWithParams:@
readAttributeBorderRouterNameWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeBorderRouterNameWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeBorderRouterNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBorderAgentIDWithParams:@
readAttributeBorderAgentIDWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeBorderAgentIDWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeBorderAgentIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeThreadVersionWithParams:@
readAttributeThreadVersionWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeThreadVersionWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeThreadVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInterfaceEnabledWithParams:@
readAttributeInterfaceEnabledWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeInterfaceEnabledWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeInterfaceEnabledWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveDatasetTimestampWithParams:@
readAttributeActiveDatasetTimestampWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeActiveDatasetTimestampWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeActiveDatasetTimestampWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePendingDatasetTimestampWithParams:@
readAttributePendingDatasetTimestampWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributePendingDatasetTimestampWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributePendingDatasetTimestampWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRReadParams params) => mtrClusterThreadBorderRouterManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThreadBorderRouterManagement params =
  sendMessage mtrClusterThreadBorderRouterManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement => mtrClusterThreadBorderRouterManagement -> IO (Id MTRClusterThreadBorderRouterManagement)
init_ mtrClusterThreadBorderRouterManagement =
  sendOwnedMessage mtrClusterThreadBorderRouterManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterThreadBorderRouterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThreadBorderRouterManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThreadBorderRouterManagement mtrClusterThreadBorderRouterManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThreadBorderRouterManagement -> device -> endpointID -> queue -> IO (Id MTRClusterThreadBorderRouterManagement)
initWithDevice_endpointID_queue mtrClusterThreadBorderRouterManagement device endpointID queue =
  sendOwnedMessage mtrClusterThreadBorderRouterManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterGetActiveDatasetRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
getActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
getActiveDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getActiveDatasetRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterGetPendingDatasetRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
getPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:@
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
getPendingDatasetRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getPendingDatasetRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
setActiveDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setActiveDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:@
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
setPendingDatasetRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setPendingDatasetRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBorderRouterNameWithParams:@
readAttributeBorderRouterNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBorderRouterNameWithParamsSelector = mkSelector "readAttributeBorderRouterNameWithParams:"

-- | @Selector@ for @readAttributeBorderAgentIDWithParams:@
readAttributeBorderAgentIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBorderAgentIDWithParamsSelector = mkSelector "readAttributeBorderAgentIDWithParams:"

-- | @Selector@ for @readAttributeThreadVersionWithParams:@
readAttributeThreadVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeThreadVersionWithParamsSelector = mkSelector "readAttributeThreadVersionWithParams:"

-- | @Selector@ for @readAttributeInterfaceEnabledWithParams:@
readAttributeInterfaceEnabledWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInterfaceEnabledWithParamsSelector = mkSelector "readAttributeInterfaceEnabledWithParams:"

-- | @Selector@ for @readAttributeActiveDatasetTimestampWithParams:@
readAttributeActiveDatasetTimestampWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveDatasetTimestampWithParamsSelector = mkSelector "readAttributeActiveDatasetTimestampWithParams:"

-- | @Selector@ for @readAttributePendingDatasetTimestampWithParams:@
readAttributePendingDatasetTimestampWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePendingDatasetTimestampWithParamsSelector = mkSelector "readAttributePendingDatasetTimestampWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterThreadBorderRouterManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterThreadBorderRouterManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterThreadBorderRouterManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

