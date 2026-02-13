{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster ICD Management    Allows servers to ensure that listed clients are notified when a server is available for communication.
--
-- Generated bindings for @MTRClusterICDManagement@.
module ObjC.Matter.MTRClusterICDManagement
  ( MTRClusterICDManagement
  , IsMTRClusterICDManagement(..)
  , registerClientWithParams_expectedValues_expectedValueInterval_completion
  , unregisterClientWithParams_expectedValues_expectedValueInterval_completion
  , stayActiveRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeIdleModeDurationWithParams
  , readAttributeActiveModeDurationWithParams
  , readAttributeActiveModeThresholdWithParams
  , readAttributeRegisteredClientsWithParams
  , readAttributeICDCounterWithParams
  , readAttributeClientsSupportedPerFabricWithParams
  , readAttributeUserActiveModeTriggerHintWithParams
  , readAttributeUserActiveModeTriggerInstructionWithParams
  , readAttributeOperatingModeWithParams
  , readAttributeMaximumCheckInBackOffWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveModeDurationWithParamsSelector
  , readAttributeActiveModeThresholdWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClientsSupportedPerFabricWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeICDCounterWithParamsSelector
  , readAttributeIdleModeDurationWithParamsSelector
  , readAttributeMaximumCheckInBackOffWithParamsSelector
  , readAttributeOperatingModeWithParamsSelector
  , readAttributeRegisteredClientsWithParamsSelector
  , readAttributeUserActiveModeTriggerHintWithParamsSelector
  , readAttributeUserActiveModeTriggerInstructionWithParamsSelector
  , registerClientWithParams_expectedValues_expectedValueInterval_completionSelector
  , stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- registerClientWithParams:expectedValues:expectedValueInterval:completion:@
registerClientWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRICDManagementClusterRegisterClientParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterICDManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
registerClientWithParams_expectedValues_expectedValueInterval_completion mtrClusterICDManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterICDManagement registerClientWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRICDManagementClusterRegisterClientParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- unregisterClientWithParams:expectedValues:expectedValueInterval:completion:@
unregisterClientWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRICDManagementClusterUnregisterClientParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterICDManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
unregisterClientWithParams_expectedValues_expectedValueInterval_completion mtrClusterICDManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterICDManagement unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRICDManagementClusterUnregisterClientParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:@
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRICDManagementClusterStayActiveRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterICDManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterICDManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterICDManagement stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRICDManagementClusterStayActiveRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeIdleModeDurationWithParams:@
readAttributeIdleModeDurationWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeIdleModeDurationWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeIdleModeDurationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveModeDurationWithParams:@
readAttributeActiveModeDurationWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeActiveModeDurationWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeActiveModeDurationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveModeThresholdWithParams:@
readAttributeActiveModeThresholdWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeActiveModeThresholdWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeActiveModeThresholdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRegisteredClientsWithParams:@
readAttributeRegisteredClientsWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeRegisteredClientsWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeRegisteredClientsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeICDCounterWithParams:@
readAttributeICDCounterWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeICDCounterWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeICDCounterWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClientsSupportedPerFabricWithParams:@
readAttributeClientsSupportedPerFabricWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeClientsSupportedPerFabricWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeClientsSupportedPerFabricWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUserActiveModeTriggerHintWithParams:@
readAttributeUserActiveModeTriggerHintWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeUserActiveModeTriggerHintWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeUserActiveModeTriggerHintWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUserActiveModeTriggerInstructionWithParams:@
readAttributeUserActiveModeTriggerInstructionWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeUserActiveModeTriggerInstructionWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeUserActiveModeTriggerInstructionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperatingModeWithParams:@
readAttributeOperatingModeWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeOperatingModeWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeOperatingModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaximumCheckInBackOffWithParams:@
readAttributeMaximumCheckInBackOffWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeMaximumCheckInBackOffWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeMaximumCheckInBackOffWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRReadParams params) => mtrClusterICDManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterICDManagement params =
  sendMessage mtrClusterICDManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterICDManagement mtrClusterICDManagement => mtrClusterICDManagement -> IO (Id MTRClusterICDManagement)
init_ mtrClusterICDManagement =
  sendOwnedMessage mtrClusterICDManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterICDManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterICDManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterICDManagement mtrClusterICDManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterICDManagement -> device -> endpointID -> queue -> IO (Id MTRClusterICDManagement)
initWithDevice_endpointID_queue mtrClusterICDManagement device endpointID queue =
  sendOwnedMessage mtrClusterICDManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerClientWithParams:expectedValues:expectedValueInterval:completion:@
registerClientWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRICDManagementClusterRegisterClientParams, Id NSArray, Id NSNumber, Ptr ()] ()
registerClientWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "registerClientWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @unregisterClientWithParams:expectedValues:expectedValueInterval:completion:@
unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRICDManagementClusterUnregisterClientParams, Id NSArray, Id NSNumber, Ptr ()] ()
unregisterClientWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "unregisterClientWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:@
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRICDManagementClusterStayActiveRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
stayActiveRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stayActiveRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeIdleModeDurationWithParams:@
readAttributeIdleModeDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIdleModeDurationWithParamsSelector = mkSelector "readAttributeIdleModeDurationWithParams:"

-- | @Selector@ for @readAttributeActiveModeDurationWithParams:@
readAttributeActiveModeDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveModeDurationWithParamsSelector = mkSelector "readAttributeActiveModeDurationWithParams:"

-- | @Selector@ for @readAttributeActiveModeThresholdWithParams:@
readAttributeActiveModeThresholdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveModeThresholdWithParamsSelector = mkSelector "readAttributeActiveModeThresholdWithParams:"

-- | @Selector@ for @readAttributeRegisteredClientsWithParams:@
readAttributeRegisteredClientsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRegisteredClientsWithParamsSelector = mkSelector "readAttributeRegisteredClientsWithParams:"

-- | @Selector@ for @readAttributeICDCounterWithParams:@
readAttributeICDCounterWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeICDCounterWithParamsSelector = mkSelector "readAttributeICDCounterWithParams:"

-- | @Selector@ for @readAttributeClientsSupportedPerFabricWithParams:@
readAttributeClientsSupportedPerFabricWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClientsSupportedPerFabricWithParamsSelector = mkSelector "readAttributeClientsSupportedPerFabricWithParams:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerHintWithParams:@
readAttributeUserActiveModeTriggerHintWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUserActiveModeTriggerHintWithParamsSelector = mkSelector "readAttributeUserActiveModeTriggerHintWithParams:"

-- | @Selector@ for @readAttributeUserActiveModeTriggerInstructionWithParams:@
readAttributeUserActiveModeTriggerInstructionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUserActiveModeTriggerInstructionWithParamsSelector = mkSelector "readAttributeUserActiveModeTriggerInstructionWithParams:"

-- | @Selector@ for @readAttributeOperatingModeWithParams:@
readAttributeOperatingModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperatingModeWithParamsSelector = mkSelector "readAttributeOperatingModeWithParams:"

-- | @Selector@ for @readAttributeMaximumCheckInBackOffWithParams:@
readAttributeMaximumCheckInBackOffWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaximumCheckInBackOffWithParamsSelector = mkSelector "readAttributeMaximumCheckInBackOffWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterICDManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterICDManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterICDManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

