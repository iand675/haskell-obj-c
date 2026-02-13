{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Device Energy Management    This cluster allows a client to manage the power draw of a device. An example of such a client could be an Energy Management System (EMS) which controls an Energy Smart Appliance (ESA).
--
-- Generated bindings for @MTRClusterDeviceEnergyManagement@.
module ObjC.Matter.MTRClusterDeviceEnergyManagement
  ( MTRClusterDeviceEnergyManagement
  , IsMTRClusterDeviceEnergyManagement(..)
  , powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completion
  , startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completion
  , pauseRequestWithParams_expectedValues_expectedValueInterval_completion
  , resumeRequestWithParams_expectedValues_expectedValueInterval_completion
  , resumeRequestWithExpectedValues_expectedValueInterval_completion
  , modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completion
  , requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completion
  , cancelRequestWithParams_expectedValues_expectedValueInterval_completion
  , cancelRequestWithExpectedValues_expectedValueInterval_completion
  , readAttributeESATypeWithParams
  , readAttributeESACanGenerateWithParams
  , readAttributeESAStateWithParams
  , readAttributeAbsMinPowerWithParams
  , readAttributeAbsMaxPowerWithParams
  , readAttributePowerAdjustmentCapabilityWithParams
  , readAttributeForecastWithParams
  , readAttributeOptOutStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector
  , cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelRequestWithExpectedValues_expectedValueInterval_completionSelector
  , cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAbsMaxPowerWithParamsSelector
  , readAttributeAbsMinPowerWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeESACanGenerateWithParamsSelector
  , readAttributeESAStateWithParamsSelector
  , readAttributeESATypeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeForecastWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOptOutStateWithParamsSelector
  , readAttributePowerAdjustmentCapabilityWithParamsSelector
  , requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector
  , resumeRequestWithExpectedValues_expectedValueInterval_completionSelector
  , resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterPowerAdjustRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- pauseRequestWithParams:expectedValues:expectedValueInterval:completion:@
pauseRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterPauseRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterPauseRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resumeRequestWithParams:expectedValues:expectedValueInterval:completion:@
resumeRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterResumeRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterResumeRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resumeRequestWithExpectedValues:expectedValueInterval:completion:@
resumeRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeRequestWithExpectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement resumeRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterModifyForecastRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:@
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDeviceEnergyManagementClusterCancelRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementClusterCancelRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelRequestWithExpectedValues:expectedValueInterval:completion:@
cancelRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelRequestWithExpectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagement cancelRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeESATypeWithParams:@
readAttributeESATypeWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeESATypeWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeESATypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeESACanGenerateWithParams:@
readAttributeESACanGenerateWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeESACanGenerateWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeESACanGenerateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeESAStateWithParams:@
readAttributeESAStateWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeESAStateWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeESAStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAbsMinPowerWithParams:@
readAttributeAbsMinPowerWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAbsMinPowerWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeAbsMinPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAbsMaxPowerWithParams:@
readAttributeAbsMaxPowerWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAbsMaxPowerWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeAbsMaxPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerAdjustmentCapabilityWithParams:@
readAttributePowerAdjustmentCapabilityWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributePowerAdjustmentCapabilityWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributePowerAdjustmentCapabilityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeForecastWithParams:@
readAttributeForecastWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeForecastWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeForecastWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOptOutStateWithParams:@
readAttributeOptOutStateWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeOptOutStateWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeOptOutStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRReadParams params) => mtrClusterDeviceEnergyManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDeviceEnergyManagement params =
  sendMessage mtrClusterDeviceEnergyManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement => mtrClusterDeviceEnergyManagement -> IO (Id MTRClusterDeviceEnergyManagement)
init_ mtrClusterDeviceEnergyManagement =
  sendOwnedMessage mtrClusterDeviceEnergyManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterDeviceEnergyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDeviceEnergyManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDeviceEnergyManagement mtrClusterDeviceEnergyManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDeviceEnergyManagement -> device -> endpointID -> queue -> IO (Id MTRClusterDeviceEnergyManagement)
initWithDevice_endpointID_queue mtrClusterDeviceEnergyManagement device endpointID queue =
  sendOwnedMessage mtrClusterDeviceEnergyManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterPowerAdjustRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
powerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "powerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterCancelPowerAdjustRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
cancelPowerAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelPowerAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:@
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
cancelPowerAdjustRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "cancelPowerAdjustRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:@
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterStartTimeAdjustRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
startTimeAdjustRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startTimeAdjustRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseRequestWithParams:expectedValues:expectedValueInterval:completion:@
pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterPauseRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
pauseRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeRequestWithParams:expectedValues:expectedValueInterval:completion:@
resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterResumeRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
resumeRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeRequestWithExpectedValues:expectedValueInterval:completion:@
resumeRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resumeRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resumeRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterModifyForecastRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
modifyForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "modifyForecastRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:@
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams, Id NSArray, Id NSNumber, Ptr ()] ()
requestConstraintBasedForecastWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "requestConstraintBasedForecastWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelRequestWithParams:expectedValues:expectedValueInterval:completion:@
cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementClusterCancelRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
cancelRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelRequestWithExpectedValues:expectedValueInterval:completion:@
cancelRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
cancelRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "cancelRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeESATypeWithParams:@
readAttributeESATypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeESATypeWithParamsSelector = mkSelector "readAttributeESATypeWithParams:"

-- | @Selector@ for @readAttributeESACanGenerateWithParams:@
readAttributeESACanGenerateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeESACanGenerateWithParamsSelector = mkSelector "readAttributeESACanGenerateWithParams:"

-- | @Selector@ for @readAttributeESAStateWithParams:@
readAttributeESAStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeESAStateWithParamsSelector = mkSelector "readAttributeESAStateWithParams:"

-- | @Selector@ for @readAttributeAbsMinPowerWithParams:@
readAttributeAbsMinPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAbsMinPowerWithParamsSelector = mkSelector "readAttributeAbsMinPowerWithParams:"

-- | @Selector@ for @readAttributeAbsMaxPowerWithParams:@
readAttributeAbsMaxPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAbsMaxPowerWithParamsSelector = mkSelector "readAttributeAbsMaxPowerWithParams:"

-- | @Selector@ for @readAttributePowerAdjustmentCapabilityWithParams:@
readAttributePowerAdjustmentCapabilityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerAdjustmentCapabilityWithParamsSelector = mkSelector "readAttributePowerAdjustmentCapabilityWithParams:"

-- | @Selector@ for @readAttributeForecastWithParams:@
readAttributeForecastWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeForecastWithParamsSelector = mkSelector "readAttributeForecastWithParams:"

-- | @Selector@ for @readAttributeOptOutStateWithParams:@
readAttributeOptOutStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOptOutStateWithParamsSelector = mkSelector "readAttributeOptOutStateWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterDeviceEnergyManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterDeviceEnergyManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterDeviceEnergyManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

