{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Smoke CO Alarm    This cluster provides an interface for observing and managing the state of smoke and CO alarms.
--
-- Generated bindings for @MTRClusterSmokeCOAlarm@.
module ObjC.Matter.MTRClusterSmokeCOAlarm
  ( MTRClusterSmokeCOAlarm
  , IsMTRClusterSmokeCOAlarm(..)
  , selfTestRequestWithParams_expectedValues_expectedValueInterval_completion
  , selfTestRequestWithExpectedValues_expectedValueInterval_completion
  , readAttributeExpressedStateWithParams
  , readAttributeSmokeStateWithParams
  , readAttributeCOStateWithParams
  , readAttributeBatteryAlertWithParams
  , readAttributeDeviceMutedWithParams
  , readAttributeTestInProgressWithParams
  , readAttributeHardwareFaultAlertWithParams
  , readAttributeEndOfServiceAlertWithParams
  , readAttributeInterconnectSmokeAlarmWithParams
  , readAttributeInterconnectCOAlarmWithParams
  , readAttributeContaminationStateWithParams
  , readAttributeSmokeSensitivityLevelWithParams
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_params
  , readAttributeExpiryDateWithParams
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
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBatteryAlertWithParamsSelector
  , readAttributeCOStateWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeContaminationStateWithParamsSelector
  , readAttributeDeviceMutedWithParamsSelector
  , readAttributeEndOfServiceAlertWithParamsSelector
  , readAttributeExpiryDateWithParamsSelector
  , readAttributeExpressedStateWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHardwareFaultAlertWithParamsSelector
  , readAttributeInterconnectCOAlarmWithParamsSelector
  , readAttributeInterconnectSmokeAlarmWithParamsSelector
  , readAttributeSmokeSensitivityLevelWithParamsSelector
  , readAttributeSmokeStateWithParamsSelector
  , readAttributeTestInProgressWithParamsSelector
  , selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector
  , selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector
  , writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:@
selfTestRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRSmokeCOAlarmClusterSelfTestRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSmokeCOAlarm -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selfTestRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterSmokeCOAlarm params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterSmokeCOAlarm selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRSmokeCOAlarmClusterSelfTestRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- selfTestRequestWithExpectedValues:expectedValueInterval:completion:@
selfTestRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterSmokeCOAlarm -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
selfTestRequestWithExpectedValues_expectedValueInterval_completion mtrClusterSmokeCOAlarm expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterSmokeCOAlarm selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeExpressedStateWithParams:@
readAttributeExpressedStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeExpressedStateWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeExpressedStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSmokeStateWithParams:@
readAttributeSmokeStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeSmokeStateWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeSmokeStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCOStateWithParams:@
readAttributeCOStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeCOStateWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeCOStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatteryAlertWithParams:@
readAttributeBatteryAlertWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeBatteryAlertWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeBatteryAlertWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDeviceMutedWithParams:@
readAttributeDeviceMutedWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeDeviceMutedWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeDeviceMutedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTestInProgressWithParams:@
readAttributeTestInProgressWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeTestInProgressWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeTestInProgressWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHardwareFaultAlertWithParams:@
readAttributeHardwareFaultAlertWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeHardwareFaultAlertWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeHardwareFaultAlertWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEndOfServiceAlertWithParams:@
readAttributeEndOfServiceAlertWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeEndOfServiceAlertWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeEndOfServiceAlertWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInterconnectSmokeAlarmWithParams:@
readAttributeInterconnectSmokeAlarmWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeInterconnectSmokeAlarmWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeInterconnectSmokeAlarmWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInterconnectCOAlarmWithParams:@
readAttributeInterconnectCOAlarmWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeInterconnectCOAlarmWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeInterconnectCOAlarmWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeContaminationStateWithParams:@
readAttributeContaminationStateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeContaminationStateWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeContaminationStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSmokeSensitivityLevelWithParams:@
readAttributeSmokeSensitivityLevelWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeSmokeSensitivityLevelWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeSmokeSensitivityLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterSmokeCOAlarm -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval mtrClusterSmokeCOAlarm dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterSmokeCOAlarm writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_params :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterSmokeCOAlarm -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_params mtrClusterSmokeCOAlarm dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterSmokeCOAlarm writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeExpiryDateWithParams:@
readAttributeExpiryDateWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeExpiryDateWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeExpiryDateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRReadParams params) => mtrClusterSmokeCOAlarm -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSmokeCOAlarm params =
  sendMessage mtrClusterSmokeCOAlarm readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm => mtrClusterSmokeCOAlarm -> IO (Id MTRClusterSmokeCOAlarm)
init_ mtrClusterSmokeCOAlarm =
  sendOwnedMessage mtrClusterSmokeCOAlarm initSelector

-- | @+ new@
new :: IO (Id MTRClusterSmokeCOAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSmokeCOAlarm"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSmokeCOAlarm mtrClusterSmokeCOAlarm, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSmokeCOAlarm -> device -> endpointID -> queue -> IO (Id MTRClusterSmokeCOAlarm)
initWithDevice_endpointID_queue mtrClusterSmokeCOAlarm device endpointID queue =
  sendOwnedMessage mtrClusterSmokeCOAlarm initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:@
selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRSmokeCOAlarmClusterSelfTestRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
selfTestRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selfTestRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @selfTestRequestWithExpectedValues:expectedValueInterval:completion:@
selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
selfTestRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "selfTestRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeExpressedStateWithParams:@
readAttributeExpressedStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeExpressedStateWithParamsSelector = mkSelector "readAttributeExpressedStateWithParams:"

-- | @Selector@ for @readAttributeSmokeStateWithParams:@
readAttributeSmokeStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSmokeStateWithParamsSelector = mkSelector "readAttributeSmokeStateWithParams:"

-- | @Selector@ for @readAttributeCOStateWithParams:@
readAttributeCOStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCOStateWithParamsSelector = mkSelector "readAttributeCOStateWithParams:"

-- | @Selector@ for @readAttributeBatteryAlertWithParams:@
readAttributeBatteryAlertWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatteryAlertWithParamsSelector = mkSelector "readAttributeBatteryAlertWithParams:"

-- | @Selector@ for @readAttributeDeviceMutedWithParams:@
readAttributeDeviceMutedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDeviceMutedWithParamsSelector = mkSelector "readAttributeDeviceMutedWithParams:"

-- | @Selector@ for @readAttributeTestInProgressWithParams:@
readAttributeTestInProgressWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTestInProgressWithParamsSelector = mkSelector "readAttributeTestInProgressWithParams:"

-- | @Selector@ for @readAttributeHardwareFaultAlertWithParams:@
readAttributeHardwareFaultAlertWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHardwareFaultAlertWithParamsSelector = mkSelector "readAttributeHardwareFaultAlertWithParams:"

-- | @Selector@ for @readAttributeEndOfServiceAlertWithParams:@
readAttributeEndOfServiceAlertWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEndOfServiceAlertWithParamsSelector = mkSelector "readAttributeEndOfServiceAlertWithParams:"

-- | @Selector@ for @readAttributeInterconnectSmokeAlarmWithParams:@
readAttributeInterconnectSmokeAlarmWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInterconnectSmokeAlarmWithParamsSelector = mkSelector "readAttributeInterconnectSmokeAlarmWithParams:"

-- | @Selector@ for @readAttributeInterconnectCOAlarmWithParams:@
readAttributeInterconnectCOAlarmWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInterconnectCOAlarmWithParamsSelector = mkSelector "readAttributeInterconnectCOAlarmWithParams:"

-- | @Selector@ for @readAttributeContaminationStateWithParams:@
readAttributeContaminationStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeContaminationStateWithParamsSelector = mkSelector "readAttributeContaminationStateWithParams:"

-- | @Selector@ for @readAttributeSmokeSensitivityLevelWithParams:@
readAttributeSmokeSensitivityLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSmokeSensitivityLevelWithParamsSelector = mkSelector "readAttributeSmokeSensitivityLevelWithParams:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSmokeSensitivityLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSmokeSensitivityLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSmokeSensitivityLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeExpiryDateWithParams:@
readAttributeExpiryDateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeExpiryDateWithParamsSelector = mkSelector "readAttributeExpiryDateWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterSmokeCOAlarm)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterSmokeCOAlarm)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterSmokeCOAlarm)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

