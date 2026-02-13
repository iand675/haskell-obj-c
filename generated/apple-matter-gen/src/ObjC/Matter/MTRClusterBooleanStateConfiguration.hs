{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Boolean State Configuration    This cluster is used to configure a boolean sensor.
--
-- Generated bindings for @MTRClusterBooleanStateConfiguration@.
module ObjC.Matter.MTRClusterBooleanStateConfiguration
  ( MTRClusterBooleanStateConfiguration
  , IsMTRClusterBooleanStateConfiguration(..)
  , suppressAlarmWithParams_expectedValues_expectedValueInterval_completion
  , enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentSensitivityLevelWithParams
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_params
  , readAttributeSupportedSensitivityLevelsWithParams
  , readAttributeDefaultSensitivityLevelWithParams
  , readAttributeAlarmsActiveWithParams
  , readAttributeAlarmsSuppressedWithParams
  , readAttributeAlarmsEnabledWithParams
  , readAttributeAlarmsSupportedWithParams
  , readAttributeSensorFaultWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAlarmsActiveWithParamsSelector
  , readAttributeAlarmsEnabledWithParamsSelector
  , readAttributeAlarmsSupportedWithParamsSelector
  , readAttributeAlarmsSuppressedWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentSensitivityLevelWithParamsSelector
  , readAttributeDefaultSensitivityLevelWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSensorFaultWithParamsSelector
  , readAttributeSupportedSensitivityLevelsWithParamsSelector
  , suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:@
suppressAlarmWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterSuppressAlarmParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBooleanStateConfiguration -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
suppressAlarmWithParams_expectedValues_expectedValueInterval_completion mtrClusterBooleanStateConfiguration params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterBooleanStateConfiguration suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRBooleanStateConfigurationClusterSuppressAlarmParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:@
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRBooleanStateConfigurationClusterEnableDisableAlarmParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBooleanStateConfiguration -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completion mtrClusterBooleanStateConfiguration params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterBooleanStateConfiguration enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRBooleanStateConfigurationClusterEnableDisableAlarmParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCurrentSensitivityLevelWithParams:@
readAttributeCurrentSensitivityLevelWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeCurrentSensitivityLevelWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeCurrentSensitivityLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBooleanStateConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval mtrClusterBooleanStateConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBooleanStateConfiguration writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_params :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBooleanStateConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_params mtrClusterBooleanStateConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBooleanStateConfiguration writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSupportedSensitivityLevelsWithParams:@
readAttributeSupportedSensitivityLevelsWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeSupportedSensitivityLevelsWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeSupportedSensitivityLevelsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDefaultSensitivityLevelWithParams:@
readAttributeDefaultSensitivityLevelWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeDefaultSensitivityLevelWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeDefaultSensitivityLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAlarmsActiveWithParams:@
readAttributeAlarmsActiveWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsActiveWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeAlarmsActiveWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAlarmsSuppressedWithParams:@
readAttributeAlarmsSuppressedWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsSuppressedWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeAlarmsSuppressedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAlarmsEnabledWithParams:@
readAttributeAlarmsEnabledWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsEnabledWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeAlarmsEnabledWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAlarmsSupportedWithParams:@
readAttributeAlarmsSupportedWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAlarmsSupportedWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeAlarmsSupportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSensorFaultWithParams:@
readAttributeSensorFaultWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeSensorFaultWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeSensorFaultWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRReadParams params) => mtrClusterBooleanStateConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBooleanStateConfiguration params =
  sendMessage mtrClusterBooleanStateConfiguration readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration => mtrClusterBooleanStateConfiguration -> IO (Id MTRClusterBooleanStateConfiguration)
init_ mtrClusterBooleanStateConfiguration =
  sendOwnedMessage mtrClusterBooleanStateConfiguration initSelector

-- | @+ new@
new :: IO (Id MTRClusterBooleanStateConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBooleanStateConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBooleanStateConfiguration mtrClusterBooleanStateConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBooleanStateConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterBooleanStateConfiguration)
initWithDevice_endpointID_queue mtrClusterBooleanStateConfiguration device endpointID queue =
  sendOwnedMessage mtrClusterBooleanStateConfiguration initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:@
suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRBooleanStateConfigurationClusterSuppressAlarmParams, Id NSArray, Id NSNumber, Ptr ()] ()
suppressAlarmWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "suppressAlarmWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:@
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRBooleanStateConfigurationClusterEnableDisableAlarmParams, Id NSArray, Id NSNumber, Ptr ()] ()
enableDisableAlarmWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableDisableAlarmWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentSensitivityLevelWithParams:@
readAttributeCurrentSensitivityLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentSensitivityLevelWithParamsSelector = mkSelector "readAttributeCurrentSensitivityLevelWithParams:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeCurrentSensitivityLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:@
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeCurrentSensitivityLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentSensitivityLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedSensitivityLevelsWithParams:@
readAttributeSupportedSensitivityLevelsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedSensitivityLevelsWithParamsSelector = mkSelector "readAttributeSupportedSensitivityLevelsWithParams:"

-- | @Selector@ for @readAttributeDefaultSensitivityLevelWithParams:@
readAttributeDefaultSensitivityLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultSensitivityLevelWithParamsSelector = mkSelector "readAttributeDefaultSensitivityLevelWithParams:"

-- | @Selector@ for @readAttributeAlarmsActiveWithParams:@
readAttributeAlarmsActiveWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAlarmsActiveWithParamsSelector = mkSelector "readAttributeAlarmsActiveWithParams:"

-- | @Selector@ for @readAttributeAlarmsSuppressedWithParams:@
readAttributeAlarmsSuppressedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAlarmsSuppressedWithParamsSelector = mkSelector "readAttributeAlarmsSuppressedWithParams:"

-- | @Selector@ for @readAttributeAlarmsEnabledWithParams:@
readAttributeAlarmsEnabledWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAlarmsEnabledWithParamsSelector = mkSelector "readAttributeAlarmsEnabledWithParams:"

-- | @Selector@ for @readAttributeAlarmsSupportedWithParams:@
readAttributeAlarmsSupportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAlarmsSupportedWithParamsSelector = mkSelector "readAttributeAlarmsSupportedWithParams:"

-- | @Selector@ for @readAttributeSensorFaultWithParams:@
readAttributeSensorFaultWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSensorFaultWithParamsSelector = mkSelector "readAttributeSensorFaultWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterBooleanStateConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBooleanStateConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBooleanStateConfiguration)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

