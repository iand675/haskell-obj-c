{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Dishwasher Alarm    Attributes and commands for configuring the Dishwasher alarm.
--
-- Generated bindings for @MTRClusterDishwasherAlarm@.
module ObjC.Matter.MTRClusterDishwasherAlarm
  ( MTRClusterDishwasherAlarm
  , IsMTRClusterDishwasherAlarm(..)
  , resetWithParams_expectedValues_expectedValueInterval_completion
  , modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaskWithParams
  , readAttributeLatchWithParams
  , readAttributeStateWithParams
  , readAttributeSupportedWithParams
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
  , modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLatchWithParamsSelector
  , readAttributeMaskWithParamsSelector
  , readAttributeStateWithParamsSelector
  , readAttributeSupportedWithParamsSelector
  , resetWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- resetWithParams:expectedValues:expectedValueInterval:completion:@
resetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRDishwasherAlarmClusterResetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDishwasherAlarm -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetWithParams_expectedValues_expectedValueInterval_completion mtrClusterDishwasherAlarm params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDishwasherAlarm resetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDishwasherAlarmClusterResetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:@
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRDishwasherAlarmClusterModifyEnabledAlarmsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDishwasherAlarm -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completion mtrClusterDishwasherAlarm params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDishwasherAlarm modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDishwasherAlarmClusterModifyEnabledAlarmsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMaskWithParams:@
readAttributeMaskWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeMaskWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeMaskWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLatchWithParams:@
readAttributeLatchWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeLatchWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeLatchWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStateWithParams:@
readAttributeStateWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeStateWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedWithParams:@
readAttributeSupportedWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeSupportedWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeSupportedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRReadParams params) => mtrClusterDishwasherAlarm -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDishwasherAlarm params =
  sendMessage mtrClusterDishwasherAlarm readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm => mtrClusterDishwasherAlarm -> IO (Id MTRClusterDishwasherAlarm)
init_ mtrClusterDishwasherAlarm =
  sendOwnedMessage mtrClusterDishwasherAlarm initSelector

-- | @+ new@
new :: IO (Id MTRClusterDishwasherAlarm)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDishwasherAlarm"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDishwasherAlarm mtrClusterDishwasherAlarm, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDishwasherAlarm -> device -> endpointID -> queue -> IO (Id MTRClusterDishwasherAlarm)
initWithDevice_endpointID_queue mtrClusterDishwasherAlarm device endpointID queue =
  sendOwnedMessage mtrClusterDishwasherAlarm initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetWithParams:expectedValues:expectedValueInterval:completion:@
resetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDishwasherAlarmClusterResetParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:@
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDishwasherAlarmClusterModifyEnabledAlarmsParams, Id NSArray, Id NSNumber, Ptr ()] ()
modifyEnabledAlarmsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "modifyEnabledAlarmsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaskWithParams:@
readAttributeMaskWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaskWithParamsSelector = mkSelector "readAttributeMaskWithParams:"

-- | @Selector@ for @readAttributeLatchWithParams:@
readAttributeLatchWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLatchWithParamsSelector = mkSelector "readAttributeLatchWithParams:"

-- | @Selector@ for @readAttributeStateWithParams:@
readAttributeStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStateWithParamsSelector = mkSelector "readAttributeStateWithParams:"

-- | @Selector@ for @readAttributeSupportedWithParams:@
readAttributeSupportedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedWithParamsSelector = mkSelector "readAttributeSupportedWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterDishwasherAlarm)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterDishwasherAlarm)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterDishwasherAlarm)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

