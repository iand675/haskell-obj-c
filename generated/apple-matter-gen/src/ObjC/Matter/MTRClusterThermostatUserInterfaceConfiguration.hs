{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thermostat User Interface Configuration    An interface for configuring the user interface of a thermostat (which may be remote from the thermostat).
--
-- Generated bindings for @MTRClusterThermostatUserInterfaceConfiguration@.
module ObjC.Matter.MTRClusterThermostatUserInterfaceConfiguration
  ( MTRClusterThermostatUserInterfaceConfiguration
  , IsMTRClusterThermostatUserInterfaceConfiguration(..)
  , readAttributeTemperatureDisplayModeWithParams
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_params
  , readAttributeKeypadLockoutWithParams
  , writeAttributeKeypadLockoutWithValue_expectedValueInterval
  , writeAttributeKeypadLockoutWithValue_expectedValueInterval_params
  , readAttributeScheduleProgrammingVisibilityWithParams
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
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
  , readAttributeKeypadLockoutWithParamsSelector
  , readAttributeScheduleProgrammingVisibilityWithParamsSelector
  , readAttributeTemperatureDisplayModeWithParamsSelector
  , writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector
  , writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector
  , writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector
  , writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeTemperatureDisplayModeWithParams:@
readAttributeTemperatureDisplayModeWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeTemperatureDisplayModeWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeTemperatureDisplayModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval mtrClusterThermostatUserInterfaceConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_params :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_params mtrClusterThermostatUserInterfaceConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeKeypadLockoutWithParams:@
readAttributeKeypadLockoutWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeKeypadLockoutWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeKeypadLockoutWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeKeypadLockoutWithValue:expectedValueInterval:@
writeAttributeKeypadLockoutWithValue_expectedValueInterval :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeKeypadLockoutWithValue_expectedValueInterval mtrClusterThermostatUserInterfaceConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:@
writeAttributeKeypadLockoutWithValue_expectedValueInterval_params :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeKeypadLockoutWithValue_expectedValueInterval_params mtrClusterThermostatUserInterfaceConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeScheduleProgrammingVisibilityWithParams:@
readAttributeScheduleProgrammingVisibilityWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeScheduleProgrammingVisibilityWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeScheduleProgrammingVisibilityWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval mtrClusterThermostatUserInterfaceConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_params :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThermostatUserInterfaceConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_params mtrClusterThermostatUserInterfaceConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRReadParams params) => mtrClusterThermostatUserInterfaceConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThermostatUserInterfaceConfiguration params =
  sendMessage mtrClusterThermostatUserInterfaceConfiguration readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration => mtrClusterThermostatUserInterfaceConfiguration -> IO (Id MTRClusterThermostatUserInterfaceConfiguration)
init_ mtrClusterThermostatUserInterfaceConfiguration =
  sendOwnedMessage mtrClusterThermostatUserInterfaceConfiguration initSelector

-- | @+ new@
new :: IO (Id MTRClusterThermostatUserInterfaceConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThermostatUserInterfaceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterThermostatUserInterfaceConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpoint_queue mtrClusterThermostatUserInterfaceConfiguration device endpoint queue =
  sendOwnedMessage mtrClusterThermostatUserInterfaceConfiguration initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThermostatUserInterfaceConfiguration mtrClusterThermostatUserInterfaceConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThermostatUserInterfaceConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpointID_queue mtrClusterThermostatUserInterfaceConfiguration device endpointID queue =
  sendOwnedMessage mtrClusterThermostatUserInterfaceConfiguration initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeTemperatureDisplayModeWithParams:@
readAttributeTemperatureDisplayModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTemperatureDisplayModeWithParamsSelector = mkSelector "readAttributeTemperatureDisplayModeWithParams:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeTemperatureDisplayModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:@
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeTemperatureDisplayModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeTemperatureDisplayModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeKeypadLockoutWithParams:@
readAttributeKeypadLockoutWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeKeypadLockoutWithParamsSelector = mkSelector "readAttributeKeypadLockoutWithParams:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:expectedValueInterval:@
writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeKeypadLockoutWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeKeypadLockoutWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:@
writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeKeypadLockoutWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeKeypadLockoutWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeScheduleProgrammingVisibilityWithParams:@
readAttributeScheduleProgrammingVisibilityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeScheduleProgrammingVisibilityWithParamsSelector = mkSelector "readAttributeScheduleProgrammingVisibilityWithParams:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:@
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeScheduleProgrammingVisibilityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeScheduleProgrammingVisibilityWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterThermostatUserInterfaceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterThermostatUserInterfaceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterThermostatUserInterfaceConfiguration)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

