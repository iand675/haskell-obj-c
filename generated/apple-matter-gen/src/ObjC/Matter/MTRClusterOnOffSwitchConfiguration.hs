{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster On/off Switch Configuration    Attributes and commands for configuring On/Off switching devices.
--
-- Generated bindings for @MTRClusterOnOffSwitchConfiguration@.
module ObjC.Matter.MTRClusterOnOffSwitchConfiguration
  ( MTRClusterOnOffSwitchConfiguration
  , IsMTRClusterOnOffSwitchConfiguration(..)
  , readAttributeSwitchTypeWithParams
  , readAttributeSwitchActionsWithParams
  , writeAttributeSwitchActionsWithValue_expectedValueInterval
  , writeAttributeSwitchActionsWithValue_expectedValueInterval_params
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
  , readAttributeSwitchActionsWithParamsSelector
  , readAttributeSwitchTypeWithParamsSelector
  , writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector
  , writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSwitchTypeWithParams:@
readAttributeSwitchTypeWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeSwitchTypeWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeSwitchTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSwitchActionsWithParams:@
readAttributeSwitchActionsWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeSwitchActionsWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeSwitchActionsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSwitchActionsWithValue:expectedValueInterval:@
writeAttributeSwitchActionsWithValue_expectedValueInterval :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOnOffSwitchConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSwitchActionsWithValue_expectedValueInterval mtrClusterOnOffSwitchConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterOnOffSwitchConfiguration writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSwitchActionsWithValue:expectedValueInterval:params:@
writeAttributeSwitchActionsWithValue_expectedValueInterval_params :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOnOffSwitchConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSwitchActionsWithValue_expectedValueInterval_params mtrClusterOnOffSwitchConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterOnOffSwitchConfiguration writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRReadParams params) => mtrClusterOnOffSwitchConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOnOffSwitchConfiguration params =
  sendMessage mtrClusterOnOffSwitchConfiguration readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration => mtrClusterOnOffSwitchConfiguration -> IO (Id MTRClusterOnOffSwitchConfiguration)
init_ mtrClusterOnOffSwitchConfiguration =
  sendOwnedMessage mtrClusterOnOffSwitchConfiguration initSelector

-- | @+ new@
new :: IO (Id MTRClusterOnOffSwitchConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOnOffSwitchConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterOnOffSwitchConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterOnOffSwitchConfiguration)
initWithDevice_endpoint_queue mtrClusterOnOffSwitchConfiguration device endpoint queue =
  sendOwnedMessage mtrClusterOnOffSwitchConfiguration initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOnOffSwitchConfiguration mtrClusterOnOffSwitchConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOnOffSwitchConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterOnOffSwitchConfiguration)
initWithDevice_endpointID_queue mtrClusterOnOffSwitchConfiguration device endpointID queue =
  sendOwnedMessage mtrClusterOnOffSwitchConfiguration initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSwitchTypeWithParams:@
readAttributeSwitchTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSwitchTypeWithParamsSelector = mkSelector "readAttributeSwitchTypeWithParams:"

-- | @Selector@ for @readAttributeSwitchActionsWithParams:@
readAttributeSwitchActionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSwitchActionsWithParamsSelector = mkSelector "readAttributeSwitchActionsWithParams:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:expectedValueInterval:@
writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSwitchActionsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSwitchActionsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSwitchActionsWithValue:expectedValueInterval:params:@
writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSwitchActionsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSwitchActionsWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterOnOffSwitchConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOnOffSwitchConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterOnOffSwitchConfiguration)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOnOffSwitchConfiguration)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

