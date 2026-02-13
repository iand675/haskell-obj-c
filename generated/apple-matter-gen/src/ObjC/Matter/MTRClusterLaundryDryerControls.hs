{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Dryer Controls    This cluster provides a way to access options associated with the operation of            a laundry dryer device type.
--
-- Generated bindings for @MTRClusterLaundryDryerControls@.
module ObjC.Matter.MTRClusterLaundryDryerControls
  ( MTRClusterLaundryDryerControls
  , IsMTRClusterLaundryDryerControls(..)
  , readAttributeSupportedDrynessLevelsWithParams
  , readAttributeSelectedDrynessLevelWithParams
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_params
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
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSelectedDrynessLevelWithParamsSelector
  , readAttributeSupportedDrynessLevelsWithParamsSelector
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector
  , writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSupportedDrynessLevelsWithParams:@
readAttributeSupportedDrynessLevelsWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeSupportedDrynessLevelsWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeSupportedDrynessLevelsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSelectedDrynessLevelWithParams:@
readAttributeSelectedDrynessLevelWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeSelectedDrynessLevelWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeSelectedDrynessLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryDryerControls -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval mtrClusterLaundryDryerControls dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLaundryDryerControls writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_params :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLaundryDryerControls -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_params mtrClusterLaundryDryerControls dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLaundryDryerControls writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRReadParams params) => mtrClusterLaundryDryerControls -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLaundryDryerControls params =
  sendMessage mtrClusterLaundryDryerControls readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls => mtrClusterLaundryDryerControls -> IO (Id MTRClusterLaundryDryerControls)
init_ mtrClusterLaundryDryerControls =
  sendOwnedMessage mtrClusterLaundryDryerControls initSelector

-- | @+ new@
new :: IO (Id MTRClusterLaundryDryerControls)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLaundryDryerControls"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLaundryDryerControls mtrClusterLaundryDryerControls, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLaundryDryerControls -> device -> endpointID -> queue -> IO (Id MTRClusterLaundryDryerControls)
initWithDevice_endpointID_queue mtrClusterLaundryDryerControls device endpointID queue =
  sendOwnedMessage mtrClusterLaundryDryerControls initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportedDrynessLevelsWithParams:@
readAttributeSupportedDrynessLevelsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedDrynessLevelsWithParamsSelector = mkSelector "readAttributeSupportedDrynessLevelsWithParams:"

-- | @Selector@ for @readAttributeSelectedDrynessLevelWithParams:@
readAttributeSelectedDrynessLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSelectedDrynessLevelWithParamsSelector = mkSelector "readAttributeSelectedDrynessLevelWithParams:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSelectedDrynessLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:@
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSelectedDrynessLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSelectedDrynessLevelWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterLaundryDryerControls)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterLaundryDryerControls)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterLaundryDryerControls)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

