{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Device Energy Management Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterDeviceEnergyManagementMode@.
module ObjC.Matter.MTRClusterDeviceEnergyManagementMode
  ( MTRClusterDeviceEnergyManagementMode
  , IsMTRClusterDeviceEnergyManagementMode(..)
  , changeToModeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentModeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedModesWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRDeviceEnergyManagementModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDeviceEnergyManagementMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterDeviceEnergyManagementMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDeviceEnergyManagementMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDeviceEnergyManagementModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRReadParams params) => mtrClusterDeviceEnergyManagementMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDeviceEnergyManagementMode params =
  sendMessage mtrClusterDeviceEnergyManagementMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode => mtrClusterDeviceEnergyManagementMode -> IO (Id MTRClusterDeviceEnergyManagementMode)
init_ mtrClusterDeviceEnergyManagementMode =
  sendOwnedMessage mtrClusterDeviceEnergyManagementMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterDeviceEnergyManagementMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDeviceEnergyManagementMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDeviceEnergyManagementMode mtrClusterDeviceEnergyManagementMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDeviceEnergyManagementMode -> device -> endpointID -> queue -> IO (Id MTRClusterDeviceEnergyManagementMode)
initWithDevice_endpointID_queue mtrClusterDeviceEnergyManagementMode device endpointID queue =
  sendOwnedMessage mtrClusterDeviceEnergyManagementMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDeviceEnergyManagementModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterDeviceEnergyManagementMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterDeviceEnergyManagementMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterDeviceEnergyManagementMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

