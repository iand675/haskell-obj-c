{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy EVSE Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterEnergyEVSEMode@.
module ObjC.Matter.MTRClusterEnergyEVSEMode
  ( MTRClusterEnergyEVSEMode
  , IsMTRClusterEnergyEVSEMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTREnergyEVSEModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSEMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSEMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSEMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRReadParams params) => mtrClusterEnergyEVSEMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEnergyEVSEMode params =
  sendMessage mtrClusterEnergyEVSEMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode => mtrClusterEnergyEVSEMode -> IO (Id MTRClusterEnergyEVSEMode)
init_ mtrClusterEnergyEVSEMode =
  sendOwnedMessage mtrClusterEnergyEVSEMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterEnergyEVSEMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEnergyEVSEMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEnergyEVSEMode mtrClusterEnergyEVSEMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEnergyEVSEMode -> device -> endpointID -> queue -> IO (Id MTRClusterEnergyEVSEMode)
initWithDevice_endpointID_queue mtrClusterEnergyEVSEMode device endpointID queue =
  sendOwnedMessage mtrClusterEnergyEVSEMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRClusterEnergyEVSEMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterEnergyEVSEMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterEnergyEVSEMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

