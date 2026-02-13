{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Water Heater Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterWaterHeaterMode@.
module ObjC.Matter.MTRClusterWaterHeaterMode
  ( MTRClusterWaterHeaterMode
  , IsMTRClusterWaterHeaterMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRWaterHeaterModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterWaterHeaterMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWaterHeaterMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWaterHeaterModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRReadParams params) => mtrClusterWaterHeaterMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWaterHeaterMode params =
  sendMessage mtrClusterWaterHeaterMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode => mtrClusterWaterHeaterMode -> IO (Id MTRClusterWaterHeaterMode)
init_ mtrClusterWaterHeaterMode =
  sendOwnedMessage mtrClusterWaterHeaterMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterWaterHeaterMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWaterHeaterMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWaterHeaterMode mtrClusterWaterHeaterMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWaterHeaterMode -> device -> endpointID -> queue -> IO (Id MTRClusterWaterHeaterMode)
initWithDevice_endpointID_queue mtrClusterWaterHeaterMode device endpointID queue =
  sendOwnedMessage mtrClusterWaterHeaterMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWaterHeaterModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRClusterWaterHeaterMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWaterHeaterMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWaterHeaterMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

