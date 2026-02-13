{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Dishwasher Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterDishwasherMode@.
module ObjC.Matter.MTRClusterDishwasherMode
  ( MTRClusterDishwasherMode
  , IsMTRClusterDishwasherMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRDishwasherModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterDishwasherMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterDishwasherMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterDishwasherMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRDishwasherModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRReadParams params) => mtrClusterDishwasherMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterDishwasherMode params =
  sendMessage mtrClusterDishwasherMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterDishwasherMode mtrClusterDishwasherMode => mtrClusterDishwasherMode -> IO (Id MTRClusterDishwasherMode)
init_ mtrClusterDishwasherMode =
  sendOwnedMessage mtrClusterDishwasherMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterDishwasherMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterDishwasherMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterDishwasherMode mtrClusterDishwasherMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterDishwasherMode -> device -> endpointID -> queue -> IO (Id MTRClusterDishwasherMode)
initWithDevice_endpointID_queue mtrClusterDishwasherMode device endpointID queue =
  sendOwnedMessage mtrClusterDishwasherMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRDishwasherModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRClusterDishwasherMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterDishwasherMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterDishwasherMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

