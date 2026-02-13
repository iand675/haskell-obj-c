{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster RVC Run Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterRVCRunMode@.
module ObjC.Matter.MTRClusterRVCRunMode
  ( MTRClusterRVCRunMode
  , IsMTRClusterRVCRunMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRRVCRunModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCRunMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCRunMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCRunMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRRVCRunModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRReadParams params) => mtrClusterRVCRunMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRVCRunMode params =
  sendMessage mtrClusterRVCRunMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterRVCRunMode mtrClusterRVCRunMode => mtrClusterRVCRunMode -> IO (Id MTRClusterRVCRunMode)
init_ mtrClusterRVCRunMode =
  sendOwnedMessage mtrClusterRVCRunMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterRVCRunMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRVCRunMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRVCRunMode mtrClusterRVCRunMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRVCRunMode -> device -> endpointID -> queue -> IO (Id MTRClusterRVCRunMode)
initWithDevice_endpointID_queue mtrClusterRVCRunMode device endpointID queue =
  sendOwnedMessage mtrClusterRVCRunMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRRVCRunModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRClusterRVCRunMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterRVCRunMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterRVCRunMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

