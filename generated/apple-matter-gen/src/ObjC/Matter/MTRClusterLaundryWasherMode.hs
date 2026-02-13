{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Washer Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterLaundryWasherMode@.
module ObjC.Matter.MTRClusterLaundryWasherMode
  ( MTRClusterLaundryWasherMode
  , IsMTRClusterLaundryWasherMode(..)
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
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRLaundryWasherModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryWasherMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterLaundryWasherMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLaundryWasherMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLaundryWasherModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRReadParams params) => mtrClusterLaundryWasherMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLaundryWasherMode params =
  sendMessage mtrClusterLaundryWasherMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode => mtrClusterLaundryWasherMode -> IO (Id MTRClusterLaundryWasherMode)
init_ mtrClusterLaundryWasherMode =
  sendOwnedMessage mtrClusterLaundryWasherMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterLaundryWasherMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLaundryWasherMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLaundryWasherMode mtrClusterLaundryWasherMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLaundryWasherMode -> device -> endpointID -> queue -> IO (Id MTRClusterLaundryWasherMode)
initWithDevice_endpointID_queue mtrClusterLaundryWasherMode device endpointID queue =
  sendOwnedMessage mtrClusterLaundryWasherMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLaundryWasherModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRClusterLaundryWasherMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterLaundryWasherMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterLaundryWasherMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

