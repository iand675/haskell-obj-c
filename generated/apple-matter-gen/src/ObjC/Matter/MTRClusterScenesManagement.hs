{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Scenes Management    Attributes and commands for scene configuration and manipulation.
--
-- Generated bindings for @MTRClusterScenesManagement@.
module ObjC.Matter.MTRClusterScenesManagement
  ( MTRClusterScenesManagement
  , IsMTRClusterScenesManagement(..)
  , addSceneWithParams_expectedValues_expectedValueInterval_completion
  , viewSceneWithParams_expectedValues_expectedValueInterval_completion
  , removeSceneWithParams_expectedValues_expectedValueInterval_completion
  , removeAllScenesWithParams_expectedValues_expectedValueInterval_completion
  , storeSceneWithParams_expectedValues_expectedValueInterval_completion
  , recallSceneWithParams_expectedValues_expectedValueInterval_completion
  , getSceneMembershipWithParams_expectedValues_expectedValueInterval_completion
  , copySceneWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSceneTableSizeWithParams
  , readAttributeFabricSceneInfoWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , copySceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFabricSceneInfoWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSceneTableSizeWithParamsSelector
  , recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector
  , viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addSceneWithParams:expectedValues:expectedValueInterval:completion:@
addSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterAddSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement addSceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterAddSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- viewSceneWithParams:expectedValues:expectedValueInterval:completion:@
viewSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterViewSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
viewSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterViewSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeSceneWithParams:expectedValues:expectedValueInterval:completion:@
removeSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterRemoveSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterRemoveSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:@
removeAllScenesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterRemoveAllScenesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeAllScenesWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterRemoveAllScenesParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- storeSceneWithParams:expectedValues:expectedValueInterval:completion:@
storeSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterStoreSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
storeSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterStoreSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- recallSceneWithParams:expectedValues:expectedValueInterval:completion:@
recallSceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterRecallSceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
recallSceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterRecallSceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:@
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterGetSceneMembershipParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterScenesManagement getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterGetSceneMembershipParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- copySceneWithParams:expectedValues:expectedValueInterval:completion:@
copySceneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRScenesManagementClusterCopySceneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterScenesManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
copySceneWithParams_expectedValues_expectedValueInterval_completion mtrClusterScenesManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendOwnedMessage mtrClusterScenesManagement copySceneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRScenesManagementClusterCopySceneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSceneTableSizeWithParams:@
readAttributeSceneTableSizeWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeSceneTableSizeWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeSceneTableSizeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFabricSceneInfoWithParams:@
readAttributeFabricSceneInfoWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeFabricSceneInfoWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeFabricSceneInfoWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRReadParams params) => mtrClusterScenesManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterScenesManagement params =
  sendMessage mtrClusterScenesManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterScenesManagement mtrClusterScenesManagement => mtrClusterScenesManagement -> IO (Id MTRClusterScenesManagement)
init_ mtrClusterScenesManagement =
  sendOwnedMessage mtrClusterScenesManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterScenesManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterScenesManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterScenesManagement mtrClusterScenesManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterScenesManagement -> device -> endpointID -> queue -> IO (Id MTRClusterScenesManagement)
initWithDevice_endpointID_queue mtrClusterScenesManagement device endpointID queue =
  sendOwnedMessage mtrClusterScenesManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addSceneWithParams:expectedValues:expectedValueInterval:completion:@
addSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterAddSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
addSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @viewSceneWithParams:expectedValues:expectedValueInterval:completion:@
viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterViewSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
viewSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "viewSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeSceneWithParams:expectedValues:expectedValueInterval:completion:@
removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterRemoveSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:@
removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterRemoveAllScenesParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeAllScenesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeAllScenesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @storeSceneWithParams:expectedValues:expectedValueInterval:completion:@
storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterStoreSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
storeSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "storeSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @recallSceneWithParams:expectedValues:expectedValueInterval:completion:@
recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterRecallSceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
recallSceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "recallSceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:@
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterGetSceneMembershipParams, Id NSArray, Id NSNumber, Ptr ()] ()
getSceneMembershipWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getSceneMembershipWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @copySceneWithParams:expectedValues:expectedValueInterval:completion:@
copySceneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRScenesManagementClusterCopySceneParams, Id NSArray, Id NSNumber, Ptr ()] ()
copySceneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "copySceneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSceneTableSizeWithParams:@
readAttributeSceneTableSizeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSceneTableSizeWithParamsSelector = mkSelector "readAttributeSceneTableSizeWithParams:"

-- | @Selector@ for @readAttributeFabricSceneInfoWithParams:@
readAttributeFabricSceneInfoWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFabricSceneInfoWithParamsSelector = mkSelector "readAttributeFabricSceneInfoWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterScenesManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterScenesManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterScenesManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

