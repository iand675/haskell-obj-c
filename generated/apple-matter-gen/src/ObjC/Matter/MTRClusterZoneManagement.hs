{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Zone Management    This cluster provides an interface to manage regions of interest, or Zones, which can be either manufacturer or user defined.
--
-- Generated bindings for @MTRClusterZoneManagement@.
module ObjC.Matter.MTRClusterZoneManagement
  ( MTRClusterZoneManagement
  , IsMTRClusterZoneManagement(..)
  , createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion
  , updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion
  , removeZoneWithParams_expectedValues_expectedValueInterval_completion
  , createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completion
  , removeTriggerWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxUserDefinedZonesWithParams
  , readAttributeMaxZonesWithParams
  , readAttributeZonesWithParams
  , readAttributeTriggersWithParams
  , readAttributeSensitivityMaxWithParams
  , readAttributeSensitivityWithParams
  , writeAttributeSensitivityWithValue_expectedValueInterval
  , writeAttributeSensitivityWithValue_expectedValueInterval_params
  , readAttributeTwoDCartesianMaxWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector
  , createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaxUserDefinedZonesWithParamsSelector
  , readAttributeMaxZonesWithParamsSelector
  , readAttributeSensitivityMaxWithParamsSelector
  , readAttributeSensitivityWithParamsSelector
  , readAttributeTriggersWithParamsSelector
  , readAttributeTwoDCartesianMaxWithParamsSelector
  , readAttributeZonesWithParamsSelector
  , removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeSensitivityWithValue_expectedValueIntervalSelector
  , writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterCreateTwoDCartesianZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterZoneManagement createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRZoneManagementClusterCreateTwoDCartesianZoneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterUpdateTwoDCartesianZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterZoneManagement updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRZoneManagementClusterUpdateTwoDCartesianZoneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeZoneWithParams:expectedValues:expectedValueInterval:completion:@
removeZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterRemoveZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterZoneManagement removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRZoneManagementClusterRemoveZoneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:@
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterCreateOrUpdateTriggerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterZoneManagement createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRZoneManagementClusterCreateOrUpdateTriggerParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeTriggerWithParams:expectedValues:expectedValueInterval:completion:@
removeTriggerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRZoneManagementClusterRemoveTriggerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeTriggerWithParams_expectedValues_expectedValueInterval_completion mtrClusterZoneManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterZoneManagement removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRZoneManagementClusterRemoveTriggerParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMaxUserDefinedZonesWithParams:@
readAttributeMaxUserDefinedZonesWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeMaxUserDefinedZonesWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeMaxUserDefinedZonesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxZonesWithParams:@
readAttributeMaxZonesWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeMaxZonesWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeMaxZonesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeZonesWithParams:@
readAttributeZonesWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeZonesWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeZonesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTriggersWithParams:@
readAttributeTriggersWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeTriggersWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeTriggersWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSensitivityMaxWithParams:@
readAttributeSensitivityMaxWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeSensitivityMaxWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeSensitivityMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSensitivityWithParams:@
readAttributeSensitivityWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeSensitivityWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeSensitivityWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSensitivityWithValue:expectedValueInterval:@
writeAttributeSensitivityWithValue_expectedValueInterval :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterZoneManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSensitivityWithValue_expectedValueInterval mtrClusterZoneManagement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterZoneManagement writeAttributeSensitivityWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeSensitivityWithValue_expectedValueInterval_params :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterZoneManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSensitivityWithValue_expectedValueInterval_params mtrClusterZoneManagement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterZoneManagement writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeTwoDCartesianMaxWithParams:@
readAttributeTwoDCartesianMaxWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeTwoDCartesianMaxWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeTwoDCartesianMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRReadParams params) => mtrClusterZoneManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterZoneManagement params =
  sendMessage mtrClusterZoneManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterZoneManagement mtrClusterZoneManagement => mtrClusterZoneManagement -> IO (Id MTRClusterZoneManagement)
init_ mtrClusterZoneManagement =
  sendOwnedMessage mtrClusterZoneManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterZoneManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterZoneManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterZoneManagement mtrClusterZoneManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterZoneManagement -> device -> endpointID -> queue -> IO (Id MTRClusterZoneManagement)
initWithDevice_endpointID_queue mtrClusterZoneManagement device endpointID queue =
  sendOwnedMessage mtrClusterZoneManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRZoneManagementClusterCreateTwoDCartesianZoneParams, Id NSArray, Id NSNumber, Ptr ()] ()
createTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "createTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:@
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRZoneManagementClusterUpdateTwoDCartesianZoneParams, Id NSArray, Id NSNumber, Ptr ()] ()
updateTwoDCartesianZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "updateTwoDCartesianZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeZoneWithParams:expectedValues:expectedValueInterval:completion:@
removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRZoneManagementClusterRemoveZoneParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:@
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRZoneManagementClusterCreateOrUpdateTriggerParams, Id NSArray, Id NSNumber, Ptr ()] ()
createOrUpdateTriggerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "createOrUpdateTriggerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeTriggerWithParams:expectedValues:expectedValueInterval:completion:@
removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRZoneManagementClusterRemoveTriggerParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeTriggerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeTriggerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxUserDefinedZonesWithParams:@
readAttributeMaxUserDefinedZonesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxUserDefinedZonesWithParamsSelector = mkSelector "readAttributeMaxUserDefinedZonesWithParams:"

-- | @Selector@ for @readAttributeMaxZonesWithParams:@
readAttributeMaxZonesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxZonesWithParamsSelector = mkSelector "readAttributeMaxZonesWithParams:"

-- | @Selector@ for @readAttributeZonesWithParams:@
readAttributeZonesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeZonesWithParamsSelector = mkSelector "readAttributeZonesWithParams:"

-- | @Selector@ for @readAttributeTriggersWithParams:@
readAttributeTriggersWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTriggersWithParamsSelector = mkSelector "readAttributeTriggersWithParams:"

-- | @Selector@ for @readAttributeSensitivityMaxWithParams:@
readAttributeSensitivityMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSensitivityMaxWithParamsSelector = mkSelector "readAttributeSensitivityMaxWithParams:"

-- | @Selector@ for @readAttributeSensitivityWithParams:@
readAttributeSensitivityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSensitivityWithParamsSelector = mkSelector "readAttributeSensitivityWithParams:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:expectedValueInterval:@
writeAttributeSensitivityWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSensitivityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSensitivityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSensitivityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSensitivityWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeTwoDCartesianMaxWithParams:@
readAttributeTwoDCartesianMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTwoDCartesianMaxWithParamsSelector = mkSelector "readAttributeTwoDCartesianMaxWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterZoneManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterZoneManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterZoneManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

