{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Camera AV Settings User Level Management    This cluster provides an interface into controls associated with the operation of a device that provides pan, tilt, and zoom functions, either mechanically, or against a digital image.
--
-- Generated bindings for @MTRClusterCameraAVSettingsUserLevelManagement@.
module ObjC.Matter.MTRClusterCameraAVSettingsUserLevelManagement
  ( MTRClusterCameraAVSettingsUserLevelManagement
  , IsMTRClusterCameraAVSettingsUserLevelManagement(..)
  , mptzSetPositionWithParams_expectedValues_expectedValueInterval_completion
  , mptzSetPositionWithExpectedValues_expectedValueInterval_completion
  , mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion
  , mptzRelativeMoveWithExpectedValues_expectedValueInterval_completion
  , mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completion
  , mptzSavePresetWithParams_expectedValues_expectedValueInterval_completion
  , mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completion
  , dptzSetViewportWithParams_expectedValues_expectedValueInterval_completion
  , dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMPTZPositionWithParams
  , readAttributeMaxPresetsWithParams
  , readAttributeMPTZPresetsWithParams
  , readAttributeDPTZStreamsWithParams
  , readAttributeZoomMaxWithParams
  , readAttributeTiltMinWithParams
  , readAttributeTiltMaxWithParams
  , readAttributePanMinWithParams
  , readAttributePanMaxWithParams
  , readAttributeMovementStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector
  , dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector
  , mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector
  , mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector
  , mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDPTZStreamsWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMPTZPositionWithParamsSelector
  , readAttributeMPTZPresetsWithParamsSelector
  , readAttributeMaxPresetsWithParamsSelector
  , readAttributeMovementStateWithParamsSelector
  , readAttributePanMaxWithParamsSelector
  , readAttributePanMinWithParamsSelector
  , readAttributeTiltMaxWithParamsSelector
  , readAttributeTiltMinWithParamsSelector
  , readAttributeZoomMaxWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:@
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:@
mptzSetPositionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzSetPositionWithExpectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:@
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completion mtrClusterCameraAVSettingsUserLevelManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMPTZPositionWithParams:@
readAttributeMPTZPositionWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMPTZPositionWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeMPTZPositionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxPresetsWithParams:@
readAttributeMaxPresetsWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMaxPresetsWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeMaxPresetsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMPTZPresetsWithParams:@
readAttributeMPTZPresetsWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMPTZPresetsWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeMPTZPresetsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDPTZStreamsWithParams:@
readAttributeDPTZStreamsWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeDPTZStreamsWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeDPTZStreamsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeZoomMaxWithParams:@
readAttributeZoomMaxWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeZoomMaxWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeZoomMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTiltMinWithParams:@
readAttributeTiltMinWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeTiltMinWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeTiltMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTiltMaxWithParams:@
readAttributeTiltMaxWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeTiltMaxWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeTiltMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePanMinWithParams:@
readAttributePanMinWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributePanMinWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributePanMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePanMaxWithParams:@
readAttributePanMaxWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributePanMaxWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributePanMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMovementStateWithParams:@
readAttributeMovementStateWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeMovementStateWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeMovementStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRReadParams params) => mtrClusterCameraAVSettingsUserLevelManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCameraAVSettingsUserLevelManagement params =
  sendMessage mtrClusterCameraAVSettingsUserLevelManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement => mtrClusterCameraAVSettingsUserLevelManagement -> IO (Id MTRClusterCameraAVSettingsUserLevelManagement)
init_ mtrClusterCameraAVSettingsUserLevelManagement =
  sendOwnedMessage mtrClusterCameraAVSettingsUserLevelManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterCameraAVSettingsUserLevelManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCameraAVSettingsUserLevelManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCameraAVSettingsUserLevelManagement mtrClusterCameraAVSettingsUserLevelManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCameraAVSettingsUserLevelManagement -> device -> endpointID -> queue -> IO (Id MTRClusterCameraAVSettingsUserLevelManagement)
initWithDevice_endpointID_queue mtrClusterCameraAVSettingsUserLevelManagement device endpointID queue =
  sendOwnedMessage mtrClusterCameraAVSettingsUserLevelManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:@
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, Id NSArray, Id NSNumber, Ptr ()] ()
mptzSetPositionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZSetPositionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:@
mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
mptzSetPositionWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZSetPositionWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, Id NSArray, Id NSNumber, Ptr ()] ()
mptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:@
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
mptzRelativeMoveWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZRelativeMoveWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams, Id NSArray, Id NSNumber, Ptr ()] ()
mptzMoveToPresetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZMoveToPresetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, Id NSArray, Id NSNumber, Ptr ()] ()
mptzSavePresetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZSavePresetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:@
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams, Id NSArray, Id NSNumber, Ptr ()] ()
mptzRemovePresetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "MPTZRemovePresetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:@
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, Id NSArray, Id NSNumber, Ptr ()] ()
dptzSetViewportWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "DPTZSetViewportWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:@
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, Id NSArray, Id NSNumber, Ptr ()] ()
dptzRelativeMoveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "DPTZRelativeMoveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMPTZPositionWithParams:@
readAttributeMPTZPositionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMPTZPositionWithParamsSelector = mkSelector "readAttributeMPTZPositionWithParams:"

-- | @Selector@ for @readAttributeMaxPresetsWithParams:@
readAttributeMaxPresetsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxPresetsWithParamsSelector = mkSelector "readAttributeMaxPresetsWithParams:"

-- | @Selector@ for @readAttributeMPTZPresetsWithParams:@
readAttributeMPTZPresetsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMPTZPresetsWithParamsSelector = mkSelector "readAttributeMPTZPresetsWithParams:"

-- | @Selector@ for @readAttributeDPTZStreamsWithParams:@
readAttributeDPTZStreamsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDPTZStreamsWithParamsSelector = mkSelector "readAttributeDPTZStreamsWithParams:"

-- | @Selector@ for @readAttributeZoomMaxWithParams:@
readAttributeZoomMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeZoomMaxWithParamsSelector = mkSelector "readAttributeZoomMaxWithParams:"

-- | @Selector@ for @readAttributeTiltMinWithParams:@
readAttributeTiltMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTiltMinWithParamsSelector = mkSelector "readAttributeTiltMinWithParams:"

-- | @Selector@ for @readAttributeTiltMaxWithParams:@
readAttributeTiltMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTiltMaxWithParamsSelector = mkSelector "readAttributeTiltMaxWithParams:"

-- | @Selector@ for @readAttributePanMinWithParams:@
readAttributePanMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePanMinWithParamsSelector = mkSelector "readAttributePanMinWithParams:"

-- | @Selector@ for @readAttributePanMaxWithParams:@
readAttributePanMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePanMaxWithParamsSelector = mkSelector "readAttributePanMaxWithParams:"

-- | @Selector@ for @readAttributeMovementStateWithParams:@
readAttributeMovementStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMovementStateWithParamsSelector = mkSelector "readAttributeMovementStateWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterCameraAVSettingsUserLevelManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCameraAVSettingsUserLevelManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCameraAVSettingsUserLevelManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

