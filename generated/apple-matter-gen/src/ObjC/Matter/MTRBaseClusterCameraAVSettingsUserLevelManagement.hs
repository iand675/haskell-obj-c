{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Camera AV Settings User Level Management
--
-- This cluster provides an interface into controls associated with the operation of a device that provides pan, tilt, and zoom functions, either mechanically, or against a digital image.
--
-- Generated bindings for @MTRBaseClusterCameraAVSettingsUserLevelManagement@.
module ObjC.Matter.MTRBaseClusterCameraAVSettingsUserLevelManagement
  ( MTRBaseClusterCameraAVSettingsUserLevelManagement
  , IsMTRBaseClusterCameraAVSettingsUserLevelManagement(..)
  , mptzSetPositionWithParams_completion
  , mptzSetPositionWithCompletion
  , mptzRelativeMoveWithParams_completion
  , mptzRelativeMoveWithCompletion
  , mptzMoveToPresetWithParams_completion
  , mptzSavePresetWithParams_completion
  , mptzRemovePresetWithParams_completion
  , dptzSetViewportWithParams_completion
  , dptzRelativeMoveWithParams_completion
  , readAttributeMPTZPositionWithCompletion
  , subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandler
  , readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxPresetsWithCompletion
  , subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completion
  , readAttributeMPTZPresetsWithCompletion
  , subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandler
  , readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completion
  , readAttributeDPTZStreamsWithCompletion
  , subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandler
  , readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completion
  , readAttributeZoomMaxWithCompletion
  , subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeTiltMinWithCompletion
  , subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandler
  , readAttributeTiltMinWithClusterStateCache_endpoint_queue_completion
  , readAttributeTiltMaxWithCompletion
  , subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributePanMinWithCompletion
  , subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandler
  , readAttributePanMinWithClusterStateCache_endpoint_queue_completion
  , readAttributePanMaxWithCompletion
  , subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandler
  , readAttributePanMaxWithClusterStateCache_endpoint_queue_completion
  , readAttributeMovementStateWithCompletion
  , subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandler
  , readAttributeMovementStateWithClusterStateCache_endpoint_queue_completion
  , readAttributeGeneratedCommandListWithCompletion
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAcceptedCommandListWithCompletion
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion
  , readAttributeAttributeListWithCompletion
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion
  , readAttributeFeatureMapWithCompletion
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion
  , readAttributeClusterRevisionWithCompletion
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion
  , init_
  , new
  , initWithDevice_endpointID_queue
  , dptzRelativeMoveWithParams_completionSelector
  , dptzSetViewportWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , mptzMoveToPresetWithParams_completionSelector
  , mptzRelativeMoveWithCompletionSelector
  , mptzRelativeMoveWithParams_completionSelector
  , mptzRemovePresetWithParams_completionSelector
  , mptzSavePresetWithParams_completionSelector
  , mptzSetPositionWithCompletionSelector
  , mptzSetPositionWithParams_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDPTZStreamsWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMPTZPositionWithCompletionSelector
  , readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMPTZPresetsWithCompletionSelector
  , readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxPresetsWithCompletionSelector
  , readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMovementStateWithCompletionSelector
  , readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePanMaxWithCompletionSelector
  , readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePanMinWithCompletionSelector
  , readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTiltMaxWithCompletionSelector
  , readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTiltMinWithCompletionSelector
  , readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeZoomMaxWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command MPTZSetPosition
--
-- This command SHALL move the camera to the provided values for pan, tilt, and zoom in the mechanical PTZ.
--
-- ObjC selector: @- MPTZSetPositionWithParams:completion:@
mptzSetPositionWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzSetPositionWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzSetPositionWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams params) completion

-- | @- MPTZSetPositionWithCompletion:@
mptzSetPositionWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
mptzSetPositionWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzSetPositionWithCompletionSelector completion

-- | Command MPTZRelativeMove
--
-- This command SHALL move the camera by the delta values relative to the currently defined position.
--
-- ObjC selector: @- MPTZRelativeMoveWithParams:completion:@
mptzRelativeMoveWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzRelativeMoveWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzRelativeMoveWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams params) completion

-- | @- MPTZRelativeMoveWithCompletion:@
mptzRelativeMoveWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
mptzRelativeMoveWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzRelativeMoveWithCompletionSelector completion

-- | Command MPTZMoveToPreset
--
-- This command SHALL move the camera to the positions specified by the Preset passed.
--
-- ObjC selector: @- MPTZMoveToPresetWithParams:completion:@
mptzMoveToPresetWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzMoveToPresetWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzMoveToPresetWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams params) completion

-- | Command MPTZSavePreset
--
-- This command allows creating a new preset or updating the values of an existing one.
--
-- ObjC selector: @- MPTZSavePresetWithParams:completion:@
mptzSavePresetWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzSavePresetWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzSavePresetWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams params) completion

-- | Command MPTZRemovePreset
--
-- This command SHALL remove a preset entry from the PresetMptzTable.
--
-- ObjC selector: @- MPTZRemovePresetWithParams:completion:@
mptzRemovePresetWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
mptzRemovePresetWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement mptzRemovePresetWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams params) completion

-- | Command DPTZSetViewport
--
-- This command allows for setting the digital viewport for a specific Video Stream.
--
-- ObjC selector: @- DPTZSetViewportWithParams:completion:@
dptzSetViewportWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
dptzSetViewportWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement dptzSetViewportWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams params) completion

-- | Command DPTZRelativeMove
--
-- This command SHALL change the per stream viewport by the amount specified in a relative fashion.
--
-- ObjC selector: @- DPTZRelativeMoveWithParams:completion:@
dptzRelativeMoveWithParams_completion :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> IO ()
dptzRelativeMoveWithParams_completion mtrBaseClusterCameraAVSettingsUserLevelManagement params completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement dptzRelativeMoveWithParams_completionSelector (toMTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams params) completion

-- | @- readAttributeMPTZPositionWithCompletion:@
readAttributeMPTZPositionWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMPTZPositionWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeMPTZPositionWithCompletionSelector completion

-- | @- subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxPresetsWithCompletion:@
readAttributeMaxPresetsWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMaxPresetsWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeMaxPresetsWithCompletionSelector completion

-- | @- subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMPTZPresetsWithCompletion:@
readAttributeMPTZPresetsWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMPTZPresetsWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeMPTZPresetsWithCompletionSelector completion

-- | @- subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDPTZStreamsWithCompletion:@
readAttributeDPTZStreamsWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeDPTZStreamsWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeDPTZStreamsWithCompletionSelector completion

-- | @- subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeZoomMaxWithCompletion:@
readAttributeZoomMaxWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeZoomMaxWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeZoomMaxWithCompletionSelector completion

-- | @- subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTiltMinWithCompletion:@
readAttributeTiltMinWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeTiltMinWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeTiltMinWithCompletionSelector completion

-- | @- subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTiltMaxWithCompletion:@
readAttributeTiltMaxWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeTiltMaxWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeTiltMaxWithCompletionSelector completion

-- | @- subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePanMinWithCompletion:@
readAttributePanMinWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributePanMinWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributePanMinWithCompletionSelector completion

-- | @- subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePanMinWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMinWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePanMinWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePanMaxWithCompletion:@
readAttributePanMaxWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributePanMaxWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributePanMaxWithCompletionSelector completion

-- | @- subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMaxWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePanMaxWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMovementStateWithCompletion:@
readAttributeMovementStateWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeMovementStateWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeMovementStateWithCompletionSelector completion

-- | @- subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCameraAVSettingsUserLevelManagement completion =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRSubscribeParams params) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCameraAVSettingsUserLevelManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCameraAVSettingsUserLevelManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement => mtrBaseClusterCameraAVSettingsUserLevelManagement -> IO (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
init_ mtrBaseClusterCameraAVSettingsUserLevelManagement =
  sendOwnedMessage mtrBaseClusterCameraAVSettingsUserLevelManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCameraAVSettingsUserLevelManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCameraAVSettingsUserLevelManagement mtrBaseClusterCameraAVSettingsUserLevelManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCameraAVSettingsUserLevelManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
initWithDevice_endpointID_queue mtrBaseClusterCameraAVSettingsUserLevelManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterCameraAVSettingsUserLevelManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @MPTZSetPositionWithParams:completion:@
mptzSetPositionWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZSetPositionParams, Ptr ()] ()
mptzSetPositionWithParams_completionSelector = mkSelector "MPTZSetPositionWithParams:completion:"

-- | @Selector@ for @MPTZSetPositionWithCompletion:@
mptzSetPositionWithCompletionSelector :: Selector '[Ptr ()] ()
mptzSetPositionWithCompletionSelector = mkSelector "MPTZSetPositionWithCompletion:"

-- | @Selector@ for @MPTZRelativeMoveWithParams:completion:@
mptzRelativeMoveWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZRelativeMoveParams, Ptr ()] ()
mptzRelativeMoveWithParams_completionSelector = mkSelector "MPTZRelativeMoveWithParams:completion:"

-- | @Selector@ for @MPTZRelativeMoveWithCompletion:@
mptzRelativeMoveWithCompletionSelector :: Selector '[Ptr ()] ()
mptzRelativeMoveWithCompletionSelector = mkSelector "MPTZRelativeMoveWithCompletion:"

-- | @Selector@ for @MPTZMoveToPresetWithParams:completion:@
mptzMoveToPresetWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZMoveToPresetParams, Ptr ()] ()
mptzMoveToPresetWithParams_completionSelector = mkSelector "MPTZMoveToPresetWithParams:completion:"

-- | @Selector@ for @MPTZSavePresetWithParams:completion:@
mptzSavePresetWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZSavePresetParams, Ptr ()] ()
mptzSavePresetWithParams_completionSelector = mkSelector "MPTZSavePresetWithParams:completion:"

-- | @Selector@ for @MPTZRemovePresetWithParams:completion:@
mptzRemovePresetWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterMPTZRemovePresetParams, Ptr ()] ()
mptzRemovePresetWithParams_completionSelector = mkSelector "MPTZRemovePresetWithParams:completion:"

-- | @Selector@ for @DPTZSetViewportWithParams:completion:@
dptzSetViewportWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterDPTZSetViewportParams, Ptr ()] ()
dptzSetViewportWithParams_completionSelector = mkSelector "DPTZSetViewportWithParams:completion:"

-- | @Selector@ for @DPTZRelativeMoveWithParams:completion:@
dptzRelativeMoveWithParams_completionSelector :: Selector '[Id MTRCameraAVSettingsUserLevelManagementClusterDPTZRelativeMoveParams, Ptr ()] ()
dptzRelativeMoveWithParams_completionSelector = mkSelector "DPTZRelativeMoveWithParams:completion:"

-- | @Selector@ for @readAttributeMPTZPositionWithCompletion:@
readAttributeMPTZPositionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMPTZPositionWithCompletionSelector = mkSelector "readAttributeMPTZPositionWithCompletion:"

-- | @Selector@ for @subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMPTZPositionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMPTZPositionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMPTZPositionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMPTZPositionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxPresetsWithCompletion:@
readAttributeMaxPresetsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxPresetsWithCompletionSelector = mkSelector "readAttributeMaxPresetsWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxPresetsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxPresetsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxPresetsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxPresetsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMPTZPresetsWithCompletion:@
readAttributeMPTZPresetsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMPTZPresetsWithCompletionSelector = mkSelector "readAttributeMPTZPresetsWithCompletion:"

-- | @Selector@ for @subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMPTZPresetsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMPTZPresetsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:@
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMPTZPresetsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMPTZPresetsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDPTZStreamsWithCompletion:@
readAttributeDPTZStreamsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDPTZStreamsWithCompletionSelector = mkSelector "readAttributeDPTZStreamsWithCompletion:"

-- | @Selector@ for @subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDPTZStreamsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDPTZStreamsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDPTZStreamsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDPTZStreamsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeZoomMaxWithCompletion:@
readAttributeZoomMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeZoomMaxWithCompletionSelector = mkSelector "readAttributeZoomMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeZoomMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeZoomMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeZoomMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeZoomMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTiltMinWithCompletion:@
readAttributeTiltMinWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTiltMinWithCompletionSelector = mkSelector "readAttributeTiltMinWithCompletion:"

-- | @Selector@ for @subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTiltMinWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTiltMinWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTiltMinWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTiltMinWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTiltMaxWithCompletion:@
readAttributeTiltMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTiltMaxWithCompletionSelector = mkSelector "readAttributeTiltMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTiltMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTiltMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTiltMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTiltMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePanMinWithCompletion:@
readAttributePanMinWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePanMinWithCompletionSelector = mkSelector "readAttributePanMinWithCompletion:"

-- | @Selector@ for @subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePanMinWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePanMinWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePanMinWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePanMinWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePanMinWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePanMaxWithCompletion:@
readAttributePanMaxWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePanMaxWithCompletionSelector = mkSelector "readAttributePanMaxWithCompletion:"

-- | @Selector@ for @subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePanMaxWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePanMaxWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:@
readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePanMaxWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePanMaxWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMovementStateWithCompletion:@
readAttributeMovementStateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMovementStateWithCompletionSelector = mkSelector "readAttributeMovementStateWithCompletion:"

-- | @Selector@ for @subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMovementStateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMovementStateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:@
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMovementStateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMovementStateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeGeneratedCommandListWithCompletionSelector = mkSelector "readAttributeGeneratedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAcceptedCommandListWithCompletionSelector = mkSelector "readAttributeAcceptedCommandListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAttributeListWithCompletionSelector = mkSelector "readAttributeAttributeListWithCompletion:"

-- | @Selector@ for @subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeFeatureMapWithCompletionSelector = mkSelector "readAttributeFeatureMapWithCompletion:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeClusterRevisionWithCompletionSelector = mkSelector "readAttributeClusterRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterCameraAVSettingsUserLevelManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

