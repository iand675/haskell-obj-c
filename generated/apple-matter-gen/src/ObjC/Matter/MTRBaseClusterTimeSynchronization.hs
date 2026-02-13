{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Synchronization
--
-- Accurate time is required for a number of reasons, including scheduling, display and validating security materials.
--
-- Generated bindings for @MTRBaseClusterTimeSynchronization@.
module ObjC.Matter.MTRBaseClusterTimeSynchronization
  ( MTRBaseClusterTimeSynchronization
  , IsMTRBaseClusterTimeSynchronization(..)
  , setUTCTimeWithParams_completion
  , setTrustedTimeSourceWithParams_completion
  , setTimeZoneWithParams_completion
  , setDSTOffsetWithParams_completion
  , setDefaultNTPWithParams_completion
  , readAttributeUTCTimeWithCompletion
  , subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeGranularityWithCompletion
  , subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandler
  , readAttributeGranularityWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeSourceWithCompletion
  , subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completion
  , readAttributeTrustedTimeSourceWithCompletion
  , subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandler
  , readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultNTPWithCompletion
  , subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeZoneWithCompletion
  , subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completion
  , readAttributeDSTOffsetWithCompletion
  , subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandler
  , readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocalTimeWithCompletion
  , subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeZoneDatabaseWithCompletion
  , subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completion
  , readAttributeNTPServerAvailableWithCompletion
  , subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandler
  , readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completion
  , readAttributeTimeZoneListMaxSizeWithCompletion
  , subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completion
  , readAttributeDSTOffsetListMaxSizeWithCompletion
  , subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandler
  , readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportsDNSResolveWithCompletion
  , subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completion
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
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDSTOffsetListMaxSizeWithCompletionSelector
  , readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDSTOffsetWithCompletionSelector
  , readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultNTPWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGranularityWithCompletionSelector
  , readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocalTimeWithCompletionSelector
  , readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNTPServerAvailableWithCompletionSelector
  , readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportsDNSResolveWithCompletionSelector
  , readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeSourceWithCompletionSelector
  , readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeZoneDatabaseWithCompletionSelector
  , readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeZoneListMaxSizeWithCompletionSelector
  , readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTimeZoneWithCompletionSelector
  , readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTrustedTimeSourceWithCompletionSelector
  , readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUTCTimeWithCompletionSelector
  , setDSTOffsetWithParams_completionSelector
  , setDefaultNTPWithParams_completionSelector
  , setTimeZoneWithParams_completionSelector
  , setTrustedTimeSourceWithParams_completionSelector
  , setUTCTimeWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command SetUTCTime
--
-- This command is used to set the UTC time of the node.
--
-- ObjC selector: @- setUTCTimeWithParams:completion:@
setUTCTimeWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetUTCTimeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setUTCTimeWithParams_completion mtrBaseClusterTimeSynchronization params completion =
  sendMessage mtrBaseClusterTimeSynchronization setUTCTimeWithParams_completionSelector (toMTRTimeSynchronizationClusterSetUTCTimeParams params) completion

-- | Command SetTrustedTimeSource
--
-- This command is used to set the TrustedTimeSource attribute.
--
-- ObjC selector: @- setTrustedTimeSourceWithParams:completion:@
setTrustedTimeSourceWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTrustedTimeSourceParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setTrustedTimeSourceWithParams_completion mtrBaseClusterTimeSynchronization params completion =
  sendMessage mtrBaseClusterTimeSynchronization setTrustedTimeSourceWithParams_completionSelector (toMTRTimeSynchronizationClusterSetTrustedTimeSourceParams params) completion

-- | Command SetTimeZone
--
-- This command is used to set the time zone of the node.
--
-- ObjC selector: @- setTimeZoneWithParams:completion:@
setTimeZoneWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTimeZoneParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setTimeZoneWithParams_completion mtrBaseClusterTimeSynchronization params completion =
  sendMessage mtrBaseClusterTimeSynchronization setTimeZoneWithParams_completionSelector (toMTRTimeSynchronizationClusterSetTimeZoneParams params) completion

-- | Command SetDSTOffset
--
-- This command is used to set the DST offsets for a node.
--
-- ObjC selector: @- setDSTOffsetWithParams:completion:@
setDSTOffsetWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDSTOffsetParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setDSTOffsetWithParams_completion mtrBaseClusterTimeSynchronization params completion =
  sendMessage mtrBaseClusterTimeSynchronization setDSTOffsetWithParams_completionSelector (toMTRTimeSynchronizationClusterSetDSTOffsetParams params) completion

-- | Command SetDefaultNTP
--
-- This command is used to set the DefaultNTP attribute.
--
-- ObjC selector: @- setDefaultNTPWithParams:completion:@
setDefaultNTPWithParams_completion :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDefaultNTPParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> IO ()
setDefaultNTPWithParams_completion mtrBaseClusterTimeSynchronization params completion =
  sendMessage mtrBaseClusterTimeSynchronization setDefaultNTPWithParams_completionSelector (toMTRTimeSynchronizationClusterSetDefaultNTPParams params) completion

-- | @- readAttributeUTCTimeWithCompletion:@
readAttributeUTCTimeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeUTCTimeWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeUTCTimeWithCompletionSelector completion

-- | @- subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGranularityWithCompletion:@
readAttributeGranularityWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeGranularityWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeGranularityWithCompletionSelector completion

-- | @- subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:@
readAttributeGranularityWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGranularityWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTimeSourceWithCompletion:@
readAttributeTimeSourceWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeSourceWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeTimeSourceWithCompletionSelector completion

-- | @- subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTrustedTimeSourceWithCompletion:@
readAttributeTrustedTimeSourceWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTrustedTimeSourceWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeTrustedTimeSourceWithCompletionSelector completion

-- | @- subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDefaultNTPWithCompletion:@
readAttributeDefaultNTPWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeDefaultNTPWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeDefaultNTPWithCompletionSelector completion

-- | @- subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTimeZoneWithCompletion:@
readAttributeTimeZoneWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeZoneWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeTimeZoneWithCompletionSelector completion

-- | @- subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDSTOffsetWithCompletion:@
readAttributeDSTOffsetWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeDSTOffsetWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeDSTOffsetWithCompletionSelector completion

-- | @- subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLocalTimeWithCompletion:@
readAttributeLocalTimeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeLocalTimeWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeLocalTimeWithCompletionSelector completion

-- | @- subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTimeZoneDatabaseWithCompletion:@
readAttributeTimeZoneDatabaseWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeZoneDatabaseWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeTimeZoneDatabaseWithCompletionSelector completion

-- | @- subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNTPServerAvailableWithCompletion:@
readAttributeNTPServerAvailableWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeNTPServerAvailableWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeNTPServerAvailableWithCompletionSelector completion

-- | @- subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTimeZoneListMaxSizeWithCompletion:@
readAttributeTimeZoneListMaxSizeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeTimeZoneListMaxSizeWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeTimeZoneListMaxSizeWithCompletionSelector completion

-- | @- subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDSTOffsetListMaxSizeWithCompletion:@
readAttributeDSTOffsetListMaxSizeWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeDSTOffsetListMaxSizeWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeDSTOffsetListMaxSizeWithCompletionSelector completion

-- | @- subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportsDNSResolveWithCompletion:@
readAttributeSupportsDNSResolveWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeSupportsDNSResolveWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeSupportsDNSResolveWithCompletionSelector completion

-- | @- subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTimeSynchronization completion =
  sendMessage mtrBaseClusterTimeSynchronization readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRSubscribeParams params) => mtrBaseClusterTimeSynchronization -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTimeSynchronization params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTimeSynchronization subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization => mtrBaseClusterTimeSynchronization -> IO (Id MTRBaseClusterTimeSynchronization)
init_ mtrBaseClusterTimeSynchronization =
  sendOwnedMessage mtrBaseClusterTimeSynchronization initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterTimeSynchronization)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTimeSynchronization"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTimeSynchronization mtrBaseClusterTimeSynchronization, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTimeSynchronization -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTimeSynchronization)
initWithDevice_endpointID_queue mtrBaseClusterTimeSynchronization device endpointID queue =
  sendOwnedMessage mtrBaseClusterTimeSynchronization initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setUTCTimeWithParams:completion:@
setUTCTimeWithParams_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetUTCTimeParams, Ptr ()] ()
setUTCTimeWithParams_completionSelector = mkSelector "setUTCTimeWithParams:completion:"

-- | @Selector@ for @setTrustedTimeSourceWithParams:completion:@
setTrustedTimeSourceWithParams_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetTrustedTimeSourceParams, Ptr ()] ()
setTrustedTimeSourceWithParams_completionSelector = mkSelector "setTrustedTimeSourceWithParams:completion:"

-- | @Selector@ for @setTimeZoneWithParams:completion:@
setTimeZoneWithParams_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetTimeZoneParams, Ptr ()] ()
setTimeZoneWithParams_completionSelector = mkSelector "setTimeZoneWithParams:completion:"

-- | @Selector@ for @setDSTOffsetWithParams:completion:@
setDSTOffsetWithParams_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetDSTOffsetParams, Ptr ()] ()
setDSTOffsetWithParams_completionSelector = mkSelector "setDSTOffsetWithParams:completion:"

-- | @Selector@ for @setDefaultNTPWithParams:completion:@
setDefaultNTPWithParams_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetDefaultNTPParams, Ptr ()] ()
setDefaultNTPWithParams_completionSelector = mkSelector "setDefaultNTPWithParams:completion:"

-- | @Selector@ for @readAttributeUTCTimeWithCompletion:@
readAttributeUTCTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUTCTimeWithCompletionSelector = mkSelector "readAttributeUTCTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUTCTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUTCTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUTCTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUTCTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeGranularityWithCompletion:@
readAttributeGranularityWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeGranularityWithCompletionSelector = mkSelector "readAttributeGranularityWithCompletion:"

-- | @Selector@ for @subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGranularityWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGranularityWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:@
readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGranularityWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeGranularityWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeSourceWithCompletion:@
readAttributeTimeSourceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTimeSourceWithCompletionSelector = mkSelector "readAttributeTimeSourceWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeSourceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTimeSourceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeSourceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTrustedTimeSourceWithCompletion:@
readAttributeTrustedTimeSourceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTrustedTimeSourceWithCompletionSelector = mkSelector "readAttributeTrustedTimeSourceWithCompletion:"

-- | @Selector@ for @subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTrustedTimeSourceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTrustedTimeSourceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTrustedTimeSourceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTrustedTimeSourceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultNTPWithCompletion:@
readAttributeDefaultNTPWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDefaultNTPWithCompletionSelector = mkSelector "readAttributeDefaultNTPWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultNTPWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultNTPWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultNTPWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultNTPWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeZoneWithCompletion:@
readAttributeTimeZoneWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTimeZoneWithCompletionSelector = mkSelector "readAttributeTimeZoneWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTimeZoneWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeZoneWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTimeZoneWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeZoneWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDSTOffsetWithCompletion:@
readAttributeDSTOffsetWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDSTOffsetWithCompletionSelector = mkSelector "readAttributeDSTOffsetWithCompletion:"

-- | @Selector@ for @subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDSTOffsetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDSTOffsetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDSTOffsetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDSTOffsetWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocalTimeWithCompletion:@
readAttributeLocalTimeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLocalTimeWithCompletionSelector = mkSelector "readAttributeLocalTimeWithCompletion:"

-- | @Selector@ for @subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLocalTimeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalTimeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLocalTimeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocalTimeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeZoneDatabaseWithCompletion:@
readAttributeTimeZoneDatabaseWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTimeZoneDatabaseWithCompletionSelector = mkSelector "readAttributeTimeZoneDatabaseWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTimeZoneDatabaseWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeZoneDatabaseWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTimeZoneDatabaseWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeZoneDatabaseWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNTPServerAvailableWithCompletion:@
readAttributeNTPServerAvailableWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNTPServerAvailableWithCompletionSelector = mkSelector "readAttributeNTPServerAvailableWithCompletion:"

-- | @Selector@ for @subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNTPServerAvailableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNTPServerAvailableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:@
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNTPServerAvailableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNTPServerAvailableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTimeZoneListMaxSizeWithCompletion:@
readAttributeTimeZoneListMaxSizeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTimeZoneListMaxSizeWithCompletionSelector = mkSelector "readAttributeTimeZoneListMaxSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTimeZoneListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTimeZoneListMaxSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTimeZoneListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTimeZoneListMaxSizeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDSTOffsetListMaxSizeWithCompletion:@
readAttributeDSTOffsetListMaxSizeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDSTOffsetListMaxSizeWithCompletionSelector = mkSelector "readAttributeDSTOffsetListMaxSizeWithCompletion:"

-- | @Selector@ for @subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDSTOffsetListMaxSizeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDSTOffsetListMaxSizeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDSTOffsetListMaxSizeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDSTOffsetListMaxSizeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportsDNSResolveWithCompletion:@
readAttributeSupportsDNSResolveWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportsDNSResolveWithCompletionSelector = mkSelector "readAttributeSupportsDNSResolveWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportsDNSResolveWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportsDNSResolveWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportsDNSResolveWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportsDNSResolveWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterTimeSynchronization)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterTimeSynchronization)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterTimeSynchronization)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

