{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Administrator Commissioning
--
-- Commands to trigger a Node to allow a new Administrator to commission it.
--
-- Generated bindings for @MTRBaseClusterAdministratorCommissioning@.
module ObjC.Matter.MTRBaseClusterAdministratorCommissioning
  ( MTRBaseClusterAdministratorCommissioning
  , IsMTRBaseClusterAdministratorCommissioning(..)
  , openCommissioningWindowWithParams_completion
  , openBasicCommissioningWindowWithParams_completion
  , revokeCommissioningWithParams_completion
  , revokeCommissioningWithCompletion
  , readAttributeWindowStatusWithCompletion
  , subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandler
  , readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completion
  , readAttributeAdminFabricIndexWithCompletion
  , subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completion
  , readAttributeAdminVendorIdWithCompletion
  , subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completion
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
  , initWithDevice_endpoint_queue
  , openCommissioningWindowWithParams_completionHandler
  , openBasicCommissioningWindowWithParams_completionHandler
  , revokeCommissioningWithParams_completionHandler
  , revokeCommissioningWithCompletionHandler
  , readAttributeWindowStatusWithCompletionHandler
  , subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAdminFabricIndexWithCompletionHandler
  , subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAdminVendorIdWithCompletionHandler
  , subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeGeneratedCommandListWithCompletionHandler
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAcceptedCommandListWithCompletionHandler
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeAttributeListWithCompletionHandler
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFeatureMapWithCompletionHandler
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeClusterRevisionWithCompletionHandler
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , openBasicCommissioningWindowWithParams_completionHandlerSelector
  , openBasicCommissioningWindowWithParams_completionSelector
  , openCommissioningWindowWithParams_completionHandlerSelector
  , openCommissioningWindowWithParams_completionSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdminFabricIndexWithCompletionHandlerSelector
  , readAttributeAdminFabricIndexWithCompletionSelector
  , readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdminVendorIdWithCompletionHandlerSelector
  , readAttributeAdminVendorIdWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeWindowStatusWithCompletionHandlerSelector
  , readAttributeWindowStatusWithCompletionSelector
  , revokeCommissioningWithCompletionHandlerSelector
  , revokeCommissioningWithCompletionSelector
  , revokeCommissioningWithParams_completionHandlerSelector
  , revokeCommissioningWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command OpenCommissioningWindow
--
-- This command is used by a current Administrator to instruct a Node to go into commissioning mode.
--
-- ObjC selector: @- openCommissioningWindowWithParams:completion:@
openCommissioningWindowWithParams_completion :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openCommissioningWindowWithParams_completion mtrBaseClusterAdministratorCommissioning params completion =
  sendMessage mtrBaseClusterAdministratorCommissioning openCommissioningWindowWithParams_completionSelector (toMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params) completion

-- | Command OpenBasicCommissioningWindow
--
-- This command MAY be used by a current Administrator to instruct a Node to go into commissioning mode, if the node supports the Basic Commissioning Method.
--
-- ObjC selector: @- openBasicCommissioningWindowWithParams:completion:@
openBasicCommissioningWindowWithParams_completion :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openBasicCommissioningWindowWithParams_completion mtrBaseClusterAdministratorCommissioning params completion =
  sendMessage mtrBaseClusterAdministratorCommissioning openBasicCommissioningWindowWithParams_completionSelector (toMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params) completion

-- | Command RevokeCommissioning
--
-- This command is used by a current Administrator to instruct a Node to revoke any active OpenCommissioningWindow or OpenBasicCommissioningWindow command.
--
-- ObjC selector: @- revokeCommissioningWithParams:completion:@
revokeCommissioningWithParams_completion :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterRevokeCommissioningParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
revokeCommissioningWithParams_completion mtrBaseClusterAdministratorCommissioning params completion =
  sendMessage mtrBaseClusterAdministratorCommissioning revokeCommissioningWithParams_completionSelector (toMTRAdministratorCommissioningClusterRevokeCommissioningParams params) completion

-- | @- revokeCommissioningWithCompletion:@
revokeCommissioningWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
revokeCommissioningWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning revokeCommissioningWithCompletionSelector completion

-- | @- readAttributeWindowStatusWithCompletion:@
readAttributeWindowStatusWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeWindowStatusWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeWindowStatusWithCompletionSelector completion

-- | @- subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAdminFabricIndexWithCompletion:@
readAttributeAdminFabricIndexWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAdminFabricIndexWithCompletionSelector completion

-- | @- subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAdminVendorIdWithCompletion:@
readAttributeAdminVendorIdWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminVendorIdWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAdminVendorIdWithCompletionSelector completion

-- | @- subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterAdministratorCommissioning completion =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> IO (Id MTRBaseClusterAdministratorCommissioning)
init_ mtrBaseClusterAdministratorCommissioning =
  sendOwnedMessage mtrBaseClusterAdministratorCommissioning initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterAdministratorCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterAdministratorCommissioning -> device -> CUShort -> queue -> IO (Id MTRBaseClusterAdministratorCommissioning)
initWithDevice_endpoint_queue mtrBaseClusterAdministratorCommissioning device endpoint queue =
  sendOwnedMessage mtrBaseClusterAdministratorCommissioning initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- openCommissioningWindowWithParams:completionHandler:@
openCommissioningWindowWithParams_completionHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openCommissioningWindowWithParams_completionHandler mtrBaseClusterAdministratorCommissioning params completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning openCommissioningWindowWithParams_completionHandlerSelector (toMTRAdministratorCommissioningClusterOpenCommissioningWindowParams params) completionHandler

-- | @- openBasicCommissioningWindowWithParams:completionHandler:@
openBasicCommissioningWindowWithParams_completionHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
openBasicCommissioningWindowWithParams_completionHandler mtrBaseClusterAdministratorCommissioning params completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning openBasicCommissioningWindowWithParams_completionHandlerSelector (toMTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams params) completionHandler

-- | @- revokeCommissioningWithParams:completionHandler:@
revokeCommissioningWithParams_completionHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRAdministratorCommissioningClusterRevokeCommissioningParams params) => mtrBaseClusterAdministratorCommissioning -> params -> Ptr () -> IO ()
revokeCommissioningWithParams_completionHandler mtrBaseClusterAdministratorCommissioning params completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning revokeCommissioningWithParams_completionHandlerSelector (toMTRAdministratorCommissioningClusterRevokeCommissioningParams params) completionHandler

-- | @- revokeCommissioningWithCompletionHandler:@
revokeCommissioningWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
revokeCommissioningWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning revokeCommissioningWithCompletionHandlerSelector completionHandler

-- | @- readAttributeWindowStatusWithCompletionHandler:@
readAttributeWindowStatusWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeWindowStatusWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeWindowStatusWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAdminFabricIndexWithCompletionHandler:@
readAttributeAdminFabricIndexWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAdminFabricIndexWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAdminVendorIdWithCompletionHandler:@
readAttributeAdminVendorIdWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAdminVendorIdWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAdminVendorIdWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning => mtrBaseClusterAdministratorCommissioning -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterAdministratorCommissioning completionHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterAdministratorCommissioning -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterAdministratorCommissioning minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterAdministratorCommissioning subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterAdministratorCommissioning"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterAdministratorCommissioning mtrBaseClusterAdministratorCommissioning, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterAdministratorCommissioning -> device -> endpointID -> queue -> IO (Id MTRBaseClusterAdministratorCommissioning)
initWithDevice_endpointID_queue mtrBaseClusterAdministratorCommissioning device endpointID queue =
  sendOwnedMessage mtrBaseClusterAdministratorCommissioning initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openCommissioningWindowWithParams:completion:@
openCommissioningWindowWithParams_completionSelector :: Selector '[Id MTRAdministratorCommissioningClusterOpenCommissioningWindowParams, Ptr ()] ()
openCommissioningWindowWithParams_completionSelector = mkSelector "openCommissioningWindowWithParams:completion:"

-- | @Selector@ for @openBasicCommissioningWindowWithParams:completion:@
openBasicCommissioningWindowWithParams_completionSelector :: Selector '[Id MTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams, Ptr ()] ()
openBasicCommissioningWindowWithParams_completionSelector = mkSelector "openBasicCommissioningWindowWithParams:completion:"

-- | @Selector@ for @revokeCommissioningWithParams:completion:@
revokeCommissioningWithParams_completionSelector :: Selector '[Id MTRAdministratorCommissioningClusterRevokeCommissioningParams, Ptr ()] ()
revokeCommissioningWithParams_completionSelector = mkSelector "revokeCommissioningWithParams:completion:"

-- | @Selector@ for @revokeCommissioningWithCompletion:@
revokeCommissioningWithCompletionSelector :: Selector '[Ptr ()] ()
revokeCommissioningWithCompletionSelector = mkSelector "revokeCommissioningWithCompletion:"

-- | @Selector@ for @readAttributeWindowStatusWithCompletion:@
readAttributeWindowStatusWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeWindowStatusWithCompletionSelector = mkSelector "readAttributeWindowStatusWithCompletion:"

-- | @Selector@ for @subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWindowStatusWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindowStatusWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:@
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWindowStatusWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeWindowStatusWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithCompletion:@
readAttributeAdminFabricIndexWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAdminFabricIndexWithCompletionSelector = mkSelector "readAttributeAdminFabricIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAdminFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminFabricIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAdminFabricIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdminFabricIndexWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeAdminVendorIdWithCompletion:@
readAttributeAdminVendorIdWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAdminVendorIdWithCompletionSelector = mkSelector "readAttributeAdminVendorIdWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAdminVendorIdWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminVendorIdWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAdminVendorIdWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdminVendorIdWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterAdministratorCommissioning)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterAdministratorCommissioning)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterAdministratorCommissioning)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @openCommissioningWindowWithParams:completionHandler:@
openCommissioningWindowWithParams_completionHandlerSelector :: Selector '[Id MTRAdministratorCommissioningClusterOpenCommissioningWindowParams, Ptr ()] ()
openCommissioningWindowWithParams_completionHandlerSelector = mkSelector "openCommissioningWindowWithParams:completionHandler:"

-- | @Selector@ for @openBasicCommissioningWindowWithParams:completionHandler:@
openBasicCommissioningWindowWithParams_completionHandlerSelector :: Selector '[Id MTRAdministratorCommissioningClusterOpenBasicCommissioningWindowParams, Ptr ()] ()
openBasicCommissioningWindowWithParams_completionHandlerSelector = mkSelector "openBasicCommissioningWindowWithParams:completionHandler:"

-- | @Selector@ for @revokeCommissioningWithParams:completionHandler:@
revokeCommissioningWithParams_completionHandlerSelector :: Selector '[Id MTRAdministratorCommissioningClusterRevokeCommissioningParams, Ptr ()] ()
revokeCommissioningWithParams_completionHandlerSelector = mkSelector "revokeCommissioningWithParams:completionHandler:"

-- | @Selector@ for @revokeCommissioningWithCompletionHandler:@
revokeCommissioningWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
revokeCommissioningWithCompletionHandlerSelector = mkSelector "revokeCommissioningWithCompletionHandler:"

-- | @Selector@ for @readAttributeWindowStatusWithCompletionHandler:@
readAttributeWindowStatusWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeWindowStatusWithCompletionHandlerSelector = mkSelector "readAttributeWindowStatusWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeWindowStatusWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeWindowStatusWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeWindowStatusWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeWindowStatusWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithCompletionHandler:@
readAttributeAdminFabricIndexWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAdminFabricIndexWithCompletionHandlerSelector = mkSelector "readAttributeAdminFabricIndexWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAdminFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAdminFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAdminFabricIndexWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAdminVendorIdWithCompletionHandler:@
readAttributeAdminVendorIdWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAdminVendorIdWithCompletionHandlerSelector = mkSelector "readAttributeAdminVendorIdWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAdminVendorIdWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdminVendorIdWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAdminVendorIdWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAdminVendorIdWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeGeneratedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAcceptedCommandListWithCompletionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeAttributeListWithCompletionHandlerSelector = mkSelector "readAttributeAttributeListWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeFeatureMapWithCompletionHandlerSelector = mkSelector "readAttributeFeatureMapWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeClusterRevisionWithCompletionHandlerSelector = mkSelector "readAttributeClusterRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterAdministratorCommissioning)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

