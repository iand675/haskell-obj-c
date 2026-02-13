{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Administrator
--
-- An instance of the Joint Fabric Administrator Cluster only applies to Joint Fabric Administrator nodes fulfilling the role of Anchor CA.
--
-- Generated bindings for @MTRBaseClusterJointFabricAdministrator@.
module ObjC.Matter.MTRBaseClusterJointFabricAdministrator
  ( MTRBaseClusterJointFabricAdministrator
  , IsMTRBaseClusterJointFabricAdministrator(..)
  , icaccsrRequestWithParams_completion
  , icaccsrRequestWithCompletion
  , addICACWithParams_completion
  , openJointCommissioningWindowWithParams_completion
  , transferAnchorRequestWithParams_completion
  , transferAnchorRequestWithCompletion
  , transferAnchorCompleteWithParams_completion
  , transferAnchorCompleteWithCompletion
  , announceJointFabricAdministratorWithParams_completion
  , readAttributeAdministratorFabricIndexWithCompletion
  , subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completion
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
  , addICACWithParams_completionSelector
  , announceJointFabricAdministratorWithParams_completionSelector
  , icaccsrRequestWithCompletionSelector
  , icaccsrRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , openJointCommissioningWindowWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAdministratorFabricIndexWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , transferAnchorCompleteWithCompletionSelector
  , transferAnchorCompleteWithParams_completionSelector
  , transferAnchorRequestWithCompletionSelector
  , transferAnchorRequestWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ICACCSRRequest
--
-- This command SHALL be generated during Joint Commissioning Method and subsequently be responded in the form of an ICACCSRResponse command.
--
-- ObjC selector: @- ICACCSRRequestWithParams:completion:@
icaccsrRequestWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterICACCSRRequestParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
icaccsrRequestWithParams_completion mtrBaseClusterJointFabricAdministrator params completion =
  sendMessage mtrBaseClusterJointFabricAdministrator icaccsrRequestWithParams_completionSelector (toMTRJointFabricAdministratorClusterICACCSRRequestParams params) completion

-- | @- ICACCSRRequestWithCompletion:@
icaccsrRequestWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
icaccsrRequestWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator icaccsrRequestWithCompletionSelector completion

-- | Command AddICAC
--
-- This command SHALL be generated and executed during Joint Commissioning Method and subsequently be responded in the form of an ICACResponse command.
--
-- ObjC selector: @- addICACWithParams:completion:@
addICACWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAddICACParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
addICACWithParams_completion mtrBaseClusterJointFabricAdministrator params completion =
  sendMessage mtrBaseClusterJointFabricAdministrator addICACWithParams_completionSelector (toMTRJointFabricAdministratorClusterAddICACParams params) completion

-- | Command OpenJointCommissioningWindow
--
-- This command SHALL fail with a InvalidAdministratorFabricIndex status code sent back to the initiator if the AdministratorFabricIndex field has the value of null.
--
-- ObjC selector: @- openJointCommissioningWindowWithParams:completion:@
openJointCommissioningWindowWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
openJointCommissioningWindowWithParams_completion mtrBaseClusterJointFabricAdministrator params completion =
  sendMessage mtrBaseClusterJointFabricAdministrator openJointCommissioningWindowWithParams_completionSelector (toMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams params) completion

-- | Command TransferAnchorRequest
--
-- This command SHALL be sent by a candidate Joint Fabric Anchor Administrator to the current Joint Fabric Anchor Administrator to request transfer of the Anchor Fabric.
--
-- ObjC selector: @- transferAnchorRequestWithParams:completion:@
transferAnchorRequestWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorRequestParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
transferAnchorRequestWithParams_completion mtrBaseClusterJointFabricAdministrator params completion =
  sendMessage mtrBaseClusterJointFabricAdministrator transferAnchorRequestWithParams_completionSelector (toMTRJointFabricAdministratorClusterTransferAnchorRequestParams params) completion

-- | @- transferAnchorRequestWithCompletion:@
transferAnchorRequestWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
transferAnchorRequestWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator transferAnchorRequestWithCompletionSelector completion

-- | Command TransferAnchorComplete
--
-- This command SHALL indicate the completion of the transfer of the Anchor Fabric to another Joint Fabric Ecosystem Administrator.
--
-- ObjC selector: @- transferAnchorCompleteWithParams:completion:@
transferAnchorCompleteWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorCompleteParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
transferAnchorCompleteWithParams_completion mtrBaseClusterJointFabricAdministrator params completion =
  sendMessage mtrBaseClusterJointFabricAdministrator transferAnchorCompleteWithParams_completionSelector (toMTRJointFabricAdministratorClusterTransferAnchorCompleteParams params) completion

-- | @- transferAnchorCompleteWithCompletion:@
transferAnchorCompleteWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
transferAnchorCompleteWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator transferAnchorCompleteWithCompletionSelector completion

-- | Command AnnounceJointFabricAdministrator
--
-- This command SHALL be used for communicating to client the endpoint that holds the Joint Fabric Administrator Cluster.
--
-- ObjC selector: @- announceJointFabricAdministratorWithParams:completion:@
announceJointFabricAdministratorWithParams_completion :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> IO ()
announceJointFabricAdministratorWithParams_completion mtrBaseClusterJointFabricAdministrator params completion =
  sendMessage mtrBaseClusterJointFabricAdministrator announceJointFabricAdministratorWithParams_completionSelector (toMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams params) completion

-- | @- readAttributeAdministratorFabricIndexWithCompletion:@
readAttributeAdministratorFabricIndexWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeAdministratorFabricIndexWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator readAttributeAdministratorFabricIndexWithCompletionSelector completion

-- | @- subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricAdministrator subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMessage cls' readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricAdministrator subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricAdministrator subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricAdministrator subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricAdministrator subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterJointFabricAdministrator completion =
  sendMessage mtrBaseClusterJointFabricAdministrator readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRSubscribeParams params) => mtrBaseClusterJointFabricAdministrator -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterJointFabricAdministrator params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterJointFabricAdministrator subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator => mtrBaseClusterJointFabricAdministrator -> IO (Id MTRBaseClusterJointFabricAdministrator)
init_ mtrBaseClusterJointFabricAdministrator =
  sendOwnedMessage mtrBaseClusterJointFabricAdministrator initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterJointFabricAdministrator)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterJointFabricAdministrator"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterJointFabricAdministrator mtrBaseClusterJointFabricAdministrator, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterJointFabricAdministrator -> device -> endpointID -> queue -> IO (Id MTRBaseClusterJointFabricAdministrator)
initWithDevice_endpointID_queue mtrBaseClusterJointFabricAdministrator device endpointID queue =
  sendOwnedMessage mtrBaseClusterJointFabricAdministrator initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ICACCSRRequestWithParams:completion:@
icaccsrRequestWithParams_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterICACCSRRequestParams, Ptr ()] ()
icaccsrRequestWithParams_completionSelector = mkSelector "ICACCSRRequestWithParams:completion:"

-- | @Selector@ for @ICACCSRRequestWithCompletion:@
icaccsrRequestWithCompletionSelector :: Selector '[Ptr ()] ()
icaccsrRequestWithCompletionSelector = mkSelector "ICACCSRRequestWithCompletion:"

-- | @Selector@ for @addICACWithParams:completion:@
addICACWithParams_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterAddICACParams, Ptr ()] ()
addICACWithParams_completionSelector = mkSelector "addICACWithParams:completion:"

-- | @Selector@ for @openJointCommissioningWindowWithParams:completion:@
openJointCommissioningWindowWithParams_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams, Ptr ()] ()
openJointCommissioningWindowWithParams_completionSelector = mkSelector "openJointCommissioningWindowWithParams:completion:"

-- | @Selector@ for @transferAnchorRequestWithParams:completion:@
transferAnchorRequestWithParams_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterTransferAnchorRequestParams, Ptr ()] ()
transferAnchorRequestWithParams_completionSelector = mkSelector "transferAnchorRequestWithParams:completion:"

-- | @Selector@ for @transferAnchorRequestWithCompletion:@
transferAnchorRequestWithCompletionSelector :: Selector '[Ptr ()] ()
transferAnchorRequestWithCompletionSelector = mkSelector "transferAnchorRequestWithCompletion:"

-- | @Selector@ for @transferAnchorCompleteWithParams:completion:@
transferAnchorCompleteWithParams_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterTransferAnchorCompleteParams, Ptr ()] ()
transferAnchorCompleteWithParams_completionSelector = mkSelector "transferAnchorCompleteWithParams:completion:"

-- | @Selector@ for @transferAnchorCompleteWithCompletion:@
transferAnchorCompleteWithCompletionSelector :: Selector '[Ptr ()] ()
transferAnchorCompleteWithCompletionSelector = mkSelector "transferAnchorCompleteWithCompletion:"

-- | @Selector@ for @announceJointFabricAdministratorWithParams:completion:@
announceJointFabricAdministratorWithParams_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams, Ptr ()] ()
announceJointFabricAdministratorWithParams_completionSelector = mkSelector "announceJointFabricAdministratorWithParams:completion:"

-- | @Selector@ for @readAttributeAdministratorFabricIndexWithCompletion:@
readAttributeAdministratorFabricIndexWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeAdministratorFabricIndexWithCompletionSelector = mkSelector "readAttributeAdministratorFabricIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeAdministratorFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeAdministratorFabricIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeAdministratorFabricIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeAdministratorFabricIndexWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterJointFabricAdministrator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterJointFabricAdministrator)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterJointFabricAdministrator)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

