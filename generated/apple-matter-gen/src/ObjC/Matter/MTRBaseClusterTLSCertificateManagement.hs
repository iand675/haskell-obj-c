{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Certificate Management
--
-- This Cluster is used to manage TLS Client Certificates and to provision      TLS endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRBaseClusterTLSCertificateManagement@.
module ObjC.Matter.MTRBaseClusterTLSCertificateManagement
  ( MTRBaseClusterTLSCertificateManagement
  , IsMTRBaseClusterTLSCertificateManagement(..)
  , provisionRootCertificateWithParams_completion
  , findRootCertificateWithParams_completion
  , lookupRootCertificateWithParams_completion
  , removeRootCertificateWithParams_completion
  , clientCSRWithParams_completion
  , provisionClientCertificateWithParams_completion
  , findClientCertificateWithParams_completion
  , lookupClientCertificateWithParams_completion
  , removeClientCertificateWithParams_completion
  , readAttributeMaxRootCertificatesWithCompletion
  , subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeProvisionedRootCertificatesWithParams_completion
  , subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxClientCertificatesWithCompletion
  , subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeProvisionedClientCertificatesWithParams_completion
  , subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completion
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
  , clientCSRWithParams_completionSelector
  , findClientCertificateWithParams_completionSelector
  , findRootCertificateWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , lookupClientCertificateWithParams_completionSelector
  , lookupRootCertificateWithParams_completionSelector
  , newSelector
  , provisionClientCertificateWithParams_completionSelector
  , provisionRootCertificateWithParams_completionSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxClientCertificatesWithCompletionSelector
  , readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxRootCertificatesWithCompletionSelector
  , readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProvisionedClientCertificatesWithParams_completionSelector
  , readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProvisionedRootCertificatesWithParams_completionSelector
  , removeClientCertificateWithParams_completionSelector
  , removeRootCertificateWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command ProvisionRootCertificate
--
-- This command SHALL provision a newly provided certificate, or rotate an existing one, based on the contents of the CAID field.
--
-- ObjC selector: @- provisionRootCertificateWithParams:completion:@
provisionRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
provisionRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement provisionRootCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterProvisionRootCertificateParams params) completion

-- | Command FindRootCertificate
--
-- This command SHALL return the specified TLS root certificate, or all provisioned TLS root certificates for the accessing fabric, based on the contents of the CAID field.
--
-- ObjC selector: @- findRootCertificateWithParams:completion:@
findRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
findRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement findRootCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterFindRootCertificateParams params) completion

-- | Command LookupRootCertificate
--
-- This command SHALL return the CAID for the passed in fingerprint.
--
-- ObjC selector: @- lookupRootCertificateWithParams:completion:@
lookupRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
lookupRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement lookupRootCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterLookupRootCertificateParams params) completion

-- | Command RemoveRootCertificate
--
-- This command SHALL be generated to request the server removes the certificate provisioned to the provided Certificate Authority ID.
--
-- ObjC selector: @- removeRootCertificateWithParams:completion:@
removeRootCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveRootCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
removeRootCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement removeRootCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterRemoveRootCertificateParams params) completion

-- | Command ClientCSR
--
-- This command SHALL be generated to request the Node generates a certificate signing request for a new TLS key pair or use an existing CCDID for certificate rotation.
--
-- ObjC selector: @- clientCSRWithParams:completion:@
clientCSRWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterClientCSRParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
clientCSRWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement clientCSRWithParams_completionSelector (toMTRTLSCertificateManagementClusterClientCSRParams params) completion

-- | Command ProvisionClientCertificate
--
-- This command SHALL be generated to request the Node provisions newly provided Client Certificate Details, or rotate an existing client certificate.
--
-- ObjC selector: @- provisionClientCertificateWithParams:completion:@
provisionClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
provisionClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement provisionClientCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterProvisionClientCertificateParams params) completion

-- | Command FindClientCertificate
--
-- This command SHALL return the TLSClientCertificateDetailStruct for the passed in CCDID, or all TLS client certificates for the accessing fabric, based on the contents of the CCDID field.
--
-- ObjC selector: @- findClientCertificateWithParams:completion:@
findClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
findClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement findClientCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterFindClientCertificateParams params) completion

-- | Command LookupClientCertificate
--
-- This command SHALL return the CCDID for the passed in Fingerprint.
--
-- ObjC selector: @- lookupClientCertificateWithParams:completion:@
lookupClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
lookupClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement lookupClientCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterLookupClientCertificateParams params) completion

-- | Command RemoveClientCertificate
--
-- This command SHALL be used to request the Node removes all stored information for the provided CCDID.
--
-- ObjC selector: @- removeClientCertificateWithParams:completion:@
removeClientCertificateWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
removeClientCertificateWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement removeClientCertificateWithParams_completionSelector (toMTRTLSCertificateManagementClusterRemoveClientCertificateParams params) completion

-- | @- readAttributeMaxRootCertificatesWithCompletion:@
readAttributeMaxRootCertificatesWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeMaxRootCertificatesWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeMaxRootCertificatesWithCompletionSelector completion

-- | @- subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProvisionedRootCertificatesWithParams:completion:@
readAttributeProvisionedRootCertificatesWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRReadParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
readAttributeProvisionedRootCertificatesWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeProvisionedRootCertificatesWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxClientCertificatesWithCompletion:@
readAttributeMaxClientCertificatesWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeMaxClientCertificatesWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeMaxClientCertificatesWithCompletionSelector completion

-- | @- subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProvisionedClientCertificatesWithParams:completion:@
readAttributeProvisionedClientCertificatesWithParams_completion :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRReadParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> IO ()
readAttributeProvisionedClientCertificatesWithParams_completion mtrBaseClusterTLSCertificateManagement params completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeProvisionedClientCertificatesWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterTLSCertificateManagement completion =
  sendMessage mtrBaseClusterTLSCertificateManagement readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRSubscribeParams params) => mtrBaseClusterTLSCertificateManagement -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterTLSCertificateManagement params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterTLSCertificateManagement subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement => mtrBaseClusterTLSCertificateManagement -> IO (Id MTRBaseClusterTLSCertificateManagement)
init_ mtrBaseClusterTLSCertificateManagement =
  sendOwnedMessage mtrBaseClusterTLSCertificateManagement initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterTLSCertificateManagement)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterTLSCertificateManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterTLSCertificateManagement mtrBaseClusterTLSCertificateManagement, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterTLSCertificateManagement -> device -> endpointID -> queue -> IO (Id MTRBaseClusterTLSCertificateManagement)
initWithDevice_endpointID_queue mtrBaseClusterTLSCertificateManagement device endpointID queue =
  sendOwnedMessage mtrBaseClusterTLSCertificateManagement initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionRootCertificateWithParams:completion:@
provisionRootCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterProvisionRootCertificateParams, Ptr ()] ()
provisionRootCertificateWithParams_completionSelector = mkSelector "provisionRootCertificateWithParams:completion:"

-- | @Selector@ for @findRootCertificateWithParams:completion:@
findRootCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterFindRootCertificateParams, Ptr ()] ()
findRootCertificateWithParams_completionSelector = mkSelector "findRootCertificateWithParams:completion:"

-- | @Selector@ for @lookupRootCertificateWithParams:completion:@
lookupRootCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterLookupRootCertificateParams, Ptr ()] ()
lookupRootCertificateWithParams_completionSelector = mkSelector "lookupRootCertificateWithParams:completion:"

-- | @Selector@ for @removeRootCertificateWithParams:completion:@
removeRootCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterRemoveRootCertificateParams, Ptr ()] ()
removeRootCertificateWithParams_completionSelector = mkSelector "removeRootCertificateWithParams:completion:"

-- | @Selector@ for @clientCSRWithParams:completion:@
clientCSRWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterClientCSRParams, Ptr ()] ()
clientCSRWithParams_completionSelector = mkSelector "clientCSRWithParams:completion:"

-- | @Selector@ for @provisionClientCertificateWithParams:completion:@
provisionClientCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterProvisionClientCertificateParams, Ptr ()] ()
provisionClientCertificateWithParams_completionSelector = mkSelector "provisionClientCertificateWithParams:completion:"

-- | @Selector@ for @findClientCertificateWithParams:completion:@
findClientCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterFindClientCertificateParams, Ptr ()] ()
findClientCertificateWithParams_completionSelector = mkSelector "findClientCertificateWithParams:completion:"

-- | @Selector@ for @lookupClientCertificateWithParams:completion:@
lookupClientCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterLookupClientCertificateParams, Ptr ()] ()
lookupClientCertificateWithParams_completionSelector = mkSelector "lookupClientCertificateWithParams:completion:"

-- | @Selector@ for @removeClientCertificateWithParams:completion:@
removeClientCertificateWithParams_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterRemoveClientCertificateParams, Ptr ()] ()
removeClientCertificateWithParams_completionSelector = mkSelector "removeClientCertificateWithParams:completion:"

-- | @Selector@ for @readAttributeMaxRootCertificatesWithCompletion:@
readAttributeMaxRootCertificatesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxRootCertificatesWithCompletionSelector = mkSelector "readAttributeMaxRootCertificatesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxRootCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxRootCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProvisionedRootCertificatesWithParams:completion:@
readAttributeProvisionedRootCertificatesWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeProvisionedRootCertificatesWithParams_completionSelector = mkSelector "readAttributeProvisionedRootCertificatesWithParams:completion:"

-- | @Selector@ for @subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProvisionedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProvisionedRootCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProvisionedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProvisionedRootCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxClientCertificatesWithCompletion:@
readAttributeMaxClientCertificatesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxClientCertificatesWithCompletionSelector = mkSelector "readAttributeMaxClientCertificatesWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxClientCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxClientCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProvisionedClientCertificatesWithParams:completion:@
readAttributeProvisionedClientCertificatesWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeProvisionedClientCertificatesWithParams_completionSelector = mkSelector "readAttributeProvisionedClientCertificatesWithParams:completion:"

-- | @Selector@ for @subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProvisionedClientCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProvisionedClientCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProvisionedClientCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProvisionedClientCertificatesWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterTLSCertificateManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterTLSCertificateManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterTLSCertificateManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

