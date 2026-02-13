{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational Credentials
--
-- This cluster is used to add or remove Operational Credentials on a Commissionee or Node, as well as manage the associated Fabrics.
--
-- Generated bindings for @MTRBaseClusterOperationalCredentials@.
module ObjC.Matter.MTRBaseClusterOperationalCredentials
  ( MTRBaseClusterOperationalCredentials
  , IsMTRBaseClusterOperationalCredentials(..)
  , attestationRequestWithParams_completion
  , certificateChainRequestWithParams_completion
  , csrRequestWithParams_completion
  , addNOCWithParams_completion
  , updateNOCWithParams_completion
  , updateFabricLabelWithParams_completion
  , removeFabricWithParams_completion
  , addTrustedRootCertificateWithParams_completion
  , setVIDVerificationStatementWithParams_completion
  , setVIDVerificationStatementWithCompletion
  , signVIDVerificationRequestWithParams_completion
  , readAttributeNOCsWithParams_completion
  , subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandler
  , readAttributeNOCsWithClusterStateCache_endpoint_queue_completion
  , readAttributeFabricsWithParams_completion
  , subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeFabricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeSupportedFabricsWithCompletion
  , subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCommissionedFabricsWithCompletion
  , subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completion
  , readAttributeTrustedRootCertificatesWithCompletion
  , subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandler
  , readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentFabricIndexWithCompletion
  , subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completion
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
  , attestationRequestWithParams_completionHandler
  , certificateChainRequestWithParams_completionHandler
  , csrRequestWithParams_completionHandler
  , addNOCWithParams_completionHandler
  , updateNOCWithParams_completionHandler
  , updateFabricLabelWithParams_completionHandler
  , removeFabricWithParams_completionHandler
  , addTrustedRootCertificateWithParams_completionHandler
  , readAttributeNOCsWithParams_completionHandler
  , subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeFabricsWithParams_completionHandler
  , subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSupportedFabricsWithCompletionHandler
  , subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCommissionedFabricsWithCompletionHandler
  , subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeTrustedRootCertificatesWithCompletionHandler
  , subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCurrentFabricIndexWithCompletionHandler
  , subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandler
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
  , addNOCWithParams_completionHandlerSelector
  , addNOCWithParams_completionSelector
  , addTrustedRootCertificateWithParams_completionHandlerSelector
  , addTrustedRootCertificateWithParams_completionSelector
  , attestationRequestWithParams_completionHandlerSelector
  , attestationRequestWithParams_completionSelector
  , certificateChainRequestWithParams_completionHandlerSelector
  , certificateChainRequestWithParams_completionSelector
  , csrRequestWithParams_completionHandlerSelector
  , csrRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCommissionedFabricsWithCompletionHandlerSelector
  , readAttributeCommissionedFabricsWithCompletionSelector
  , readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentFabricIndexWithCompletionHandlerSelector
  , readAttributeCurrentFabricIndexWithCompletionSelector
  , readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFabricsWithParams_completionHandlerSelector
  , readAttributeFabricsWithParams_completionSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNOCsWithParams_completionHandlerSelector
  , readAttributeNOCsWithParams_completionSelector
  , readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSupportedFabricsWithCompletionHandlerSelector
  , readAttributeSupportedFabricsWithCompletionSelector
  , readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTrustedRootCertificatesWithCompletionHandlerSelector
  , readAttributeTrustedRootCertificatesWithCompletionSelector
  , removeFabricWithParams_completionHandlerSelector
  , removeFabricWithParams_completionSelector
  , setVIDVerificationStatementWithCompletionSelector
  , setVIDVerificationStatementWithParams_completionSelector
  , signVIDVerificationRequestWithParams_completionSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector
  , updateFabricLabelWithParams_completionHandlerSelector
  , updateFabricLabelWithParams_completionSelector
  , updateNOCWithParams_completionHandlerSelector
  , updateNOCWithParams_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command AttestationRequest
--
-- Sender is requesting attestation information from the receiver.
--
-- ObjC selector: @- attestationRequestWithParams:completion:@
attestationRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
attestationRequestWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials attestationRequestWithParams_completionSelector (toMTROperationalCredentialsClusterAttestationRequestParams params) completion

-- | Command CertificateChainRequest
--
-- Sender is requesting a device attestation certificate from the receiver.
--
-- ObjC selector: @- certificateChainRequestWithParams:completion:@
certificateChainRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
certificateChainRequestWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials certificateChainRequestWithParams_completionSelector (toMTROperationalCredentialsClusterCertificateChainRequestParams params) completion

-- | Command CSRRequest
--
-- Sender is requesting a certificate signing request (CSR) from the receiver.
--
-- ObjC selector: @- CSRRequestWithParams:completion:@
csrRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
csrRequestWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials csrRequestWithParams_completionSelector (toMTROperationalCredentialsClusterCSRRequestParams params) completion

-- | Command AddNOC
--
-- Sender is requesting to add the new node operational certificates.
--
-- ObjC selector: @- addNOCWithParams:completion:@
addNOCWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addNOCWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials addNOCWithParams_completionSelector (toMTROperationalCredentialsClusterAddNOCParams params) completion

-- | Command UpdateNOC
--
-- This command SHALL replace the NOC and optional associated ICAC (if present) scoped under the accessing fabric upon successful validation of all arguments and preconditions.
--
-- ObjC selector: @- updateNOCWithParams:completion:@
updateNOCWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateNOCWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials updateNOCWithParams_completionSelector (toMTROperationalCredentialsClusterUpdateNOCParams params) completion

-- | Command UpdateFabricLabel
--
-- This command SHALL be used by an Administrative Node to set the user-visible Label field for a given Fabric, as reflected by entries in the Fabrics attribute.
--
-- ObjC selector: @- updateFabricLabelWithParams:completion:@
updateFabricLabelWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateFabricLabelWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials updateFabricLabelWithParams_completionSelector (toMTROperationalCredentialsClusterUpdateFabricLabelParams params) completion

-- | Command RemoveFabric
--
-- This command is used by Administrative Nodes to remove a given fabric index and delete all associated fabric-scoped data.
--
-- ObjC selector: @- removeFabricWithParams:completion:@
removeFabricWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
removeFabricWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials removeFabricWithParams_completionSelector (toMTROperationalCredentialsClusterRemoveFabricParams params) completion

-- | Command AddTrustedRootCertificate
--
-- This command SHALL add a Trusted Root CA Certificate, provided as its CHIP Certificate representation.
--
-- ObjC selector: @- addTrustedRootCertificateWithParams:completion:@
addTrustedRootCertificateWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials addTrustedRootCertificateWithParams_completionSelector (toMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) completion

-- | Command SetVIDVerificationStatement
--
-- This command SHALL be used to update any of the accessing fabric's associated VendorID, VidVerificatioNStatement or VVSC (Vendor Verification Signing Certificate).
--
-- ObjC selector: @- setVIDVerificationStatementWithParams:completion:@
setVIDVerificationStatementWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
setVIDVerificationStatementWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials setVIDVerificationStatementWithParams_completionSelector (toMTROperationalCredentialsClusterSetVIDVerificationStatementParams params) completion

-- | @- setVIDVerificationStatementWithCompletion:@
setVIDVerificationStatementWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
setVIDVerificationStatementWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials setVIDVerificationStatementWithCompletionSelector completion

-- | Command SignVIDVerificationRequest
--
-- This command SHALL be used to request that the server authenticate the fabric associated with the FabricIndex given.
--
-- ObjC selector: @- signVIDVerificationRequestWithParams:completion:@
signVIDVerificationRequestWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterSignVIDVerificationRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
signVIDVerificationRequestWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials signVIDVerificationRequestWithParams_completionSelector (toMTROperationalCredentialsClusterSignVIDVerificationRequestParams params) completion

-- | @- readAttributeNOCsWithParams:completion:@
readAttributeNOCsWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeNOCsWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeNOCsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNOCsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNOCsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFabricsWithParams:completion:@
readAttributeFabricsWithParams_completion :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeFabricsWithParams_completion mtrBaseClusterOperationalCredentials params completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeFabricsWithParams_completionSelector (toMTRReadParams params) completion

-- | @- subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFabricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSupportedFabricsWithCompletion:@
readAttributeSupportedFabricsWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeSupportedFabricsWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeSupportedFabricsWithCompletionSelector completion

-- | @- subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCommissionedFabricsWithCompletion:@
readAttributeCommissionedFabricsWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeCommissionedFabricsWithCompletionSelector completion

-- | @- subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTrustedRootCertificatesWithCompletion:@
readAttributeTrustedRootCertificatesWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeTrustedRootCertificatesWithCompletionSelector completion

-- | @- subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentFabricIndexWithCompletion:@
readAttributeCurrentFabricIndexWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeCurrentFabricIndexWithCompletionSelector completion

-- | @- subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterOperationalCredentials completion =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> IO (Id MTRBaseClusterOperationalCredentials)
init_ mtrBaseClusterOperationalCredentials =
  sendOwnedMessage mtrBaseClusterOperationalCredentials initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterOperationalCredentials)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterOperationalCredentials -> device -> CUShort -> queue -> IO (Id MTRBaseClusterOperationalCredentials)
initWithDevice_endpoint_queue mtrBaseClusterOperationalCredentials device endpoint queue =
  sendOwnedMessage mtrBaseClusterOperationalCredentials initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- attestationRequestWithParams:completionHandler:@
attestationRequestWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAttestationRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
attestationRequestWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials attestationRequestWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterAttestationRequestParams params) completionHandler

-- | @- certificateChainRequestWithParams:completionHandler:@
certificateChainRequestWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCertificateChainRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
certificateChainRequestWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials certificateChainRequestWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterCertificateChainRequestParams params) completionHandler

-- | @- CSRRequestWithParams:completionHandler:@
csrRequestWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterCSRRequestParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
csrRequestWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials csrRequestWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterCSRRequestParams params) completionHandler

-- | @- addNOCWithParams:completionHandler:@
addNOCWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addNOCWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials addNOCWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterAddNOCParams params) completionHandler

-- | @- updateNOCWithParams:completionHandler:@
updateNOCWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateNOCParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateNOCWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials updateNOCWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterUpdateNOCParams params) completionHandler

-- | @- updateFabricLabelWithParams:completionHandler:@
updateFabricLabelWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterUpdateFabricLabelParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
updateFabricLabelWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials updateFabricLabelWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterUpdateFabricLabelParams params) completionHandler

-- | @- removeFabricWithParams:completionHandler:@
removeFabricWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterRemoveFabricParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
removeFabricWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials removeFabricWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterRemoveFabricParams params) completionHandler

-- | @- addTrustedRootCertificateWithParams:completionHandler:@
addTrustedRootCertificateWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
addTrustedRootCertificateWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials addTrustedRootCertificateWithParams_completionHandlerSelector (toMTROperationalCredentialsClusterAddTrustedRootCertificateParams params) completionHandler

-- | @- readAttributeNOCsWithParams:completionHandler:@
readAttributeNOCsWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeNOCsWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeNOCsWithParams_completionHandlerSelector (toMTRReadParams params) completionHandler

-- | @- subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFabricsWithParams:completionHandler:@
readAttributeFabricsWithParams_completionHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRReadParams params) => mtrBaseClusterOperationalCredentials -> params -> Ptr () -> IO ()
readAttributeFabricsWithParams_completionHandler mtrBaseClusterOperationalCredentials params completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeFabricsWithParams_completionHandlerSelector (toMTRReadParams params) completionHandler

-- | @- subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSupportedFabricsWithCompletionHandler:@
readAttributeSupportedFabricsWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeSupportedFabricsWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeSupportedFabricsWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCommissionedFabricsWithCompletionHandler:@
readAttributeCommissionedFabricsWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeCommissionedFabricsWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeTrustedRootCertificatesWithCompletionHandler:@
readAttributeTrustedRootCertificatesWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeTrustedRootCertificatesWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCurrentFabricIndexWithCompletionHandler:@
readAttributeCurrentFabricIndexWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeCurrentFabricIndexWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials => mtrBaseClusterOperationalCredentials -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterOperationalCredentials completionHandler =
  sendMessage mtrBaseClusterOperationalCredentials readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterOperationalCredentials -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterOperationalCredentials minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterOperationalCredentials subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterOperationalCredentials"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterOperationalCredentials mtrBaseClusterOperationalCredentials, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterOperationalCredentials -> device -> endpointID -> queue -> IO (Id MTRBaseClusterOperationalCredentials)
initWithDevice_endpointID_queue mtrBaseClusterOperationalCredentials device endpointID queue =
  sendOwnedMessage mtrBaseClusterOperationalCredentials initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attestationRequestWithParams:completion:@
attestationRequestWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterAttestationRequestParams, Ptr ()] ()
attestationRequestWithParams_completionSelector = mkSelector "attestationRequestWithParams:completion:"

-- | @Selector@ for @certificateChainRequestWithParams:completion:@
certificateChainRequestWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterCertificateChainRequestParams, Ptr ()] ()
certificateChainRequestWithParams_completionSelector = mkSelector "certificateChainRequestWithParams:completion:"

-- | @Selector@ for @CSRRequestWithParams:completion:@
csrRequestWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterCSRRequestParams, Ptr ()] ()
csrRequestWithParams_completionSelector = mkSelector "CSRRequestWithParams:completion:"

-- | @Selector@ for @addNOCWithParams:completion:@
addNOCWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterAddNOCParams, Ptr ()] ()
addNOCWithParams_completionSelector = mkSelector "addNOCWithParams:completion:"

-- | @Selector@ for @updateNOCWithParams:completion:@
updateNOCWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateNOCParams, Ptr ()] ()
updateNOCWithParams_completionSelector = mkSelector "updateNOCWithParams:completion:"

-- | @Selector@ for @updateFabricLabelWithParams:completion:@
updateFabricLabelWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateFabricLabelParams, Ptr ()] ()
updateFabricLabelWithParams_completionSelector = mkSelector "updateFabricLabelWithParams:completion:"

-- | @Selector@ for @removeFabricWithParams:completion:@
removeFabricWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterRemoveFabricParams, Ptr ()] ()
removeFabricWithParams_completionSelector = mkSelector "removeFabricWithParams:completion:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:completion:@
addTrustedRootCertificateWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterAddTrustedRootCertificateParams, Ptr ()] ()
addTrustedRootCertificateWithParams_completionSelector = mkSelector "addTrustedRootCertificateWithParams:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithParams:completion:@
setVIDVerificationStatementWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterSetVIDVerificationStatementParams, Ptr ()] ()
setVIDVerificationStatementWithParams_completionSelector = mkSelector "setVIDVerificationStatementWithParams:completion:"

-- | @Selector@ for @setVIDVerificationStatementWithCompletion:@
setVIDVerificationStatementWithCompletionSelector :: Selector '[Ptr ()] ()
setVIDVerificationStatementWithCompletionSelector = mkSelector "setVIDVerificationStatementWithCompletion:"

-- | @Selector@ for @signVIDVerificationRequestWithParams:completion:@
signVIDVerificationRequestWithParams_completionSelector :: Selector '[Id MTROperationalCredentialsClusterSignVIDVerificationRequestParams, Ptr ()] ()
signVIDVerificationRequestWithParams_completionSelector = mkSelector "signVIDVerificationRequestWithParams:completion:"

-- | @Selector@ for @readAttributeNOCsWithParams:completion:@
readAttributeNOCsWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeNOCsWithParams_completionSelector = mkSelector "readAttributeNOCsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNOCsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNOCsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNOCsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNOCsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeFabricsWithParams:completion:@
readAttributeFabricsWithParams_completionSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeFabricsWithParams_completionSelector = mkSelector "readAttributeFabricsWithParams:completion:"

-- | @Selector@ for @subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFabricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFabricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFabricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeFabricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSupportedFabricsWithCompletion:@
readAttributeSupportedFabricsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSupportedFabricsWithCompletionSelector = mkSelector "readAttributeSupportedFabricsWithCompletion:"

-- | @Selector@ for @subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedFabricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedFabricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedFabricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSupportedFabricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithCompletion:@
readAttributeCommissionedFabricsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCommissionedFabricsWithCompletionSelector = mkSelector "readAttributeCommissionedFabricsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCommissionedFabricsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCommissionedFabricsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCommissionedFabricsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCommissionedFabricsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithCompletion:@
readAttributeTrustedRootCertificatesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTrustedRootCertificatesWithCompletionSelector = mkSelector "readAttributeTrustedRootCertificatesWithCompletion:"

-- | @Selector@ for @subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTrustedRootCertificatesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTrustedRootCertificatesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:@
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTrustedRootCertificatesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTrustedRootCertificatesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithCompletion:@
readAttributeCurrentFabricIndexWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentFabricIndexWithCompletionSelector = mkSelector "readAttributeCurrentFabricIndexWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentFabricIndexWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentFabricIndexWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentFabricIndexWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentFabricIndexWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterOperationalCredentials)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterOperationalCredentials)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterOperationalCredentials)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @attestationRequestWithParams:completionHandler:@
attestationRequestWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterAttestationRequestParams, Ptr ()] ()
attestationRequestWithParams_completionHandlerSelector = mkSelector "attestationRequestWithParams:completionHandler:"

-- | @Selector@ for @certificateChainRequestWithParams:completionHandler:@
certificateChainRequestWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterCertificateChainRequestParams, Ptr ()] ()
certificateChainRequestWithParams_completionHandlerSelector = mkSelector "certificateChainRequestWithParams:completionHandler:"

-- | @Selector@ for @CSRRequestWithParams:completionHandler:@
csrRequestWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterCSRRequestParams, Ptr ()] ()
csrRequestWithParams_completionHandlerSelector = mkSelector "CSRRequestWithParams:completionHandler:"

-- | @Selector@ for @addNOCWithParams:completionHandler:@
addNOCWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterAddNOCParams, Ptr ()] ()
addNOCWithParams_completionHandlerSelector = mkSelector "addNOCWithParams:completionHandler:"

-- | @Selector@ for @updateNOCWithParams:completionHandler:@
updateNOCWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateNOCParams, Ptr ()] ()
updateNOCWithParams_completionHandlerSelector = mkSelector "updateNOCWithParams:completionHandler:"

-- | @Selector@ for @updateFabricLabelWithParams:completionHandler:@
updateFabricLabelWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterUpdateFabricLabelParams, Ptr ()] ()
updateFabricLabelWithParams_completionHandlerSelector = mkSelector "updateFabricLabelWithParams:completionHandler:"

-- | @Selector@ for @removeFabricWithParams:completionHandler:@
removeFabricWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterRemoveFabricParams, Ptr ()] ()
removeFabricWithParams_completionHandlerSelector = mkSelector "removeFabricWithParams:completionHandler:"

-- | @Selector@ for @addTrustedRootCertificateWithParams:completionHandler:@
addTrustedRootCertificateWithParams_completionHandlerSelector :: Selector '[Id MTROperationalCredentialsClusterAddTrustedRootCertificateParams, Ptr ()] ()
addTrustedRootCertificateWithParams_completionHandlerSelector = mkSelector "addTrustedRootCertificateWithParams:completionHandler:"

-- | @Selector@ for @readAttributeNOCsWithParams:completionHandler:@
readAttributeNOCsWithParams_completionHandlerSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeNOCsWithParams_completionHandlerSelector = mkSelector "readAttributeNOCsWithParams:completionHandler:"

-- | @Selector@ for @subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNOCsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNOCsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNOCsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNOCsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeFabricsWithParams:completionHandler:@
readAttributeFabricsWithParams_completionHandlerSelector :: Selector '[Id MTRReadParams, Ptr ()] ()
readAttributeFabricsWithParams_completionHandlerSelector = mkSelector "readAttributeFabricsWithParams:completionHandler:"

-- | @Selector@ for @subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeFabricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSupportedFabricsWithCompletionHandler:@
readAttributeSupportedFabricsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSupportedFabricsWithCompletionHandlerSelector = mkSelector "readAttributeSupportedFabricsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSupportedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSupportedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSupportedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSupportedFabricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithCompletionHandler:@
readAttributeCommissionedFabricsWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCommissionedFabricsWithCompletionHandlerSelector = mkSelector "readAttributeCommissionedFabricsWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCommissionedFabricsWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCommissionedFabricsWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCommissionedFabricsWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCommissionedFabricsWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithCompletionHandler:@
readAttributeTrustedRootCertificatesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeTrustedRootCertificatesWithCompletionHandlerSelector = mkSelector "readAttributeTrustedRootCertificatesWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTrustedRootCertificatesWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTrustedRootCertificatesWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTrustedRootCertificatesWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeTrustedRootCertificatesWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithCompletionHandler:@
readAttributeCurrentFabricIndexWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCurrentFabricIndexWithCompletionHandlerSelector = mkSelector "readAttributeCurrentFabricIndexWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentFabricIndexWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentFabricIndexWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentFabricIndexWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCurrentFabricIndexWithAttributeCache:endpoint:queue:completionHandler:"

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
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterOperationalCredentials)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

