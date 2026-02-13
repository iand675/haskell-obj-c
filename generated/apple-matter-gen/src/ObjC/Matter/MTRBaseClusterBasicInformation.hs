{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Basic Information
--
-- This cluster provides attributes and events for determining basic information about Nodes, which supports both      Commissioning and operational determination of Node characteristics, such as Vendor ID, Product ID and serial number,      which apply to the whole Node. Also allows setting user device information such as location.
--
-- Generated bindings for @MTRBaseClusterBasicInformation@.
module ObjC.Matter.MTRBaseClusterBasicInformation
  ( MTRBaseClusterBasicInformation
  , IsMTRBaseClusterBasicInformation(..)
  , readAttributeDataModelRevisionWithCompletion
  , subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandler
  , readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completion
  , readAttributeVendorNameWithCompletion
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeVendorIDWithCompletion
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductNameWithCompletion
  , subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductNameWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductIDWithCompletion
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeNodeLabelWithCompletion
  , writeAttributeNodeLabelWithValue_completion
  , writeAttributeNodeLabelWithValue_params_completion
  , subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler
  , readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocationWithCompletion
  , writeAttributeLocationWithValue_completion
  , writeAttributeLocationWithValue_params_completion
  , subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocationWithClusterStateCache_endpoint_queue_completion
  , readAttributeHardwareVersionWithCompletion
  , subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeHardwareVersionStringWithCompletion
  , subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion
  , readAttributeSoftwareVersionWithCompletion
  , subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeSoftwareVersionStringWithCompletion
  , subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion
  , readAttributeManufacturingDateWithCompletion
  , subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion
  , readAttributePartNumberWithCompletion
  , subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributePartNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductURLWithCompletion
  , subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductURLWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductLabelWithCompletion
  , subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion
  , readAttributeSerialNumberWithCompletion
  , subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler
  , readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion
  , readAttributeLocalConfigDisabledWithCompletion
  , writeAttributeLocalConfigDisabledWithValue_completion
  , writeAttributeLocalConfigDisabledWithValue_params_completion
  , subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandler
  , readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completion
  , readAttributeReachableWithCompletion
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completion
  , readAttributeUniqueIDWithCompletion
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeCapabilityMinimaWithCompletion
  , subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandler
  , readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductAppearanceWithCompletion
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion
  , readAttributeSpecificationVersionWithCompletion
  , subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completion
  , readAttributeMaxPathsPerInvokeWithCompletion
  , subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandler
  , readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completion
  , readAttributeConfigurationVersionWithCompletion
  , subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler
  , readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion
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
  , readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCapabilityMinimaWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeConfigurationVersionWithCompletionSelector
  , readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDataModelRevisionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareVersionStringWithCompletionSelector
  , readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareVersionWithCompletionSelector
  , readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocalConfigDisabledWithCompletionSelector
  , readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeLocationWithCompletionSelector
  , readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeManufacturingDateWithCompletionSelector
  , readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeMaxPathsPerInvokeWithCompletionSelector
  , readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNodeLabelWithCompletionSelector
  , readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePartNumberWithCompletionSelector
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductAppearanceWithCompletionSelector
  , readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductIDWithCompletionSelector
  , readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductLabelWithCompletionSelector
  , readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductNameWithCompletionSelector
  , readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeProductURLWithCompletionSelector
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeReachableWithCompletionSelector
  , readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSerialNumberWithCompletionSelector
  , readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSoftwareVersionStringWithCompletionSelector
  , readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSoftwareVersionWithCompletionSelector
  , readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeSpecificationVersionWithCompletionSelector
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUniqueIDWithCompletionSelector
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorIDWithCompletionSelector
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorNameWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector
  , writeAttributeLocalConfigDisabledWithValue_completionSelector
  , writeAttributeLocalConfigDisabledWithValue_params_completionSelector
  , writeAttributeLocationWithValue_completionSelector
  , writeAttributeLocationWithValue_params_completionSelector
  , writeAttributeNodeLabelWithValue_completionSelector
  , writeAttributeNodeLabelWithValue_params_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeDataModelRevisionWithCompletion:@
readAttributeDataModelRevisionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeDataModelRevisionWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeDataModelRevisionWithCompletionSelector completion

-- | @- subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeVendorNameWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeVendorNameWithCompletionSelector completion

-- | @- subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeVendorIDWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeVendorIDWithCompletionSelector completion

-- | @- subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductNameWithCompletion:@
readAttributeProductNameWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductNameWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeProductNameWithCompletionSelector completion

-- | @- subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductIDWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeProductIDWithCompletionSelector completion

-- | @- subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNodeLabelWithCompletion:@
readAttributeNodeLabelWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeNodeLabelWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeNodeLabelWithCompletionSelector completion

-- | @- writeAttributeNodeLabelWithValue:completion:@
writeAttributeNodeLabelWithValue_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value) => mtrBaseClusterBasicInformation -> value -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_completion mtrBaseClusterBasicInformation value completion =
  sendMessage mtrBaseClusterBasicInformation writeAttributeNodeLabelWithValue_completionSelector (toNSString value) completion

-- | @- writeAttributeNodeLabelWithValue:params:completion:@
writeAttributeNodeLabelWithValue_params_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_params_completion mtrBaseClusterBasicInformation value params completion =
  sendMessage mtrBaseClusterBasicInformation writeAttributeNodeLabelWithValue_params_completionSelector (toNSString value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLocationWithCompletion:@
readAttributeLocationWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeLocationWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeLocationWithCompletionSelector completion

-- | @- writeAttributeLocationWithValue:completion:@
writeAttributeLocationWithValue_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value) => mtrBaseClusterBasicInformation -> value -> Ptr () -> IO ()
writeAttributeLocationWithValue_completion mtrBaseClusterBasicInformation value completion =
  sendMessage mtrBaseClusterBasicInformation writeAttributeLocationWithValue_completionSelector (toNSString value) completion

-- | @- writeAttributeLocationWithValue:params:completion:@
writeAttributeLocationWithValue_params_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeLocationWithValue_params_completion mtrBaseClusterBasicInformation value params completion =
  sendMessage mtrBaseClusterBasicInformation writeAttributeLocationWithValue_params_completionSelector (toNSString value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLocationWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocationWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocationWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHardwareVersionWithCompletion:@
readAttributeHardwareVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeHardwareVersionWithCompletionSelector completion

-- | @- subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHardwareVersionStringWithCompletion:@
readAttributeHardwareVersionStringWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeHardwareVersionStringWithCompletionSelector completion

-- | @- subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSoftwareVersionWithCompletion:@
readAttributeSoftwareVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeSoftwareVersionWithCompletionSelector completion

-- | @- subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSoftwareVersionStringWithCompletion:@
readAttributeSoftwareVersionStringWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeSoftwareVersionStringWithCompletionSelector completion

-- | @- subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeManufacturingDateWithCompletion:@
readAttributeManufacturingDateWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeManufacturingDateWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeManufacturingDateWithCompletionSelector completion

-- | @- subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePartNumberWithCompletion:@
readAttributePartNumberWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributePartNumberWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributePartNumberWithCompletionSelector completion

-- | @- subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductURLWithCompletion:@
readAttributeProductURLWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductURLWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeProductURLWithCompletionSelector completion

-- | @- subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductLabelWithCompletion:@
readAttributeProductLabelWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductLabelWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeProductLabelWithCompletionSelector completion

-- | @- subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSerialNumberWithCompletion:@
readAttributeSerialNumberWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSerialNumberWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeSerialNumberWithCompletionSelector completion

-- | @- subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeLocalConfigDisabledWithCompletion:@
readAttributeLocalConfigDisabledWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeLocalConfigDisabledWithCompletionSelector completion

-- | @- writeAttributeLocalConfigDisabledWithValue:completion:@
writeAttributeLocalConfigDisabledWithValue_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSNumber value) => mtrBaseClusterBasicInformation -> value -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_completion mtrBaseClusterBasicInformation value completion =
  sendMessage mtrBaseClusterBasicInformation writeAttributeLocalConfigDisabledWithValue_completionSelector (toNSNumber value) completion

-- | @- writeAttributeLocalConfigDisabledWithValue:params:completion:@
writeAttributeLocalConfigDisabledWithValue_params_completion :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_params_completion mtrBaseClusterBasicInformation value params completion =
  sendMessage mtrBaseClusterBasicInformation writeAttributeLocalConfigDisabledWithValue_params_completionSelector (toNSNumber value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReachableWithCompletion:@
readAttributeReachableWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeReachableWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeReachableWithCompletionSelector completion

-- | @- subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReachableWithClusterStateCache:endpoint:queue:completion:@
readAttributeReachableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReachableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUniqueIDWithCompletion:@
readAttributeUniqueIDWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeUniqueIDWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeUniqueIDWithCompletionSelector completion

-- | @- subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCapabilityMinimaWithCompletion:@
readAttributeCapabilityMinimaWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeCapabilityMinimaWithCompletionSelector completion

-- | @- subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeProductAppearanceWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeProductAppearanceWithCompletionSelector completion

-- | @- subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSpecificationVersionWithCompletion:@
readAttributeSpecificationVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeSpecificationVersionWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeSpecificationVersionWithCompletionSelector completion

-- | @- subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeMaxPathsPerInvokeWithCompletion:@
readAttributeMaxPathsPerInvokeWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeMaxPathsPerInvokeWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeMaxPathsPerInvokeWithCompletionSelector completion

-- | @- subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeConfigurationVersionWithCompletion:@
readAttributeConfigurationVersionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeConfigurationVersionWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeConfigurationVersionWithCompletionSelector completion

-- | @- subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBasicInformation completion =
  sendMessage mtrBaseClusterBasicInformation readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBasicInformation subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation => mtrBaseClusterBasicInformation -> IO (Id MTRBaseClusterBasicInformation)
init_ mtrBaseClusterBasicInformation =
  sendOwnedMessage mtrBaseClusterBasicInformation initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasicInformation"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBasicInformation mtrBaseClusterBasicInformation, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBasicInformation -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBasicInformation)
initWithDevice_endpointID_queue mtrBaseClusterBasicInformation device endpointID queue =
  sendOwnedMessage mtrBaseClusterBasicInformation initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDataModelRevisionWithCompletion:@
readAttributeDataModelRevisionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDataModelRevisionWithCompletionSelector = mkSelector "readAttributeDataModelRevisionWithCompletion:"

-- | @Selector@ for @subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDataModelRevisionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDataModelRevisionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDataModelRevisionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDataModelRevisionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeVendorNameWithCompletionSelector = mkSelector "readAttributeVendorNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeVendorIDWithCompletionSelector = mkSelector "readAttributeVendorIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductNameWithCompletion:@
readAttributeProductNameWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductNameWithCompletionSelector = mkSelector "readAttributeProductNameWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductIDWithCompletionSelector = mkSelector "readAttributeProductIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNodeLabelWithCompletion:@
readAttributeNodeLabelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNodeLabelWithCompletionSelector = mkSelector "readAttributeNodeLabelWithCompletion:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:completion:@
writeAttributeNodeLabelWithValue_completionSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeNodeLabelWithValue_completionSelector = mkSelector "writeAttributeNodeLabelWithValue:completion:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:params:completion:@
writeAttributeNodeLabelWithValue_params_completionSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeNodeLabelWithValue_params_completionSelector = mkSelector "writeAttributeNodeLabelWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocationWithCompletion:@
readAttributeLocationWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLocationWithCompletionSelector = mkSelector "readAttributeLocationWithCompletion:"

-- | @Selector@ for @writeAttributeLocationWithValue:completion:@
writeAttributeLocationWithValue_completionSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeLocationWithValue_completionSelector = mkSelector "writeAttributeLocationWithValue:completion:"

-- | @Selector@ for @writeAttributeLocationWithValue:params:completion:@
writeAttributeLocationWithValue_params_completionSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeLocationWithValue_params_completionSelector = mkSelector "writeAttributeLocationWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLocationWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocationWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocationWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLocationWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocationWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHardwareVersionWithCompletion:@
readAttributeHardwareVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHardwareVersionWithCompletionSelector = mkSelector "readAttributeHardwareVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithCompletion:@
readAttributeHardwareVersionStringWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeHardwareVersionStringWithCompletionSelector = mkSelector "readAttributeHardwareVersionStringWithCompletion:"

-- | @Selector@ for @subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSoftwareVersionWithCompletion:@
readAttributeSoftwareVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSoftwareVersionWithCompletionSelector = mkSelector "readAttributeSoftwareVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithCompletion:@
readAttributeSoftwareVersionStringWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSoftwareVersionStringWithCompletionSelector = mkSelector "readAttributeSoftwareVersionStringWithCompletion:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeManufacturingDateWithCompletion:@
readAttributeManufacturingDateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeManufacturingDateWithCompletionSelector = mkSelector "readAttributeManufacturingDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePartNumberWithCompletion:@
readAttributePartNumberWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePartNumberWithCompletionSelector = mkSelector "readAttributePartNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductURLWithCompletion:@
readAttributeProductURLWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductURLWithCompletionSelector = mkSelector "readAttributeProductURLWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductLabelWithCompletion:@
readAttributeProductLabelWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductLabelWithCompletionSelector = mkSelector "readAttributeProductLabelWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSerialNumberWithCompletion:@
readAttributeSerialNumberWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSerialNumberWithCompletionSelector = mkSelector "readAttributeSerialNumberWithCompletion:"

-- | @Selector@ for @subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithCompletion:@
readAttributeLocalConfigDisabledWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeLocalConfigDisabledWithCompletionSelector = mkSelector "readAttributeLocalConfigDisabledWithCompletion:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:completion:@
writeAttributeLocalConfigDisabledWithValue_completionSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeLocalConfigDisabledWithValue_completionSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:completion:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:params:completion:@
writeAttributeLocalConfigDisabledWithValue_params_completionSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeLocalConfigDisabledWithValue_params_completionSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:params:completion:"

-- | @Selector@ for @subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLocalConfigDisabledWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalConfigDisabledWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:@
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLocalConfigDisabledWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeLocalConfigDisabledWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeReachableWithCompletion:@
readAttributeReachableWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeReachableWithCompletionSelector = mkSelector "readAttributeReachableWithCompletion:"

-- | @Selector@ for @subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReachableWithClusterStateCache:endpoint:queue:completion:@
readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeReachableWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeUniqueIDWithCompletion:@
readAttributeUniqueIDWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeUniqueIDWithCompletionSelector = mkSelector "readAttributeUniqueIDWithCompletion:"

-- | @Selector@ for @subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithCompletion:@
readAttributeCapabilityMinimaWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCapabilityMinimaWithCompletionSelector = mkSelector "readAttributeCapabilityMinimaWithCompletion:"

-- | @Selector@ for @subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCapabilityMinimaWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCapabilityMinimaWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:@
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCapabilityMinimaWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCapabilityMinimaWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductAppearanceWithCompletionSelector = mkSelector "readAttributeProductAppearanceWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeSpecificationVersionWithCompletion:@
readAttributeSpecificationVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeSpecificationVersionWithCompletionSelector = mkSelector "readAttributeSpecificationVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSpecificationVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSpecificationVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSpecificationVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeSpecificationVersionWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeMaxPathsPerInvokeWithCompletion:@
readAttributeMaxPathsPerInvokeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeMaxPathsPerInvokeWithCompletionSelector = mkSelector "readAttributeMaxPathsPerInvokeWithCompletion:"

-- | @Selector@ for @subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeMaxPathsPerInvokeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeMaxPathsPerInvokeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:@
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeMaxPathsPerInvokeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeMaxPathsPerInvokeWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeConfigurationVersionWithCompletion:@
readAttributeConfigurationVersionWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeConfigurationVersionWithCompletionSelector = mkSelector "readAttributeConfigurationVersionWithCompletion:"

-- | @Selector@ for @subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterBasicInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterBasicInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterBasicInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

