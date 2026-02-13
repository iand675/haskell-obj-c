{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Bridged Device Basic Information
--
-- This Cluster serves two purposes towards a Node communicating with a Bridge: indicate that the functionality on          the Endpoint where it is placed (and its Parts) is bridged from a non-CHIP technology; and provide a centralized          collection of attributes that the Node MAY collect to aid in conveying information regarding the Bridged Device to a user,          such as the vendor name, the model name, or user-assigned name.
--
-- Generated bindings for @MTRBaseClusterBridgedDeviceBasicInformation@.
module ObjC.Matter.MTRBaseClusterBridgedDeviceBasicInformation
  ( MTRBaseClusterBridgedDeviceBasicInformation
  , IsMTRBaseClusterBridgedDeviceBasicInformation(..)
  , keepActiveWithParams_completion
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
  , readAttributeReachableWithCompletion
  , subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler
  , readAttributeReachableWithClusterStateCache_endpoint_queue_completion
  , readAttributeUniqueIDWithCompletion
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion
  , readAttributeProductAppearanceWithCompletion
  , subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler
  , readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion
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
  , keepActiveWithParams_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeConfigurationVersionWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareVersionStringWithCompletionSelector
  , readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeHardwareVersionWithCompletionSelector
  , readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeManufacturingDateWithCompletionSelector
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
  , readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeUniqueIDWithCompletionSelector
  , readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorIDWithCompletionSelector
  , readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeVendorNameWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector
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
  , subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector
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

-- | Command KeepActive
--
-- Upon receipt, the server SHALL attempt to keep the bridged device active for the duration specified by the command, when the device is next active.
--
-- ObjC selector: @- keepActiveWithParams:completion:@
keepActiveWithParams_completion :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> IO ()
keepActiveWithParams_completion mtrBaseClusterBridgedDeviceBasicInformation params completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation keepActiveWithParams_completionSelector (toMTRBridgedDeviceBasicInformationClusterKeepActiveParams params) completion

-- | @- readAttributeVendorNameWithCompletion:@
readAttributeVendorNameWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeVendorNameWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeVendorNameWithCompletionSelector completion

-- | @- subscribeAttributeVendorNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeVendorNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVendorNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeVendorNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeVendorIDWithCompletion:@
readAttributeVendorIDWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeVendorIDWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeVendorIDWithCompletionSelector completion

-- | @- subscribeAttributeVendorIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeVendorIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeVendorIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeVendorIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductNameWithCompletion:@
readAttributeProductNameWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductNameWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeProductNameWithCompletionSelector completion

-- | @- subscribeAttributeProductNameWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeProductNameWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductNameWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductNameWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeProductNameWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductIDWithCompletion:@
readAttributeProductIDWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductIDWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeProductIDWithCompletionSelector completion

-- | @- subscribeAttributeProductIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeProductIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeProductIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNodeLabelWithCompletion:@
readAttributeNodeLabelWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeNodeLabelWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeNodeLabelWithCompletionSelector completion

-- | @- writeAttributeNodeLabelWithValue:completion:@
writeAttributeNodeLabelWithValue_completion :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsNSString value) => mtrBaseClusterBridgedDeviceBasicInformation -> value -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_completion mtrBaseClusterBridgedDeviceBasicInformation value completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation writeAttributeNodeLabelWithValue_completionSelector (toNSString value) completion

-- | @- writeAttributeNodeLabelWithValue:params:completion:@
writeAttributeNodeLabelWithValue_params_completion :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> value -> params -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_params_completion mtrBaseClusterBridgedDeviceBasicInformation value params completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation writeAttributeNodeLabelWithValue_params_completionSelector (toNSString value) (toMTRWriteParams params) completion

-- | @- subscribeAttributeNodeLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeNodeLabelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNodeLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeNodeLabelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHardwareVersionWithCompletion:@
readAttributeHardwareVersionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeHardwareVersionWithCompletionSelector completion

-- | @- subscribeAttributeHardwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeHardwareVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHardwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeHardwareVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeHardwareVersionStringWithCompletion:@
readAttributeHardwareVersionStringWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeHardwareVersionStringWithCompletionSelector completion

-- | @- subscribeAttributeHardwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeHardwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeHardwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeHardwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSoftwareVersionWithCompletion:@
readAttributeSoftwareVersionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeSoftwareVersionWithCompletionSelector completion

-- | @- subscribeAttributeSoftwareVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeSoftwareVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSoftwareVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeSoftwareVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSoftwareVersionStringWithCompletion:@
readAttributeSoftwareVersionStringWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeSoftwareVersionStringWithCompletionSelector completion

-- | @- subscribeAttributeSoftwareVersionStringWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeSoftwareVersionStringWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSoftwareVersionStringWithClusterStateCache:endpoint:queue:completion:@
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeSoftwareVersionStringWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeManufacturingDateWithCompletion:@
readAttributeManufacturingDateWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeManufacturingDateWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeManufacturingDateWithCompletionSelector completion

-- | @- subscribeAttributeManufacturingDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeManufacturingDateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeManufacturingDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeManufacturingDateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePartNumberWithCompletion:@
readAttributePartNumberWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributePartNumberWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributePartNumberWithCompletionSelector completion

-- | @- subscribeAttributePartNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributePartNumberWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePartNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributePartNumberWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductURLWithCompletion:@
readAttributeProductURLWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductURLWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeProductURLWithCompletionSelector completion

-- | @- subscribeAttributeProductURLWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeProductURLWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductURLWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductURLWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeProductURLWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductLabelWithCompletion:@
readAttributeProductLabelWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductLabelWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeProductLabelWithCompletionSelector completion

-- | @- subscribeAttributeProductLabelWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeProductLabelWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductLabelWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductLabelWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeProductLabelWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeSerialNumberWithCompletion:@
readAttributeSerialNumberWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeSerialNumberWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeSerialNumberWithCompletionSelector completion

-- | @- subscribeAttributeSerialNumberWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeSerialNumberWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeSerialNumberWithClusterStateCache:endpoint:queue:completion:@
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeSerialNumberWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeReachableWithCompletion:@
readAttributeReachableWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeReachableWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeReachableWithCompletionSelector completion

-- | @- subscribeAttributeReachableWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeReachableWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeReachableWithClusterStateCache:endpoint:queue:completion:@
readAttributeReachableWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReachableWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeReachableWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeUniqueIDWithCompletion:@
readAttributeUniqueIDWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeUniqueIDWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeUniqueIDWithCompletionSelector completion

-- | @- subscribeAttributeUniqueIDWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeUniqueIDWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeUniqueIDWithClusterStateCache:endpoint:queue:completion:@
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeUniqueIDWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeProductAppearanceWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeProductAppearanceWithCompletionSelector completion

-- | @- subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeConfigurationVersionWithCompletion:@
readAttributeConfigurationVersionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeConfigurationVersionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeConfigurationVersionWithCompletionSelector completion

-- | @- subscribeAttributeConfigurationVersionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeConfigurationVersionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeConfigurationVersionWithClusterStateCache:endpoint:queue:completion:@
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeConfigurationVersionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterBridgedDeviceBasicInformation completion =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRSubscribeParams params) => mtrBaseClusterBridgedDeviceBasicInformation -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterBridgedDeviceBasicInformation params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterBridgedDeviceBasicInformation subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation => mtrBaseClusterBridgedDeviceBasicInformation -> IO (Id MTRBaseClusterBridgedDeviceBasicInformation)
init_ mtrBaseClusterBridgedDeviceBasicInformation =
  sendOwnedMessage mtrBaseClusterBridgedDeviceBasicInformation initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterBridgedDeviceBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterBridgedDeviceBasicInformation"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterBridgedDeviceBasicInformation mtrBaseClusterBridgedDeviceBasicInformation, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterBridgedDeviceBasicInformation -> device -> endpointID -> queue -> IO (Id MTRBaseClusterBridgedDeviceBasicInformation)
initWithDevice_endpointID_queue mtrBaseClusterBridgedDeviceBasicInformation device endpointID queue =
  sendOwnedMessage mtrBaseClusterBridgedDeviceBasicInformation initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keepActiveWithParams:completion:@
keepActiveWithParams_completionSelector :: Selector '[Id MTRBridgedDeviceBasicInformationClusterKeepActiveParams, Ptr ()] ()
keepActiveWithParams_completionSelector = mkSelector "keepActiveWithParams:completion:"

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

-- | @Selector@ for @readAttributeProductAppearanceWithCompletion:@
readAttributeProductAppearanceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeProductAppearanceWithCompletionSelector = mkSelector "readAttributeProductAppearanceWithCompletion:"

-- | @Selector@ for @subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductAppearanceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductAppearanceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:@
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductAppearanceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeProductAppearanceWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterBridgedDeviceBasicInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterBridgedDeviceBasicInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterBridgedDeviceBasicInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

