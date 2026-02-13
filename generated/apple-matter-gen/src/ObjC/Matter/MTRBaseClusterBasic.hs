{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBaseClusterBasic@.
module ObjC.Matter.MTRBaseClusterBasic
  ( MTRBaseClusterBasic
  , IsMTRBaseClusterBasic(..)
  , initWithDevice_endpoint_queue
  , mfgSpecificPingWithParams_completionHandler
  , mfgSpecificPingWithCompletionHandler
  , readAttributeDataModelRevisionWithCompletionHandler
  , subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeVendorNameWithCompletionHandler
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeVendorIDWithCompletionHandler
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductNameWithCompletionHandler
  , subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductIDWithCompletionHandler
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeNodeLabelWithCompletionHandler
  , writeAttributeNodeLabelWithValue_completionHandler
  , writeAttributeNodeLabelWithValue_params_completionHandler
  , subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLocationWithCompletionHandler
  , writeAttributeLocationWithValue_completionHandler
  , writeAttributeLocationWithValue_params_completionHandler
  , subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLocationWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeHardwareVersionWithCompletionHandler
  , subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeHardwareVersionStringWithCompletionHandler
  , subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSoftwareVersionWithCompletionHandler
  , subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSoftwareVersionStringWithCompletionHandler
  , subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeManufacturingDateWithCompletionHandler
  , subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandler
  , readAttributePartNumberWithCompletionHandler
  , subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductURLWithCompletionHandler
  , subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeProductLabelWithCompletionHandler
  , subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeSerialNumberWithCompletionHandler
  , subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeLocalConfigDisabledWithCompletionHandler
  , writeAttributeLocalConfigDisabledWithValue_completionHandler
  , writeAttributeLocalConfigDisabledWithValue_params_completionHandler
  , subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeReachableWithCompletionHandler
  , subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeReachableWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeUniqueIDWithCompletionHandler
  , subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandler
  , readAttributeCapabilityMinimaWithCompletionHandler
  , subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler
  , readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandler
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
  , initWithDevice_endpoint_queueSelector
  , mfgSpecificPingWithCompletionHandlerSelector
  , mfgSpecificPingWithParams_completionHandlerSelector
  , readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAcceptedCommandListWithCompletionHandlerSelector
  , readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeAttributeListWithCompletionHandlerSelector
  , readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeCapabilityMinimaWithCompletionHandlerSelector
  , readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeClusterRevisionWithCompletionHandlerSelector
  , readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeDataModelRevisionWithCompletionHandlerSelector
  , readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeFeatureMapWithCompletionHandlerSelector
  , readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeGeneratedCommandListWithCompletionHandlerSelector
  , readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeHardwareVersionStringWithCompletionHandlerSelector
  , readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeHardwareVersionWithCompletionHandlerSelector
  , readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLocalConfigDisabledWithCompletionHandlerSelector
  , readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeLocationWithCompletionHandlerSelector
  , readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeManufacturingDateWithCompletionHandlerSelector
  , readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeNodeLabelWithCompletionHandlerSelector
  , readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributePartNumberWithCompletionHandlerSelector
  , readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductIDWithCompletionHandlerSelector
  , readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductLabelWithCompletionHandlerSelector
  , readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductNameWithCompletionHandlerSelector
  , readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeProductURLWithCompletionHandlerSelector
  , readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeReachableWithCompletionHandlerSelector
  , readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSerialNumberWithCompletionHandlerSelector
  , readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSoftwareVersionStringWithCompletionHandlerSelector
  , readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeSoftwareVersionWithCompletionHandlerSelector
  , readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeUniqueIDWithCompletionHandlerSelector
  , readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorIDWithCompletionHandlerSelector
  , readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector
  , readAttributeVendorNameWithCompletionHandlerSelector
  , subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector
  , writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector
  , writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector
  , writeAttributeLocationWithValue_completionHandlerSelector
  , writeAttributeLocationWithValue_params_completionHandlerSelector
  , writeAttributeNodeLabelWithValue_completionHandlerSelector
  , writeAttributeNodeLabelWithValue_params_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsMTRBaseDevice device, IsNSObject queue) => mtrBaseClusterBasic -> device -> CUShort -> queue -> IO (Id MTRBaseClusterBasic)
initWithDevice_endpoint_queue mtrBaseClusterBasic device endpoint queue =
  sendOwnedMessage mtrBaseClusterBasic initWithDevice_endpoint_queueSelector (toMTRBaseDevice device) endpoint (toNSObject queue)

-- | @- mfgSpecificPingWithParams:completionHandler:@
mfgSpecificPingWithParams_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsMTRBasicClusterMfgSpecificPingParams params) => mtrBaseClusterBasic -> params -> Ptr () -> IO ()
mfgSpecificPingWithParams_completionHandler mtrBaseClusterBasic params completionHandler =
  sendMessage mtrBaseClusterBasic mfgSpecificPingWithParams_completionHandlerSelector (toMTRBasicClusterMfgSpecificPingParams params) completionHandler

-- | @- mfgSpecificPingWithCompletionHandler:@
mfgSpecificPingWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
mfgSpecificPingWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic mfgSpecificPingWithCompletionHandlerSelector completionHandler

-- | @- readAttributeDataModelRevisionWithCompletionHandler:@
readAttributeDataModelRevisionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeDataModelRevisionWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeDataModelRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeVendorNameWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeVendorNameWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeVendorIDWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeVendorIDWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeProductNameWithCompletionHandler:@
readAttributeProductNameWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductNameWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeProductNameWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductIDWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeProductIDWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeNodeLabelWithCompletionHandler:@
readAttributeNodeLabelWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeNodeLabelWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeNodeLabelWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeNodeLabelWithValue:completionHandler:@
writeAttributeNodeLabelWithValue_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value) => mtrBaseClusterBasic -> value -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_completionHandler mtrBaseClusterBasic value completionHandler =
  sendMessage mtrBaseClusterBasic writeAttributeNodeLabelWithValue_completionHandlerSelector (toNSString value) completionHandler

-- | @- writeAttributeNodeLabelWithValue:params:completionHandler:@
writeAttributeNodeLabelWithValue_params_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasic -> value -> params -> Ptr () -> IO ()
writeAttributeNodeLabelWithValue_params_completionHandler mtrBaseClusterBasic value params completionHandler =
  sendMessage mtrBaseClusterBasic writeAttributeNodeLabelWithValue_params_completionHandlerSelector (toNSString value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeLocationWithCompletionHandler:@
readAttributeLocationWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeLocationWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeLocationWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeLocationWithValue:completionHandler:@
writeAttributeLocationWithValue_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value) => mtrBaseClusterBasic -> value -> Ptr () -> IO ()
writeAttributeLocationWithValue_completionHandler mtrBaseClusterBasic value completionHandler =
  sendMessage mtrBaseClusterBasic writeAttributeLocationWithValue_completionHandlerSelector (toNSString value) completionHandler

-- | @- writeAttributeLocationWithValue:params:completionHandler:@
writeAttributeLocationWithValue_params_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSString value, IsMTRWriteParams params) => mtrBaseClusterBasic -> value -> params -> Ptr () -> IO ()
writeAttributeLocationWithValue_params_completionHandler mtrBaseClusterBasic value params completionHandler =
  sendMessage mtrBaseClusterBasic writeAttributeLocationWithValue_params_completionHandlerSelector (toNSString value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeHardwareVersionWithCompletionHandler:@
readAttributeHardwareVersionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeHardwareVersionWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeHardwareVersionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeHardwareVersionStringWithCompletionHandler:@
readAttributeHardwareVersionStringWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeHardwareVersionStringWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSoftwareVersionWithCompletionHandler:@
readAttributeSoftwareVersionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeSoftwareVersionWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeSoftwareVersionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSoftwareVersionStringWithCompletionHandler:@
readAttributeSoftwareVersionStringWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeSoftwareVersionStringWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeManufacturingDateWithCompletionHandler:@
readAttributeManufacturingDateWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeManufacturingDateWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeManufacturingDateWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributePartNumberWithCompletionHandler:@
readAttributePartNumberWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributePartNumberWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributePartNumberWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeProductURLWithCompletionHandler:@
readAttributeProductURLWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductURLWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeProductURLWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeProductLabelWithCompletionHandler:@
readAttributeProductLabelWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeProductLabelWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeProductLabelWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeSerialNumberWithCompletionHandler:@
readAttributeSerialNumberWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeSerialNumberWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeSerialNumberWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeLocalConfigDisabledWithCompletionHandler:@
readAttributeLocalConfigDisabledWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeLocalConfigDisabledWithCompletionHandlerSelector completionHandler

-- | @- writeAttributeLocalConfigDisabledWithValue:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber value) => mtrBaseClusterBasic -> value -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_completionHandler mtrBaseClusterBasic value completionHandler =
  sendMessage mtrBaseClusterBasic writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector (toNSNumber value) completionHandler

-- | @- writeAttributeLocalConfigDisabledWithValue:params:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_params_completionHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber value, IsMTRWriteParams params) => mtrBaseClusterBasic -> value -> params -> Ptr () -> IO ()
writeAttributeLocalConfigDisabledWithValue_params_completionHandler mtrBaseClusterBasic value params completionHandler =
  sendMessage mtrBaseClusterBasic writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector (toNSNumber value) (toMTRWriteParams params) completionHandler

-- | @- subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeReachableWithCompletionHandler:@
readAttributeReachableWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeReachableWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeReachableWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeUniqueIDWithCompletionHandler:@
readAttributeUniqueIDWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeUniqueIDWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeUniqueIDWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeCapabilityMinimaWithCompletionHandler:@
readAttributeCapabilityMinimaWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeCapabilityMinimaWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeGeneratedCommandListWithCompletionHandler:@
readAttributeGeneratedCommandListWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeGeneratedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeGeneratedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeGeneratedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeGeneratedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeGeneratedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAcceptedCommandListWithCompletionHandler:@
readAttributeAcceptedCommandListWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeAcceptedCommandListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAcceptedCommandListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeAcceptedCommandListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAcceptedCommandListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeAcceptedCommandListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeAttributeListWithCompletionHandler:@
readAttributeAttributeListWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeAttributeListWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeAttributeListWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeAttributeListWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeAttributeListWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeAttributeListWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeAttributeListWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeFeatureMapWithCompletionHandler:@
readAttributeFeatureMapWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeFeatureMapWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeFeatureMapWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeFeatureMapWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeFeatureMapWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeFeatureMapWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- | @- readAttributeClusterRevisionWithCompletionHandler:@
readAttributeClusterRevisionWithCompletionHandler :: IsMTRBaseClusterBasic mtrBaseClusterBasic => mtrBaseClusterBasic -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletionHandler mtrBaseClusterBasic completionHandler =
  sendMessage mtrBaseClusterBasic readAttributeClusterRevisionWithCompletionHandlerSelector completionHandler

-- | @- subscribeAttributeClusterRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterBasic mtrBaseClusterBasic, IsNSNumber minInterval, IsNSNumber maxInterval, IsMTRSubscribeParams params) => mtrBaseClusterBasic -> minInterval -> maxInterval -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandler mtrBaseClusterBasic minInterval maxInterval params subscriptionEstablishedHandler reportHandler =
  sendMessage mtrBaseClusterBasic subscribeAttributeClusterRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector (toNSNumber minInterval) (toNSNumber maxInterval) (toMTRSubscribeParams params) subscriptionEstablishedHandler reportHandler

-- | @+ readAttributeClusterRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler :: (IsMTRAttributeCacheContainer attributeCacheContainer, IsNSNumber endpoint, IsNSObject queue) => attributeCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandler attributeCacheContainer endpoint queue completionHandler =
  do
    cls' <- getRequiredClass "MTRBaseClusterBasic"
    sendClassMessage cls' readAttributeClusterRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector (toMTRAttributeCacheContainer attributeCacheContainer) (toNSNumber endpoint) (toNSObject queue) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRBaseDevice, CUShort, Id NSObject] (Id MTRBaseClusterBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @mfgSpecificPingWithParams:completionHandler:@
mfgSpecificPingWithParams_completionHandlerSelector :: Selector '[Id MTRBasicClusterMfgSpecificPingParams, Ptr ()] ()
mfgSpecificPingWithParams_completionHandlerSelector = mkSelector "mfgSpecificPingWithParams:completionHandler:"

-- | @Selector@ for @mfgSpecificPingWithCompletionHandler:@
mfgSpecificPingWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
mfgSpecificPingWithCompletionHandlerSelector = mkSelector "mfgSpecificPingWithCompletionHandler:"

-- | @Selector@ for @readAttributeDataModelRevisionWithCompletionHandler:@
readAttributeDataModelRevisionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeDataModelRevisionWithCompletionHandlerSelector = mkSelector "readAttributeDataModelRevisionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDataModelRevisionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDataModelRevisionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDataModelRevisionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeDataModelRevisionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeVendorNameWithCompletionHandler:@
readAttributeVendorNameWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeVendorNameWithCompletionHandlerSelector = mkSelector "readAttributeVendorNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeVendorIDWithCompletionHandler:@
readAttributeVendorIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeVendorIDWithCompletionHandlerSelector = mkSelector "readAttributeVendorIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeVendorIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeVendorIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeVendorIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeVendorIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductNameWithCompletionHandler:@
readAttributeProductNameWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeProductNameWithCompletionHandlerSelector = mkSelector "readAttributeProductNameWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductNameWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductNameWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductNameWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductNameWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductIDWithCompletionHandler:@
readAttributeProductIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeProductIDWithCompletionHandlerSelector = mkSelector "readAttributeProductIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeNodeLabelWithCompletionHandler:@
readAttributeNodeLabelWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeNodeLabelWithCompletionHandlerSelector = mkSelector "readAttributeNodeLabelWithCompletionHandler:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:completionHandler:@
writeAttributeNodeLabelWithValue_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeNodeLabelWithValue_completionHandlerSelector = mkSelector "writeAttributeNodeLabelWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:params:completionHandler:@
writeAttributeNodeLabelWithValue_params_completionHandlerSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeNodeLabelWithValue_params_completionHandlerSelector = mkSelector "writeAttributeNodeLabelWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNodeLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNodeLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNodeLabelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeNodeLabelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLocationWithCompletionHandler:@
readAttributeLocationWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeLocationWithCompletionHandlerSelector = mkSelector "readAttributeLocationWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLocationWithValue:completionHandler:@
writeAttributeLocationWithValue_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
writeAttributeLocationWithValue_completionHandlerSelector = mkSelector "writeAttributeLocationWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLocationWithValue:params:completionHandler:@
writeAttributeLocationWithValue_params_completionHandlerSelector :: Selector '[Id NSString, Id MTRWriteParams, Ptr ()] ()
writeAttributeLocationWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLocationWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLocationWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocationWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLocationWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLocationWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeHardwareVersionWithCompletionHandler:@
readAttributeHardwareVersionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeHardwareVersionWithCompletionHandlerSelector = mkSelector "readAttributeHardwareVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHardwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHardwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeHardwareVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithCompletionHandler:@
readAttributeHardwareVersionStringWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeHardwareVersionStringWithCompletionHandlerSelector = mkSelector "readAttributeHardwareVersionStringWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeHardwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeHardwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeHardwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeHardwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionWithCompletionHandler:@
readAttributeSoftwareVersionWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSoftwareVersionWithCompletionHandlerSelector = mkSelector "readAttributeSoftwareVersionWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSoftwareVersionWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSoftwareVersionWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSoftwareVersionWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithCompletionHandler:@
readAttributeSoftwareVersionStringWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSoftwareVersionStringWithCompletionHandlerSelector = mkSelector "readAttributeSoftwareVersionStringWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSoftwareVersionStringWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSoftwareVersionStringWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSoftwareVersionStringWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSoftwareVersionStringWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeManufacturingDateWithCompletionHandler:@
readAttributeManufacturingDateWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeManufacturingDateWithCompletionHandlerSelector = mkSelector "readAttributeManufacturingDateWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeManufacturingDateWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeManufacturingDateWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeManufacturingDateWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeManufacturingDateWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributePartNumberWithCompletionHandler:@
readAttributePartNumberWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributePartNumberWithCompletionHandlerSelector = mkSelector "readAttributePartNumberWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePartNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePartNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePartNumberWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributePartNumberWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductURLWithCompletionHandler:@
readAttributeProductURLWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeProductURLWithCompletionHandlerSelector = mkSelector "readAttributeProductURLWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductURLWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductURLWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductURLWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductURLWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeProductLabelWithCompletionHandler:@
readAttributeProductLabelWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeProductLabelWithCompletionHandlerSelector = mkSelector "readAttributeProductLabelWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeProductLabelWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeProductLabelWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeProductLabelWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeProductLabelWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeSerialNumberWithCompletionHandler:@
readAttributeSerialNumberWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeSerialNumberWithCompletionHandlerSelector = mkSelector "readAttributeSerialNumberWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeSerialNumberWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeSerialNumberWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeSerialNumberWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeSerialNumberWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithCompletionHandler:@
readAttributeLocalConfigDisabledWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeLocalConfigDisabledWithCompletionHandlerSelector = mkSelector "readAttributeLocalConfigDisabledWithCompletionHandler:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector :: Selector '[Id NSNumber, Ptr ()] ()
writeAttributeLocalConfigDisabledWithValue_completionHandlerSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:completionHandler:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:params:completionHandler:@
writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector :: Selector '[Id NSNumber, Id MTRWriteParams, Ptr ()] ()
writeAttributeLocalConfigDisabledWithValue_params_completionHandlerSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:params:completionHandler:"

-- | @Selector@ for @subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeLocalConfigDisabledWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeLocalConfigDisabledWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeLocalConfigDisabledWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeLocalConfigDisabledWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeReachableWithCompletionHandler:@
readAttributeReachableWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeReachableWithCompletionHandlerSelector = mkSelector "readAttributeReachableWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeReachableWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeReachableWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeReachableWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeReachableWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeUniqueIDWithCompletionHandler:@
readAttributeUniqueIDWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeUniqueIDWithCompletionHandlerSelector = mkSelector "readAttributeUniqueIDWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeUniqueIDWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeUniqueIDWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeUniqueIDWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeUniqueIDWithAttributeCache:endpoint:queue:completionHandler:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithCompletionHandler:@
readAttributeCapabilityMinimaWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
readAttributeCapabilityMinimaWithCompletionHandlerSelector = mkSelector "readAttributeCapabilityMinimaWithCompletionHandler:"

-- | @Selector@ for @subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:@
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector :: Selector '[Id NSNumber, Id NSNumber, Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCapabilityMinimaWithMinInterval_maxInterval_params_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCapabilityMinimaWithMinInterval:maxInterval:params:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:@
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector :: Selector '[Id MTRAttributeCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCapabilityMinimaWithAttributeCache_endpoint_queue_completionHandlerSelector = mkSelector "readAttributeCapabilityMinimaWithAttributeCache:endpoint:queue:completionHandler:"

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

