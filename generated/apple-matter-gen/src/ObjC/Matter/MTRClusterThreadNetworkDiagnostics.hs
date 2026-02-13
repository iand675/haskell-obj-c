{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Network Diagnostics    The Thread Network Diagnostics Cluster provides a means to acquire standardized diagnostics metrics that MAY be used by a Node to assist a user or Administrative Node in diagnosing potential problems
--
-- Generated bindings for @MTRClusterThreadNetworkDiagnostics@.
module ObjC.Matter.MTRClusterThreadNetworkDiagnostics
  ( MTRClusterThreadNetworkDiagnostics
  , IsMTRClusterThreadNetworkDiagnostics(..)
  , resetCountsWithParams_expectedValues_expectedValueInterval_completion
  , resetCountsWithExpectedValues_expectedValueInterval_completion
  , readAttributeChannelWithParams
  , readAttributeRoutingRoleWithParams
  , readAttributeNetworkNameWithParams
  , readAttributePanIdWithParams
  , readAttributeExtendedPanIdWithParams
  , readAttributeMeshLocalPrefixWithParams
  , readAttributeOverrunCountWithParams
  , readAttributeNeighborTableWithParams
  , readAttributeRouteTableWithParams
  , readAttributePartitionIdWithParams
  , readAttributeWeightingWithParams
  , readAttributeDataVersionWithParams
  , readAttributeStableDataVersionWithParams
  , readAttributeLeaderRouterIdWithParams
  , readAttributeDetachedRoleCountWithParams
  , readAttributeChildRoleCountWithParams
  , readAttributeRouterRoleCountWithParams
  , readAttributeLeaderRoleCountWithParams
  , readAttributeAttachAttemptCountWithParams
  , readAttributePartitionIdChangeCountWithParams
  , readAttributeBetterPartitionAttachAttemptCountWithParams
  , readAttributeParentChangeCountWithParams
  , readAttributeTxTotalCountWithParams
  , readAttributeTxUnicastCountWithParams
  , readAttributeTxBroadcastCountWithParams
  , readAttributeTxAckRequestedCountWithParams
  , readAttributeTxAckedCountWithParams
  , readAttributeTxNoAckRequestedCountWithParams
  , readAttributeTxDataCountWithParams
  , readAttributeTxDataPollCountWithParams
  , readAttributeTxBeaconCountWithParams
  , readAttributeTxBeaconRequestCountWithParams
  , readAttributeTxOtherCountWithParams
  , readAttributeTxRetryCountWithParams
  , readAttributeTxDirectMaxRetryExpiryCountWithParams
  , readAttributeTxIndirectMaxRetryExpiryCountWithParams
  , readAttributeTxErrCcaCountWithParams
  , readAttributeTxErrAbortCountWithParams
  , readAttributeTxErrBusyChannelCountWithParams
  , readAttributeRxTotalCountWithParams
  , readAttributeRxUnicastCountWithParams
  , readAttributeRxBroadcastCountWithParams
  , readAttributeRxDataCountWithParams
  , readAttributeRxDataPollCountWithParams
  , readAttributeRxBeaconCountWithParams
  , readAttributeRxBeaconRequestCountWithParams
  , readAttributeRxOtherCountWithParams
  , readAttributeRxAddressFilteredCountWithParams
  , readAttributeRxDestAddrFilteredCountWithParams
  , readAttributeRxDuplicatedCountWithParams
  , readAttributeRxErrNoFrameCountWithParams
  , readAttributeRxErrUnknownNeighborCountWithParams
  , readAttributeRxErrInvalidSrcAddrCountWithParams
  , readAttributeRxErrSecCountWithParams
  , readAttributeRxErrFcsCountWithParams
  , readAttributeRxErrOtherCountWithParams
  , readAttributeActiveTimestampWithParams
  , readAttributePendingTimestampWithParams
  , readAttributeDelayWithParams
  , readAttributeSecurityPolicyWithParams
  , readAttributeChannelPage0MaskWithParams
  , readAttributeOperationalDatasetComponentsWithParams
  , readAttributeActiveNetworkFaultsListWithParams
  , readAttributeExtAddressWithParams
  , readAttributeRloc16WithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandler
  , readAttributeNeighborTableListWithParams
  , readAttributeRouteTableListWithParams
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveNetworkFaultsListWithParamsSelector
  , readAttributeActiveTimestampWithParamsSelector
  , readAttributeAttachAttemptCountWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBetterPartitionAttachAttemptCountWithParamsSelector
  , readAttributeChannelPage0MaskWithParamsSelector
  , readAttributeChannelWithParamsSelector
  , readAttributeChildRoleCountWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDataVersionWithParamsSelector
  , readAttributeDelayWithParamsSelector
  , readAttributeDetachedRoleCountWithParamsSelector
  , readAttributeExtAddressWithParamsSelector
  , readAttributeExtendedPanIdWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLeaderRoleCountWithParamsSelector
  , readAttributeLeaderRouterIdWithParamsSelector
  , readAttributeMeshLocalPrefixWithParamsSelector
  , readAttributeNeighborTableListWithParamsSelector
  , readAttributeNeighborTableWithParamsSelector
  , readAttributeNetworkNameWithParamsSelector
  , readAttributeOperationalDatasetComponentsWithParamsSelector
  , readAttributeOverrunCountWithParamsSelector
  , readAttributePanIdWithParamsSelector
  , readAttributeParentChangeCountWithParamsSelector
  , readAttributePartitionIdChangeCountWithParamsSelector
  , readAttributePartitionIdWithParamsSelector
  , readAttributePendingTimestampWithParamsSelector
  , readAttributeRloc16WithParamsSelector
  , readAttributeRouteTableListWithParamsSelector
  , readAttributeRouteTableWithParamsSelector
  , readAttributeRouterRoleCountWithParamsSelector
  , readAttributeRoutingRoleWithParamsSelector
  , readAttributeRxAddressFilteredCountWithParamsSelector
  , readAttributeRxBeaconCountWithParamsSelector
  , readAttributeRxBeaconRequestCountWithParamsSelector
  , readAttributeRxBroadcastCountWithParamsSelector
  , readAttributeRxDataCountWithParamsSelector
  , readAttributeRxDataPollCountWithParamsSelector
  , readAttributeRxDestAddrFilteredCountWithParamsSelector
  , readAttributeRxDuplicatedCountWithParamsSelector
  , readAttributeRxErrFcsCountWithParamsSelector
  , readAttributeRxErrInvalidSrcAddrCountWithParamsSelector
  , readAttributeRxErrNoFrameCountWithParamsSelector
  , readAttributeRxErrOtherCountWithParamsSelector
  , readAttributeRxErrSecCountWithParamsSelector
  , readAttributeRxErrUnknownNeighborCountWithParamsSelector
  , readAttributeRxOtherCountWithParamsSelector
  , readAttributeRxTotalCountWithParamsSelector
  , readAttributeRxUnicastCountWithParamsSelector
  , readAttributeSecurityPolicyWithParamsSelector
  , readAttributeStableDataVersionWithParamsSelector
  , readAttributeTxAckRequestedCountWithParamsSelector
  , readAttributeTxAckedCountWithParamsSelector
  , readAttributeTxBeaconCountWithParamsSelector
  , readAttributeTxBeaconRequestCountWithParamsSelector
  , readAttributeTxBroadcastCountWithParamsSelector
  , readAttributeTxDataCountWithParamsSelector
  , readAttributeTxDataPollCountWithParamsSelector
  , readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector
  , readAttributeTxErrAbortCountWithParamsSelector
  , readAttributeTxErrBusyChannelCountWithParamsSelector
  , readAttributeTxErrCcaCountWithParamsSelector
  , readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector
  , readAttributeTxNoAckRequestedCountWithParamsSelector
  , readAttributeTxOtherCountWithParamsSelector
  , readAttributeTxRetryCountWithParamsSelector
  , readAttributeTxTotalCountWithParamsSelector
  , readAttributeTxUnicastCountWithParamsSelector
  , readAttributeWeightingWithParamsSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , resetCountsWithExpectedValues_expectedValueInterval_completionSelector
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRThreadNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDiagnostics params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadNetworkDiagnostics resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadNetworkDiagnosticsClusterResetCountsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDiagnostics expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadNetworkDiagnostics resetCountsWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeChannelWithParams:@
readAttributeChannelWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChannelWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeChannelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRoutingRoleWithParams:@
readAttributeRoutingRoleWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRoutingRoleWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRoutingRoleWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNetworkNameWithParams:@
readAttributeNetworkNameWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNetworkNameWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeNetworkNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePanIdWithParams:@
readAttributePanIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePanIdWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributePanIdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeExtendedPanIdWithParams:@
readAttributeExtendedPanIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeExtendedPanIdWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeExtendedPanIdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeshLocalPrefixWithParams:@
readAttributeMeshLocalPrefixWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeMeshLocalPrefixWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeMeshLocalPrefixWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOverrunCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeOverrunCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNeighborTableWithParams:@
readAttributeNeighborTableWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNeighborTableWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeNeighborTableWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRouteTableWithParams:@
readAttributeRouteTableWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRouteTableWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRouteTableWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePartitionIdWithParams:@
readAttributePartitionIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePartitionIdWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributePartitionIdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWeightingWithParams:@
readAttributeWeightingWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeWeightingWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeWeightingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDataVersionWithParams:@
readAttributeDataVersionWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeDataVersionWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeDataVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStableDataVersionWithParams:@
readAttributeStableDataVersionWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeStableDataVersionWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeStableDataVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLeaderRouterIdWithParams:@
readAttributeLeaderRouterIdWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeLeaderRouterIdWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeLeaderRouterIdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDetachedRoleCountWithParams:@
readAttributeDetachedRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeDetachedRoleCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeDetachedRoleCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeChildRoleCountWithParams:@
readAttributeChildRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChildRoleCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeChildRoleCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRouterRoleCountWithParams:@
readAttributeRouterRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRouterRoleCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRouterRoleCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLeaderRoleCountWithParams:@
readAttributeLeaderRoleCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeLeaderRoleCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeLeaderRoleCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttachAttemptCountWithParams:@
readAttributeAttachAttemptCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttachAttemptCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeAttachAttemptCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePartitionIdChangeCountWithParams:@
readAttributePartitionIdChangeCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePartitionIdChangeCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributePartitionIdChangeCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBetterPartitionAttachAttemptCountWithParams:@
readAttributeBetterPartitionAttachAttemptCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeBetterPartitionAttachAttemptCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeBetterPartitionAttachAttemptCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeParentChangeCountWithParams:@
readAttributeParentChangeCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeParentChangeCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeParentChangeCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxTotalCountWithParams:@
readAttributeTxTotalCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxTotalCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxTotalCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxUnicastCountWithParams:@
readAttributeTxUnicastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxUnicastCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxUnicastCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxBroadcastCountWithParams:@
readAttributeTxBroadcastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxBroadcastCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxBroadcastCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxAckRequestedCountWithParams:@
readAttributeTxAckRequestedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxAckRequestedCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxAckRequestedCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxAckedCountWithParams:@
readAttributeTxAckedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxAckedCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxAckedCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxNoAckRequestedCountWithParams:@
readAttributeTxNoAckRequestedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxNoAckRequestedCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxNoAckRequestedCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxDataCountWithParams:@
readAttributeTxDataCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxDataCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxDataCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxDataPollCountWithParams:@
readAttributeTxDataPollCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxDataPollCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxDataPollCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxBeaconCountWithParams:@
readAttributeTxBeaconCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxBeaconCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxBeaconCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxBeaconRequestCountWithParams:@
readAttributeTxBeaconRequestCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxBeaconRequestCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxBeaconRequestCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxOtherCountWithParams:@
readAttributeTxOtherCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxOtherCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxOtherCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxRetryCountWithParams:@
readAttributeTxRetryCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxRetryCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxRetryCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxDirectMaxRetryExpiryCountWithParams:@
readAttributeTxDirectMaxRetryExpiryCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxDirectMaxRetryExpiryCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxIndirectMaxRetryExpiryCountWithParams:@
readAttributeTxIndirectMaxRetryExpiryCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxIndirectMaxRetryExpiryCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxErrCcaCountWithParams:@
readAttributeTxErrCcaCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrCcaCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxErrCcaCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxErrAbortCountWithParams:@
readAttributeTxErrAbortCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrAbortCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxErrAbortCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTxErrBusyChannelCountWithParams:@
readAttributeTxErrBusyChannelCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeTxErrBusyChannelCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeTxErrBusyChannelCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxTotalCountWithParams:@
readAttributeRxTotalCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxTotalCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxTotalCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxUnicastCountWithParams:@
readAttributeRxUnicastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxUnicastCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxUnicastCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxBroadcastCountWithParams:@
readAttributeRxBroadcastCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxBroadcastCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxBroadcastCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxDataCountWithParams:@
readAttributeRxDataCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDataCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxDataCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxDataPollCountWithParams:@
readAttributeRxDataPollCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDataPollCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxDataPollCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxBeaconCountWithParams:@
readAttributeRxBeaconCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxBeaconCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxBeaconCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxBeaconRequestCountWithParams:@
readAttributeRxBeaconRequestCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxBeaconRequestCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxBeaconRequestCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxOtherCountWithParams:@
readAttributeRxOtherCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxOtherCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxOtherCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxAddressFilteredCountWithParams:@
readAttributeRxAddressFilteredCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxAddressFilteredCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxAddressFilteredCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxDestAddrFilteredCountWithParams:@
readAttributeRxDestAddrFilteredCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDestAddrFilteredCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxDestAddrFilteredCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxDuplicatedCountWithParams:@
readAttributeRxDuplicatedCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxDuplicatedCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxDuplicatedCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxErrNoFrameCountWithParams:@
readAttributeRxErrNoFrameCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrNoFrameCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxErrNoFrameCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxErrUnknownNeighborCountWithParams:@
readAttributeRxErrUnknownNeighborCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrUnknownNeighborCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxErrUnknownNeighborCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxErrInvalidSrcAddrCountWithParams:@
readAttributeRxErrInvalidSrcAddrCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrInvalidSrcAddrCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxErrInvalidSrcAddrCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxErrSecCountWithParams:@
readAttributeRxErrSecCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrSecCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxErrSecCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxErrFcsCountWithParams:@
readAttributeRxErrFcsCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrFcsCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxErrFcsCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRxErrOtherCountWithParams:@
readAttributeRxErrOtherCountWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRxErrOtherCountWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRxErrOtherCountWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveTimestampWithParams:@
readAttributeActiveTimestampWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveTimestampWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeActiveTimestampWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePendingTimestampWithParams:@
readAttributePendingTimestampWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributePendingTimestampWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributePendingTimestampWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDelayWithParams:@
readAttributeDelayWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeDelayWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeDelayWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSecurityPolicyWithParams:@
readAttributeSecurityPolicyWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeSecurityPolicyWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeSecurityPolicyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeChannelPage0MaskWithParams:@
readAttributeChannelPage0MaskWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeChannelPage0MaskWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeChannelPage0MaskWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalDatasetComponentsWithParams:@
readAttributeOperationalDatasetComponentsWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeOperationalDatasetComponentsWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeOperationalDatasetComponentsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveNetworkFaultsListWithParams:@
readAttributeActiveNetworkFaultsListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeActiveNetworkFaultsListWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeActiveNetworkFaultsListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeExtAddressWithParams:@
readAttributeExtAddressWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeExtAddressWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeExtAddressWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRloc16WithParams:@
readAttributeRloc16WithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRloc16WithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRloc16WithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics => mtrClusterThreadNetworkDiagnostics -> IO (Id MTRClusterThreadNetworkDiagnostics)
init_ mtrClusterThreadNetworkDiagnostics =
  sendOwnedMessage mtrClusterThreadNetworkDiagnostics initSelector

-- | @+ new@
new :: IO (Id MTRClusterThreadNetworkDiagnostics)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThreadNetworkDiagnostics"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRDevice device, IsNSObject queue) => mtrClusterThreadNetworkDiagnostics -> device -> CUShort -> queue -> IO (Id MTRClusterThreadNetworkDiagnostics)
initWithDevice_endpoint_queue mtrClusterThreadNetworkDiagnostics device endpoint queue =
  sendOwnedMessage mtrClusterThreadNetworkDiagnostics initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRThreadNetworkDiagnosticsClusterResetCountsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterThreadNetworkDiagnostics params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterThreadNetworkDiagnostics resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRThreadNetworkDiagnosticsClusterResetCountsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDiagnostics -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandler mtrClusterThreadNetworkDiagnostics expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterThreadNetworkDiagnostics resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- readAttributeNeighborTableListWithParams:@
readAttributeNeighborTableListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeNeighborTableListWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeNeighborTableListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRouteTableListWithParams:@
readAttributeRouteTableListWithParams :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRReadParams params) => mtrClusterThreadNetworkDiagnostics -> params -> IO (Id NSDictionary)
readAttributeRouteTableListWithParams mtrClusterThreadNetworkDiagnostics params =
  sendMessage mtrClusterThreadNetworkDiagnostics readAttributeRouteTableListWithParamsSelector (toMTRReadParams params)

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThreadNetworkDiagnostics mtrClusterThreadNetworkDiagnostics, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThreadNetworkDiagnostics -> device -> endpointID -> queue -> IO (Id MTRClusterThreadNetworkDiagnostics)
initWithDevice_endpointID_queue mtrClusterThreadNetworkDiagnostics device endpointID queue =
  sendOwnedMessage mtrClusterThreadNetworkDiagnostics initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completion:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadNetworkDiagnosticsClusterResetCountsParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completion:@
resetCountsWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeChannelWithParams:@
readAttributeChannelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeChannelWithParamsSelector = mkSelector "readAttributeChannelWithParams:"

-- | @Selector@ for @readAttributeRoutingRoleWithParams:@
readAttributeRoutingRoleWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRoutingRoleWithParamsSelector = mkSelector "readAttributeRoutingRoleWithParams:"

-- | @Selector@ for @readAttributeNetworkNameWithParams:@
readAttributeNetworkNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNetworkNameWithParamsSelector = mkSelector "readAttributeNetworkNameWithParams:"

-- | @Selector@ for @readAttributePanIdWithParams:@
readAttributePanIdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePanIdWithParamsSelector = mkSelector "readAttributePanIdWithParams:"

-- | @Selector@ for @readAttributeExtendedPanIdWithParams:@
readAttributeExtendedPanIdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeExtendedPanIdWithParamsSelector = mkSelector "readAttributeExtendedPanIdWithParams:"

-- | @Selector@ for @readAttributeMeshLocalPrefixWithParams:@
readAttributeMeshLocalPrefixWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeshLocalPrefixWithParamsSelector = mkSelector "readAttributeMeshLocalPrefixWithParams:"

-- | @Selector@ for @readAttributeOverrunCountWithParams:@
readAttributeOverrunCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOverrunCountWithParamsSelector = mkSelector "readAttributeOverrunCountWithParams:"

-- | @Selector@ for @readAttributeNeighborTableWithParams:@
readAttributeNeighborTableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNeighborTableWithParamsSelector = mkSelector "readAttributeNeighborTableWithParams:"

-- | @Selector@ for @readAttributeRouteTableWithParams:@
readAttributeRouteTableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRouteTableWithParamsSelector = mkSelector "readAttributeRouteTableWithParams:"

-- | @Selector@ for @readAttributePartitionIdWithParams:@
readAttributePartitionIdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePartitionIdWithParamsSelector = mkSelector "readAttributePartitionIdWithParams:"

-- | @Selector@ for @readAttributeWeightingWithParams:@
readAttributeWeightingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWeightingWithParamsSelector = mkSelector "readAttributeWeightingWithParams:"

-- | @Selector@ for @readAttributeDataVersionWithParams:@
readAttributeDataVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDataVersionWithParamsSelector = mkSelector "readAttributeDataVersionWithParams:"

-- | @Selector@ for @readAttributeStableDataVersionWithParams:@
readAttributeStableDataVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStableDataVersionWithParamsSelector = mkSelector "readAttributeStableDataVersionWithParams:"

-- | @Selector@ for @readAttributeLeaderRouterIdWithParams:@
readAttributeLeaderRouterIdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLeaderRouterIdWithParamsSelector = mkSelector "readAttributeLeaderRouterIdWithParams:"

-- | @Selector@ for @readAttributeDetachedRoleCountWithParams:@
readAttributeDetachedRoleCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDetachedRoleCountWithParamsSelector = mkSelector "readAttributeDetachedRoleCountWithParams:"

-- | @Selector@ for @readAttributeChildRoleCountWithParams:@
readAttributeChildRoleCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeChildRoleCountWithParamsSelector = mkSelector "readAttributeChildRoleCountWithParams:"

-- | @Selector@ for @readAttributeRouterRoleCountWithParams:@
readAttributeRouterRoleCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRouterRoleCountWithParamsSelector = mkSelector "readAttributeRouterRoleCountWithParams:"

-- | @Selector@ for @readAttributeLeaderRoleCountWithParams:@
readAttributeLeaderRoleCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLeaderRoleCountWithParamsSelector = mkSelector "readAttributeLeaderRoleCountWithParams:"

-- | @Selector@ for @readAttributeAttachAttemptCountWithParams:@
readAttributeAttachAttemptCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttachAttemptCountWithParamsSelector = mkSelector "readAttributeAttachAttemptCountWithParams:"

-- | @Selector@ for @readAttributePartitionIdChangeCountWithParams:@
readAttributePartitionIdChangeCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePartitionIdChangeCountWithParamsSelector = mkSelector "readAttributePartitionIdChangeCountWithParams:"

-- | @Selector@ for @readAttributeBetterPartitionAttachAttemptCountWithParams:@
readAttributeBetterPartitionAttachAttemptCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBetterPartitionAttachAttemptCountWithParamsSelector = mkSelector "readAttributeBetterPartitionAttachAttemptCountWithParams:"

-- | @Selector@ for @readAttributeParentChangeCountWithParams:@
readAttributeParentChangeCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeParentChangeCountWithParamsSelector = mkSelector "readAttributeParentChangeCountWithParams:"

-- | @Selector@ for @readAttributeTxTotalCountWithParams:@
readAttributeTxTotalCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxTotalCountWithParamsSelector = mkSelector "readAttributeTxTotalCountWithParams:"

-- | @Selector@ for @readAttributeTxUnicastCountWithParams:@
readAttributeTxUnicastCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxUnicastCountWithParamsSelector = mkSelector "readAttributeTxUnicastCountWithParams:"

-- | @Selector@ for @readAttributeTxBroadcastCountWithParams:@
readAttributeTxBroadcastCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxBroadcastCountWithParamsSelector = mkSelector "readAttributeTxBroadcastCountWithParams:"

-- | @Selector@ for @readAttributeTxAckRequestedCountWithParams:@
readAttributeTxAckRequestedCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxAckRequestedCountWithParamsSelector = mkSelector "readAttributeTxAckRequestedCountWithParams:"

-- | @Selector@ for @readAttributeTxAckedCountWithParams:@
readAttributeTxAckedCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxAckedCountWithParamsSelector = mkSelector "readAttributeTxAckedCountWithParams:"

-- | @Selector@ for @readAttributeTxNoAckRequestedCountWithParams:@
readAttributeTxNoAckRequestedCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxNoAckRequestedCountWithParamsSelector = mkSelector "readAttributeTxNoAckRequestedCountWithParams:"

-- | @Selector@ for @readAttributeTxDataCountWithParams:@
readAttributeTxDataCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxDataCountWithParamsSelector = mkSelector "readAttributeTxDataCountWithParams:"

-- | @Selector@ for @readAttributeTxDataPollCountWithParams:@
readAttributeTxDataPollCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxDataPollCountWithParamsSelector = mkSelector "readAttributeTxDataPollCountWithParams:"

-- | @Selector@ for @readAttributeTxBeaconCountWithParams:@
readAttributeTxBeaconCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxBeaconCountWithParamsSelector = mkSelector "readAttributeTxBeaconCountWithParams:"

-- | @Selector@ for @readAttributeTxBeaconRequestCountWithParams:@
readAttributeTxBeaconRequestCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxBeaconRequestCountWithParamsSelector = mkSelector "readAttributeTxBeaconRequestCountWithParams:"

-- | @Selector@ for @readAttributeTxOtherCountWithParams:@
readAttributeTxOtherCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxOtherCountWithParamsSelector = mkSelector "readAttributeTxOtherCountWithParams:"

-- | @Selector@ for @readAttributeTxRetryCountWithParams:@
readAttributeTxRetryCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxRetryCountWithParamsSelector = mkSelector "readAttributeTxRetryCountWithParams:"

-- | @Selector@ for @readAttributeTxDirectMaxRetryExpiryCountWithParams:@
readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxDirectMaxRetryExpiryCountWithParamsSelector = mkSelector "readAttributeTxDirectMaxRetryExpiryCountWithParams:"

-- | @Selector@ for @readAttributeTxIndirectMaxRetryExpiryCountWithParams:@
readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxIndirectMaxRetryExpiryCountWithParamsSelector = mkSelector "readAttributeTxIndirectMaxRetryExpiryCountWithParams:"

-- | @Selector@ for @readAttributeTxErrCcaCountWithParams:@
readAttributeTxErrCcaCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxErrCcaCountWithParamsSelector = mkSelector "readAttributeTxErrCcaCountWithParams:"

-- | @Selector@ for @readAttributeTxErrAbortCountWithParams:@
readAttributeTxErrAbortCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxErrAbortCountWithParamsSelector = mkSelector "readAttributeTxErrAbortCountWithParams:"

-- | @Selector@ for @readAttributeTxErrBusyChannelCountWithParams:@
readAttributeTxErrBusyChannelCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTxErrBusyChannelCountWithParamsSelector = mkSelector "readAttributeTxErrBusyChannelCountWithParams:"

-- | @Selector@ for @readAttributeRxTotalCountWithParams:@
readAttributeRxTotalCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxTotalCountWithParamsSelector = mkSelector "readAttributeRxTotalCountWithParams:"

-- | @Selector@ for @readAttributeRxUnicastCountWithParams:@
readAttributeRxUnicastCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxUnicastCountWithParamsSelector = mkSelector "readAttributeRxUnicastCountWithParams:"

-- | @Selector@ for @readAttributeRxBroadcastCountWithParams:@
readAttributeRxBroadcastCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxBroadcastCountWithParamsSelector = mkSelector "readAttributeRxBroadcastCountWithParams:"

-- | @Selector@ for @readAttributeRxDataCountWithParams:@
readAttributeRxDataCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxDataCountWithParamsSelector = mkSelector "readAttributeRxDataCountWithParams:"

-- | @Selector@ for @readAttributeRxDataPollCountWithParams:@
readAttributeRxDataPollCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxDataPollCountWithParamsSelector = mkSelector "readAttributeRxDataPollCountWithParams:"

-- | @Selector@ for @readAttributeRxBeaconCountWithParams:@
readAttributeRxBeaconCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxBeaconCountWithParamsSelector = mkSelector "readAttributeRxBeaconCountWithParams:"

-- | @Selector@ for @readAttributeRxBeaconRequestCountWithParams:@
readAttributeRxBeaconRequestCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxBeaconRequestCountWithParamsSelector = mkSelector "readAttributeRxBeaconRequestCountWithParams:"

-- | @Selector@ for @readAttributeRxOtherCountWithParams:@
readAttributeRxOtherCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxOtherCountWithParamsSelector = mkSelector "readAttributeRxOtherCountWithParams:"

-- | @Selector@ for @readAttributeRxAddressFilteredCountWithParams:@
readAttributeRxAddressFilteredCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxAddressFilteredCountWithParamsSelector = mkSelector "readAttributeRxAddressFilteredCountWithParams:"

-- | @Selector@ for @readAttributeRxDestAddrFilteredCountWithParams:@
readAttributeRxDestAddrFilteredCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxDestAddrFilteredCountWithParamsSelector = mkSelector "readAttributeRxDestAddrFilteredCountWithParams:"

-- | @Selector@ for @readAttributeRxDuplicatedCountWithParams:@
readAttributeRxDuplicatedCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxDuplicatedCountWithParamsSelector = mkSelector "readAttributeRxDuplicatedCountWithParams:"

-- | @Selector@ for @readAttributeRxErrNoFrameCountWithParams:@
readAttributeRxErrNoFrameCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxErrNoFrameCountWithParamsSelector = mkSelector "readAttributeRxErrNoFrameCountWithParams:"

-- | @Selector@ for @readAttributeRxErrUnknownNeighborCountWithParams:@
readAttributeRxErrUnknownNeighborCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxErrUnknownNeighborCountWithParamsSelector = mkSelector "readAttributeRxErrUnknownNeighborCountWithParams:"

-- | @Selector@ for @readAttributeRxErrInvalidSrcAddrCountWithParams:@
readAttributeRxErrInvalidSrcAddrCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxErrInvalidSrcAddrCountWithParamsSelector = mkSelector "readAttributeRxErrInvalidSrcAddrCountWithParams:"

-- | @Selector@ for @readAttributeRxErrSecCountWithParams:@
readAttributeRxErrSecCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxErrSecCountWithParamsSelector = mkSelector "readAttributeRxErrSecCountWithParams:"

-- | @Selector@ for @readAttributeRxErrFcsCountWithParams:@
readAttributeRxErrFcsCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxErrFcsCountWithParamsSelector = mkSelector "readAttributeRxErrFcsCountWithParams:"

-- | @Selector@ for @readAttributeRxErrOtherCountWithParams:@
readAttributeRxErrOtherCountWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRxErrOtherCountWithParamsSelector = mkSelector "readAttributeRxErrOtherCountWithParams:"

-- | @Selector@ for @readAttributeActiveTimestampWithParams:@
readAttributeActiveTimestampWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveTimestampWithParamsSelector = mkSelector "readAttributeActiveTimestampWithParams:"

-- | @Selector@ for @readAttributePendingTimestampWithParams:@
readAttributePendingTimestampWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePendingTimestampWithParamsSelector = mkSelector "readAttributePendingTimestampWithParams:"

-- | @Selector@ for @readAttributeDelayWithParams:@
readAttributeDelayWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDelayWithParamsSelector = mkSelector "readAttributeDelayWithParams:"

-- | @Selector@ for @readAttributeSecurityPolicyWithParams:@
readAttributeSecurityPolicyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSecurityPolicyWithParamsSelector = mkSelector "readAttributeSecurityPolicyWithParams:"

-- | @Selector@ for @readAttributeChannelPage0MaskWithParams:@
readAttributeChannelPage0MaskWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeChannelPage0MaskWithParamsSelector = mkSelector "readAttributeChannelPage0MaskWithParams:"

-- | @Selector@ for @readAttributeOperationalDatasetComponentsWithParams:@
readAttributeOperationalDatasetComponentsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperationalDatasetComponentsWithParamsSelector = mkSelector "readAttributeOperationalDatasetComponentsWithParams:"

-- | @Selector@ for @readAttributeActiveNetworkFaultsListWithParams:@
readAttributeActiveNetworkFaultsListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveNetworkFaultsListWithParamsSelector = mkSelector "readAttributeActiveNetworkFaultsListWithParams:"

-- | @Selector@ for @readAttributeExtAddressWithParams:@
readAttributeExtAddressWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeExtAddressWithParamsSelector = mkSelector "readAttributeExtAddressWithParams:"

-- | @Selector@ for @readAttributeRloc16WithParams:@
readAttributeRloc16WithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRloc16WithParamsSelector = mkSelector "readAttributeRloc16WithParams:"

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
initSelector :: Selector '[] (Id MTRClusterThreadNetworkDiagnostics)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterThreadNetworkDiagnostics)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterThreadNetworkDiagnostics)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:@
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRThreadNetworkDiagnosticsClusterResetCountsParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @resetCountsWithExpectedValues:expectedValueInterval:completionHandler:@
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetCountsWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "resetCountsWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @readAttributeNeighborTableListWithParams:@
readAttributeNeighborTableListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNeighborTableListWithParamsSelector = mkSelector "readAttributeNeighborTableListWithParams:"

-- | @Selector@ for @readAttributeRouteTableListWithParams:@
readAttributeRouteTableListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRouteTableListWithParamsSelector = mkSelector "readAttributeRouteTableListWithParams:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterThreadNetworkDiagnostics)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

