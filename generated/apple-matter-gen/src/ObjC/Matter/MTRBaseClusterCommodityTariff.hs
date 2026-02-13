{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Tariff
--
-- The CommodityTariffCluster provides the mechanism for communicating Commodity Tariff information within the premises.
--
-- Generated bindings for @MTRBaseClusterCommodityTariff@.
module ObjC.Matter.MTRBaseClusterCommodityTariff
  ( MTRBaseClusterCommodityTariff
  , IsMTRBaseClusterCommodityTariff(..)
  , getTariffComponentWithParams_completion
  , getDayEntryWithParams_completion
  , readAttributeTariffInfoWithCompletion
  , subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffUnitWithCompletion
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeStartDateWithCompletion
  , subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeStartDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeDayEntriesWithCompletion
  , subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandler
  , readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completion
  , readAttributeDayPatternsWithCompletion
  , subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandler
  , readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCalendarPeriodsWithCompletion
  , subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completion
  , readAttributeIndividualDaysWithCompletion
  , subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandler
  , readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentDayWithCompletion
  , subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextDayWithCompletion
  , subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextDayWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentDayEntryWithCompletion
  , subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentDayEntryDateWithCompletion
  , subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextDayEntryWithCompletion
  , subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextDayEntryDateWithCompletion
  , subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffComponentsWithCompletion
  , subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeTariffPeriodsWithCompletion
  , subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentTariffComponentsWithCompletion
  , subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeNextTariffComponentsWithCompletion
  , subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandler
  , readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultRandomizationOffsetWithCompletion
  , subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completion
  , readAttributeDefaultRandomizationTypeWithCompletion
  , subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandler
  , readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completion
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
  , getDayEntryWithParams_completionSelector
  , getTariffComponentWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCalendarPeriodsWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentDayEntryDateWithCompletionSelector
  , readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentDayEntryWithCompletionSelector
  , readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentDayWithCompletionSelector
  , readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentTariffComponentsWithCompletionSelector
  , readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDayEntriesWithCompletionSelector
  , readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDayPatternsWithCompletionSelector
  , readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultRandomizationOffsetWithCompletionSelector
  , readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeDefaultRandomizationTypeWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeIndividualDaysWithCompletionSelector
  , readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextDayEntryDateWithCompletionSelector
  , readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextDayEntryWithCompletionSelector
  , readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextDayWithCompletionSelector
  , readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeNextTariffComponentsWithCompletionSelector
  , readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeStartDateWithCompletionSelector
  , readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffComponentsWithCompletionSelector
  , readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffInfoWithCompletionSelector
  , readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffPeriodsWithCompletionSelector
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffUnitWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Command GetTariffComponent
--
-- The GetTariffComponent command allows a client to request information for a tariff component identifier that may no longer be available in the TariffPeriods attributes.
--
-- ObjC selector: @- getTariffComponentWithParams:completion:@
getTariffComponentWithParams_completion :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRCommodityTariffClusterGetTariffComponentParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> IO ()
getTariffComponentWithParams_completion mtrBaseClusterCommodityTariff params completion =
  sendMessage mtrBaseClusterCommodityTariff getTariffComponentWithParams_completionSelector (toMTRCommodityTariffClusterGetTariffComponentParams params) completion

-- | Command GetDayEntry
--
-- The GetDayEntry command allows a client to request information for a calendar day entry identifier that may no longer be available in the CalendarPeriods or IndividualDays attributes.
--
-- ObjC selector: @- getDayEntryWithParams:completion:@
getDayEntryWithParams_completion :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRCommodityTariffClusterGetDayEntryParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> IO ()
getDayEntryWithParams_completion mtrBaseClusterCommodityTariff params completion =
  sendMessage mtrBaseClusterCommodityTariff getDayEntryWithParams_completionSelector (toMTRCommodityTariffClusterGetDayEntryParams params) completion

-- | @- readAttributeTariffInfoWithCompletion:@
readAttributeTariffInfoWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffInfoWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeTariffInfoWithCompletionSelector completion

-- | @- subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffUnitWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeTariffUnitWithCompletionSelector completion

-- | @- subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeStartDateWithCompletion:@
readAttributeStartDateWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeStartDateWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeStartDateWithCompletionSelector completion

-- | @- subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeStartDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDayEntriesWithCompletion:@
readAttributeDayEntriesWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDayEntriesWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeDayEntriesWithCompletionSelector completion

-- | @- subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDayPatternsWithCompletion:@
readAttributeDayPatternsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDayPatternsWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeDayPatternsWithCompletionSelector completion

-- | @- subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCalendarPeriodsWithCompletion:@
readAttributeCalendarPeriodsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCalendarPeriodsWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeCalendarPeriodsWithCompletionSelector completion

-- | @- subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeIndividualDaysWithCompletion:@
readAttributeIndividualDaysWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeIndividualDaysWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeIndividualDaysWithCompletionSelector completion

-- | @- subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:@
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentDayWithCompletion:@
readAttributeCurrentDayWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentDayWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeCurrentDayWithCompletionSelector completion

-- | @- subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextDayWithCompletion:@
readAttributeNextDayWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextDayWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeNextDayWithCompletionSelector completion

-- | @- subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextDayWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentDayEntryWithCompletion:@
readAttributeCurrentDayEntryWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentDayEntryWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeCurrentDayEntryWithCompletionSelector completion

-- | @- subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentDayEntryDateWithCompletion:@
readAttributeCurrentDayEntryDateWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentDayEntryDateWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeCurrentDayEntryDateWithCompletionSelector completion

-- | @- subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextDayEntryWithCompletion:@
readAttributeNextDayEntryWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextDayEntryWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeNextDayEntryWithCompletionSelector completion

-- | @- subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextDayEntryDateWithCompletion:@
readAttributeNextDayEntryDateWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextDayEntryDateWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeNextDayEntryDateWithCompletionSelector completion

-- | @- subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTariffComponentsWithCompletion:@
readAttributeTariffComponentsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffComponentsWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeTariffComponentsWithCompletionSelector completion

-- | @- subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeTariffPeriodsWithCompletion:@
readAttributeTariffPeriodsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeTariffPeriodsWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeTariffPeriodsWithCompletionSelector completion

-- | @- subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentTariffComponentsWithCompletion:@
readAttributeCurrentTariffComponentsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeCurrentTariffComponentsWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeCurrentTariffComponentsWithCompletionSelector completion

-- | @- subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeNextTariffComponentsWithCompletion:@
readAttributeNextTariffComponentsWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeNextTariffComponentsWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeNextTariffComponentsWithCompletionSelector completion

-- | @- subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDefaultRandomizationOffsetWithCompletion:@
readAttributeDefaultRandomizationOffsetWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDefaultRandomizationOffsetWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeDefaultRandomizationOffsetWithCompletionSelector completion

-- | @- subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeDefaultRandomizationTypeWithCompletion:@
readAttributeDefaultRandomizationTypeWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeDefaultRandomizationTypeWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeDefaultRandomizationTypeWithCompletionSelector completion

-- | @- subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCommodityTariff completion =
  sendMessage mtrBaseClusterCommodityTariff readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRSubscribeParams params) => mtrBaseClusterCommodityTariff -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityTariff params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityTariff subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff => mtrBaseClusterCommodityTariff -> IO (Id MTRBaseClusterCommodityTariff)
init_ mtrBaseClusterCommodityTariff =
  sendOwnedMessage mtrBaseClusterCommodityTariff initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterCommodityTariff)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityTariff"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCommodityTariff mtrBaseClusterCommodityTariff, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCommodityTariff -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCommodityTariff)
initWithDevice_endpointID_queue mtrBaseClusterCommodityTariff device endpointID queue =
  sendOwnedMessage mtrBaseClusterCommodityTariff initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getTariffComponentWithParams:completion:@
getTariffComponentWithParams_completionSelector :: Selector '[Id MTRCommodityTariffClusterGetTariffComponentParams, Ptr ()] ()
getTariffComponentWithParams_completionSelector = mkSelector "getTariffComponentWithParams:completion:"

-- | @Selector@ for @getDayEntryWithParams:completion:@
getDayEntryWithParams_completionSelector :: Selector '[Id MTRCommodityTariffClusterGetDayEntryParams, Ptr ()] ()
getDayEntryWithParams_completionSelector = mkSelector "getDayEntryWithParams:completion:"

-- | @Selector@ for @readAttributeTariffInfoWithCompletion:@
readAttributeTariffInfoWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTariffInfoWithCompletionSelector = mkSelector "readAttributeTariffInfoWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTariffInfoWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffInfoWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTariffInfoWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffInfoWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTariffUnitWithCompletionSelector = mkSelector "readAttributeTariffUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeStartDateWithCompletion:@
readAttributeStartDateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeStartDateWithCompletionSelector = mkSelector "readAttributeStartDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeStartDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeStartDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeStartDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeStartDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDayEntriesWithCompletion:@
readAttributeDayEntriesWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDayEntriesWithCompletionSelector = mkSelector "readAttributeDayEntriesWithCompletion:"

-- | @Selector@ for @subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDayEntriesWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDayEntriesWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDayEntriesWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDayEntriesWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDayPatternsWithCompletion:@
readAttributeDayPatternsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDayPatternsWithCompletionSelector = mkSelector "readAttributeDayPatternsWithCompletion:"

-- | @Selector@ for @subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDayPatternsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDayPatternsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:@
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDayPatternsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDayPatternsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCalendarPeriodsWithCompletion:@
readAttributeCalendarPeriodsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCalendarPeriodsWithCompletionSelector = mkSelector "readAttributeCalendarPeriodsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCalendarPeriodsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCalendarPeriodsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCalendarPeriodsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCalendarPeriodsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeIndividualDaysWithCompletion:@
readAttributeIndividualDaysWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeIndividualDaysWithCompletionSelector = mkSelector "readAttributeIndividualDaysWithCompletion:"

-- | @Selector@ for @subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeIndividualDaysWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeIndividualDaysWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:@
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeIndividualDaysWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeIndividualDaysWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentDayWithCompletion:@
readAttributeCurrentDayWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentDayWithCompletionSelector = mkSelector "readAttributeCurrentDayWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentDayWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentDayWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentDayWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentDayWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextDayWithCompletion:@
readAttributeNextDayWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextDayWithCompletionSelector = mkSelector "readAttributeNextDayWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextDayWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextDayWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextDayWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextDayWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentDayEntryWithCompletion:@
readAttributeCurrentDayEntryWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentDayEntryWithCompletionSelector = mkSelector "readAttributeCurrentDayEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentDayEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentDayEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentDayEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentDayEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentDayEntryDateWithCompletion:@
readAttributeCurrentDayEntryDateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentDayEntryDateWithCompletionSelector = mkSelector "readAttributeCurrentDayEntryDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentDayEntryDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentDayEntryDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextDayEntryWithCompletion:@
readAttributeNextDayEntryWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextDayEntryWithCompletionSelector = mkSelector "readAttributeNextDayEntryWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextDayEntryWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextDayEntryWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextDayEntryWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextDayEntryWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextDayEntryDateWithCompletion:@
readAttributeNextDayEntryDateWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextDayEntryDateWithCompletionSelector = mkSelector "readAttributeNextDayEntryDateWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextDayEntryDateWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextDayEntryDateWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextDayEntryDateWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextDayEntryDateWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffComponentsWithCompletion:@
readAttributeTariffComponentsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTariffComponentsWithCompletionSelector = mkSelector "readAttributeTariffComponentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffComponentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffComponentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeTariffPeriodsWithCompletion:@
readAttributeTariffPeriodsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTariffPeriodsWithCompletionSelector = mkSelector "readAttributeTariffPeriodsWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTariffPeriodsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffPeriodsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTariffPeriodsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffPeriodsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentTariffComponentsWithCompletion:@
readAttributeCurrentTariffComponentsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentTariffComponentsWithCompletionSelector = mkSelector "readAttributeCurrentTariffComponentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentTariffComponentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentTariffComponentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeNextTariffComponentsWithCompletion:@
readAttributeNextTariffComponentsWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeNextTariffComponentsWithCompletionSelector = mkSelector "readAttributeNextTariffComponentsWithCompletion:"

-- | @Selector@ for @subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeNextTariffComponentsWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeNextTariffComponentsWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:@
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeNextTariffComponentsWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeNextTariffComponentsWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultRandomizationOffsetWithCompletion:@
readAttributeDefaultRandomizationOffsetWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDefaultRandomizationOffsetWithCompletionSelector = mkSelector "readAttributeDefaultRandomizationOffsetWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultRandomizationOffsetWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultRandomizationOffsetWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultRandomizationOffsetWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultRandomizationOffsetWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeDefaultRandomizationTypeWithCompletion:@
readAttributeDefaultRandomizationTypeWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeDefaultRandomizationTypeWithCompletionSelector = mkSelector "readAttributeDefaultRandomizationTypeWithCompletion:"

-- | @Selector@ for @subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeDefaultRandomizationTypeWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeDefaultRandomizationTypeWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:@
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeDefaultRandomizationTypeWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeDefaultRandomizationTypeWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterCommodityTariff)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterCommodityTariff)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterCommodityTariff)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

