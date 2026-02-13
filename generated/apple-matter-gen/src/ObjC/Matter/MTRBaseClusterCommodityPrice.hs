{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Price
--
-- The Commodity Price Cluster provides the mechanism for communicating Gas, Energy, or Water pricing information within the premises.
--
-- Generated bindings for @MTRBaseClusterCommodityPrice@.
module ObjC.Matter.MTRBaseClusterCommodityPrice
  ( MTRBaseClusterCommodityPrice
  , IsMTRBaseClusterCommodityPrice(..)
  , getDetailedPriceRequestWithParams_completion
  , getDetailedForecastRequestWithParams_completion
  , readAttributeTariffUnitWithCompletion
  , subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrencyWithCompletion
  , subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrencyWithClusterStateCache_endpoint_queue_completion
  , readAttributeCurrentPriceWithCompletion
  , subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandler
  , readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completion
  , readAttributePriceForecastWithCompletion
  , subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandler
  , readAttributePriceForecastWithClusterStateCache_endpoint_queue_completion
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
  , getDetailedForecastRequestWithParams_completionSelector
  , getDetailedPriceRequestWithParams_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAcceptedCommandListWithCompletionSelector
  , readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeAttributeListWithCompletionSelector
  , readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeClusterRevisionWithCompletionSelector
  , readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrencyWithCompletionSelector
  , readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeCurrentPriceWithCompletionSelector
  , readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeFeatureMapWithCompletionSelector
  , readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeGeneratedCommandListWithCompletionSelector
  , readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributePriceForecastWithCompletionSelector
  , readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector
  , readAttributeTariffUnitWithCompletionSelector
  , subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector
  , subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector
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

-- | Command GetDetailedPriceRequest
--
-- Upon receipt, this SHALL generate a GetDetailedPrice Response command.
--
-- ObjC selector: @- getDetailedPriceRequestWithParams:completion:@
getDetailedPriceRequestWithParams_completion :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedPriceRequestParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> IO ()
getDetailedPriceRequestWithParams_completion mtrBaseClusterCommodityPrice params completion =
  sendMessage mtrBaseClusterCommodityPrice getDetailedPriceRequestWithParams_completionSelector (toMTRCommodityPriceClusterGetDetailedPriceRequestParams params) completion

-- | Command GetDetailedForecastRequest
--
-- Upon receipt, this SHALL generate a GetDetailedForecast Response command.
--
-- ObjC selector: @- getDetailedForecastRequestWithParams:completion:@
getDetailedForecastRequestWithParams_completion :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedForecastRequestParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> IO ()
getDetailedForecastRequestWithParams_completion mtrBaseClusterCommodityPrice params completion =
  sendMessage mtrBaseClusterCommodityPrice getDetailedForecastRequestWithParams_completionSelector (toMTRCommodityPriceClusterGetDetailedForecastRequestParams params) completion

-- | @- readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeTariffUnitWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeTariffUnitWithCompletionSelector completion

-- | @- subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrencyWithCompletion:@
readAttributeCurrencyWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeCurrencyWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeCurrencyWithCompletionSelector completion

-- | @- subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeCurrentPriceWithCompletion:@
readAttributeCurrentPriceWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeCurrentPriceWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeCurrentPriceWithCompletionSelector completion

-- | @- subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributePriceForecastWithCompletion:@
readAttributePriceForecastWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributePriceForecastWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributePriceForecastWithCompletionSelector completion

-- | @- subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeGeneratedCommandListWithCompletion:@
readAttributeGeneratedCommandListWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeGeneratedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeGeneratedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeGeneratedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeGeneratedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeGeneratedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAcceptedCommandListWithCompletion:@
readAttributeAcceptedCommandListWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeAcceptedCommandListWithCompletionSelector completion

-- | @- subscribeAttributeAcceptedCommandListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeAcceptedCommandListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAcceptedCommandListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeAcceptedCommandListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeAttributeListWithCompletion:@
readAttributeAttributeListWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeAttributeListWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeAttributeListWithCompletionSelector completion

-- | @- subscribeAttributeAttributeListWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeAttributeListWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeAttributeListWithClusterStateCache:endpoint:queue:completion:@
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeAttributeListWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeAttributeListWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeFeatureMapWithCompletion:@
readAttributeFeatureMapWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeFeatureMapWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeFeatureMapWithCompletionSelector completion

-- | @- subscribeAttributeFeatureMapWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeFeatureMapWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeFeatureMapWithClusterStateCache:endpoint:queue:completion:@
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeFeatureMapWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- readAttributeClusterRevisionWithCompletion:@
readAttributeClusterRevisionWithCompletion :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> Ptr () -> IO ()
readAttributeClusterRevisionWithCompletion mtrBaseClusterCommodityPrice completion =
  sendMessage mtrBaseClusterCommodityPrice readAttributeClusterRevisionWithCompletionSelector completion

-- | @- subscribeAttributeClusterRevisionWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRSubscribeParams params) => mtrBaseClusterCommodityPrice -> params -> Ptr () -> Ptr () -> IO ()
subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandler mtrBaseClusterCommodityPrice params subscriptionEstablished reportHandler =
  sendMessage mtrBaseClusterCommodityPrice subscribeAttributeClusterRevisionWithParams_subscriptionEstablished_reportHandlerSelector (toMTRSubscribeParams params) subscriptionEstablished reportHandler

-- | @+ readAttributeClusterRevisionWithClusterStateCache:endpoint:queue:completion:@
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion :: (IsMTRClusterStateCacheContainer clusterStateCacheContainer, IsNSNumber endpoint, IsNSObject queue) => clusterStateCacheContainer -> endpoint -> queue -> Ptr () -> IO ()
readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completion clusterStateCacheContainer endpoint queue completion =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendClassMessage cls' readAttributeClusterRevisionWithClusterStateCache_endpoint_queue_completionSelector (toMTRClusterStateCacheContainer clusterStateCacheContainer) (toNSNumber endpoint) (toNSObject queue) completion

-- | @- init@
init_ :: IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice => mtrBaseClusterCommodityPrice -> IO (Id MTRBaseClusterCommodityPrice)
init_ mtrBaseClusterCommodityPrice =
  sendOwnedMessage mtrBaseClusterCommodityPrice initSelector

-- | @+ new@
new :: IO (Id MTRBaseClusterCommodityPrice)
new  =
  do
    cls' <- getRequiredClass "MTRBaseClusterCommodityPrice"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods (reads, writes, commands) that take a completion, the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRBaseClusterCommodityPrice mtrBaseClusterCommodityPrice, IsMTRBaseDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrBaseClusterCommodityPrice -> device -> endpointID -> queue -> IO (Id MTRBaseClusterCommodityPrice)
initWithDevice_endpointID_queue mtrBaseClusterCommodityPrice device endpointID queue =
  sendOwnedMessage mtrBaseClusterCommodityPrice initWithDevice_endpointID_queueSelector (toMTRBaseDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getDetailedPriceRequestWithParams:completion:@
getDetailedPriceRequestWithParams_completionSelector :: Selector '[Id MTRCommodityPriceClusterGetDetailedPriceRequestParams, Ptr ()] ()
getDetailedPriceRequestWithParams_completionSelector = mkSelector "getDetailedPriceRequestWithParams:completion:"

-- | @Selector@ for @getDetailedForecastRequestWithParams:completion:@
getDetailedForecastRequestWithParams_completionSelector :: Selector '[Id MTRCommodityPriceClusterGetDetailedForecastRequestParams, Ptr ()] ()
getDetailedForecastRequestWithParams_completionSelector = mkSelector "getDetailedForecastRequestWithParams:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithCompletion:@
readAttributeTariffUnitWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeTariffUnitWithCompletionSelector = mkSelector "readAttributeTariffUnitWithCompletion:"

-- | @Selector@ for @subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeTariffUnitWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeTariffUnitWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:@
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeTariffUnitWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeTariffUnitWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrencyWithCompletion:@
readAttributeCurrencyWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrencyWithCompletionSelector = mkSelector "readAttributeCurrencyWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrencyWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrencyWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrencyWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrencyWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributeCurrentPriceWithCompletion:@
readAttributeCurrentPriceWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributeCurrentPriceWithCompletionSelector = mkSelector "readAttributeCurrentPriceWithCompletion:"

-- | @Selector@ for @subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributeCurrentPriceWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributeCurrentPriceWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:@
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributeCurrentPriceWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributeCurrentPriceWithClusterStateCache:endpoint:queue:completion:"

-- | @Selector@ for @readAttributePriceForecastWithCompletion:@
readAttributePriceForecastWithCompletionSelector :: Selector '[Ptr ()] ()
readAttributePriceForecastWithCompletionSelector = mkSelector "readAttributePriceForecastWithCompletion:"

-- | @Selector@ for @subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:@
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector :: Selector '[Id MTRSubscribeParams, Ptr (), Ptr ()] ()
subscribeAttributePriceForecastWithParams_subscriptionEstablished_reportHandlerSelector = mkSelector "subscribeAttributePriceForecastWithParams:subscriptionEstablished:reportHandler:"

-- | @Selector@ for @readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:@
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector :: Selector '[Id MTRClusterStateCacheContainer, Id NSNumber, Id NSObject, Ptr ()] ()
readAttributePriceForecastWithClusterStateCache_endpoint_queue_completionSelector = mkSelector "readAttributePriceForecastWithClusterStateCache:endpoint:queue:completion:"

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
initSelector :: Selector '[] (Id MTRBaseClusterCommodityPrice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRBaseClusterCommodityPrice)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRBaseDevice, Id NSNumber, Id NSObject] (Id MTRBaseClusterCommodityPrice)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

