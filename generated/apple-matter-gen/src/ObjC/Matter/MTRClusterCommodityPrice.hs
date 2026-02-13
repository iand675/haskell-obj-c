{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Price    The Commodity Price Cluster provides the mechanism for communicating Gas, Energy, or Water pricing information within the premises.
--
-- Generated bindings for @MTRClusterCommodityPrice@.
module ObjC.Matter.MTRClusterCommodityPrice
  ( MTRClusterCommodityPrice
  , IsMTRClusterCommodityPrice(..)
  , getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completion
  , getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTariffUnitWithParams
  , readAttributeCurrencyWithParams
  , readAttributeCurrentPriceWithParams
  , readAttributePriceForecastWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrencyWithParamsSelector
  , readAttributeCurrentPriceWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePriceForecastWithParamsSelector
  , readAttributeTariffUnitWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedPriceRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityPrice -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityPrice params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCommodityPrice getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCommodityPriceClusterGetDetailedPriceRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRCommodityPriceClusterGetDetailedForecastRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityPrice -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityPrice params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCommodityPrice getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCommodityPriceClusterGetDetailedForecastRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeTariffUnitWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeTariffUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrencyWithParams:@
readAttributeCurrencyWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeCurrencyWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeCurrencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPriceWithParams:@
readAttributeCurrentPriceWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeCurrentPriceWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeCurrentPriceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePriceForecastWithParams:@
readAttributePriceForecastWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributePriceForecastWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributePriceForecastWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRReadParams params) => mtrClusterCommodityPrice -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommodityPrice params =
  sendMessage mtrClusterCommodityPrice readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCommodityPrice mtrClusterCommodityPrice => mtrClusterCommodityPrice -> IO (Id MTRClusterCommodityPrice)
init_ mtrClusterCommodityPrice =
  sendOwnedMessage mtrClusterCommodityPrice initSelector

-- | @+ new@
new :: IO (Id MTRClusterCommodityPrice)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommodityPrice"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommodityPrice mtrClusterCommodityPrice, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommodityPrice -> device -> endpointID -> queue -> IO (Id MTRClusterCommodityPrice)
initWithDevice_endpointID_queue mtrClusterCommodityPrice device endpointID queue =
  sendOwnedMessage mtrClusterCommodityPrice initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCommodityPriceClusterGetDetailedPriceRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
getDetailedPriceRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getDetailedPriceRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:@
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCommodityPriceClusterGetDetailedForecastRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
getDetailedForecastRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getDetailedForecastRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTariffUnitWithParamsSelector = mkSelector "readAttributeTariffUnitWithParams:"

-- | @Selector@ for @readAttributeCurrencyWithParams:@
readAttributeCurrencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrencyWithParamsSelector = mkSelector "readAttributeCurrencyWithParams:"

-- | @Selector@ for @readAttributeCurrentPriceWithParams:@
readAttributeCurrentPriceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPriceWithParamsSelector = mkSelector "readAttributeCurrentPriceWithParams:"

-- | @Selector@ for @readAttributePriceForecastWithParams:@
readAttributePriceForecastWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePriceForecastWithParamsSelector = mkSelector "readAttributePriceForecastWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterCommodityPrice)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCommodityPrice)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCommodityPrice)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

