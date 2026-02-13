{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Tariff    The CommodityTariffCluster provides the mechanism for communicating Commodity Tariff information within the premises.
--
-- Generated bindings for @MTRClusterCommodityTariff@.
module ObjC.Matter.MTRClusterCommodityTariff
  ( MTRClusterCommodityTariff
  , IsMTRClusterCommodityTariff(..)
  , getTariffComponentWithParams_expectedValues_expectedValueInterval_completion
  , getDayEntryWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTariffInfoWithParams
  , readAttributeTariffUnitWithParams
  , readAttributeStartDateWithParams
  , readAttributeDayEntriesWithParams
  , readAttributeDayPatternsWithParams
  , readAttributeCalendarPeriodsWithParams
  , readAttributeIndividualDaysWithParams
  , readAttributeCurrentDayWithParams
  , readAttributeNextDayWithParams
  , readAttributeCurrentDayEntryWithParams
  , readAttributeCurrentDayEntryDateWithParams
  , readAttributeNextDayEntryWithParams
  , readAttributeNextDayEntryDateWithParams
  , readAttributeTariffComponentsWithParams
  , readAttributeTariffPeriodsWithParams
  , readAttributeCurrentTariffComponentsWithParams
  , readAttributeNextTariffComponentsWithParams
  , readAttributeDefaultRandomizationOffsetWithParams
  , readAttributeDefaultRandomizationTypeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector
  , getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeCalendarPeriodsWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentDayEntryDateWithParamsSelector
  , readAttributeCurrentDayEntryWithParamsSelector
  , readAttributeCurrentDayWithParamsSelector
  , readAttributeCurrentTariffComponentsWithParamsSelector
  , readAttributeDayEntriesWithParamsSelector
  , readAttributeDayPatternsWithParamsSelector
  , readAttributeDefaultRandomizationOffsetWithParamsSelector
  , readAttributeDefaultRandomizationTypeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeIndividualDaysWithParamsSelector
  , readAttributeNextDayEntryDateWithParamsSelector
  , readAttributeNextDayEntryWithParamsSelector
  , readAttributeNextDayWithParamsSelector
  , readAttributeNextTariffComponentsWithParamsSelector
  , readAttributeStartDateWithParamsSelector
  , readAttributeTariffComponentsWithParamsSelector
  , readAttributeTariffInfoWithParamsSelector
  , readAttributeTariffPeriodsWithParamsSelector
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

-- | @- getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:@
getTariffComponentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRCommodityTariffClusterGetTariffComponentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityTariff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getTariffComponentWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityTariff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCommodityTariff getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCommodityTariffClusterGetTariffComponentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getDayEntryWithParams:expectedValues:expectedValueInterval:completion:@
getDayEntryWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRCommodityTariffClusterGetDayEntryParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommodityTariff -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getDayEntryWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommodityTariff params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCommodityTariff getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCommodityTariffClusterGetDayEntryParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeTariffInfoWithParams:@
readAttributeTariffInfoWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffInfoWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeTariffInfoWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffUnitWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeTariffUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStartDateWithParams:@
readAttributeStartDateWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeStartDateWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeStartDateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDayEntriesWithParams:@
readAttributeDayEntriesWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDayEntriesWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeDayEntriesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDayPatternsWithParams:@
readAttributeDayPatternsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDayPatternsWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeDayPatternsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCalendarPeriodsWithParams:@
readAttributeCalendarPeriodsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCalendarPeriodsWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeCalendarPeriodsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeIndividualDaysWithParams:@
readAttributeIndividualDaysWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeIndividualDaysWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeIndividualDaysWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentDayWithParams:@
readAttributeCurrentDayWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentDayWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeCurrentDayWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextDayWithParams:@
readAttributeNextDayWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextDayWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeNextDayWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentDayEntryWithParams:@
readAttributeCurrentDayEntryWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentDayEntryWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeCurrentDayEntryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentDayEntryDateWithParams:@
readAttributeCurrentDayEntryDateWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentDayEntryDateWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeCurrentDayEntryDateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextDayEntryWithParams:@
readAttributeNextDayEntryWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextDayEntryWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeNextDayEntryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextDayEntryDateWithParams:@
readAttributeNextDayEntryDateWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextDayEntryDateWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeNextDayEntryDateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTariffComponentsWithParams:@
readAttributeTariffComponentsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffComponentsWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeTariffComponentsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTariffPeriodsWithParams:@
readAttributeTariffPeriodsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeTariffPeriodsWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeTariffPeriodsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentTariffComponentsWithParams:@
readAttributeCurrentTariffComponentsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeCurrentTariffComponentsWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeCurrentTariffComponentsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextTariffComponentsWithParams:@
readAttributeNextTariffComponentsWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeNextTariffComponentsWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeNextTariffComponentsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDefaultRandomizationOffsetWithParams:@
readAttributeDefaultRandomizationOffsetWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDefaultRandomizationOffsetWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeDefaultRandomizationOffsetWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDefaultRandomizationTypeWithParams:@
readAttributeDefaultRandomizationTypeWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeDefaultRandomizationTypeWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeDefaultRandomizationTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRReadParams params) => mtrClusterCommodityTariff -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommodityTariff params =
  sendMessage mtrClusterCommodityTariff readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCommodityTariff mtrClusterCommodityTariff => mtrClusterCommodityTariff -> IO (Id MTRClusterCommodityTariff)
init_ mtrClusterCommodityTariff =
  sendOwnedMessage mtrClusterCommodityTariff initSelector

-- | @+ new@
new :: IO (Id MTRClusterCommodityTariff)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommodityTariff"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommodityTariff mtrClusterCommodityTariff, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommodityTariff -> device -> endpointID -> queue -> IO (Id MTRClusterCommodityTariff)
initWithDevice_endpointID_queue mtrClusterCommodityTariff device endpointID queue =
  sendOwnedMessage mtrClusterCommodityTariff initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:@
getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCommodityTariffClusterGetTariffComponentParams, Id NSArray, Id NSNumber, Ptr ()] ()
getTariffComponentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getTariffComponentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getDayEntryWithParams:expectedValues:expectedValueInterval:completion:@
getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCommodityTariffClusterGetDayEntryParams, Id NSArray, Id NSNumber, Ptr ()] ()
getDayEntryWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getDayEntryWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTariffInfoWithParams:@
readAttributeTariffInfoWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTariffInfoWithParamsSelector = mkSelector "readAttributeTariffInfoWithParams:"

-- | @Selector@ for @readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTariffUnitWithParamsSelector = mkSelector "readAttributeTariffUnitWithParams:"

-- | @Selector@ for @readAttributeStartDateWithParams:@
readAttributeStartDateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStartDateWithParamsSelector = mkSelector "readAttributeStartDateWithParams:"

-- | @Selector@ for @readAttributeDayEntriesWithParams:@
readAttributeDayEntriesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDayEntriesWithParamsSelector = mkSelector "readAttributeDayEntriesWithParams:"

-- | @Selector@ for @readAttributeDayPatternsWithParams:@
readAttributeDayPatternsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDayPatternsWithParamsSelector = mkSelector "readAttributeDayPatternsWithParams:"

-- | @Selector@ for @readAttributeCalendarPeriodsWithParams:@
readAttributeCalendarPeriodsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCalendarPeriodsWithParamsSelector = mkSelector "readAttributeCalendarPeriodsWithParams:"

-- | @Selector@ for @readAttributeIndividualDaysWithParams:@
readAttributeIndividualDaysWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIndividualDaysWithParamsSelector = mkSelector "readAttributeIndividualDaysWithParams:"

-- | @Selector@ for @readAttributeCurrentDayWithParams:@
readAttributeCurrentDayWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentDayWithParamsSelector = mkSelector "readAttributeCurrentDayWithParams:"

-- | @Selector@ for @readAttributeNextDayWithParams:@
readAttributeNextDayWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextDayWithParamsSelector = mkSelector "readAttributeNextDayWithParams:"

-- | @Selector@ for @readAttributeCurrentDayEntryWithParams:@
readAttributeCurrentDayEntryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentDayEntryWithParamsSelector = mkSelector "readAttributeCurrentDayEntryWithParams:"

-- | @Selector@ for @readAttributeCurrentDayEntryDateWithParams:@
readAttributeCurrentDayEntryDateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentDayEntryDateWithParamsSelector = mkSelector "readAttributeCurrentDayEntryDateWithParams:"

-- | @Selector@ for @readAttributeNextDayEntryWithParams:@
readAttributeNextDayEntryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextDayEntryWithParamsSelector = mkSelector "readAttributeNextDayEntryWithParams:"

-- | @Selector@ for @readAttributeNextDayEntryDateWithParams:@
readAttributeNextDayEntryDateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextDayEntryDateWithParamsSelector = mkSelector "readAttributeNextDayEntryDateWithParams:"

-- | @Selector@ for @readAttributeTariffComponentsWithParams:@
readAttributeTariffComponentsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTariffComponentsWithParamsSelector = mkSelector "readAttributeTariffComponentsWithParams:"

-- | @Selector@ for @readAttributeTariffPeriodsWithParams:@
readAttributeTariffPeriodsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTariffPeriodsWithParamsSelector = mkSelector "readAttributeTariffPeriodsWithParams:"

-- | @Selector@ for @readAttributeCurrentTariffComponentsWithParams:@
readAttributeCurrentTariffComponentsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentTariffComponentsWithParamsSelector = mkSelector "readAttributeCurrentTariffComponentsWithParams:"

-- | @Selector@ for @readAttributeNextTariffComponentsWithParams:@
readAttributeNextTariffComponentsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextTariffComponentsWithParamsSelector = mkSelector "readAttributeNextTariffComponentsWithParams:"

-- | @Selector@ for @readAttributeDefaultRandomizationOffsetWithParams:@
readAttributeDefaultRandomizationOffsetWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultRandomizationOffsetWithParamsSelector = mkSelector "readAttributeDefaultRandomizationOffsetWithParams:"

-- | @Selector@ for @readAttributeDefaultRandomizationTypeWithParams:@
readAttributeDefaultRandomizationTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultRandomizationTypeWithParamsSelector = mkSelector "readAttributeDefaultRandomizationTypeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterCommodityTariff)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCommodityTariff)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCommodityTariff)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

