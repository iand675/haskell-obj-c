{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Grid Conditions    The Electrical Grid Conditions Cluster provides the mechanism for communicating electricity grid carbon intensity to devices within the premises in units of Grams of CO2e per kWh.
--
-- Generated bindings for @MTRClusterElectricalGridConditions@.
module ObjC.Matter.MTRClusterElectricalGridConditions
  ( MTRClusterElectricalGridConditions
  , IsMTRClusterElectricalGridConditions(..)
  , readAttributeLocalGenerationAvailableWithParams
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_params
  , readAttributeCurrentConditionsWithParams
  , readAttributeForecastConditionsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentConditionsWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeForecastConditionsWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLocalGenerationAvailableWithParamsSelector
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector
  , writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeLocalGenerationAvailableWithParams:@
readAttributeLocalGenerationAvailableWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeLocalGenerationAvailableWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeLocalGenerationAvailableWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalGridConditions -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval mtrClusterElectricalGridConditions dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalGridConditions writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalGridConditions -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_params mtrClusterElectricalGridConditions dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalGridConditions writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeCurrentConditionsWithParams:@
readAttributeCurrentConditionsWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeCurrentConditionsWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeCurrentConditionsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeForecastConditionsWithParams:@
readAttributeForecastConditionsWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeForecastConditionsWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeForecastConditionsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRReadParams params) => mtrClusterElectricalGridConditions -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalGridConditions params =
  sendMessage mtrClusterElectricalGridConditions readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions => mtrClusterElectricalGridConditions -> IO (Id MTRClusterElectricalGridConditions)
init_ mtrClusterElectricalGridConditions =
  sendOwnedMessage mtrClusterElectricalGridConditions initSelector

-- | @+ new@
new :: IO (Id MTRClusterElectricalGridConditions)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalGridConditions"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalGridConditions mtrClusterElectricalGridConditions, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalGridConditions -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalGridConditions)
initWithDevice_endpointID_queue mtrClusterElectricalGridConditions device endpointID queue =
  sendOwnedMessage mtrClusterElectricalGridConditions initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeLocalGenerationAvailableWithParams:@
readAttributeLocalGenerationAvailableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLocalGenerationAvailableWithParamsSelector = mkSelector "readAttributeLocalGenerationAvailableWithParams:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLocalGenerationAvailableWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:@
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLocalGenerationAvailableWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalGenerationAvailableWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeCurrentConditionsWithParams:@
readAttributeCurrentConditionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentConditionsWithParamsSelector = mkSelector "readAttributeCurrentConditionsWithParams:"

-- | @Selector@ for @readAttributeForecastConditionsWithParams:@
readAttributeForecastConditionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeForecastConditionsWithParamsSelector = mkSelector "readAttributeForecastConditionsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterElectricalGridConditions)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterElectricalGridConditions)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterElectricalGridConditions)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

