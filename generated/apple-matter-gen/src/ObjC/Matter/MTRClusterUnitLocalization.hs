{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Unit Localization    Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing preferences for the units in which values are conveyed in communication to a      user. As such, Nodes that visually or audibly convey measurable values to the user need a      mechanism by which they can be configured to use a user’s preferred unit.
--
-- Generated bindings for @MTRClusterUnitLocalization@.
module ObjC.Matter.MTRClusterUnitLocalization
  ( MTRClusterUnitLocalization
  , IsMTRClusterUnitLocalization(..)
  , readAttributeTemperatureUnitWithParams
  , writeAttributeTemperatureUnitWithValue_expectedValueInterval
  , writeAttributeTemperatureUnitWithValue_expectedValueInterval_params
  , readAttributeSupportedTemperatureUnitsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedTemperatureUnitsWithParamsSelector
  , readAttributeTemperatureUnitWithParamsSelector
  , writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector
  , writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeTemperatureUnitWithParams:@
readAttributeTemperatureUnitWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeTemperatureUnitWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeTemperatureUnitWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeTemperatureUnitWithValue:expectedValueInterval:@
writeAttributeTemperatureUnitWithValue_expectedValueInterval :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterUnitLocalization -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeTemperatureUnitWithValue_expectedValueInterval mtrClusterUnitLocalization dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterUnitLocalization writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:@
writeAttributeTemperatureUnitWithValue_expectedValueInterval_params :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterUnitLocalization -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeTemperatureUnitWithValue_expectedValueInterval_params mtrClusterUnitLocalization dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterUnitLocalization writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSupportedTemperatureUnitsWithParams:@
readAttributeSupportedTemperatureUnitsWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeSupportedTemperatureUnitsWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeSupportedTemperatureUnitsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRReadParams params) => mtrClusterUnitLocalization -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterUnitLocalization params =
  sendMessage mtrClusterUnitLocalization readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterUnitLocalization mtrClusterUnitLocalization => mtrClusterUnitLocalization -> IO (Id MTRClusterUnitLocalization)
init_ mtrClusterUnitLocalization =
  sendOwnedMessage mtrClusterUnitLocalization initSelector

-- | @+ new@
new :: IO (Id MTRClusterUnitLocalization)
new  =
  do
    cls' <- getRequiredClass "MTRClusterUnitLocalization"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRDevice device, IsNSObject queue) => mtrClusterUnitLocalization -> device -> CUShort -> queue -> IO (Id MTRClusterUnitLocalization)
initWithDevice_endpoint_queue mtrClusterUnitLocalization device endpoint queue =
  sendOwnedMessage mtrClusterUnitLocalization initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterUnitLocalization mtrClusterUnitLocalization, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterUnitLocalization -> device -> endpointID -> queue -> IO (Id MTRClusterUnitLocalization)
initWithDevice_endpointID_queue mtrClusterUnitLocalization device endpointID queue =
  sendOwnedMessage mtrClusterUnitLocalization initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeTemperatureUnitWithParams:@
readAttributeTemperatureUnitWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTemperatureUnitWithParamsSelector = mkSelector "readAttributeTemperatureUnitWithParams:"

-- | @Selector@ for @writeAttributeTemperatureUnitWithValue:expectedValueInterval:@
writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeTemperatureUnitWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeTemperatureUnitWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:@
writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeTemperatureUnitWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeTemperatureUnitWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedTemperatureUnitsWithParams:@
readAttributeSupportedTemperatureUnitsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedTemperatureUnitsWithParamsSelector = mkSelector "readAttributeSupportedTemperatureUnitsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterUnitLocalization)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterUnitLocalization)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterUnitLocalization)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterUnitLocalization)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

