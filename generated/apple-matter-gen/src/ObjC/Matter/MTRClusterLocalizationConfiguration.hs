{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Localization Configuration    Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing common languages, units of measurements, and numerical formatting      standards. As such, Nodes that visually or audibly convey information need a mechanism by which      they can be configured to use a user’s preferred language, units, etc
--
-- Generated bindings for @MTRClusterLocalizationConfiguration@.
module ObjC.Matter.MTRClusterLocalizationConfiguration
  ( MTRClusterLocalizationConfiguration
  , IsMTRClusterLocalizationConfiguration(..)
  , readAttributeActiveLocaleWithParams
  , writeAttributeActiveLocaleWithValue_expectedValueInterval
  , writeAttributeActiveLocaleWithValue_expectedValueInterval_params
  , readAttributeSupportedLocalesWithParams
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
  , readAttributeActiveLocaleWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedLocalesWithParamsSelector
  , writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector
  , writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeActiveLocaleWithParams:@
readAttributeActiveLocaleWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeActiveLocaleWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeActiveLocaleWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeActiveLocaleWithValue:expectedValueInterval:@
writeAttributeActiveLocaleWithValue_expectedValueInterval :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLocalizationConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeActiveLocaleWithValue_expectedValueInterval mtrClusterLocalizationConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLocalizationConfiguration writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeActiveLocaleWithValue:expectedValueInterval:params:@
writeAttributeActiveLocaleWithValue_expectedValueInterval_params :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLocalizationConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeActiveLocaleWithValue_expectedValueInterval_params mtrClusterLocalizationConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLocalizationConfiguration writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSupportedLocalesWithParams:@
readAttributeSupportedLocalesWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeSupportedLocalesWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeSupportedLocalesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRReadParams params) => mtrClusterLocalizationConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLocalizationConfiguration params =
  sendMessage mtrClusterLocalizationConfiguration readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration => mtrClusterLocalizationConfiguration -> IO (Id MTRClusterLocalizationConfiguration)
init_ mtrClusterLocalizationConfiguration =
  sendOwnedMessage mtrClusterLocalizationConfiguration initSelector

-- | @+ new@
new :: IO (Id MTRClusterLocalizationConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLocalizationConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterLocalizationConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterLocalizationConfiguration)
initWithDevice_endpoint_queue mtrClusterLocalizationConfiguration device endpoint queue =
  sendOwnedMessage mtrClusterLocalizationConfiguration initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLocalizationConfiguration mtrClusterLocalizationConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLocalizationConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterLocalizationConfiguration)
initWithDevice_endpointID_queue mtrClusterLocalizationConfiguration device endpointID queue =
  sendOwnedMessage mtrClusterLocalizationConfiguration initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeActiveLocaleWithParams:@
readAttributeActiveLocaleWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveLocaleWithParamsSelector = mkSelector "readAttributeActiveLocaleWithParams:"

-- | @Selector@ for @writeAttributeActiveLocaleWithValue:expectedValueInterval:@
writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeActiveLocaleWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeActiveLocaleWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeActiveLocaleWithValue:expectedValueInterval:params:@
writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeActiveLocaleWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeActiveLocaleWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedLocalesWithParams:@
readAttributeSupportedLocalesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedLocalesWithParamsSelector = mkSelector "readAttributeSupportedLocalesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterLocalizationConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterLocalizationConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterLocalizationConfiguration)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterLocalizationConfiguration)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

