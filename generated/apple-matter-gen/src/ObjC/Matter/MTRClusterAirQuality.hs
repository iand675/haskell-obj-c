{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Air Quality    Attributes for reporting air quality classification
--
-- Generated bindings for @MTRClusterAirQuality@.
module ObjC.Matter.MTRClusterAirQuality
  ( MTRClusterAirQuality
  , IsMTRClusterAirQuality(..)
  , readAttributeAirQualityWithParams
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
  , readAttributeAirQualityWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeAirQualityWithParams:@
readAttributeAirQualityWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeAirQualityWithParams mtrClusterAirQuality params =
  sendMessage mtrClusterAirQuality readAttributeAirQualityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAirQuality params =
  sendMessage mtrClusterAirQuality readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAirQuality params =
  sendMessage mtrClusterAirQuality readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAirQuality params =
  sendMessage mtrClusterAirQuality readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAirQuality params =
  sendMessage mtrClusterAirQuality readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRReadParams params) => mtrClusterAirQuality -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAirQuality params =
  sendMessage mtrClusterAirQuality readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterAirQuality mtrClusterAirQuality => mtrClusterAirQuality -> IO (Id MTRClusterAirQuality)
init_ mtrClusterAirQuality =
  sendOwnedMessage mtrClusterAirQuality initSelector

-- | @+ new@
new :: IO (Id MTRClusterAirQuality)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAirQuality"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAirQuality mtrClusterAirQuality, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAirQuality -> device -> endpointID -> queue -> IO (Id MTRClusterAirQuality)
initWithDevice_endpointID_queue mtrClusterAirQuality device endpointID queue =
  sendOwnedMessage mtrClusterAirQuality initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAirQualityWithParams:@
readAttributeAirQualityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAirQualityWithParamsSelector = mkSelector "readAttributeAirQualityWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterAirQuality)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterAirQuality)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterAirQuality)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

