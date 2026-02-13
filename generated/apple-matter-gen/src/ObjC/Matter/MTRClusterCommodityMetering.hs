{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commodity Metering    The Commodity Metering Cluster provides the mechanism for communicating commodity consumption information within a premises.
--
-- Generated bindings for @MTRClusterCommodityMetering@.
module ObjC.Matter.MTRClusterCommodityMetering
  ( MTRClusterCommodityMetering
  , IsMTRClusterCommodityMetering(..)
  , readAttributeMeteredQuantityWithParams
  , readAttributeMeteredQuantityTimestampWithParams
  , readAttributeTariffUnitWithParams
  , readAttributeMaximumMeteredQuantitiesWithParams
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
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaximumMeteredQuantitiesWithParamsSelector
  , readAttributeMeteredQuantityTimestampWithParamsSelector
  , readAttributeMeteredQuantityWithParamsSelector
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

-- | @- readAttributeMeteredQuantityWithParams:@
readAttributeMeteredQuantityWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeMeteredQuantityWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeMeteredQuantityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeteredQuantityTimestampWithParams:@
readAttributeMeteredQuantityTimestampWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeMeteredQuantityTimestampWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeMeteredQuantityTimestampWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeTariffUnitWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeTariffUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaximumMeteredQuantitiesWithParams:@
readAttributeMaximumMeteredQuantitiesWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeMaximumMeteredQuantitiesWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeMaximumMeteredQuantitiesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRReadParams params) => mtrClusterCommodityMetering -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommodityMetering params =
  sendMessage mtrClusterCommodityMetering readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCommodityMetering mtrClusterCommodityMetering => mtrClusterCommodityMetering -> IO (Id MTRClusterCommodityMetering)
init_ mtrClusterCommodityMetering =
  sendOwnedMessage mtrClusterCommodityMetering initSelector

-- | @+ new@
new :: IO (Id MTRClusterCommodityMetering)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommodityMetering"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommodityMetering mtrClusterCommodityMetering, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommodityMetering -> device -> endpointID -> queue -> IO (Id MTRClusterCommodityMetering)
initWithDevice_endpointID_queue mtrClusterCommodityMetering device endpointID queue =
  sendOwnedMessage mtrClusterCommodityMetering initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeteredQuantityWithParams:@
readAttributeMeteredQuantityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeteredQuantityWithParamsSelector = mkSelector "readAttributeMeteredQuantityWithParams:"

-- | @Selector@ for @readAttributeMeteredQuantityTimestampWithParams:@
readAttributeMeteredQuantityTimestampWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeteredQuantityTimestampWithParamsSelector = mkSelector "readAttributeMeteredQuantityTimestampWithParams:"

-- | @Selector@ for @readAttributeTariffUnitWithParams:@
readAttributeTariffUnitWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTariffUnitWithParamsSelector = mkSelector "readAttributeTariffUnitWithParams:"

-- | @Selector@ for @readAttributeMaximumMeteredQuantitiesWithParams:@
readAttributeMaximumMeteredQuantitiesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaximumMeteredQuantitiesWithParamsSelector = mkSelector "readAttributeMaximumMeteredQuantitiesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterCommodityMetering)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCommodityMetering)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCommodityMetering)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

