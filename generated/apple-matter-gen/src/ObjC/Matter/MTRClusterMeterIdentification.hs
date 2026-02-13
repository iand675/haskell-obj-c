{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Meter Identification    This Meter Identification Cluster provides attributes for determining advanced information about utility metering device.
--
-- Generated bindings for @MTRClusterMeterIdentification@.
module ObjC.Matter.MTRClusterMeterIdentification
  ( MTRClusterMeterIdentification
  , IsMTRClusterMeterIdentification(..)
  , readAttributeMeterTypeWithParams
  , readAttributePointOfDeliveryWithParams
  , readAttributeMeterSerialNumberWithParams
  , readAttributeProtocolVersionWithParams
  , readAttributePowerThresholdWithParams
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
  , readAttributeMeterSerialNumberWithParamsSelector
  , readAttributeMeterTypeWithParamsSelector
  , readAttributePointOfDeliveryWithParamsSelector
  , readAttributePowerThresholdWithParamsSelector
  , readAttributeProtocolVersionWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeMeterTypeWithParams:@
readAttributeMeterTypeWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeMeterTypeWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeMeterTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePointOfDeliveryWithParams:@
readAttributePointOfDeliveryWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributePointOfDeliveryWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributePointOfDeliveryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeterSerialNumberWithParams:@
readAttributeMeterSerialNumberWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeMeterSerialNumberWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeMeterSerialNumberWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProtocolVersionWithParams:@
readAttributeProtocolVersionWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeProtocolVersionWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeProtocolVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerThresholdWithParams:@
readAttributePowerThresholdWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributePowerThresholdWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributePowerThresholdWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRReadParams params) => mtrClusterMeterIdentification -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMeterIdentification params =
  sendMessage mtrClusterMeterIdentification readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterMeterIdentification mtrClusterMeterIdentification => mtrClusterMeterIdentification -> IO (Id MTRClusterMeterIdentification)
init_ mtrClusterMeterIdentification =
  sendOwnedMessage mtrClusterMeterIdentification initSelector

-- | @+ new@
new :: IO (Id MTRClusterMeterIdentification)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMeterIdentification"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMeterIdentification mtrClusterMeterIdentification, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMeterIdentification -> device -> endpointID -> queue -> IO (Id MTRClusterMeterIdentification)
initWithDevice_endpointID_queue mtrClusterMeterIdentification device endpointID queue =
  sendOwnedMessage mtrClusterMeterIdentification initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeMeterTypeWithParams:@
readAttributeMeterTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeterTypeWithParamsSelector = mkSelector "readAttributeMeterTypeWithParams:"

-- | @Selector@ for @readAttributePointOfDeliveryWithParams:@
readAttributePointOfDeliveryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePointOfDeliveryWithParamsSelector = mkSelector "readAttributePointOfDeliveryWithParams:"

-- | @Selector@ for @readAttributeMeterSerialNumberWithParams:@
readAttributeMeterSerialNumberWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeterSerialNumberWithParamsSelector = mkSelector "readAttributeMeterSerialNumberWithParams:"

-- | @Selector@ for @readAttributeProtocolVersionWithParams:@
readAttributeProtocolVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProtocolVersionWithParamsSelector = mkSelector "readAttributeProtocolVersionWithParams:"

-- | @Selector@ for @readAttributePowerThresholdWithParams:@
readAttributePowerThresholdWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerThresholdWithParamsSelector = mkSelector "readAttributePowerThresholdWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterMeterIdentification)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterMeterIdentification)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterMeterIdentification)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

