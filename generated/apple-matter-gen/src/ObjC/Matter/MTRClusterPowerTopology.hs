{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Topology    The Power Topology Cluster provides a mechanism for expressing how power is flowing between endpoints.
--
-- Generated bindings for @MTRClusterPowerTopology@.
module ObjC.Matter.MTRClusterPowerTopology
  ( MTRClusterPowerTopology
  , IsMTRClusterPowerTopology(..)
  , readAttributeAvailableEndpointsWithParams
  , readAttributeActiveEndpointsWithParams
  , readAttributeElectricalCircuitNodesWithParams
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_params
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
  , readAttributeActiveEndpointsWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeAvailableEndpointsWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeElectricalCircuitNodesWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector
  , writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeAvailableEndpointsWithParams:@
readAttributeAvailableEndpointsWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeAvailableEndpointsWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeAvailableEndpointsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveEndpointsWithParams:@
readAttributeActiveEndpointsWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeActiveEndpointsWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeActiveEndpointsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeElectricalCircuitNodesWithParams:@
readAttributeElectricalCircuitNodesWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeElectricalCircuitNodesWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeElectricalCircuitNodesWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterPowerTopology -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval mtrClusterPowerTopology dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterPowerTopology writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_params :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterPowerTopology -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_params mtrClusterPowerTopology dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterPowerTopology writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRReadParams params) => mtrClusterPowerTopology -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPowerTopology params =
  sendMessage mtrClusterPowerTopology readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPowerTopology mtrClusterPowerTopology => mtrClusterPowerTopology -> IO (Id MTRClusterPowerTopology)
init_ mtrClusterPowerTopology =
  sendOwnedMessage mtrClusterPowerTopology initSelector

-- | @+ new@
new :: IO (Id MTRClusterPowerTopology)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPowerTopology"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPowerTopology mtrClusterPowerTopology, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPowerTopology -> device -> endpointID -> queue -> IO (Id MTRClusterPowerTopology)
initWithDevice_endpointID_queue mtrClusterPowerTopology device endpointID queue =
  sendOwnedMessage mtrClusterPowerTopology initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeAvailableEndpointsWithParams:@
readAttributeAvailableEndpointsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAvailableEndpointsWithParamsSelector = mkSelector "readAttributeAvailableEndpointsWithParams:"

-- | @Selector@ for @readAttributeActiveEndpointsWithParams:@
readAttributeActiveEndpointsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveEndpointsWithParamsSelector = mkSelector "readAttributeActiveEndpointsWithParams:"

-- | @Selector@ for @readAttributeElectricalCircuitNodesWithParams:@
readAttributeElectricalCircuitNodesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeElectricalCircuitNodesWithParamsSelector = mkSelector "readAttributeElectricalCircuitNodesWithParams:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeElectricalCircuitNodesWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:@
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeElectricalCircuitNodesWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeElectricalCircuitNodesWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterPowerTopology)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPowerTopology)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPowerTopology)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

