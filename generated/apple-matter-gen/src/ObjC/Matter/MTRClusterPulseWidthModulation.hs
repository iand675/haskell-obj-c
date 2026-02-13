{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Pulse Width Modulation    Cluster to control pulse width modulation
--
-- Generated bindings for @MTRClusterPulseWidthModulation@.
module ObjC.Matter.MTRClusterPulseWidthModulation
  ( MTRClusterPulseWidthModulation
  , IsMTRClusterPulseWidthModulation(..)
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


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPulseWidthModulation params =
  sendMessage mtrClusterPulseWidthModulation readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPulseWidthModulation params =
  sendMessage mtrClusterPulseWidthModulation readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPulseWidthModulation params =
  sendMessage mtrClusterPulseWidthModulation readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPulseWidthModulation params =
  sendMessage mtrClusterPulseWidthModulation readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRReadParams params) => mtrClusterPulseWidthModulation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPulseWidthModulation params =
  sendMessage mtrClusterPulseWidthModulation readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation => mtrClusterPulseWidthModulation -> IO (Id MTRClusterPulseWidthModulation)
init_ mtrClusterPulseWidthModulation =
  sendOwnedMessage mtrClusterPulseWidthModulation initSelector

-- | @+ new@
new :: IO (Id MTRClusterPulseWidthModulation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPulseWidthModulation"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPulseWidthModulation mtrClusterPulseWidthModulation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPulseWidthModulation -> device -> endpointID -> queue -> IO (Id MTRClusterPulseWidthModulation)
initWithDevice_endpointID_queue mtrClusterPulseWidthModulation device endpointID queue =
  sendOwnedMessage mtrClusterPulseWidthModulation initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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
initSelector :: Selector '[] (Id MTRClusterPulseWidthModulation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPulseWidthModulation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPulseWidthModulation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

