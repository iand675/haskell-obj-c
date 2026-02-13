{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Microwave Oven Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterMicrowaveOvenMode@.
module ObjC.Matter.MTRClusterMicrowaveOvenMode
  ( MTRClusterMicrowaveOvenMode
  , IsMTRClusterMicrowaveOvenMode(..)
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
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
  , readAttributeCurrentModeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedModesWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRReadParams params) => mtrClusterMicrowaveOvenMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMicrowaveOvenMode params =
  sendMessage mtrClusterMicrowaveOvenMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode => mtrClusterMicrowaveOvenMode -> IO (Id MTRClusterMicrowaveOvenMode)
init_ mtrClusterMicrowaveOvenMode =
  sendOwnedMessage mtrClusterMicrowaveOvenMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterMicrowaveOvenMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMicrowaveOvenMode"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMicrowaveOvenMode mtrClusterMicrowaveOvenMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMicrowaveOvenMode -> device -> endpointID -> queue -> IO (Id MTRClusterMicrowaveOvenMode)
initWithDevice_endpointID_queue mtrClusterMicrowaveOvenMode device endpointID queue =
  sendOwnedMessage mtrClusterMicrowaveOvenMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterMicrowaveOvenMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterMicrowaveOvenMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterMicrowaveOvenMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

