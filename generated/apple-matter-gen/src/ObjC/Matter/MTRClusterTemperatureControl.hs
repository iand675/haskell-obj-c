{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Temperature Control    Attributes and commands for configuring the temperature control, and reporting temperature.
--
-- Generated bindings for @MTRClusterTemperatureControl@.
module ObjC.Matter.MTRClusterTemperatureControl
  ( MTRClusterTemperatureControl
  , IsMTRClusterTemperatureControl(..)
  , setTemperatureWithParams_expectedValues_expectedValueInterval_completion
  , setTemperatureWithExpectedValues_expectedValueInterval_completion
  , readAttributeTemperatureSetpointWithParams
  , readAttributeMinTemperatureWithParams
  , readAttributeMaxTemperatureWithParams
  , readAttributeStepWithParams
  , readAttributeSelectedTemperatureLevelWithParams
  , readAttributeSupportedTemperatureLevelsWithParams
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
  , readAttributeMaxTemperatureWithParamsSelector
  , readAttributeMinTemperatureWithParamsSelector
  , readAttributeSelectedTemperatureLevelWithParamsSelector
  , readAttributeStepWithParamsSelector
  , readAttributeSupportedTemperatureLevelsWithParamsSelector
  , readAttributeTemperatureSetpointWithParamsSelector
  , setTemperatureWithExpectedValues_expectedValueInterval_completionSelector
  , setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
setTemperatureWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRTemperatureControlClusterSetTemperatureParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTemperatureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTemperatureWithParams_expectedValues_expectedValueInterval_completion mtrClusterTemperatureControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTemperatureControl setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTemperatureControlClusterSetTemperatureParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTemperatureWithExpectedValues:expectedValueInterval:completion:@
setTemperatureWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTemperatureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setTemperatureWithExpectedValues_expectedValueInterval_completion mtrClusterTemperatureControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterTemperatureControl setTemperatureWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeTemperatureSetpointWithParams:@
readAttributeTemperatureSetpointWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeTemperatureSetpointWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeTemperatureSetpointWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinTemperatureWithParams:@
readAttributeMinTemperatureWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeMinTemperatureWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeMinTemperatureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxTemperatureWithParams:@
readAttributeMaxTemperatureWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeMaxTemperatureWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeMaxTemperatureWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStepWithParams:@
readAttributeStepWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeStepWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeStepWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSelectedTemperatureLevelWithParams:@
readAttributeSelectedTemperatureLevelWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeSelectedTemperatureLevelWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeSelectedTemperatureLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedTemperatureLevelsWithParams:@
readAttributeSupportedTemperatureLevelsWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeSupportedTemperatureLevelsWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeSupportedTemperatureLevelsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRReadParams params) => mtrClusterTemperatureControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTemperatureControl params =
  sendMessage mtrClusterTemperatureControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTemperatureControl mtrClusterTemperatureControl => mtrClusterTemperatureControl -> IO (Id MTRClusterTemperatureControl)
init_ mtrClusterTemperatureControl =
  sendOwnedMessage mtrClusterTemperatureControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterTemperatureControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTemperatureControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTemperatureControl mtrClusterTemperatureControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTemperatureControl -> device -> endpointID -> queue -> IO (Id MTRClusterTemperatureControl)
initWithDevice_endpointID_queue mtrClusterTemperatureControl device endpointID queue =
  sendOwnedMessage mtrClusterTemperatureControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTemperatureWithParams:expectedValues:expectedValueInterval:completion:@
setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTemperatureControlClusterSetTemperatureParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTemperatureWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTemperatureWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTemperatureWithExpectedValues:expectedValueInterval:completion:@
setTemperatureWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
setTemperatureWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setTemperatureWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTemperatureSetpointWithParams:@
readAttributeTemperatureSetpointWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTemperatureSetpointWithParamsSelector = mkSelector "readAttributeTemperatureSetpointWithParams:"

-- | @Selector@ for @readAttributeMinTemperatureWithParams:@
readAttributeMinTemperatureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinTemperatureWithParamsSelector = mkSelector "readAttributeMinTemperatureWithParams:"

-- | @Selector@ for @readAttributeMaxTemperatureWithParams:@
readAttributeMaxTemperatureWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxTemperatureWithParamsSelector = mkSelector "readAttributeMaxTemperatureWithParams:"

-- | @Selector@ for @readAttributeStepWithParams:@
readAttributeStepWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStepWithParamsSelector = mkSelector "readAttributeStepWithParams:"

-- | @Selector@ for @readAttributeSelectedTemperatureLevelWithParams:@
readAttributeSelectedTemperatureLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSelectedTemperatureLevelWithParamsSelector = mkSelector "readAttributeSelectedTemperatureLevelWithParams:"

-- | @Selector@ for @readAttributeSupportedTemperatureLevelsWithParams:@
readAttributeSupportedTemperatureLevelsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedTemperatureLevelsWithParamsSelector = mkSelector "readAttributeSupportedTemperatureLevelsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterTemperatureControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTemperatureControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTemperatureControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

