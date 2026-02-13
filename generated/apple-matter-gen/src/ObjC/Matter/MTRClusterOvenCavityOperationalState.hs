{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Oven Cavity Operational State    This cluster supports remotely monitoring and, where supported, changing the operational state of an Oven.
--
-- Generated bindings for @MTRClusterOvenCavityOperationalState@.
module ObjC.Matter.MTRClusterOvenCavityOperationalState
  ( MTRClusterOvenCavityOperationalState
  , IsMTRClusterOvenCavityOperationalState(..)
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , startWithParams_expectedValues_expectedValueInterval_completion
  , startWithExpectedValues_expectedValueInterval_completion
  , readAttributePhaseListWithParams
  , readAttributeCurrentPhaseWithParams
  , readAttributeCountdownTimeWithParams
  , readAttributeOperationalStateListWithParams
  , readAttributeOperationalStateWithParams
  , readAttributeOperationalErrorWithParams
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
  , readAttributeCountdownTimeWithParamsSelector
  , readAttributeCurrentPhaseWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOperationalErrorWithParamsSelector
  , readAttributeOperationalStateListWithParamsSelector
  , readAttributeOperationalStateWithParamsSelector
  , readAttributePhaseListWithParamsSelector
  , startWithExpectedValues_expectedValueInterval_completionSelector
  , startWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithExpectedValues_expectedValueInterval_completionSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTROvenCavityOperationalStateClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOvenCavityOperationalState stopWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROvenCavityOperationalStateClusterStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOvenCavityOperationalState stopWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTROvenCavityOperationalStateClusterStartParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithParams_expectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOvenCavityOperationalState startWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROvenCavityOperationalStateClusterStartParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOvenCavityOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithExpectedValues_expectedValueInterval_completion mtrClusterOvenCavityOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOvenCavityOperationalState startWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributePhaseListWithParams:@
readAttributePhaseListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributePhaseListWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributePhaseListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeCurrentPhaseWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeCurrentPhaseWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeCountdownTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateListWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeOperationalStateListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeOperationalStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalErrorWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeOperationalErrorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRReadParams params) => mtrClusterOvenCavityOperationalState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOvenCavityOperationalState params =
  sendMessage mtrClusterOvenCavityOperationalState readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState => mtrClusterOvenCavityOperationalState -> IO (Id MTRClusterOvenCavityOperationalState)
init_ mtrClusterOvenCavityOperationalState =
  sendOwnedMessage mtrClusterOvenCavityOperationalState initSelector

-- | @+ new@
new :: IO (Id MTRClusterOvenCavityOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOvenCavityOperationalState"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOvenCavityOperationalState mtrClusterOvenCavityOperationalState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOvenCavityOperationalState -> device -> endpointID -> queue -> IO (Id MTRClusterOvenCavityOperationalState)
initWithDevice_endpointID_queue mtrClusterOvenCavityOperationalState device endpointID queue =
  sendOwnedMessage mtrClusterOvenCavityOperationalState initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROvenCavityOperationalStateClusterStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROvenCavityOperationalStateClusterStartParams, Id NSArray, Id NSNumber, Ptr ()] ()
startWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
startWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributePhaseListWithParams:@
readAttributePhaseListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePhaseListWithParamsSelector = mkSelector "readAttributePhaseListWithParams:"

-- | @Selector@ for @readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPhaseWithParamsSelector = mkSelector "readAttributeCurrentPhaseWithParams:"

-- | @Selector@ for @readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCountdownTimeWithParamsSelector = mkSelector "readAttributeCountdownTimeWithParams:"

-- | @Selector@ for @readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperationalStateListWithParamsSelector = mkSelector "readAttributeOperationalStateListWithParams:"

-- | @Selector@ for @readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperationalStateWithParamsSelector = mkSelector "readAttributeOperationalStateWithParams:"

-- | @Selector@ for @readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperationalErrorWithParamsSelector = mkSelector "readAttributeOperationalErrorWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterOvenCavityOperationalState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOvenCavityOperationalState)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOvenCavityOperationalState)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

