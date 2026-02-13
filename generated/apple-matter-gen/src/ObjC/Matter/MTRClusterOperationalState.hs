{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Operational State    This cluster supports remotely monitoring and, where supported, changing the operational state of any device where a state machine is a part of the operation.
--
-- Generated bindings for @MTRClusterOperationalState@.
module ObjC.Matter.MTRClusterOperationalState
  ( MTRClusterOperationalState
  , IsMTRClusterOperationalState(..)
  , pauseWithParams_expectedValues_expectedValueInterval_completion
  , pauseWithExpectedValues_expectedValueInterval_completion
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , startWithParams_expectedValues_expectedValueInterval_completion
  , startWithExpectedValues_expectedValueInterval_completion
  , resumeWithParams_expectedValues_expectedValueInterval_completion
  , resumeWithExpectedValues_expectedValueInterval_completion
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
  , pauseWithExpectedValues_expectedValueInterval_completionSelector
  , pauseWithParams_expectedValues_expectedValueInterval_completionSelector
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
  , resumeWithExpectedValues_expectedValueInterval_completionSelector
  , resumeWithParams_expectedValues_expectedValueInterval_completionSelector
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

-- | @- pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState pauseWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalStateClusterPauseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState pauseWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState stopWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalStateClusterStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState stopWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterStartParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState startWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalStateClusterStartParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState startWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTROperationalStateClusterResumeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithParams_expectedValues_expectedValueInterval_completion mtrClusterOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState resumeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROperationalStateClusterResumeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithExpectedValues_expectedValueInterval_completion mtrClusterOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterOperationalState resumeWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributePhaseListWithParams:@
readAttributePhaseListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributePhaseListWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributePhaseListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeCurrentPhaseWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeCurrentPhaseWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeCountdownTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateListWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeOperationalStateListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeOperationalStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalErrorWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeOperationalErrorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRReadParams params) => mtrClusterOperationalState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOperationalState params =
  sendMessage mtrClusterOperationalState readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOperationalState mtrClusterOperationalState => mtrClusterOperationalState -> IO (Id MTRClusterOperationalState)
init_ mtrClusterOperationalState =
  sendOwnedMessage mtrClusterOperationalState initSelector

-- | @+ new@
new :: IO (Id MTRClusterOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOperationalState"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOperationalState mtrClusterOperationalState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOperationalState -> device -> endpointID -> queue -> IO (Id MTRClusterOperationalState)
initWithDevice_endpointID_queue mtrClusterOperationalState device endpointID queue =
  sendOwnedMessage mtrClusterOperationalState initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalStateClusterPauseParams, Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalStateClusterStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startWithParams:expectedValues:expectedValueInterval:completion:@
startWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalStateClusterStartParams, Id NSArray, Id NSNumber, Ptr ()] ()
startWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startWithExpectedValues:expectedValueInterval:completion:@
startWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
startWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROperationalStateClusterResumeParams, Id NSArray, Id NSNumber, Ptr ()] ()
resumeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resumeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithExpectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterOperationalState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOperationalState)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOperationalState)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

