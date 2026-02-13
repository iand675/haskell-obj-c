{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster RVC Operational State    This cluster supports remotely monitoring and, where supported, changing the operational state of a Robotic Vacuum.
--
-- Generated bindings for @MTRClusterRVCOperationalState@.
module ObjC.Matter.MTRClusterRVCOperationalState
  ( MTRClusterRVCOperationalState
  , IsMTRClusterRVCOperationalState(..)
  , pauseWithParams_expectedValues_expectedValueInterval_completion
  , pauseWithExpectedValues_expectedValueInterval_completion
  , resumeWithParams_expectedValues_expectedValueInterval_completion
  , resumeWithExpectedValues_expectedValueInterval_completion
  , goHomeWithParams_expectedValues_expectedValueInterval_completion
  , goHomeWithExpectedValues_expectedValueInterval_completion
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
  , goHomeWithExpectedValues_expectedValueInterval_completionSelector
  , goHomeWithParams_expectedValues_expectedValueInterval_completionSelector
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
pauseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRRVCOperationalStateClusterPauseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCOperationalState pauseWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRRVCOperationalStateClusterPauseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pauseWithExpectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCOperationalState pauseWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRRVCOperationalStateClusterResumeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCOperationalState resumeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRRVCOperationalStateClusterResumeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resumeWithExpectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCOperationalState resumeWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- goHomeWithParams:expectedValues:expectedValueInterval:completion:@
goHomeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRRVCOperationalStateClusterGoHomeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goHomeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCOperationalState goHomeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRRVCOperationalStateClusterGoHomeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- goHomeWithExpectedValues:expectedValueInterval:completion:@
goHomeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterRVCOperationalState -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
goHomeWithExpectedValues_expectedValueInterval_completion mtrClusterRVCOperationalState expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterRVCOperationalState goHomeWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributePhaseListWithParams:@
readAttributePhaseListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributePhaseListWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributePhaseListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPhaseWithParams:@
readAttributeCurrentPhaseWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeCurrentPhaseWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeCurrentPhaseWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeCountdownTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStateListWithParams:@
readAttributeOperationalStateListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateListWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeOperationalStateListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStateWithParams:@
readAttributeOperationalStateWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalStateWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeOperationalStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalErrorWithParams:@
readAttributeOperationalErrorWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeOperationalErrorWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeOperationalErrorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRReadParams params) => mtrClusterRVCOperationalState -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRVCOperationalState params =
  sendMessage mtrClusterRVCOperationalState readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState => mtrClusterRVCOperationalState -> IO (Id MTRClusterRVCOperationalState)
init_ mtrClusterRVCOperationalState =
  sendOwnedMessage mtrClusterRVCOperationalState initSelector

-- | @+ new@
new :: IO (Id MTRClusterRVCOperationalState)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRVCOperationalState"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRVCOperationalState mtrClusterRVCOperationalState, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRVCOperationalState -> device -> endpointID -> queue -> IO (Id MTRClusterRVCOperationalState)
initWithDevice_endpointID_queue mtrClusterRVCOperationalState device endpointID queue =
  sendOwnedMessage mtrClusterRVCOperationalState initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pauseWithParams:expectedValues:expectedValueInterval:completion:@
pauseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRRVCOperationalStateClusterPauseParams, Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pauseWithExpectedValues:expectedValueInterval:completion:@
pauseWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
pauseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pauseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithParams:expectedValues:expectedValueInterval:completion:@
resumeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRRVCOperationalStateClusterResumeParams, Id NSArray, Id NSNumber, Ptr ()] ()
resumeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resumeWithExpectedValues:expectedValueInterval:completion:@
resumeWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resumeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resumeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goHomeWithParams:expectedValues:expectedValueInterval:completion:@
goHomeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRRVCOperationalStateClusterGoHomeParams, Id NSArray, Id NSNumber, Ptr ()] ()
goHomeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goHomeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goHomeWithExpectedValues:expectedValueInterval:completion:@
goHomeWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
goHomeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "goHomeWithExpectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterRVCOperationalState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterRVCOperationalState)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterRVCOperationalState)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

