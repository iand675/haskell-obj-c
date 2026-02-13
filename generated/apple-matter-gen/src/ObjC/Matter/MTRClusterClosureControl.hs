{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Control    This cluster provides an interface for controlling a Closure.
--
-- Generated bindings for @MTRClusterClosureControl@.
module ObjC.Matter.MTRClusterClosureControl
  ( MTRClusterClosureControl
  , IsMTRClusterClosureControl(..)
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , stopWithExpectedValues_expectedValueInterval_completion
  , moveToWithParams_expectedValues_expectedValueInterval_completion
  , moveToWithExpectedValues_expectedValueInterval_completion
  , calibrateWithParams_expectedValues_expectedValueInterval_completion
  , calibrateWithExpectedValues_expectedValueInterval_completion
  , readAttributeCountdownTimeWithParams
  , readAttributeMainStateWithParams
  , readAttributeCurrentErrorListWithParams
  , readAttributeOverallCurrentStateWithParams
  , readAttributeOverallTargetStateWithParams
  , readAttributeLatchControlModesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , calibrateWithExpectedValues_expectedValueInterval_completionSelector
  , calibrateWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , moveToWithExpectedValues_expectedValueInterval_completionSelector
  , moveToWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCountdownTimeWithParamsSelector
  , readAttributeCurrentErrorListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLatchControlModesWithParamsSelector
  , readAttributeMainStateWithParamsSelector
  , readAttributeOverallCurrentStateWithParamsSelector
  , readAttributeOverallTargetStateWithParamsSelector
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
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRClosureControlClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureControl stopWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRClosureControlClusterStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithExpectedValues_expectedValueInterval_completion mtrClusterClosureControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureControl stopWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- moveToWithParams:expectedValues:expectedValueInterval:completion:@
moveToWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRClosureControlClusterMoveToParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureControl moveToWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRClosureControlClusterMoveToParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- moveToWithExpectedValues:expectedValueInterval:completion:@
moveToWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToWithExpectedValues_expectedValueInterval_completion mtrClusterClosureControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureControl moveToWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- calibrateWithParams:expectedValues:expectedValueInterval:completion:@
calibrateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRClosureControlClusterCalibrateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
calibrateWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureControl calibrateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRClosureControlClusterCalibrateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- calibrateWithExpectedValues:expectedValueInterval:completion:@
calibrateWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
calibrateWithExpectedValues_expectedValueInterval_completion mtrClusterClosureControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureControl calibrateWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeCountdownTimeWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeCountdownTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMainStateWithParams:@
readAttributeMainStateWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeMainStateWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeMainStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentErrorListWithParams:@
readAttributeCurrentErrorListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeCurrentErrorListWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeCurrentErrorListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOverallCurrentStateWithParams:@
readAttributeOverallCurrentStateWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeOverallCurrentStateWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeOverallCurrentStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOverallTargetStateWithParams:@
readAttributeOverallTargetStateWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeOverallTargetStateWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeOverallTargetStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeLatchControlModesWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeLatchControlModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRReadParams params) => mtrClusterClosureControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterClosureControl params =
  sendMessage mtrClusterClosureControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterClosureControl mtrClusterClosureControl => mtrClusterClosureControl -> IO (Id MTRClusterClosureControl)
init_ mtrClusterClosureControl =
  sendOwnedMessage mtrClusterClosureControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterClosureControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterClosureControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterClosureControl mtrClusterClosureControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterClosureControl -> device -> endpointID -> queue -> IO (Id MTRClusterClosureControl)
initWithDevice_endpointID_queue mtrClusterClosureControl device endpointID queue =
  sendOwnedMessage mtrClusterClosureControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRClosureControlClusterStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithExpectedValues:expectedValueInterval:completion:@
stopWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToWithParams:expectedValues:expectedValueInterval:completion:@
moveToWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRClosureControlClusterMoveToParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToWithExpectedValues:expectedValueInterval:completion:@
moveToWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
moveToWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "moveToWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @calibrateWithParams:expectedValues:expectedValueInterval:completion:@
calibrateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRClosureControlClusterCalibrateParams, Id NSArray, Id NSNumber, Ptr ()] ()
calibrateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "calibrateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @calibrateWithExpectedValues:expectedValueInterval:completion:@
calibrateWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
calibrateWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "calibrateWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCountdownTimeWithParams:@
readAttributeCountdownTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCountdownTimeWithParamsSelector = mkSelector "readAttributeCountdownTimeWithParams:"

-- | @Selector@ for @readAttributeMainStateWithParams:@
readAttributeMainStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMainStateWithParamsSelector = mkSelector "readAttributeMainStateWithParams:"

-- | @Selector@ for @readAttributeCurrentErrorListWithParams:@
readAttributeCurrentErrorListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentErrorListWithParamsSelector = mkSelector "readAttributeCurrentErrorListWithParams:"

-- | @Selector@ for @readAttributeOverallCurrentStateWithParams:@
readAttributeOverallCurrentStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOverallCurrentStateWithParamsSelector = mkSelector "readAttributeOverallCurrentStateWithParams:"

-- | @Selector@ for @readAttributeOverallTargetStateWithParams:@
readAttributeOverallTargetStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOverallTargetStateWithParamsSelector = mkSelector "readAttributeOverallTargetStateWithParams:"

-- | @Selector@ for @readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLatchControlModesWithParamsSelector = mkSelector "readAttributeLatchControlModesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterClosureControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterClosureControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterClosureControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

