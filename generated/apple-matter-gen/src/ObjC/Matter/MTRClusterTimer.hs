{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Timer    This cluster supports creating a simple timer functionality.
--
-- Generated bindings for @MTRClusterTimer@.
module ObjC.Matter.MTRClusterTimer
  ( MTRClusterTimer
  , IsMTRClusterTimer(..)
  , setTimerWithParams_expectedValues_expectedValueInterval_completion
  , resetTimerWithParams_expectedValues_expectedValueInterval_completion
  , resetTimerWithExpectedValues_expectedValueInterval_completion
  , addTimeWithParams_expectedValues_expectedValueInterval_completion
  , reduceTimeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSetTimeWithParams
  , readAttributeTimeRemainingWithParams
  , readAttributeTimerStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSetTimeWithParamsSelector
  , readAttributeTimeRemainingWithParamsSelector
  , readAttributeTimerStateWithParamsSelector
  , reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , resetTimerWithExpectedValues_expectedValueInterval_completionSelector
  , resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTimerWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setTimerWithParams:expectedValues:expectedValueInterval:completion:@
setTimerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterSetTimerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTimerWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimer setTimerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimerClusterSetTimerParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetTimerWithParams:expectedValues:expectedValueInterval:completion:@
resetTimerWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterResetTimerParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetTimerWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimer resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimerClusterResetTimerParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetTimerWithExpectedValues:expectedValueInterval:completion:@
resetTimerWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetTimerWithExpectedValues_expectedValueInterval_completion mtrClusterTimer expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterTimer resetTimerWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- addTimeWithParams:expectedValues:expectedValueInterval:completion:@
addTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterAddTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimer addTimeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimerClusterAddTimeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- reduceTimeWithParams:expectedValues:expectedValueInterval:completion:@
reduceTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimer mtrClusterTimer, IsMTRTimerClusterReduceTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimer -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
reduceTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimer params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimer reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimerClusterReduceTimeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSetTimeWithParams:@
readAttributeSetTimeWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeSetTimeWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeSetTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimeRemainingWithParams:@
readAttributeTimeRemainingWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeTimeRemainingWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeTimeRemainingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimerStateWithParams:@
readAttributeTimerStateWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeTimerStateWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeTimerStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTimer mtrClusterTimer, IsMTRReadParams params) => mtrClusterTimer -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTimer params =
  sendMessage mtrClusterTimer readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTimer mtrClusterTimer => mtrClusterTimer -> IO (Id MTRClusterTimer)
init_ mtrClusterTimer =
  sendOwnedMessage mtrClusterTimer initSelector

-- | @+ new@
new :: IO (Id MTRClusterTimer)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTimer"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTimer mtrClusterTimer, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTimer -> device -> endpointID -> queue -> IO (Id MTRClusterTimer)
initWithDevice_endpointID_queue mtrClusterTimer device endpointID queue =
  sendOwnedMessage mtrClusterTimer initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTimerWithParams:expectedValues:expectedValueInterval:completion:@
setTimerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimerClusterSetTimerParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTimerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTimerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetTimerWithParams:expectedValues:expectedValueInterval:completion:@
resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimerClusterResetTimerParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetTimerWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetTimerWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetTimerWithExpectedValues:expectedValueInterval:completion:@
resetTimerWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetTimerWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetTimerWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addTimeWithParams:expectedValues:expectedValueInterval:completion:@
addTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimerClusterAddTimeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @reduceTimeWithParams:expectedValues:expectedValueInterval:completion:@
reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimerClusterReduceTimeParams, Id NSArray, Id NSNumber, Ptr ()] ()
reduceTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "reduceTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSetTimeWithParams:@
readAttributeSetTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSetTimeWithParamsSelector = mkSelector "readAttributeSetTimeWithParams:"

-- | @Selector@ for @readAttributeTimeRemainingWithParams:@
readAttributeTimeRemainingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimeRemainingWithParamsSelector = mkSelector "readAttributeTimeRemainingWithParams:"

-- | @Selector@ for @readAttributeTimerStateWithParams:@
readAttributeTimerStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimerStateWithParamsSelector = mkSelector "readAttributeTimerStateWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterTimer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTimer)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTimer)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

