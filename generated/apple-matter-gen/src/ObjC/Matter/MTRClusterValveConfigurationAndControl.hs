{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Valve Configuration and Control    This cluster is used to configure a valve.
--
-- Generated bindings for @MTRClusterValveConfigurationAndControl@.
module ObjC.Matter.MTRClusterValveConfigurationAndControl
  ( MTRClusterValveConfigurationAndControl
  , IsMTRClusterValveConfigurationAndControl(..)
  , openWithParams_expectedValues_expectedValueInterval_completion
  , openWithExpectedValues_expectedValueInterval_completion
  , closeWithParams_expectedValues_expectedValueInterval_completion
  , closeWithExpectedValues_expectedValueInterval_completion
  , readAttributeOpenDurationWithParams
  , readAttributeDefaultOpenDurationWithParams
  , writeAttributeDefaultOpenDurationWithValue_expectedValueInterval
  , writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_params
  , readAttributeAutoCloseTimeWithParams
  , readAttributeRemainingDurationWithParams
  , readAttributeCurrentStateWithParams
  , readAttributeTargetStateWithParams
  , readAttributeCurrentLevelWithParams
  , readAttributeTargetLevelWithParams
  , readAttributeDefaultOpenLevelWithParams
  , writeAttributeDefaultOpenLevelWithValue_expectedValueInterval
  , writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_params
  , readAttributeValveFaultWithParams
  , readAttributeLevelStepWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , closeWithExpectedValues_expectedValueInterval_completionSelector
  , closeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , openWithExpectedValues_expectedValueInterval_completionSelector
  , openWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeAutoCloseTimeWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentLevelWithParamsSelector
  , readAttributeCurrentStateWithParamsSelector
  , readAttributeDefaultOpenDurationWithParamsSelector
  , readAttributeDefaultOpenLevelWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLevelStepWithParamsSelector
  , readAttributeOpenDurationWithParamsSelector
  , readAttributeRemainingDurationWithParamsSelector
  , readAttributeTargetLevelWithParamsSelector
  , readAttributeTargetStateWithParamsSelector
  , readAttributeValveFaultWithParamsSelector
  , writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector
  , writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- openWithParams:expectedValues:expectedValueInterval:completion:@
openWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterOpenParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openWithParams_expectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterValveConfigurationAndControl openWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRValveConfigurationAndControlClusterOpenParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- openWithExpectedValues:expectedValueInterval:completion:@
openWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
openWithExpectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterValveConfigurationAndControl openWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- closeWithParams:expectedValues:expectedValueInterval:completion:@
closeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRValveConfigurationAndControlClusterCloseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
closeWithParams_expectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterValveConfigurationAndControl closeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRValveConfigurationAndControlClusterCloseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- closeWithExpectedValues:expectedValueInterval:completion:@
closeWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
closeWithExpectedValues_expectedValueInterval_completion mtrClusterValveConfigurationAndControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterValveConfigurationAndControl closeWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeOpenDurationWithParams:@
readAttributeOpenDurationWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeOpenDurationWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeOpenDurationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDefaultOpenDurationWithParams:@
readAttributeDefaultOpenDurationWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeDefaultOpenDurationWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeDefaultOpenDurationWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:@
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval mtrClusterValveConfigurationAndControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterValveConfigurationAndControl writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_params :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_params mtrClusterValveConfigurationAndControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterValveConfigurationAndControl writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeAutoCloseTimeWithParams:@
readAttributeAutoCloseTimeWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAutoCloseTimeWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeAutoCloseTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRemainingDurationWithParams:@
readAttributeRemainingDurationWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeRemainingDurationWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeRemainingDurationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeCurrentStateWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeCurrentStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeTargetStateWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeTargetStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeCurrentLevelWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeCurrentLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTargetLevelWithParams:@
readAttributeTargetLevelWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeTargetLevelWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeTargetLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDefaultOpenLevelWithParams:@
readAttributeDefaultOpenLevelWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeDefaultOpenLevelWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeDefaultOpenLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:@
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval mtrClusterValveConfigurationAndControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterValveConfigurationAndControl writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_params :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterValveConfigurationAndControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_params mtrClusterValveConfigurationAndControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterValveConfigurationAndControl writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeValveFaultWithParams:@
readAttributeValveFaultWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeValveFaultWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeValveFaultWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLevelStepWithParams:@
readAttributeLevelStepWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeLevelStepWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeLevelStepWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRReadParams params) => mtrClusterValveConfigurationAndControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterValveConfigurationAndControl params =
  sendMessage mtrClusterValveConfigurationAndControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl => mtrClusterValveConfigurationAndControl -> IO (Id MTRClusterValveConfigurationAndControl)
init_ mtrClusterValveConfigurationAndControl =
  sendOwnedMessage mtrClusterValveConfigurationAndControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterValveConfigurationAndControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterValveConfigurationAndControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterValveConfigurationAndControl mtrClusterValveConfigurationAndControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterValveConfigurationAndControl -> device -> endpointID -> queue -> IO (Id MTRClusterValveConfigurationAndControl)
initWithDevice_endpointID_queue mtrClusterValveConfigurationAndControl device endpointID queue =
  sendOwnedMessage mtrClusterValveConfigurationAndControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @openWithParams:expectedValues:expectedValueInterval:completion:@
openWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRValveConfigurationAndControlClusterOpenParams, Id NSArray, Id NSNumber, Ptr ()] ()
openWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "openWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @openWithExpectedValues:expectedValueInterval:completion:@
openWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
openWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "openWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @closeWithParams:expectedValues:expectedValueInterval:completion:@
closeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRValveConfigurationAndControlClusterCloseParams, Id NSArray, Id NSNumber, Ptr ()] ()
closeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "closeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @closeWithExpectedValues:expectedValueInterval:completion:@
closeWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
closeWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "closeWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeOpenDurationWithParams:@
readAttributeOpenDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOpenDurationWithParamsSelector = mkSelector "readAttributeOpenDurationWithParams:"

-- | @Selector@ for @readAttributeDefaultOpenDurationWithParams:@
readAttributeDefaultOpenDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultOpenDurationWithParamsSelector = mkSelector "readAttributeDefaultOpenDurationWithParams:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:@
writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeDefaultOpenDurationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeDefaultOpenDurationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultOpenDurationWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAutoCloseTimeWithParams:@
readAttributeAutoCloseTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAutoCloseTimeWithParamsSelector = mkSelector "readAttributeAutoCloseTimeWithParams:"

-- | @Selector@ for @readAttributeRemainingDurationWithParams:@
readAttributeRemainingDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRemainingDurationWithParamsSelector = mkSelector "readAttributeRemainingDurationWithParams:"

-- | @Selector@ for @readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentStateWithParamsSelector = mkSelector "readAttributeCurrentStateWithParams:"

-- | @Selector@ for @readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTargetStateWithParamsSelector = mkSelector "readAttributeTargetStateWithParams:"

-- | @Selector@ for @readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentLevelWithParamsSelector = mkSelector "readAttributeCurrentLevelWithParams:"

-- | @Selector@ for @readAttributeTargetLevelWithParams:@
readAttributeTargetLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTargetLevelWithParamsSelector = mkSelector "readAttributeTargetLevelWithParams:"

-- | @Selector@ for @readAttributeDefaultOpenLevelWithParams:@
readAttributeDefaultOpenLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultOpenLevelWithParamsSelector = mkSelector "readAttributeDefaultOpenLevelWithParams:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:@
writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeDefaultOpenLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:@
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeDefaultOpenLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultOpenLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeValveFaultWithParams:@
readAttributeValveFaultWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeValveFaultWithParamsSelector = mkSelector "readAttributeValveFaultWithParams:"

-- | @Selector@ for @readAttributeLevelStepWithParams:@
readAttributeLevelStepWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLevelStepWithParamsSelector = mkSelector "readAttributeLevelStepWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterValveConfigurationAndControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterValveConfigurationAndControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterValveConfigurationAndControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

