{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Chime    This cluster provides facilities to configure and play Chime sounds, such as those used in a doorbell.
--
-- Generated bindings for @MTRClusterChime@.
module ObjC.Matter.MTRClusterChime
  ( MTRClusterChime
  , IsMTRClusterChime(..)
  , playChimeSoundWithParams_expectedValues_expectedValueInterval_completion
  , playChimeSoundWithExpectedValues_expectedValueInterval_completion
  , readAttributeInstalledChimeSoundsWithParams
  , readAttributeSelectedChimeWithParams
  , writeAttributeSelectedChimeWithValue_expectedValueInterval
  , writeAttributeSelectedChimeWithValue_expectedValueInterval_params
  , readAttributeEnabledWithParams
  , writeAttributeEnabledWithValue_expectedValueInterval
  , writeAttributeEnabledWithValue_expectedValueInterval_params
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
  , playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector
  , playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeEnabledWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeInstalledChimeSoundsWithParamsSelector
  , readAttributeSelectedChimeWithParamsSelector
  , writeAttributeEnabledWithValue_expectedValueIntervalSelector
  , writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector
  , writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector
  , writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:@
playChimeSoundWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterChime mtrClusterChime, IsMTRChimeClusterPlayChimeSoundParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
playChimeSoundWithParams_expectedValues_expectedValueInterval_completion mtrClusterChime params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterChime playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRChimeClusterPlayChimeSoundParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- playChimeSoundWithExpectedValues:expectedValueInterval:completion:@
playChimeSoundWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterChime mtrClusterChime, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
playChimeSoundWithExpectedValues_expectedValueInterval_completion mtrClusterChime expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterChime playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeInstalledChimeSoundsWithParams:@
readAttributeInstalledChimeSoundsWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeInstalledChimeSoundsWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeInstalledChimeSoundsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSelectedChimeWithParams:@
readAttributeSelectedChimeWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeSelectedChimeWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeSelectedChimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSelectedChimeWithValue:expectedValueInterval:@
writeAttributeSelectedChimeWithValue_expectedValueInterval :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSelectedChimeWithValue_expectedValueInterval mtrClusterChime dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterChime writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSelectedChimeWithValue:expectedValueInterval:params:@
writeAttributeSelectedChimeWithValue_expectedValueInterval_params :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSelectedChimeWithValue_expectedValueInterval_params mtrClusterChime dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterChime writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeEnabledWithParams:@
readAttributeEnabledWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeEnabledWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeEnabledWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeEnabledWithValue:expectedValueInterval:@
writeAttributeEnabledWithValue_expectedValueInterval :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeEnabledWithValue_expectedValueInterval mtrClusterChime dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterChime writeAttributeEnabledWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeEnabledWithValue:expectedValueInterval:params:@
writeAttributeEnabledWithValue_expectedValueInterval_params :: (IsMTRClusterChime mtrClusterChime, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterChime -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeEnabledWithValue_expectedValueInterval_params mtrClusterChime dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterChime writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterChime mtrClusterChime, IsMTRReadParams params) => mtrClusterChime -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterChime params =
  sendMessage mtrClusterChime readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterChime mtrClusterChime => mtrClusterChime -> IO (Id MTRClusterChime)
init_ mtrClusterChime =
  sendOwnedMessage mtrClusterChime initSelector

-- | @+ new@
new :: IO (Id MTRClusterChime)
new  =
  do
    cls' <- getRequiredClass "MTRClusterChime"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterChime mtrClusterChime, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterChime -> device -> endpointID -> queue -> IO (Id MTRClusterChime)
initWithDevice_endpointID_queue mtrClusterChime device endpointID queue =
  sendOwnedMessage mtrClusterChime initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:@
playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRChimeClusterPlayChimeSoundParams, Id NSArray, Id NSNumber, Ptr ()] ()
playChimeSoundWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "playChimeSoundWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @playChimeSoundWithExpectedValues:expectedValueInterval:completion:@
playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
playChimeSoundWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "playChimeSoundWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeInstalledChimeSoundsWithParams:@
readAttributeInstalledChimeSoundsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstalledChimeSoundsWithParamsSelector = mkSelector "readAttributeInstalledChimeSoundsWithParams:"

-- | @Selector@ for @readAttributeSelectedChimeWithParams:@
readAttributeSelectedChimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSelectedChimeWithParamsSelector = mkSelector "readAttributeSelectedChimeWithParams:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:expectedValueInterval:@
writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSelectedChimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSelectedChimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSelectedChimeWithValue:expectedValueInterval:params:@
writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSelectedChimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSelectedChimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnabledWithParams:@
readAttributeEnabledWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEnabledWithParamsSelector = mkSelector "readAttributeEnabledWithParams:"

-- | @Selector@ for @writeAttributeEnabledWithValue:expectedValueInterval:@
writeAttributeEnabledWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeEnabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeEnabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeEnabledWithValue:expectedValueInterval:params:@
writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeEnabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeEnabledWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterChime)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterChime)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterChime)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

