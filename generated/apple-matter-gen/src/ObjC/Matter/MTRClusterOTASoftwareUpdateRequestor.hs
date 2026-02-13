{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster OTA Software Update Requestor    Provides an interface for downloading and applying OTA software updates
--
-- Generated bindings for @MTRClusterOTASoftwareUpdateRequestor@.
module ObjC.Matter.MTRClusterOTASoftwareUpdateRequestor
  ( MTRClusterOTASoftwareUpdateRequestor
  , IsMTRClusterOTASoftwareUpdateRequestor(..)
  , announceOTAProviderWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeDefaultOTAProvidersWithParams
  , writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval
  , writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_params
  , readAttributeUpdatePossibleWithParams
  , readAttributeUpdateStateWithParams
  , readAttributeUpdateStateProgressWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , announceOTAProviderWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDefaultOTAProvidersWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeUpdatePossibleWithParamsSelector
  , readAttributeUpdateStateProgressWithParamsSelector
  , readAttributeUpdateStateWithParamsSelector
  , writeAttributeDefaultOTAProvidersWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- announceOTAProviderWithParams:expectedValues:expectedValueInterval:completion:@
announceOTAProviderWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterOTASoftwareUpdateRequestor -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
announceOTAProviderWithParams_expectedValues_expectedValueInterval_completion mtrClusterOTASoftwareUpdateRequestor params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterOTASoftwareUpdateRequestor announceOTAProviderWithParams_expectedValues_expectedValueInterval_completionSelector (toMTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeDefaultOTAProvidersWithParams:@
readAttributeDefaultOTAProvidersWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeDefaultOTAProvidersWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeDefaultOTAProvidersWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeDefaultOTAProvidersWithValue:expectedValueInterval:@
writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterOTASoftwareUpdateRequestor -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval mtrClusterOTASoftwareUpdateRequestor dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterOTASoftwareUpdateRequestor writeAttributeDefaultOTAProvidersWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeDefaultOTAProvidersWithValue:expectedValueInterval:params:@
writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_params :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterOTASoftwareUpdateRequestor -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_params mtrClusterOTASoftwareUpdateRequestor dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeUpdatePossibleWithParams:@
readAttributeUpdatePossibleWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeUpdatePossibleWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeUpdatePossibleWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUpdateStateWithParams:@
readAttributeUpdateStateWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeUpdateStateWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeUpdateStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUpdateStateProgressWithParams:@
readAttributeUpdateStateProgressWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeUpdateStateProgressWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeUpdateStateProgressWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRReadParams params) => mtrClusterOTASoftwareUpdateRequestor -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterOTASoftwareUpdateRequestor params =
  sendMessage mtrClusterOTASoftwareUpdateRequestor readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor => mtrClusterOTASoftwareUpdateRequestor -> IO (Id MTRClusterOTASoftwareUpdateRequestor)
init_ mtrClusterOTASoftwareUpdateRequestor =
  sendOwnedMessage mtrClusterOTASoftwareUpdateRequestor initSelector

-- | @+ new@
new :: IO (Id MTRClusterOTASoftwareUpdateRequestor)
new  =
  do
    cls' <- getRequiredClass "MTRClusterOTASoftwareUpdateRequestor"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterOTASoftwareUpdateRequestor mtrClusterOTASoftwareUpdateRequestor, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterOTASoftwareUpdateRequestor -> device -> endpointID -> queue -> IO (Id MTRClusterOTASoftwareUpdateRequestor)
initWithDevice_endpointID_queue mtrClusterOTASoftwareUpdateRequestor device endpointID queue =
  sendOwnedMessage mtrClusterOTASoftwareUpdateRequestor initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @announceOTAProviderWithParams:expectedValues:expectedValueInterval:completion:@
announceOTAProviderWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTROTASoftwareUpdateRequestorClusterAnnounceOTAProviderParams, Id NSArray, Id NSNumber, Ptr ()] ()
announceOTAProviderWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "announceOTAProviderWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeDefaultOTAProvidersWithParams:@
readAttributeDefaultOTAProvidersWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultOTAProvidersWithParamsSelector = mkSelector "readAttributeDefaultOTAProvidersWithParams:"

-- | @Selector@ for @writeAttributeDefaultOTAProvidersWithValue:expectedValueInterval:@
writeAttributeDefaultOTAProvidersWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeDefaultOTAProvidersWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultOTAProvidersWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultOTAProvidersWithValue:expectedValueInterval:params:@
writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeDefaultOTAProvidersWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultOTAProvidersWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeUpdatePossibleWithParams:@
readAttributeUpdatePossibleWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUpdatePossibleWithParamsSelector = mkSelector "readAttributeUpdatePossibleWithParams:"

-- | @Selector@ for @readAttributeUpdateStateWithParams:@
readAttributeUpdateStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUpdateStateWithParamsSelector = mkSelector "readAttributeUpdateStateWithParams:"

-- | @Selector@ for @readAttributeUpdateStateProgressWithParams:@
readAttributeUpdateStateProgressWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUpdateStateProgressWithParamsSelector = mkSelector "readAttributeUpdateStateProgressWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterOTASoftwareUpdateRequestor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterOTASoftwareUpdateRequestor)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterOTASoftwareUpdateRequestor)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

