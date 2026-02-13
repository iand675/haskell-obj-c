{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Thread Network Directory    Manages the names and credentials of Thread networks visible to the user.
--
-- Generated bindings for @MTRClusterThreadNetworkDirectory@.
module ObjC.Matter.MTRClusterThreadNetworkDirectory
  ( MTRClusterThreadNetworkDirectory
  , IsMTRClusterThreadNetworkDirectory(..)
  , addNetworkWithParams_expectedValues_expectedValueInterval_completion
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completion
  , getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completion
  , readAttributePreferredExtendedPanIDWithParams
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_params
  , readAttributeThreadNetworksWithParams
  , readAttributeThreadNetworkTableSizeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePreferredExtendedPanIDWithParamsSelector
  , readAttributeThreadNetworkTableSizeWithParamsSelector
  , readAttributeThreadNetworksWithParamsSelector
  , removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector
  , writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterAddNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDirectory params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadNetworkDirectory addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadNetworkDirectoryClusterAddNetworkParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeNetworkWithParams:expectedValues:expectedValueInterval:completion:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterRemoveNetworkParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeNetworkWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDirectory params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadNetworkDirectory removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadNetworkDirectoryClusterRemoveNetworkParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:@
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRThreadNetworkDirectoryClusterGetOperationalDatasetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completion mtrClusterThreadNetworkDirectory params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterThreadNetworkDirectory getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRThreadNetworkDirectoryClusterGetOperationalDatasetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributePreferredExtendedPanIDWithParams:@
readAttributePreferredExtendedPanIDWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributePreferredExtendedPanIDWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributePreferredExtendedPanIDWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterThreadNetworkDirectory -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval mtrClusterThreadNetworkDirectory dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterThreadNetworkDirectory writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_params :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterThreadNetworkDirectory -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_params mtrClusterThreadNetworkDirectory dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterThreadNetworkDirectory writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeThreadNetworksWithParams:@
readAttributeThreadNetworksWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeThreadNetworksWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeThreadNetworksWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeThreadNetworkTableSizeWithParams:@
readAttributeThreadNetworkTableSizeWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeThreadNetworkTableSizeWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeThreadNetworkTableSizeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRReadParams params) => mtrClusterThreadNetworkDirectory -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterThreadNetworkDirectory params =
  sendMessage mtrClusterThreadNetworkDirectory readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory => mtrClusterThreadNetworkDirectory -> IO (Id MTRClusterThreadNetworkDirectory)
init_ mtrClusterThreadNetworkDirectory =
  sendOwnedMessage mtrClusterThreadNetworkDirectory initSelector

-- | @+ new@
new :: IO (Id MTRClusterThreadNetworkDirectory)
new  =
  do
    cls' <- getRequiredClass "MTRClusterThreadNetworkDirectory"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterThreadNetworkDirectory mtrClusterThreadNetworkDirectory, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterThreadNetworkDirectory -> device -> endpointID -> queue -> IO (Id MTRClusterThreadNetworkDirectory)
initWithDevice_endpointID_queue mtrClusterThreadNetworkDirectory device endpointID queue =
  sendOwnedMessage mtrClusterThreadNetworkDirectory initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addNetworkWithParams:expectedValues:expectedValueInterval:completion:@
addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadNetworkDirectoryClusterAddNetworkParams, Id NSArray, Id NSNumber, Ptr ()] ()
addNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeNetworkWithParams:expectedValues:expectedValueInterval:completion:@
removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadNetworkDirectoryClusterRemoveNetworkParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeNetworkWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeNetworkWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:@
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRThreadNetworkDirectoryClusterGetOperationalDatasetParams, Id NSArray, Id NSNumber, Ptr ()] ()
getOperationalDatasetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getOperationalDatasetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributePreferredExtendedPanIDWithParams:@
readAttributePreferredExtendedPanIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePreferredExtendedPanIDWithParamsSelector = mkSelector "readAttributePreferredExtendedPanIDWithParams:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributePreferredExtendedPanIDWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:@
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributePreferredExtendedPanIDWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePreferredExtendedPanIDWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeThreadNetworksWithParams:@
readAttributeThreadNetworksWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeThreadNetworksWithParamsSelector = mkSelector "readAttributeThreadNetworksWithParams:"

-- | @Selector@ for @readAttributeThreadNetworkTableSizeWithParams:@
readAttributeThreadNetworkTableSizeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeThreadNetworkTableSizeWithParamsSelector = mkSelector "readAttributeThreadNetworkTableSizeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterThreadNetworkDirectory)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterThreadNetworkDirectory)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterThreadNetworkDirectory)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

