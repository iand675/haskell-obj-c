{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Group Key Management    The Group Key Management Cluster is the mechanism by which group keys are managed.
--
-- Generated bindings for @MTRClusterGroupKeyManagement@.
module ObjC.Matter.MTRClusterGroupKeyManagement
  ( MTRClusterGroupKeyManagement
  , IsMTRClusterGroupKeyManagement(..)
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completion
  , keySetReadWithParams_expectedValues_expectedValueInterval_completion
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completion
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completion
  , keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completion
  , readAttributeGroupKeyMapWithParams
  , writeAttributeGroupKeyMapWithValue_expectedValueInterval
  , writeAttributeGroupKeyMapWithValue_expectedValueInterval_params
  , readAttributeGroupTableWithParams
  , readAttributeMaxGroupsPerFabricWithParams
  , readAttributeMaxGroupKeysPerFabricWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandler
  , keySetReadWithParams_expectedValues_expectedValueInterval_completionHandler
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandler
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeGroupKeyMapWithParamsSelector
  , readAttributeGroupTableWithParamsSelector
  , readAttributeMaxGroupKeysPerFabricWithParamsSelector
  , readAttributeMaxGroupsPerFabricWithParamsSelector
  , writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector
  , writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- keySetWriteWithParams:expectedValues:expectedValueInterval:completion:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetWriteWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupKeyManagement keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupKeyManagementClusterKeySetWriteParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- keySetReadWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupKeyManagement keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupKeyManagementClusterKeySetReadParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetRemoveWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupKeyManagement keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupKeyManagementClusterKeySetRemoveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupKeyManagement keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completion mtrClusterGroupKeyManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterGroupKeyManagement keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGroupKeyMapWithParams:@
readAttributeGroupKeyMapWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeGroupKeyMapWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeGroupKeyMapWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeGroupKeyMapWithValue:expectedValueInterval:@
writeAttributeGroupKeyMapWithValue_expectedValueInterval :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeGroupKeyMapWithValue_expectedValueInterval mtrClusterGroupKeyManagement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterGroupKeyManagement writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:@
writeAttributeGroupKeyMapWithValue_expectedValueInterval_params :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterGroupKeyManagement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeGroupKeyMapWithValue_expectedValueInterval_params mtrClusterGroupKeyManagement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterGroupKeyManagement writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGroupTableWithParams:@
readAttributeGroupTableWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeGroupTableWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeGroupTableWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxGroupsPerFabricWithParams:@
readAttributeMaxGroupsPerFabricWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeMaxGroupsPerFabricWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeMaxGroupsPerFabricWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxGroupKeysPerFabricWithParams:@
readAttributeMaxGroupKeysPerFabricWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeMaxGroupKeysPerFabricWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeMaxGroupKeysPerFabricWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRReadParams params) => mtrClusterGroupKeyManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGroupKeyManagement params =
  sendMessage mtrClusterGroupKeyManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement => mtrClusterGroupKeyManagement -> IO (Id MTRClusterGroupKeyManagement)
init_ mtrClusterGroupKeyManagement =
  sendOwnedMessage mtrClusterGroupKeyManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterGroupKeyManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGroupKeyManagement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRDevice device, IsNSObject queue) => mtrClusterGroupKeyManagement -> device -> CUShort -> queue -> IO (Id MTRClusterGroupKeyManagement)
initWithDevice_endpoint_queue mtrClusterGroupKeyManagement device endpoint queue =
  sendOwnedMessage mtrClusterGroupKeyManagement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetWriteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGroupKeyManagement keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetWriteParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGroupKeyManagement keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetReadParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetRemoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGroupKeyManagement keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetRemoveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGroupKeyManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGroupKeyManagement params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGroupKeyManagement keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGroupKeyManagementClusterKeySetReadAllIndicesParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGroupKeyManagement mtrClusterGroupKeyManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGroupKeyManagement -> device -> endpointID -> queue -> IO (Id MTRClusterGroupKeyManagement)
initWithDevice_endpointID_queue mtrClusterGroupKeyManagement device endpointID queue =
  sendOwnedMessage mtrClusterGroupKeyManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keySetWriteWithParams:expectedValues:expectedValueInterval:completion:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetWriteParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetWriteWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetWriteWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetReadWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetReadWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetReadWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetRemoveParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetRemoveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadAllIndicesParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:@
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
keySetReadAllIndicesWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "keySetReadAllIndicesWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeGroupKeyMapWithParams:@
readAttributeGroupKeyMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGroupKeyMapWithParamsSelector = mkSelector "readAttributeGroupKeyMapWithParams:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:expectedValueInterval:@
writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeGroupKeyMapWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeGroupKeyMapWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:@
writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeGroupKeyMapWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeGroupKeyMapWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeGroupTableWithParams:@
readAttributeGroupTableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGroupTableWithParamsSelector = mkSelector "readAttributeGroupTableWithParams:"

-- | @Selector@ for @readAttributeMaxGroupsPerFabricWithParams:@
readAttributeMaxGroupsPerFabricWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxGroupsPerFabricWithParamsSelector = mkSelector "readAttributeMaxGroupsPerFabricWithParams:"

-- | @Selector@ for @readAttributeMaxGroupKeysPerFabricWithParams:@
readAttributeMaxGroupKeysPerFabricWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxGroupKeysPerFabricWithParamsSelector = mkSelector "readAttributeMaxGroupKeysPerFabricWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterGroupKeyManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterGroupKeyManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterGroupKeyManagement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetWriteParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetWriteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetWriteWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetReadWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetReadWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetRemoveParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetRemoveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetRemoveWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:@
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGroupKeyManagementClusterKeySetReadAllIndicesParams, Id NSArray, Id NSNumber, Ptr ()] ()
keySetReadAllIndicesWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "keySetReadAllIndicesWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterGroupKeyManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

