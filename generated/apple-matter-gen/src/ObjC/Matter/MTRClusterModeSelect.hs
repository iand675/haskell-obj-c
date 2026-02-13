{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Mode Select    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterModeSelect@.
module ObjC.Matter.MTRClusterModeSelect
  ( MTRClusterModeSelect
  , IsMTRClusterModeSelect(..)
  , changeToModeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeDescriptionWithParams
  , readAttributeStandardNamespaceWithParams
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
  , readAttributeStartUpModeWithParams
  , writeAttributeStartUpModeWithValue_expectedValueInterval
  , writeAttributeStartUpModeWithValue_expectedValueInterval_params
  , readAttributeOnModeWithParams
  , writeAttributeOnModeWithValue_expectedValueInterval
  , writeAttributeOnModeWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentModeWithParamsSelector
  , readAttributeDescriptionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOnModeWithParamsSelector
  , readAttributeStandardNamespaceWithParamsSelector
  , readAttributeStartUpModeWithParamsSelector
  , readAttributeSupportedModesWithParamsSelector
  , writeAttributeOnModeWithValue_expectedValueIntervalSelector
  , writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeStartUpModeWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterModeSelect params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterModeSelect changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRModeSelectClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeDescriptionWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeDescriptionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStandardNamespaceWithParams:@
readAttributeStandardNamespaceWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeStandardNamespaceWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeStandardNamespaceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStartUpModeWithParams:@
readAttributeStartUpModeWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeStartUpModeWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeStartUpModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeStartUpModeWithValue:expectedValueInterval:@
writeAttributeStartUpModeWithValue_expectedValueInterval :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpModeWithValue_expectedValueInterval mtrClusterModeSelect dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterModeSelect writeAttributeStartUpModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeStartUpModeWithValue:expectedValueInterval:params:@
writeAttributeStartUpModeWithValue_expectedValueInterval_params :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpModeWithValue_expectedValueInterval_params mtrClusterModeSelect dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterModeSelect writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOnModeWithParams:@
readAttributeOnModeWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeOnModeWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeOnModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOnModeWithValue:expectedValueInterval:@
writeAttributeOnModeWithValue_expectedValueInterval :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnModeWithValue_expectedValueInterval mtrClusterModeSelect dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterModeSelect writeAttributeOnModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOnModeWithValue:expectedValueInterval:params:@
writeAttributeOnModeWithValue_expectedValueInterval_params :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterModeSelect -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnModeWithValue_expectedValueInterval_params mtrClusterModeSelect dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterModeSelect writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRReadParams params) => mtrClusterModeSelect -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterModeSelect params =
  sendMessage mtrClusterModeSelect readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterModeSelect mtrClusterModeSelect => mtrClusterModeSelect -> IO (Id MTRClusterModeSelect)
init_ mtrClusterModeSelect =
  sendOwnedMessage mtrClusterModeSelect initSelector

-- | @+ new@
new :: IO (Id MTRClusterModeSelect)
new  =
  do
    cls' <- getRequiredClass "MTRClusterModeSelect"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRDevice device, IsNSObject queue) => mtrClusterModeSelect -> device -> CUShort -> queue -> IO (Id MTRClusterModeSelect)
initWithDevice_endpoint_queue mtrClusterModeSelect device endpoint queue =
  sendOwnedMessage mtrClusterModeSelect initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRModeSelectClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterModeSelect -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterModeSelect params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterModeSelect changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRModeSelectClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterModeSelect mtrClusterModeSelect, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterModeSelect -> device -> endpointID -> queue -> IO (Id MTRClusterModeSelect)
initWithDevice_endpointID_queue mtrClusterModeSelect device endpointID queue =
  sendOwnedMessage mtrClusterModeSelect initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRModeSelectClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDescriptionWithParamsSelector = mkSelector "readAttributeDescriptionWithParams:"

-- | @Selector@ for @readAttributeStandardNamespaceWithParams:@
readAttributeStandardNamespaceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStandardNamespaceWithParamsSelector = mkSelector "readAttributeStandardNamespaceWithParams:"

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

-- | @Selector@ for @readAttributeStartUpModeWithParams:@
readAttributeStartUpModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStartUpModeWithParamsSelector = mkSelector "readAttributeStartUpModeWithParams:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:expectedValueInterval:@
writeAttributeStartUpModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeStartUpModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpModeWithValue:expectedValueInterval:params:@
writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeStartUpModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnModeWithParams:@
readAttributeOnModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnModeWithParamsSelector = mkSelector "readAttributeOnModeWithParams:"

-- | @Selector@ for @writeAttributeOnModeWithValue:expectedValueInterval:@
writeAttributeOnModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOnModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnModeWithValue:expectedValueInterval:params:@
writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOnModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnModeWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterModeSelect)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterModeSelect)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterModeSelect)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRModeSelectClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeToModeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterModeSelect)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

