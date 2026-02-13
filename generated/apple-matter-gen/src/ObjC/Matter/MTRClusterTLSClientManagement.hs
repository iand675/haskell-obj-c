{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Client Management    This Cluster is used to provision TLS Endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRClusterTLSClientManagement@.
module ObjC.Matter.MTRClusterTLSClientManagement
  ( MTRClusterTLSClientManagement
  , IsMTRClusterTLSClientManagement(..)
  , provisionEndpointWithParams_expectedValues_expectedValueInterval_completion
  , findEndpointWithParams_expectedValues_expectedValueInterval_completion
  , removeEndpointWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxProvisionedWithParams
  , readAttributeProvisionedEndpointsWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaxProvisionedWithParamsSelector
  , readAttributeProvisionedEndpointsWithParamsSelector
  , removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:@
provisionEndpointWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRTLSClientManagementClusterProvisionEndpointParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSClientManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provisionEndpointWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSClientManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSClientManagement provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSClientManagementClusterProvisionEndpointParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- findEndpointWithParams:expectedValues:expectedValueInterval:completion:@
findEndpointWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRTLSClientManagementClusterFindEndpointParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSClientManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findEndpointWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSClientManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSClientManagement findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSClientManagementClusterFindEndpointParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeEndpointWithParams:expectedValues:expectedValueInterval:completion:@
removeEndpointWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRTLSClientManagementClusterRemoveEndpointParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSClientManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeEndpointWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSClientManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSClientManagement removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSClientManagementClusterRemoveEndpointParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMaxProvisionedWithParams:@
readAttributeMaxProvisionedWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeMaxProvisionedWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeMaxProvisionedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProvisionedEndpointsWithParams:@
readAttributeProvisionedEndpointsWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeProvisionedEndpointsWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeProvisionedEndpointsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRReadParams params) => mtrClusterTLSClientManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTLSClientManagement params =
  sendMessage mtrClusterTLSClientManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement => mtrClusterTLSClientManagement -> IO (Id MTRClusterTLSClientManagement)
init_ mtrClusterTLSClientManagement =
  sendOwnedMessage mtrClusterTLSClientManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterTLSClientManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTLSClientManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTLSClientManagement mtrClusterTLSClientManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTLSClientManagement -> device -> endpointID -> queue -> IO (Id MTRClusterTLSClientManagement)
initWithDevice_endpointID_queue mtrClusterTLSClientManagement device endpointID queue =
  sendOwnedMessage mtrClusterTLSClientManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:@
provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSClientManagementClusterProvisionEndpointParams, Id NSArray, Id NSNumber, Ptr ()] ()
provisionEndpointWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provisionEndpointWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findEndpointWithParams:expectedValues:expectedValueInterval:completion:@
findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSClientManagementClusterFindEndpointParams, Id NSArray, Id NSNumber, Ptr ()] ()
findEndpointWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findEndpointWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeEndpointWithParams:expectedValues:expectedValueInterval:completion:@
removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSClientManagementClusterRemoveEndpointParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeEndpointWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeEndpointWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxProvisionedWithParams:@
readAttributeMaxProvisionedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxProvisionedWithParamsSelector = mkSelector "readAttributeMaxProvisionedWithParams:"

-- | @Selector@ for @readAttributeProvisionedEndpointsWithParams:@
readAttributeProvisionedEndpointsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProvisionedEndpointsWithParamsSelector = mkSelector "readAttributeProvisionedEndpointsWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterTLSClientManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTLSClientManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTLSClientManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

