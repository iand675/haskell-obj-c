{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster TLS Certificate Management    This Cluster is used to manage TLS Client Certificates and to provision      TLS endpoints with enough information to facilitate subsequent connection.
--
-- Generated bindings for @MTRClusterTLSCertificateManagement@.
module ObjC.Matter.MTRClusterTLSCertificateManagement
  ( MTRClusterTLSCertificateManagement
  , IsMTRClusterTLSCertificateManagement(..)
  , provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , findRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , removeRootCertificateWithParams_expectedValues_expectedValueInterval_completion
  , clientCSRWithParams_expectedValues_expectedValueInterval_completion
  , provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , findClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , removeClientCertificateWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMaxRootCertificatesWithParams
  , readAttributeProvisionedRootCertificatesWithParams
  , readAttributeMaxClientCertificatesWithParams
  , readAttributeProvisionedClientCertificatesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector
  , findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaxClientCertificatesWithParamsSelector
  , readAttributeMaxRootCertificatesWithParamsSelector
  , readAttributeProvisionedClientCertificatesWithParamsSelector
  , readAttributeProvisionedRootCertificatesWithParamsSelector
  , removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector
  , removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterProvisionRootCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterFindRootCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterLookupRootCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveRootCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterRemoveRootCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- clientCSRWithParams:expectedValues:expectedValueInterval:completion:@
clientCSRWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterClientCSRParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clientCSRWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterClientCSRParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterProvisionClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterProvisionClientCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterFindClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
findClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterFindClientCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterLookupClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterLookupClientCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRTLSCertificateManagementClusterRemoveClientCertificateParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTLSCertificateManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completion mtrClusterTLSCertificateManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTLSCertificateManagement removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTLSCertificateManagementClusterRemoveClientCertificateParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMaxRootCertificatesWithParams:@
readAttributeMaxRootCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeMaxRootCertificatesWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeMaxRootCertificatesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProvisionedRootCertificatesWithParams:@
readAttributeProvisionedRootCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeProvisionedRootCertificatesWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeProvisionedRootCertificatesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxClientCertificatesWithParams:@
readAttributeMaxClientCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeMaxClientCertificatesWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeMaxClientCertificatesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProvisionedClientCertificatesWithParams:@
readAttributeProvisionedClientCertificatesWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeProvisionedClientCertificatesWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeProvisionedClientCertificatesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRReadParams params) => mtrClusterTLSCertificateManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTLSCertificateManagement params =
  sendMessage mtrClusterTLSCertificateManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement => mtrClusterTLSCertificateManagement -> IO (Id MTRClusterTLSCertificateManagement)
init_ mtrClusterTLSCertificateManagement =
  sendOwnedMessage mtrClusterTLSCertificateManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterTLSCertificateManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTLSCertificateManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTLSCertificateManagement mtrClusterTLSCertificateManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTLSCertificateManagement -> device -> endpointID -> queue -> IO (Id MTRClusterTLSCertificateManagement)
initWithDevice_endpointID_queue mtrClusterTLSCertificateManagement device endpointID queue =
  sendOwnedMessage mtrClusterTLSCertificateManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterProvisionRootCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
provisionRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provisionRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterFindRootCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
findRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterLookupRootCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
lookupRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "lookupRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterRemoveRootCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeRootCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeRootCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clientCSRWithParams:expectedValues:expectedValueInterval:completion:@
clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterClientCSRParams, Id NSArray, Id NSNumber, Ptr ()] ()
clientCSRWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clientCSRWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterProvisionClientCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
provisionClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "provisionClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterFindClientCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
findClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "findClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterLookupClientCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
lookupClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "lookupClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:@
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTLSCertificateManagementClusterRemoveClientCertificateParams, Id NSArray, Id NSNumber, Ptr ()] ()
removeClientCertificateWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "removeClientCertificateWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMaxRootCertificatesWithParams:@
readAttributeMaxRootCertificatesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxRootCertificatesWithParamsSelector = mkSelector "readAttributeMaxRootCertificatesWithParams:"

-- | @Selector@ for @readAttributeProvisionedRootCertificatesWithParams:@
readAttributeProvisionedRootCertificatesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProvisionedRootCertificatesWithParamsSelector = mkSelector "readAttributeProvisionedRootCertificatesWithParams:"

-- | @Selector@ for @readAttributeMaxClientCertificatesWithParams:@
readAttributeMaxClientCertificatesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxClientCertificatesWithParamsSelector = mkSelector "readAttributeMaxClientCertificatesWithParams:"

-- | @Selector@ for @readAttributeProvisionedClientCertificatesWithParams:@
readAttributeProvisionedClientCertificatesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProvisionedClientCertificatesWithParamsSelector = mkSelector "readAttributeProvisionedClientCertificatesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterTLSCertificateManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTLSCertificateManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTLSCertificateManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

