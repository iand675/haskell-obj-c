{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Access Control    The Access Control Cluster exposes a data model view of a      Node's Access Control List (ACL), which codifies the rules used to manage      and enforce Access Control for the Node's endpoints and their associated      cluster instances.
--
-- Generated bindings for @MTRClusterAccessControl@.
module ObjC.Matter.MTRClusterAccessControl
  ( MTRClusterAccessControl
  , IsMTRClusterAccessControl(..)
  , reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeACLWithParams
  , writeAttributeACLWithValue_expectedValueInterval
  , writeAttributeACLWithValue_expectedValueInterval_params
  , readAttributeExtensionWithParams
  , writeAttributeExtensionWithValue_expectedValueInterval
  , writeAttributeExtensionWithValue_expectedValueInterval_params
  , readAttributeSubjectsPerAccessControlEntryWithParams
  , readAttributeTargetsPerAccessControlEntryWithParams
  , readAttributeAccessControlEntriesPerFabricWithParams
  , readAttributeCommissioningARLWithParams
  , readAttributeARLWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributeAclWithParams
  , writeAttributeAclWithValue_expectedValueInterval
  , writeAttributeAclWithValue_expectedValueInterval_params
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeACLWithParamsSelector
  , readAttributeARLWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAccessControlEntriesPerFabricWithParamsSelector
  , readAttributeAclWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCommissioningARLWithParamsSelector
  , readAttributeExtensionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSubjectsPerAccessControlEntryWithParamsSelector
  , readAttributeTargetsPerAccessControlEntryWithParamsSelector
  , reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeACLWithValue_expectedValueIntervalSelector
  , writeAttributeACLWithValue_expectedValueInterval_paramsSelector
  , writeAttributeAclWithValue_expectedValueIntervalSelector
  , writeAttributeAclWithValue_expectedValueInterval_paramsSelector
  , writeAttributeExtensionWithValue_expectedValueIntervalSelector
  , writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:@
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRAccessControlClusterReviewFabricRestrictionsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completion mtrClusterAccessControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterAccessControl reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRAccessControlClusterReviewFabricRestrictionsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeACLWithParams:@
readAttributeACLWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeACLWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeACLWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeACLWithValue:expectedValueInterval:@
writeAttributeACLWithValue_expectedValueInterval :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeACLWithValue_expectedValueInterval mtrClusterAccessControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterAccessControl writeAttributeACLWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeACLWithValue:expectedValueInterval:params:@
writeAttributeACLWithValue_expectedValueInterval_params :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeACLWithValue_expectedValueInterval_params mtrClusterAccessControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterAccessControl writeAttributeACLWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeExtensionWithParams:@
readAttributeExtensionWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeExtensionWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeExtensionWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeExtensionWithValue:expectedValueInterval:@
writeAttributeExtensionWithValue_expectedValueInterval :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeExtensionWithValue_expectedValueInterval mtrClusterAccessControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterAccessControl writeAttributeExtensionWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeExtensionWithValue:expectedValueInterval:params:@
writeAttributeExtensionWithValue_expectedValueInterval_params :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeExtensionWithValue_expectedValueInterval_params mtrClusterAccessControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterAccessControl writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSubjectsPerAccessControlEntryWithParams:@
readAttributeSubjectsPerAccessControlEntryWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeSubjectsPerAccessControlEntryWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeSubjectsPerAccessControlEntryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTargetsPerAccessControlEntryWithParams:@
readAttributeTargetsPerAccessControlEntryWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeTargetsPerAccessControlEntryWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeTargetsPerAccessControlEntryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAccessControlEntriesPerFabricWithParams:@
readAttributeAccessControlEntriesPerFabricWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAccessControlEntriesPerFabricWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeAccessControlEntriesPerFabricWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCommissioningARLWithParams:@
readAttributeCommissioningARLWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeCommissioningARLWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeCommissioningARLWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeARLWithParams:@
readAttributeARLWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeARLWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeARLWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterAccessControl mtrClusterAccessControl => mtrClusterAccessControl -> IO (Id MTRClusterAccessControl)
init_ mtrClusterAccessControl =
  sendOwnedMessage mtrClusterAccessControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterAccessControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAccessControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRDevice device, IsNSObject queue) => mtrClusterAccessControl -> device -> CUShort -> queue -> IO (Id MTRClusterAccessControl)
initWithDevice_endpoint_queue mtrClusterAccessControl device endpoint queue =
  sendOwnedMessage mtrClusterAccessControl initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- readAttributeAclWithParams:@
readAttributeAclWithParams :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRReadParams params) => mtrClusterAccessControl -> params -> IO (Id NSDictionary)
readAttributeAclWithParams mtrClusterAccessControl params =
  sendMessage mtrClusterAccessControl readAttributeAclWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeAclWithValue:expectedValueInterval:@
writeAttributeAclWithValue_expectedValueInterval :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAclWithValue_expectedValueInterval mtrClusterAccessControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterAccessControl writeAttributeAclWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeAclWithValue:expectedValueInterval:params:@
writeAttributeAclWithValue_expectedValueInterval_params :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAccessControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAclWithValue_expectedValueInterval_params mtrClusterAccessControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterAccessControl writeAttributeAclWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAccessControl mtrClusterAccessControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAccessControl -> device -> endpointID -> queue -> IO (Id MTRClusterAccessControl)
initWithDevice_endpointID_queue mtrClusterAccessControl device endpointID queue =
  sendOwnedMessage mtrClusterAccessControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:@
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRAccessControlClusterReviewFabricRestrictionsParams, Id NSArray, Id NSNumber, Ptr ()] ()
reviewFabricRestrictionsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "reviewFabricRestrictionsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeACLWithParams:@
readAttributeACLWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeACLWithParamsSelector = mkSelector "readAttributeACLWithParams:"

-- | @Selector@ for @writeAttributeACLWithValue:expectedValueInterval:@
writeAttributeACLWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeACLWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeACLWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeACLWithValue:expectedValueInterval:params:@
writeAttributeACLWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeACLWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeACLWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeExtensionWithParams:@
readAttributeExtensionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeExtensionWithParamsSelector = mkSelector "readAttributeExtensionWithParams:"

-- | @Selector@ for @writeAttributeExtensionWithValue:expectedValueInterval:@
writeAttributeExtensionWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeExtensionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeExtensionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeExtensionWithValue:expectedValueInterval:params:@
writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeExtensionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeExtensionWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSubjectsPerAccessControlEntryWithParams:@
readAttributeSubjectsPerAccessControlEntryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSubjectsPerAccessControlEntryWithParamsSelector = mkSelector "readAttributeSubjectsPerAccessControlEntryWithParams:"

-- | @Selector@ for @readAttributeTargetsPerAccessControlEntryWithParams:@
readAttributeTargetsPerAccessControlEntryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTargetsPerAccessControlEntryWithParamsSelector = mkSelector "readAttributeTargetsPerAccessControlEntryWithParams:"

-- | @Selector@ for @readAttributeAccessControlEntriesPerFabricWithParams:@
readAttributeAccessControlEntriesPerFabricWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAccessControlEntriesPerFabricWithParamsSelector = mkSelector "readAttributeAccessControlEntriesPerFabricWithParams:"

-- | @Selector@ for @readAttributeCommissioningARLWithParams:@
readAttributeCommissioningARLWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCommissioningARLWithParamsSelector = mkSelector "readAttributeCommissioningARLWithParams:"

-- | @Selector@ for @readAttributeARLWithParams:@
readAttributeARLWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeARLWithParamsSelector = mkSelector "readAttributeARLWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterAccessControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterAccessControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterAccessControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeAclWithParams:@
readAttributeAclWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAclWithParamsSelector = mkSelector "readAttributeAclWithParams:"

-- | @Selector@ for @writeAttributeAclWithValue:expectedValueInterval:@
writeAttributeAclWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeAclWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAclWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAclWithValue:expectedValueInterval:params:@
writeAttributeAclWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeAclWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAclWithValue:expectedValueInterval:params:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterAccessControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

