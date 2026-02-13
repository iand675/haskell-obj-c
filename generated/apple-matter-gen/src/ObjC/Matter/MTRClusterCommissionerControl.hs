{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Commissioner Control    Supports the ability for clients to request the commissioning of themselves or other nodes onto a fabric which the cluster server can commission onto.
--
-- Generated bindings for @MTRClusterCommissionerControl@.
module ObjC.Matter.MTRClusterCommissionerControl
  ( MTRClusterCommissionerControl
  , IsMTRClusterCommissionerControl(..)
  , requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completion
  , commissionNodeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedDeviceCategoriesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedDeviceCategoriesWithParamsSelector
  , requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:@
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRCommissionerControlClusterRequestCommissioningApprovalParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommissionerControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommissionerControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCommissionerControl requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCommissionerControlClusterRequestCommissioningApprovalParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- commissionNodeWithParams:expectedValues:expectedValueInterval:completion:@
commissionNodeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRCommissionerControlClusterCommissionNodeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterCommissionerControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
commissionNodeWithParams_expectedValues_expectedValueInterval_completion mtrClusterCommissionerControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterCommissionerControl commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRCommissionerControlClusterCommissionNodeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedDeviceCategoriesWithParams:@
readAttributeSupportedDeviceCategoriesWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeSupportedDeviceCategoriesWithParams mtrClusterCommissionerControl params =
  sendMessage mtrClusterCommissionerControl readAttributeSupportedDeviceCategoriesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterCommissionerControl params =
  sendMessage mtrClusterCommissionerControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterCommissionerControl params =
  sendMessage mtrClusterCommissionerControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterCommissionerControl params =
  sendMessage mtrClusterCommissionerControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterCommissionerControl params =
  sendMessage mtrClusterCommissionerControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRReadParams params) => mtrClusterCommissionerControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterCommissionerControl params =
  sendMessage mtrClusterCommissionerControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterCommissionerControl mtrClusterCommissionerControl => mtrClusterCommissionerControl -> IO (Id MTRClusterCommissionerControl)
init_ mtrClusterCommissionerControl =
  sendOwnedMessage mtrClusterCommissionerControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterCommissionerControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterCommissionerControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterCommissionerControl mtrClusterCommissionerControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterCommissionerControl -> device -> endpointID -> queue -> IO (Id MTRClusterCommissionerControl)
initWithDevice_endpointID_queue mtrClusterCommissionerControl device endpointID queue =
  sendOwnedMessage mtrClusterCommissionerControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:@
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCommissionerControlClusterRequestCommissioningApprovalParams, Id NSArray, Id NSNumber, Ptr ()] ()
requestCommissioningApprovalWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "requestCommissioningApprovalWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @commissionNodeWithParams:expectedValues:expectedValueInterval:completion:@
commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRCommissionerControlClusterCommissionNodeParams, Id NSArray, Id NSNumber, Ptr ()] ()
commissionNodeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "commissionNodeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedDeviceCategoriesWithParams:@
readAttributeSupportedDeviceCategoriesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedDeviceCategoriesWithParamsSelector = mkSelector "readAttributeSupportedDeviceCategoriesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterCommissionerControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterCommissionerControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterCommissionerControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

