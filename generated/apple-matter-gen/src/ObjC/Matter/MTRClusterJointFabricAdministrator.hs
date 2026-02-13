{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Joint Fabric Administrator    An instance of the Joint Fabric Administrator Cluster only applies to Joint Fabric Administrator nodes fulfilling the role of Anchor CA.
--
-- Generated bindings for @MTRClusterJointFabricAdministrator@.
module ObjC.Matter.MTRClusterJointFabricAdministrator
  ( MTRClusterJointFabricAdministrator
  , IsMTRClusterJointFabricAdministrator(..)
  , icaccsrRequestWithParams_expectedValues_expectedValueInterval_completion
  , icaccsrRequestWithExpectedValues_expectedValueInterval_completion
  , addICACWithParams_expectedValues_expectedValueInterval_completion
  , openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion
  , transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completion
  , transferAnchorRequestWithExpectedValues_expectedValueInterval_completion
  , transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completion
  , transferAnchorCompleteWithExpectedValues_expectedValueInterval_completion
  , announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeAdministratorFabricIndexWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addICACWithParams_expectedValues_expectedValueInterval_completionSelector
  , announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector
  , icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector
  , icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAdministratorFabricIndexWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector
  , transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector
  , transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector
  , transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterICACCSRRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricAdministratorClusterICACCSRRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:@
icaccsrRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
icaccsrRequestWithExpectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- addICACWithParams:expectedValues:expectedValueInterval:completion:@
addICACWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAddICACParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addICACWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator addICACWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricAdministratorClusterAddICACParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorRequestParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricAdministratorClusterTransferAnchorRequestParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorRequestWithExpectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterTransferAnchorCompleteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricAdministratorClusterTransferAnchorCompleteParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:@
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterJointFabricAdministrator -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completion mtrClusterJointFabricAdministrator params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterJointFabricAdministrator announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeAdministratorFabricIndexWithParams:@
readAttributeAdministratorFabricIndexWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeAdministratorFabricIndexWithParams mtrClusterJointFabricAdministrator params =
  sendMessage mtrClusterJointFabricAdministrator readAttributeAdministratorFabricIndexWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterJointFabricAdministrator params =
  sendMessage mtrClusterJointFabricAdministrator readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterJointFabricAdministrator params =
  sendMessage mtrClusterJointFabricAdministrator readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterJointFabricAdministrator params =
  sendMessage mtrClusterJointFabricAdministrator readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterJointFabricAdministrator params =
  sendMessage mtrClusterJointFabricAdministrator readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRReadParams params) => mtrClusterJointFabricAdministrator -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterJointFabricAdministrator params =
  sendMessage mtrClusterJointFabricAdministrator readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator => mtrClusterJointFabricAdministrator -> IO (Id MTRClusterJointFabricAdministrator)
init_ mtrClusterJointFabricAdministrator =
  sendOwnedMessage mtrClusterJointFabricAdministrator initSelector

-- | @+ new@
new :: IO (Id MTRClusterJointFabricAdministrator)
new  =
  do
    cls' <- getRequiredClass "MTRClusterJointFabricAdministrator"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterJointFabricAdministrator mtrClusterJointFabricAdministrator, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterJointFabricAdministrator -> device -> endpointID -> queue -> IO (Id MTRClusterJointFabricAdministrator)
initWithDevice_endpointID_queue mtrClusterJointFabricAdministrator device endpointID queue =
  sendOwnedMessage mtrClusterJointFabricAdministrator initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:@
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterICACCSRRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
icaccsrRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "ICACCSRRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:@
icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
icaccsrRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "ICACCSRRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addICACWithParams:expectedValues:expectedValueInterval:completion:@
addICACWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterAddICACParams, Id NSArray, Id NSNumber, Ptr ()] ()
addICACWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addICACWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:@
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterOpenJointCommissioningWindowParams, Id NSArray, Id NSNumber, Ptr ()] ()
openJointCommissioningWindowWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "openJointCommissioningWindowWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterTransferAnchorRequestParams, Id NSArray, Id NSNumber, Ptr ()] ()
transferAnchorRequestWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorRequestWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:@
transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
transferAnchorRequestWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorRequestWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterTransferAnchorCompleteParams, Id NSArray, Id NSNumber, Ptr ()] ()
transferAnchorCompleteWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorCompleteWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:@
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
transferAnchorCompleteWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "transferAnchorCompleteWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:@
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRJointFabricAdministratorClusterAnnounceJointFabricAdministratorParams, Id NSArray, Id NSNumber, Ptr ()] ()
announceJointFabricAdministratorWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "announceJointFabricAdministratorWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeAdministratorFabricIndexWithParams:@
readAttributeAdministratorFabricIndexWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAdministratorFabricIndexWithParamsSelector = mkSelector "readAttributeAdministratorFabricIndexWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterJointFabricAdministrator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterJointFabricAdministrator)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterJointFabricAdministrator)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

