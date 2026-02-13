{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster General Commissioning    This cluster is used to manage global aspects of the Commissioning flow.
--
-- Generated bindings for @MTRClusterGeneralCommissioning@.
module ObjC.Matter.MTRClusterGeneralCommissioning
  ( MTRClusterGeneralCommissioning
  , IsMTRClusterGeneralCommissioning(..)
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completion
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completion
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completion
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completion
  , setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeBreadcrumbWithParams
  , writeAttributeBreadcrumbWithValue_expectedValueInterval
  , writeAttributeBreadcrumbWithValue_expectedValueInterval_params
  , readAttributeBasicCommissioningInfoWithParams
  , readAttributeRegulatoryConfigWithParams
  , readAttributeLocationCapabilityWithParams
  , readAttributeSupportsConcurrentConnectionWithParams
  , readAttributeTCAcceptedVersionWithParams
  , readAttributeTCMinRequiredVersionWithParams
  , readAttributeTCAcknowledgementsWithParams
  , readAttributeTCAcknowledgementsRequiredWithParams
  , readAttributeTCUpdateDeadlineWithParams
  , readAttributeRecoveryIdentifierWithParams
  , readAttributeNetworkRecoveryReasonWithParams
  , readAttributeIsCommissioningWithoutPowerWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandler
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandler
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandler
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBasicCommissioningInfoWithParamsSelector
  , readAttributeBreadcrumbWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeIsCommissioningWithoutPowerWithParamsSelector
  , readAttributeLocationCapabilityWithParamsSelector
  , readAttributeNetworkRecoveryReasonWithParamsSelector
  , readAttributeRecoveryIdentifierWithParamsSelector
  , readAttributeRegulatoryConfigWithParamsSelector
  , readAttributeSupportsConcurrentConnectionWithParamsSelector
  , readAttributeTCAcceptedVersionWithParamsSelector
  , readAttributeTCAcknowledgementsRequiredWithParamsSelector
  , readAttributeTCAcknowledgementsWithParamsSelector
  , readAttributeTCMinRequiredVersionWithParamsSelector
  , readAttributeTCUpdateDeadlineWithParamsSelector
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector
  , writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- armFailSafeWithParams:expectedValues:expectedValueInterval:completion:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterArmFailSafeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
armFailSafeWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGeneralCommissioning armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGeneralCommissioningClusterArmFailSafeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGeneralCommissioning setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGeneralCommissioningClusterSetRegulatoryConfigParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterCommissioningCompleteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGeneralCommissioning commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGeneralCommissioningClusterCommissioningCompleteParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- commissioningCompleteWithExpectedValues:expectedValueInterval:completion:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithExpectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterGeneralCommissioning commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:@
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetTCAcknowledgementsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completion mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterGeneralCommissioning setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRGeneralCommissioningClusterSetTCAcknowledgementsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeBreadcrumbWithParams:@
readAttributeBreadcrumbWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeBreadcrumbWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeBreadcrumbWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBreadcrumbWithValue:expectedValueInterval:@
writeAttributeBreadcrumbWithValue_expectedValueInterval :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBreadcrumbWithValue_expectedValueInterval mtrClusterGeneralCommissioning dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterGeneralCommissioning writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBreadcrumbWithValue:expectedValueInterval:params:@
writeAttributeBreadcrumbWithValue_expectedValueInterval_params :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterGeneralCommissioning -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBreadcrumbWithValue_expectedValueInterval_params mtrClusterGeneralCommissioning dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterGeneralCommissioning writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBasicCommissioningInfoWithParams:@
readAttributeBasicCommissioningInfoWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeBasicCommissioningInfoWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeBasicCommissioningInfoWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRegulatoryConfigWithParams:@
readAttributeRegulatoryConfigWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeRegulatoryConfigWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeRegulatoryConfigWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLocationCapabilityWithParams:@
readAttributeLocationCapabilityWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeLocationCapabilityWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeLocationCapabilityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportsConcurrentConnectionWithParams:@
readAttributeSupportsConcurrentConnectionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeSupportsConcurrentConnectionWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeSupportsConcurrentConnectionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTCAcceptedVersionWithParams:@
readAttributeTCAcceptedVersionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCAcceptedVersionWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeTCAcceptedVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTCMinRequiredVersionWithParams:@
readAttributeTCMinRequiredVersionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCMinRequiredVersionWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeTCMinRequiredVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTCAcknowledgementsWithParams:@
readAttributeTCAcknowledgementsWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCAcknowledgementsWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeTCAcknowledgementsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTCAcknowledgementsRequiredWithParams:@
readAttributeTCAcknowledgementsRequiredWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCAcknowledgementsRequiredWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeTCAcknowledgementsRequiredWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTCUpdateDeadlineWithParams:@
readAttributeTCUpdateDeadlineWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeTCUpdateDeadlineWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeTCUpdateDeadlineWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRecoveryIdentifierWithParams:@
readAttributeRecoveryIdentifierWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeRecoveryIdentifierWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeRecoveryIdentifierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNetworkRecoveryReasonWithParams:@
readAttributeNetworkRecoveryReasonWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeNetworkRecoveryReasonWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeNetworkRecoveryReasonWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeIsCommissioningWithoutPowerWithParams:@
readAttributeIsCommissioningWithoutPowerWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeIsCommissioningWithoutPowerWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeIsCommissioningWithoutPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRReadParams params) => mtrClusterGeneralCommissioning -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterGeneralCommissioning params =
  sendMessage mtrClusterGeneralCommissioning readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning => mtrClusterGeneralCommissioning -> IO (Id MTRClusterGeneralCommissioning)
init_ mtrClusterGeneralCommissioning =
  sendOwnedMessage mtrClusterGeneralCommissioning initSelector

-- | @+ new@
new :: IO (Id MTRClusterGeneralCommissioning)
new  =
  do
    cls' <- getRequiredClass "MTRClusterGeneralCommissioning"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRDevice device, IsNSObject queue) => mtrClusterGeneralCommissioning -> device -> CUShort -> queue -> IO (Id MTRClusterGeneralCommissioning)
initWithDevice_endpoint_queue mtrClusterGeneralCommissioning device endpoint queue =
  sendOwnedMessage mtrClusterGeneralCommissioning initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterArmFailSafeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGeneralCommissioning armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGeneralCommissioningClusterArmFailSafeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterSetRegulatoryConfigParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGeneralCommissioning setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGeneralCommissioningClusterSetRegulatoryConfigParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRGeneralCommissioningClusterCommissioningCompleteParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGeneralCommissioning commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRGeneralCommissioningClusterCommissioningCompleteParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterGeneralCommissioning -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandler mtrClusterGeneralCommissioning expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterGeneralCommissioning commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterGeneralCommissioning mtrClusterGeneralCommissioning, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterGeneralCommissioning -> device -> endpointID -> queue -> IO (Id MTRClusterGeneralCommissioning)
initWithDevice_endpointID_queue mtrClusterGeneralCommissioning device endpointID queue =
  sendOwnedMessage mtrClusterGeneralCommissioning initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @armFailSafeWithParams:expectedValues:expectedValueInterval:completion:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGeneralCommissioningClusterArmFailSafeParams, Id NSArray, Id NSNumber, Ptr ()] ()
armFailSafeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "armFailSafeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGeneralCommissioningClusterSetRegulatoryConfigParams, Id NSArray, Id NSNumber, Ptr ()] ()
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGeneralCommissioningClusterCommissioningCompleteParams, Id NSArray, Id NSNumber, Ptr ()] ()
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "commissioningCompleteWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @commissioningCompleteWithExpectedValues:expectedValueInterval:completion:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
commissioningCompleteWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "commissioningCompleteWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:@
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRGeneralCommissioningClusterSetTCAcknowledgementsParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTCAcknowledgementsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTCAcknowledgementsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBreadcrumbWithParams:@
readAttributeBreadcrumbWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBreadcrumbWithParamsSelector = mkSelector "readAttributeBreadcrumbWithParams:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:expectedValueInterval:@
writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBreadcrumbWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBreadcrumbWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBreadcrumbWithValue:expectedValueInterval:params:@
writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBreadcrumbWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBreadcrumbWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBasicCommissioningInfoWithParams:@
readAttributeBasicCommissioningInfoWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBasicCommissioningInfoWithParamsSelector = mkSelector "readAttributeBasicCommissioningInfoWithParams:"

-- | @Selector@ for @readAttributeRegulatoryConfigWithParams:@
readAttributeRegulatoryConfigWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRegulatoryConfigWithParamsSelector = mkSelector "readAttributeRegulatoryConfigWithParams:"

-- | @Selector@ for @readAttributeLocationCapabilityWithParams:@
readAttributeLocationCapabilityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLocationCapabilityWithParamsSelector = mkSelector "readAttributeLocationCapabilityWithParams:"

-- | @Selector@ for @readAttributeSupportsConcurrentConnectionWithParams:@
readAttributeSupportsConcurrentConnectionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportsConcurrentConnectionWithParamsSelector = mkSelector "readAttributeSupportsConcurrentConnectionWithParams:"

-- | @Selector@ for @readAttributeTCAcceptedVersionWithParams:@
readAttributeTCAcceptedVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTCAcceptedVersionWithParamsSelector = mkSelector "readAttributeTCAcceptedVersionWithParams:"

-- | @Selector@ for @readAttributeTCMinRequiredVersionWithParams:@
readAttributeTCMinRequiredVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTCMinRequiredVersionWithParamsSelector = mkSelector "readAttributeTCMinRequiredVersionWithParams:"

-- | @Selector@ for @readAttributeTCAcknowledgementsWithParams:@
readAttributeTCAcknowledgementsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTCAcknowledgementsWithParamsSelector = mkSelector "readAttributeTCAcknowledgementsWithParams:"

-- | @Selector@ for @readAttributeTCAcknowledgementsRequiredWithParams:@
readAttributeTCAcknowledgementsRequiredWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTCAcknowledgementsRequiredWithParamsSelector = mkSelector "readAttributeTCAcknowledgementsRequiredWithParams:"

-- | @Selector@ for @readAttributeTCUpdateDeadlineWithParams:@
readAttributeTCUpdateDeadlineWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTCUpdateDeadlineWithParamsSelector = mkSelector "readAttributeTCUpdateDeadlineWithParams:"

-- | @Selector@ for @readAttributeRecoveryIdentifierWithParams:@
readAttributeRecoveryIdentifierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRecoveryIdentifierWithParamsSelector = mkSelector "readAttributeRecoveryIdentifierWithParams:"

-- | @Selector@ for @readAttributeNetworkRecoveryReasonWithParams:@
readAttributeNetworkRecoveryReasonWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNetworkRecoveryReasonWithParamsSelector = mkSelector "readAttributeNetworkRecoveryReasonWithParams:"

-- | @Selector@ for @readAttributeIsCommissioningWithoutPowerWithParams:@
readAttributeIsCommissioningWithoutPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIsCommissioningWithoutPowerWithParamsSelector = mkSelector "readAttributeIsCommissioningWithoutPowerWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterGeneralCommissioning)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterGeneralCommissioning)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterGeneralCommissioning)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:@
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGeneralCommissioningClusterArmFailSafeParams, Id NSArray, Id NSNumber, Ptr ()] ()
armFailSafeWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "armFailSafeWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:@
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGeneralCommissioningClusterSetRegulatoryConfigParams, Id NSArray, Id NSNumber, Ptr ()] ()
setRegulatoryConfigWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "setRegulatoryConfigWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRGeneralCommissioningClusterCommissioningCompleteParams, Id NSArray, Id NSNumber, Ptr ()] ()
commissioningCompleteWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "commissioningCompleteWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:@
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
commissioningCompleteWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "commissioningCompleteWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterGeneralCommissioning)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

