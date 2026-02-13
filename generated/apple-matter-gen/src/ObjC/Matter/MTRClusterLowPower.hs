{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Low Power    This cluster provides an interface for managing low power mode on a device.
--
-- Generated bindings for @MTRClusterLowPower@.
module ObjC.Matter.MTRClusterLowPower
  ( MTRClusterLowPower
  , IsMTRClusterLowPower(..)
  , sleepWithParams_expectedValues_expectedValueInterval_completion
  , sleepWithExpectedValues_expectedValueInterval_completion
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , sleepWithParams_expectedValues_expectedValueInterval_completionHandler
  , sleepWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , sleepWithExpectedValues_expectedValueInterval_completionSelector
  , sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , sleepWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sleepWithParams:expectedValues:expectedValueInterval:completion:@
sleepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRLowPowerClusterSleepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithParams_expectedValues_expectedValueInterval_completion mtrClusterLowPower params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLowPower sleepWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLowPowerClusterSleepParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- sleepWithExpectedValues:expectedValueInterval:completion:@
sleepWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterLowPower mtrClusterLowPower, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithExpectedValues_expectedValueInterval_completion mtrClusterLowPower expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterLowPower sleepWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLowPower params =
  sendMessage mtrClusterLowPower readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLowPower params =
  sendMessage mtrClusterLowPower readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLowPower params =
  sendMessage mtrClusterLowPower readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLowPower params =
  sendMessage mtrClusterLowPower readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRReadParams params) => mtrClusterLowPower -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLowPower params =
  sendMessage mtrClusterLowPower readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterLowPower mtrClusterLowPower => mtrClusterLowPower -> IO (Id MTRClusterLowPower)
init_ mtrClusterLowPower =
  sendOwnedMessage mtrClusterLowPower initSelector

-- | @+ new@
new :: IO (Id MTRClusterLowPower)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLowPower"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRDevice device, IsNSObject queue) => mtrClusterLowPower -> device -> CUShort -> queue -> IO (Id MTRClusterLowPower)
initWithDevice_endpoint_queue mtrClusterLowPower device endpoint queue =
  sendOwnedMessage mtrClusterLowPower initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- sleepWithParams:expectedValues:expectedValueInterval:completionHandler:@
sleepWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRLowPowerClusterSleepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLowPower params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLowPower sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLowPowerClusterSleepParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- sleepWithExpectedValues:expectedValueInterval:completionHandler:@
sleepWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLowPower mtrClusterLowPower, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterLowPower -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
sleepWithExpectedValues_expectedValueInterval_completionHandler mtrClusterLowPower expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLowPower sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLowPower mtrClusterLowPower, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLowPower -> device -> endpointID -> queue -> IO (Id MTRClusterLowPower)
initWithDevice_endpointID_queue mtrClusterLowPower device endpointID queue =
  sendOwnedMessage mtrClusterLowPower initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sleepWithParams:expectedValues:expectedValueInterval:completion:@
sleepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLowPowerClusterSleepParams, Id NSArray, Id NSNumber, Ptr ()] ()
sleepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "sleepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @sleepWithExpectedValues:expectedValueInterval:completion:@
sleepWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
sleepWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "sleepWithExpectedValues:expectedValueInterval:completion:"

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
initSelector :: Selector '[] (Id MTRClusterLowPower)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterLowPower)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterLowPower)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @sleepWithParams:expectedValues:expectedValueInterval:completionHandler:@
sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLowPowerClusterSleepParams, Id NSArray, Id NSNumber, Ptr ()] ()
sleepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "sleepWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @sleepWithExpectedValues:expectedValueInterval:completionHandler:@
sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
sleepWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "sleepWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterLowPower)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

