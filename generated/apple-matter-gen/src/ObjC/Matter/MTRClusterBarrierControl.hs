{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Barrier Control    This cluster provides control of a barrier (garage door).
--
-- Generated bindings for @MTRClusterBarrierControl@.
module ObjC.Matter.MTRClusterBarrierControl
  ( MTRClusterBarrierControl
  , IsMTRClusterBarrierControl(..)
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completion
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completion
  , barrierControlStopWithExpectedValues_expectedValueInterval_completion
  , readAttributeBarrierMovingStateWithParams
  , readAttributeBarrierSafetyStatusWithParams
  , readAttributeBarrierCapabilitiesWithParams
  , readAttributeBarrierOpenEventsWithParams
  , writeAttributeBarrierOpenEventsWithValue_expectedValueInterval
  , writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierCloseEventsWithParams
  , writeAttributeBarrierCloseEventsWithValue_expectedValueInterval
  , writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierCommandOpenEventsWithParams
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierCommandCloseEventsWithParams
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_params
  , readAttributeBarrierOpenPeriodWithParams
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_params
  , readAttributeBarrierClosePeriodWithParams
  , writeAttributeBarrierClosePeriodWithValue_expectedValueInterval
  , writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_params
  , readAttributeBarrierPositionWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandler
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandler
  , barrierControlStopWithExpectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector
  , barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBarrierCapabilitiesWithParamsSelector
  , readAttributeBarrierCloseEventsWithParamsSelector
  , readAttributeBarrierClosePeriodWithParamsSelector
  , readAttributeBarrierCommandCloseEventsWithParamsSelector
  , readAttributeBarrierCommandOpenEventsWithParamsSelector
  , readAttributeBarrierMovingStateWithParamsSelector
  , readAttributeBarrierOpenEventsWithParamsSelector
  , readAttributeBarrierOpenPeriodWithParamsSelector
  , readAttributeBarrierPositionWithParamsSelector
  , readAttributeBarrierSafetyStatusWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector
  , writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector
  , writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlGoToPercentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completion mtrClusterBarrierControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterBarrierControl barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRBarrierControlClusterBarrierControlGoToPercentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithParams_expectedValues_expectedValueInterval_completion mtrClusterBarrierControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterBarrierControl barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRBarrierControlClusterBarrierControlStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- barrierControlStopWithExpectedValues:expectedValueInterval:completion:@
barrierControlStopWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithExpectedValues_expectedValueInterval_completion mtrClusterBarrierControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterBarrierControl barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeBarrierMovingStateWithParams:@
readAttributeBarrierMovingStateWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierMovingStateWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierMovingStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBarrierSafetyStatusWithParams:@
readAttributeBarrierSafetyStatusWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierSafetyStatusWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierSafetyStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBarrierCapabilitiesWithParams:@
readAttributeBarrierCapabilitiesWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCapabilitiesWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierCapabilitiesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBarrierOpenEventsWithParams:@
readAttributeBarrierOpenEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierOpenEventsWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierOpenEventsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBarrierCloseEventsWithParams:@
readAttributeBarrierCloseEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCloseEventsWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierCloseEventsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBarrierCommandOpenEventsWithParams:@
readAttributeBarrierCommandOpenEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCommandOpenEventsWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierCommandOpenEventsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBarrierCommandCloseEventsWithParams:@
readAttributeBarrierCommandCloseEventsWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierCommandCloseEventsWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierCommandCloseEventsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_params mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBarrierOpenPeriodWithParams:@
readAttributeBarrierOpenPeriodWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierOpenPeriodWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierOpenPeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_params mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBarrierClosePeriodWithParams:@
readAttributeBarrierClosePeriodWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierClosePeriodWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierClosePeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:@
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_params :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBarrierControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_params mtrClusterBarrierControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBarrierControl writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBarrierPositionWithParams:@
readAttributeBarrierPositionWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeBarrierPositionWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeBarrierPositionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRReadParams params) => mtrClusterBarrierControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBarrierControl params =
  sendMessage mtrClusterBarrierControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBarrierControl mtrClusterBarrierControl => mtrClusterBarrierControl -> IO (Id MTRClusterBarrierControl)
init_ mtrClusterBarrierControl =
  sendOwnedMessage mtrClusterBarrierControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterBarrierControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBarrierControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRDevice device, IsNSObject queue) => mtrClusterBarrierControl -> device -> CUShort -> queue -> IO (Id MTRClusterBarrierControl)
initWithDevice_endpoint_queue mtrClusterBarrierControl device endpoint queue =
  sendOwnedMessage mtrClusterBarrierControl initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlGoToPercentParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterBarrierControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterBarrierControl barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRBarrierControlClusterBarrierControlGoToPercentParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRBarrierControlClusterBarrierControlStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterBarrierControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterBarrierControl barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRBarrierControlClusterBarrierControlStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterBarrierControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandler mtrClusterBarrierControl expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterBarrierControl barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBarrierControl mtrClusterBarrierControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBarrierControl -> device -> endpointID -> queue -> IO (Id MTRClusterBarrierControl)
initWithDevice_endpointID_queue mtrClusterBarrierControl device endpointID queue =
  sendOwnedMessage mtrClusterBarrierControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRBarrierControlClusterBarrierControlGoToPercentParams, Id NSArray, Id NSNumber, Ptr ()] ()
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRBarrierControlClusterBarrierControlStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "barrierControlStopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @barrierControlStopWithExpectedValues:expectedValueInterval:completion:@
barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
barrierControlStopWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "barrierControlStopWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeBarrierMovingStateWithParams:@
readAttributeBarrierMovingStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierMovingStateWithParamsSelector = mkSelector "readAttributeBarrierMovingStateWithParams:"

-- | @Selector@ for @readAttributeBarrierSafetyStatusWithParams:@
readAttributeBarrierSafetyStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierSafetyStatusWithParamsSelector = mkSelector "readAttributeBarrierSafetyStatusWithParams:"

-- | @Selector@ for @readAttributeBarrierCapabilitiesWithParams:@
readAttributeBarrierCapabilitiesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierCapabilitiesWithParamsSelector = mkSelector "readAttributeBarrierCapabilitiesWithParams:"

-- | @Selector@ for @readAttributeBarrierOpenEventsWithParams:@
readAttributeBarrierOpenEventsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierOpenEventsWithParamsSelector = mkSelector "readAttributeBarrierOpenEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBarrierOpenEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBarrierOpenEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierOpenEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierCloseEventsWithParams:@
readAttributeBarrierCloseEventsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierCloseEventsWithParamsSelector = mkSelector "readAttributeBarrierCloseEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBarrierCloseEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBarrierCloseEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierCloseEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierCommandOpenEventsWithParams:@
readAttributeBarrierCommandOpenEventsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierCommandOpenEventsWithParamsSelector = mkSelector "readAttributeBarrierCommandOpenEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBarrierCommandOpenEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierCommandOpenEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierCommandCloseEventsWithParams:@
readAttributeBarrierCommandCloseEventsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierCommandCloseEventsWithParamsSelector = mkSelector "readAttributeBarrierCommandCloseEventsWithParams:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:@
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBarrierCommandCloseEventsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierCommandCloseEventsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierOpenPeriodWithParams:@
readAttributeBarrierOpenPeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierOpenPeriodWithParamsSelector = mkSelector "readAttributeBarrierOpenPeriodWithParams:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBarrierOpenPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBarrierOpenPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierOpenPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierClosePeriodWithParams:@
readAttributeBarrierClosePeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierClosePeriodWithParamsSelector = mkSelector "readAttributeBarrierClosePeriodWithParams:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:@
writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBarrierClosePeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:@
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBarrierClosePeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBarrierClosePeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBarrierPositionWithParams:@
readAttributeBarrierPositionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBarrierPositionWithParamsSelector = mkSelector "readAttributeBarrierPositionWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterBarrierControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBarrierControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterBarrierControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRBarrierControlClusterBarrierControlGoToPercentParams, Id NSArray, Id NSNumber, Ptr ()] ()
barrierControlGoToPercentWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "barrierControlGoToPercentWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRBarrierControlClusterBarrierControlStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
barrierControlStopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "barrierControlStopWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:@
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
barrierControlStopWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "barrierControlStopWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBarrierControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

