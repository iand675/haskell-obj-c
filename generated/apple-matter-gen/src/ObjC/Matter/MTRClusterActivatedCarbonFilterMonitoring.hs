{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Activated Carbon Filter Monitoring    Attributes and commands for monitoring activated carbon filters in a device
--
-- Generated bindings for @MTRClusterActivatedCarbonFilterMonitoring@.
module ObjC.Matter.MTRClusterActivatedCarbonFilterMonitoring
  ( MTRClusterActivatedCarbonFilterMonitoring
  , IsMTRClusterActivatedCarbonFilterMonitoring(..)
  , resetConditionWithParams_expectedValues_expectedValueInterval_completion
  , resetConditionWithExpectedValues_expectedValueInterval_completion
  , readAttributeConditionWithParams
  , readAttributeDegradationDirectionWithParams
  , readAttributeChangeIndicationWithParams
  , readAttributeInPlaceIndicatorWithParams
  , readAttributeLastChangedTimeWithParams
  , writeAttributeLastChangedTimeWithValue_expectedValueInterval
  , writeAttributeLastChangedTimeWithValue_expectedValueInterval_params
  , readAttributeReplacementProductListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeChangeIndicationWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeConditionWithParamsSelector
  , readAttributeDegradationDirectionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeInPlaceIndicatorWithParamsSelector
  , readAttributeLastChangedTimeWithParamsSelector
  , readAttributeReplacementProductListWithParamsSelector
  , resetConditionWithExpectedValues_expectedValueInterval_completionSelector
  , resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector
  , writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- resetConditionWithParams:expectedValues:expectedValueInterval:completion:@
resetConditionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRActivatedCarbonFilterMonitoringClusterResetConditionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterActivatedCarbonFilterMonitoring -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetConditionWithParams_expectedValues_expectedValueInterval_completion mtrClusterActivatedCarbonFilterMonitoring params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRActivatedCarbonFilterMonitoringClusterResetConditionParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetConditionWithExpectedValues:expectedValueInterval:completion:@
resetConditionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterActivatedCarbonFilterMonitoring -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetConditionWithExpectedValues_expectedValueInterval_completion mtrClusterActivatedCarbonFilterMonitoring expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring resetConditionWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeConditionWithParams:@
readAttributeConditionWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeConditionWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeConditionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDegradationDirectionWithParams:@
readAttributeDegradationDirectionWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeDegradationDirectionWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeDegradationDirectionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeChangeIndicationWithParams:@
readAttributeChangeIndicationWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeChangeIndicationWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeChangeIndicationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInPlaceIndicatorWithParams:@
readAttributeInPlaceIndicatorWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeInPlaceIndicatorWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeInPlaceIndicatorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLastChangedTimeWithParams:@
readAttributeLastChangedTimeWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeLastChangedTimeWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeLastChangedTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLastChangedTimeWithValue:expectedValueInterval:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterActivatedCarbonFilterMonitoring -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval mtrClusterActivatedCarbonFilterMonitoring dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval_params :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterActivatedCarbonFilterMonitoring -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval_params mtrClusterActivatedCarbonFilterMonitoring dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeReplacementProductListWithParams:@
readAttributeReplacementProductListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeReplacementProductListWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeReplacementProductListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRReadParams params) => mtrClusterActivatedCarbonFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterActivatedCarbonFilterMonitoring params =
  sendMessage mtrClusterActivatedCarbonFilterMonitoring readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring => mtrClusterActivatedCarbonFilterMonitoring -> IO (Id MTRClusterActivatedCarbonFilterMonitoring)
init_ mtrClusterActivatedCarbonFilterMonitoring =
  sendOwnedMessage mtrClusterActivatedCarbonFilterMonitoring initSelector

-- | @+ new@
new :: IO (Id MTRClusterActivatedCarbonFilterMonitoring)
new  =
  do
    cls' <- getRequiredClass "MTRClusterActivatedCarbonFilterMonitoring"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterActivatedCarbonFilterMonitoring mtrClusterActivatedCarbonFilterMonitoring, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterActivatedCarbonFilterMonitoring -> device -> endpointID -> queue -> IO (Id MTRClusterActivatedCarbonFilterMonitoring)
initWithDevice_endpointID_queue mtrClusterActivatedCarbonFilterMonitoring device endpointID queue =
  sendOwnedMessage mtrClusterActivatedCarbonFilterMonitoring initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetConditionWithParams:expectedValues:expectedValueInterval:completion:@
resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRActivatedCarbonFilterMonitoringClusterResetConditionParams, Id NSArray, Id NSNumber, Ptr ()] ()
resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "resetConditionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @resetConditionWithExpectedValues:expectedValueInterval:completion:@
resetConditionWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
resetConditionWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "resetConditionWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeConditionWithParams:@
readAttributeConditionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeConditionWithParamsSelector = mkSelector "readAttributeConditionWithParams:"

-- | @Selector@ for @readAttributeDegradationDirectionWithParams:@
readAttributeDegradationDirectionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDegradationDirectionWithParamsSelector = mkSelector "readAttributeDegradationDirectionWithParams:"

-- | @Selector@ for @readAttributeChangeIndicationWithParams:@
readAttributeChangeIndicationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeChangeIndicationWithParamsSelector = mkSelector "readAttributeChangeIndicationWithParams:"

-- | @Selector@ for @readAttributeInPlaceIndicatorWithParams:@
readAttributeInPlaceIndicatorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInPlaceIndicatorWithParamsSelector = mkSelector "readAttributeInPlaceIndicatorWithParams:"

-- | @Selector@ for @readAttributeLastChangedTimeWithParams:@
readAttributeLastChangedTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLastChangedTimeWithParamsSelector = mkSelector "readAttributeLastChangedTimeWithParams:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:expectedValueInterval:@
writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLastChangedTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeReplacementProductListWithParams:@
readAttributeReplacementProductListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReplacementProductListWithParamsSelector = mkSelector "readAttributeReplacementProductListWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterActivatedCarbonFilterMonitoring)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterActivatedCarbonFilterMonitoring)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterActivatedCarbonFilterMonitoring)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

