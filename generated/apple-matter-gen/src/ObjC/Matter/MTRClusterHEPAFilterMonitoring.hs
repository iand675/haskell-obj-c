{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster HEPA Filter Monitoring    Attributes and commands for monitoring HEPA filters in a device
--
-- Generated bindings for @MTRClusterHEPAFilterMonitoring@.
module ObjC.Matter.MTRClusterHEPAFilterMonitoring
  ( MTRClusterHEPAFilterMonitoring
  , IsMTRClusterHEPAFilterMonitoring(..)
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
resetConditionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRHEPAFilterMonitoringClusterResetConditionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterHEPAFilterMonitoring -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
resetConditionWithParams_expectedValues_expectedValueInterval_completion mtrClusterHEPAFilterMonitoring params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterHEPAFilterMonitoring resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRHEPAFilterMonitoringClusterResetConditionParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- resetConditionWithExpectedValues:expectedValueInterval:completion:@
resetConditionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterHEPAFilterMonitoring -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
resetConditionWithExpectedValues_expectedValueInterval_completion mtrClusterHEPAFilterMonitoring expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterHEPAFilterMonitoring resetConditionWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeConditionWithParams:@
readAttributeConditionWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeConditionWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeConditionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDegradationDirectionWithParams:@
readAttributeDegradationDirectionWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeDegradationDirectionWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeDegradationDirectionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeChangeIndicationWithParams:@
readAttributeChangeIndicationWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeChangeIndicationWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeChangeIndicationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInPlaceIndicatorWithParams:@
readAttributeInPlaceIndicatorWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeInPlaceIndicatorWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeInPlaceIndicatorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLastChangedTimeWithParams:@
readAttributeLastChangedTimeWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeLastChangedTimeWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeLastChangedTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLastChangedTimeWithValue:expectedValueInterval:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterHEPAFilterMonitoring -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval mtrClusterHEPAFilterMonitoring dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterHEPAFilterMonitoring writeAttributeLastChangedTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLastChangedTimeWithValue:expectedValueInterval:params:@
writeAttributeLastChangedTimeWithValue_expectedValueInterval_params :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterHEPAFilterMonitoring -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLastChangedTimeWithValue_expectedValueInterval_params mtrClusterHEPAFilterMonitoring dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterHEPAFilterMonitoring writeAttributeLastChangedTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeReplacementProductListWithParams:@
readAttributeReplacementProductListWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeReplacementProductListWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeReplacementProductListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRReadParams params) => mtrClusterHEPAFilterMonitoring -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterHEPAFilterMonitoring params =
  sendMessage mtrClusterHEPAFilterMonitoring readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring => mtrClusterHEPAFilterMonitoring -> IO (Id MTRClusterHEPAFilterMonitoring)
init_ mtrClusterHEPAFilterMonitoring =
  sendOwnedMessage mtrClusterHEPAFilterMonitoring initSelector

-- | @+ new@
new :: IO (Id MTRClusterHEPAFilterMonitoring)
new  =
  do
    cls' <- getRequiredClass "MTRClusterHEPAFilterMonitoring"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterHEPAFilterMonitoring mtrClusterHEPAFilterMonitoring, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterHEPAFilterMonitoring -> device -> endpointID -> queue -> IO (Id MTRClusterHEPAFilterMonitoring)
initWithDevice_endpointID_queue mtrClusterHEPAFilterMonitoring device endpointID queue =
  sendOwnedMessage mtrClusterHEPAFilterMonitoring initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetConditionWithParams:expectedValues:expectedValueInterval:completion:@
resetConditionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRHEPAFilterMonitoringClusterResetConditionParams, Id NSArray, Id NSNumber, Ptr ()] ()
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
initSelector :: Selector '[] (Id MTRClusterHEPAFilterMonitoring)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterHEPAFilterMonitoring)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterHEPAFilterMonitoring)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

