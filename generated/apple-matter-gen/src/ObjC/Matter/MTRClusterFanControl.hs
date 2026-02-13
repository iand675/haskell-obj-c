{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Fan Control    An interface for controlling a fan in a heating/cooling system.
--
-- Generated bindings for @MTRClusterFanControl@.
module ObjC.Matter.MTRClusterFanControl
  ( MTRClusterFanControl
  , IsMTRClusterFanControl(..)
  , stepWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeFanModeWithParams
  , writeAttributeFanModeWithValue_expectedValueInterval
  , writeAttributeFanModeWithValue_expectedValueInterval_params
  , readAttributeFanModeSequenceWithParams
  , writeAttributeFanModeSequenceWithValue_expectedValueInterval
  , writeAttributeFanModeSequenceWithValue_expectedValueInterval_params
  , readAttributePercentSettingWithParams
  , writeAttributePercentSettingWithValue_expectedValueInterval
  , writeAttributePercentSettingWithValue_expectedValueInterval_params
  , readAttributePercentCurrentWithParams
  , readAttributeSpeedMaxWithParams
  , readAttributeSpeedSettingWithParams
  , writeAttributeSpeedSettingWithValue_expectedValueInterval
  , writeAttributeSpeedSettingWithValue_expectedValueInterval_params
  , readAttributeSpeedCurrentWithParams
  , readAttributeRockSupportWithParams
  , readAttributeRockSettingWithParams
  , writeAttributeRockSettingWithValue_expectedValueInterval
  , writeAttributeRockSettingWithValue_expectedValueInterval_params
  , readAttributeWindSupportWithParams
  , readAttributeWindSettingWithParams
  , writeAttributeWindSettingWithValue_expectedValueInterval
  , writeAttributeWindSettingWithValue_expectedValueInterval_params
  , readAttributeAirflowDirectionWithParams
  , writeAttributeAirflowDirectionWithValue_expectedValueInterval
  , writeAttributeAirflowDirectionWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAirflowDirectionWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFanModeSequenceWithParamsSelector
  , readAttributeFanModeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributePercentCurrentWithParamsSelector
  , readAttributePercentSettingWithParamsSelector
  , readAttributeRockSettingWithParamsSelector
  , readAttributeRockSupportWithParamsSelector
  , readAttributeSpeedCurrentWithParamsSelector
  , readAttributeSpeedMaxWithParamsSelector
  , readAttributeSpeedSettingWithParamsSelector
  , readAttributeWindSettingWithParamsSelector
  , readAttributeWindSupportWithParamsSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector
  , writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector
  , writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector
  , writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector
  , writeAttributeFanModeWithValue_expectedValueIntervalSelector
  , writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector
  , writeAttributePercentSettingWithValue_expectedValueIntervalSelector
  , writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector
  , writeAttributeRockSettingWithValue_expectedValueIntervalSelector
  , writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector
  , writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector
  , writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector
  , writeAttributeWindSettingWithValue_expectedValueIntervalSelector
  , writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRFanControlClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completion mtrClusterFanControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterFanControl stepWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRFanControlClusterStepParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeFanModeWithParams:@
readAttributeFanModeWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeFanModeWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeFanModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeFanModeWithValue:expectedValueInterval:@
writeAttributeFanModeWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeFanModeWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributeFanModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeFanModeWithValue:expectedValueInterval:params:@
writeAttributeFanModeWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeFanModeWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeFanModeSequenceWithParams:@
readAttributeFanModeSequenceWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeFanModeSequenceWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeFanModeSequenceWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeFanModeSequenceWithValue:expectedValueInterval:@
writeAttributeFanModeSequenceWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeFanModeSequenceWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:@
writeAttributeFanModeSequenceWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeFanModeSequenceWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributePercentSettingWithParams:@
readAttributePercentSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributePercentSettingWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributePercentSettingWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributePercentSettingWithValue:expectedValueInterval:@
writeAttributePercentSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributePercentSettingWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributePercentSettingWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributePercentSettingWithValue:expectedValueInterval:params:@
writeAttributePercentSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributePercentSettingWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributePercentCurrentWithParams:@
readAttributePercentCurrentWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributePercentCurrentWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributePercentCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSpeedMaxWithParams:@
readAttributeSpeedMaxWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeSpeedMaxWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeSpeedMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSpeedSettingWithParams:@
readAttributeSpeedSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeSpeedSettingWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeSpeedSettingWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSpeedSettingWithValue:expectedValueInterval:@
writeAttributeSpeedSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSpeedSettingWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSpeedSettingWithValue:expectedValueInterval:params:@
writeAttributeSpeedSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSpeedSettingWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSpeedCurrentWithParams:@
readAttributeSpeedCurrentWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeSpeedCurrentWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeSpeedCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRockSupportWithParams:@
readAttributeRockSupportWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeRockSupportWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeRockSupportWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRockSettingWithParams:@
readAttributeRockSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeRockSettingWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeRockSettingWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeRockSettingWithValue:expectedValueInterval:@
writeAttributeRockSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRockSettingWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributeRockSettingWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeRockSettingWithValue:expectedValueInterval:params:@
writeAttributeRockSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRockSettingWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeWindSupportWithParams:@
readAttributeWindSupportWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeWindSupportWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeWindSupportWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWindSettingWithParams:@
readAttributeWindSettingWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeWindSettingWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeWindSettingWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeWindSettingWithValue:expectedValueInterval:@
writeAttributeWindSettingWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeWindSettingWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributeWindSettingWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeWindSettingWithValue:expectedValueInterval:params:@
writeAttributeWindSettingWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeWindSettingWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeAirflowDirectionWithParams:@
readAttributeAirflowDirectionWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeAirflowDirectionWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeAirflowDirectionWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeAirflowDirectionWithValue:expectedValueInterval:@
writeAttributeAirflowDirectionWithValue_expectedValueInterval :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAirflowDirectionWithValue_expectedValueInterval mtrClusterFanControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterFanControl writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:@
writeAttributeAirflowDirectionWithValue_expectedValueInterval_params :: (IsMTRClusterFanControl mtrClusterFanControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterFanControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAirflowDirectionWithValue_expectedValueInterval_params mtrClusterFanControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterFanControl writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRReadParams params) => mtrClusterFanControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterFanControl params =
  sendMessage mtrClusterFanControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterFanControl mtrClusterFanControl => mtrClusterFanControl -> IO (Id MTRClusterFanControl)
init_ mtrClusterFanControl =
  sendOwnedMessage mtrClusterFanControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterFanControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterFanControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRDevice device, IsNSObject queue) => mtrClusterFanControl -> device -> CUShort -> queue -> IO (Id MTRClusterFanControl)
initWithDevice_endpoint_queue mtrClusterFanControl device endpoint queue =
  sendOwnedMessage mtrClusterFanControl initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterFanControl mtrClusterFanControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterFanControl -> device -> endpointID -> queue -> IO (Id MTRClusterFanControl)
initWithDevice_endpointID_queue mtrClusterFanControl device endpointID queue =
  sendOwnedMessage mtrClusterFanControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRFanControlClusterStepParams, Id NSArray, Id NSNumber, Ptr ()] ()
stepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeFanModeWithParams:@
readAttributeFanModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFanModeWithParamsSelector = mkSelector "readAttributeFanModeWithParams:"

-- | @Selector@ for @writeAttributeFanModeWithValue:expectedValueInterval:@
writeAttributeFanModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeFanModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeFanModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeFanModeWithValue:expectedValueInterval:params:@
writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeFanModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeFanModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeFanModeSequenceWithParams:@
readAttributeFanModeSequenceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFanModeSequenceWithParamsSelector = mkSelector "readAttributeFanModeSequenceWithParams:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:expectedValueInterval:@
writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeFanModeSequenceWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeFanModeSequenceWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:@
writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeFanModeSequenceWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeFanModeSequenceWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePercentSettingWithParams:@
readAttributePercentSettingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePercentSettingWithParamsSelector = mkSelector "readAttributePercentSettingWithParams:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:expectedValueInterval:@
writeAttributePercentSettingWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributePercentSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributePercentSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributePercentSettingWithValue:expectedValueInterval:params:@
writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributePercentSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributePercentSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributePercentCurrentWithParams:@
readAttributePercentCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePercentCurrentWithParamsSelector = mkSelector "readAttributePercentCurrentWithParams:"

-- | @Selector@ for @readAttributeSpeedMaxWithParams:@
readAttributeSpeedMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpeedMaxWithParamsSelector = mkSelector "readAttributeSpeedMaxWithParams:"

-- | @Selector@ for @readAttributeSpeedSettingWithParams:@
readAttributeSpeedSettingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpeedSettingWithParamsSelector = mkSelector "readAttributeSpeedSettingWithParams:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:expectedValueInterval:@
writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSpeedSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSpeedSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSpeedSettingWithValue:expectedValueInterval:params:@
writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSpeedSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSpeedSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSpeedCurrentWithParams:@
readAttributeSpeedCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpeedCurrentWithParamsSelector = mkSelector "readAttributeSpeedCurrentWithParams:"

-- | @Selector@ for @readAttributeRockSupportWithParams:@
readAttributeRockSupportWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRockSupportWithParamsSelector = mkSelector "readAttributeRockSupportWithParams:"

-- | @Selector@ for @readAttributeRockSettingWithParams:@
readAttributeRockSettingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRockSettingWithParamsSelector = mkSelector "readAttributeRockSettingWithParams:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:expectedValueInterval:@
writeAttributeRockSettingWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeRockSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRockSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRockSettingWithValue:expectedValueInterval:params:@
writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeRockSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRockSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeWindSupportWithParams:@
readAttributeWindSupportWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWindSupportWithParamsSelector = mkSelector "readAttributeWindSupportWithParams:"

-- | @Selector@ for @readAttributeWindSettingWithParams:@
readAttributeWindSettingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWindSettingWithParamsSelector = mkSelector "readAttributeWindSettingWithParams:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:expectedValueInterval:@
writeAttributeWindSettingWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeWindSettingWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeWindSettingWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeWindSettingWithValue:expectedValueInterval:params:@
writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeWindSettingWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeWindSettingWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAirflowDirectionWithParams:@
readAttributeAirflowDirectionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAirflowDirectionWithParamsSelector = mkSelector "readAttributeAirflowDirectionWithParams:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:expectedValueInterval:@
writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeAirflowDirectionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAirflowDirectionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:@
writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeAirflowDirectionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAirflowDirectionWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterFanControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterFanControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterFanControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterFanControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

