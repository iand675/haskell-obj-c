{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Level Control    Attributes and commands for controlling devices that can be set to a level between fully 'On' and fully 'Off.'
--
-- Generated bindings for @MTRClusterLevelControl@.
module ObjC.Matter.MTRClusterLevelControl
  ( MTRClusterLevelControl
  , IsMTRClusterLevelControl(..)
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completion
  , moveWithParams_expectedValues_expectedValueInterval_completion
  , stepWithParams_expectedValues_expectedValueInterval_completion
  , stopWithParams_expectedValues_expectedValueInterval_completion
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completion
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentLevelWithParams
  , readAttributeRemainingTimeWithParams
  , readAttributeMinLevelWithParams
  , readAttributeMaxLevelWithParams
  , readAttributeCurrentFrequencyWithParams
  , readAttributeMinFrequencyWithParams
  , readAttributeMaxFrequencyWithParams
  , readAttributeOptionsWithParams
  , writeAttributeOptionsWithValue_expectedValueInterval
  , writeAttributeOptionsWithValue_expectedValueInterval_params
  , readAttributeOnOffTransitionTimeWithParams
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_params
  , readAttributeOnLevelWithParams
  , writeAttributeOnLevelWithValue_expectedValueInterval
  , writeAttributeOnLevelWithValue_expectedValueInterval_params
  , readAttributeOnTransitionTimeWithParams
  , writeAttributeOnTransitionTimeWithValue_expectedValueInterval
  , writeAttributeOnTransitionTimeWithValue_expectedValueInterval_params
  , readAttributeOffTransitionTimeWithParams
  , writeAttributeOffTransitionTimeWithValue_expectedValueInterval
  , writeAttributeOffTransitionTimeWithValue_expectedValueInterval_params
  , readAttributeDefaultMoveRateWithParams
  , writeAttributeDefaultMoveRateWithValue_expectedValueInterval
  , writeAttributeDefaultMoveRateWithValue_expectedValueInterval_params
  , readAttributeStartUpCurrentLevelWithParams
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , moveWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentFrequencyWithParamsSelector
  , readAttributeCurrentLevelWithParamsSelector
  , readAttributeDefaultMoveRateWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaxFrequencyWithParamsSelector
  , readAttributeMaxLevelWithParamsSelector
  , readAttributeMinFrequencyWithParamsSelector
  , readAttributeMinLevelWithParamsSelector
  , readAttributeOffTransitionTimeWithParamsSelector
  , readAttributeOnLevelWithParamsSelector
  , readAttributeOnOffTransitionTimeWithParamsSelector
  , readAttributeOnTransitionTimeWithParamsSelector
  , readAttributeOptionsWithParamsSelector
  , readAttributeRemainingTimeWithParamsSelector
  , readAttributeStartUpCurrentLevelWithParamsSelector
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector
  , writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOnLevelWithValue_expectedValueIntervalSelector
  , writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector
  , writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOptionsWithValue_expectedValueIntervalSelector
  , writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector
  , writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- moveToLevelWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterMoveToLevelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- moveWithParams:expectedValues:expectedValueInterval:completion:@
moveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl moveWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterMoveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl stepWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterStepParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl stopWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterMoveToLevelWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterMoveWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterStepWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterStopWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToClosestFrequencyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completion mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterLevelControl moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRLevelControlClusterMoveToClosestFrequencyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeCurrentLevelWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeCurrentLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRemainingTimeWithParams:@
readAttributeRemainingTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeRemainingTimeWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeRemainingTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMinLevelWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeMinLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMaxLevelWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeMaxLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentFrequencyWithParams:@
readAttributeCurrentFrequencyWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeCurrentFrequencyWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeCurrentFrequencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinFrequencyWithParams:@
readAttributeMinFrequencyWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMinFrequencyWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeMinFrequencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxFrequencyWithParams:@
readAttributeMaxFrequencyWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeMaxFrequencyWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeMaxFrequencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOptionsWithParams:@
readAttributeOptionsWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOptionsWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeOptionsWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOptionsWithValue:expectedValueInterval:@
writeAttributeOptionsWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOptionsWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeOptionsWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOptionsWithValue:expectedValueInterval:params:@
writeAttributeOptionsWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOptionsWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOnOffTransitionTimeWithParams:@
readAttributeOnOffTransitionTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOnOffTransitionTimeWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeOnOffTransitionTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOnLevelWithParams:@
readAttributeOnLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOnLevelWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeOnLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOnLevelWithValue:expectedValueInterval:@
writeAttributeOnLevelWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnLevelWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeOnLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOnLevelWithValue:expectedValueInterval:params:@
writeAttributeOnLevelWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnLevelWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOnTransitionTimeWithParams:@
readAttributeOnTransitionTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOnTransitionTimeWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeOnTransitionTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOnTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnTransitionTimeWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOnTransitionTimeWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeOffTransitionTimeWithParams:@
readAttributeOffTransitionTimeWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeOffTransitionTimeWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeOffTransitionTimeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOffTransitionTimeWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOffTransitionTimeWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeDefaultMoveRateWithParams:@
readAttributeDefaultMoveRateWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeDefaultMoveRateWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeDefaultMoveRateWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeDefaultMoveRateWithValue:expectedValueInterval:@
writeAttributeDefaultMoveRateWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeDefaultMoveRateWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:@
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeStartUpCurrentLevelWithParams:@
readAttributeStartUpCurrentLevelWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeStartUpCurrentLevelWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeStartUpCurrentLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLevelControl writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_params :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLevelControl -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_params mtrClusterLevelControl dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLevelControl writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRReadParams params) => mtrClusterLevelControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLevelControl params =
  sendMessage mtrClusterLevelControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterLevelControl mtrClusterLevelControl => mtrClusterLevelControl -> IO (Id MTRClusterLevelControl)
init_ mtrClusterLevelControl =
  sendOwnedMessage mtrClusterLevelControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterLevelControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLevelControl"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRDevice device, IsNSObject queue) => mtrClusterLevelControl -> device -> CUShort -> queue -> IO (Id MTRClusterLevelControl)
initWithDevice_endpoint_queue mtrClusterLevelControl device endpoint queue =
  sendOwnedMessage mtrClusterLevelControl initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterMoveToLevelParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- moveWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterMoveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stepWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterStepParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterStopParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToLevelWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterMoveToLevelWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterMoveWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStepWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterStepWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterStopWithOnOffParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterStopWithOnOffParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRLevelControlClusterMoveToClosestFrequencyParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterLevelControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterLevelControl params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterLevelControl moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRLevelControlClusterMoveToClosestFrequencyParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLevelControl mtrClusterLevelControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLevelControl -> device -> endpointID -> queue -> IO (Id MTRClusterLevelControl)
initWithDevice_endpointID_queue mtrClusterLevelControl device endpointID queue =
  sendOwnedMessage mtrClusterLevelControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @moveToLevelWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterMoveToLevelParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToLevelWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToLevelWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveWithParams:expectedValues:expectedValueInterval:completion:@
moveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterMoveParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterStepParams, Id NSArray, Id NSNumber, Ptr ()] ()
stepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completion:@
stopWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterMoveToLevelWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterMoveWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterStepWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterStopWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopWithOnOffWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRLevelControlClusterMoveToClosestFrequencyParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentLevelWithParams:@
readAttributeCurrentLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentLevelWithParamsSelector = mkSelector "readAttributeCurrentLevelWithParams:"

-- | @Selector@ for @readAttributeRemainingTimeWithParams:@
readAttributeRemainingTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRemainingTimeWithParamsSelector = mkSelector "readAttributeRemainingTimeWithParams:"

-- | @Selector@ for @readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinLevelWithParamsSelector = mkSelector "readAttributeMinLevelWithParams:"

-- | @Selector@ for @readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxLevelWithParamsSelector = mkSelector "readAttributeMaxLevelWithParams:"

-- | @Selector@ for @readAttributeCurrentFrequencyWithParams:@
readAttributeCurrentFrequencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentFrequencyWithParamsSelector = mkSelector "readAttributeCurrentFrequencyWithParams:"

-- | @Selector@ for @readAttributeMinFrequencyWithParams:@
readAttributeMinFrequencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinFrequencyWithParamsSelector = mkSelector "readAttributeMinFrequencyWithParams:"

-- | @Selector@ for @readAttributeMaxFrequencyWithParams:@
readAttributeMaxFrequencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxFrequencyWithParamsSelector = mkSelector "readAttributeMaxFrequencyWithParams:"

-- | @Selector@ for @readAttributeOptionsWithParams:@
readAttributeOptionsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOptionsWithParamsSelector = mkSelector "readAttributeOptionsWithParams:"

-- | @Selector@ for @writeAttributeOptionsWithValue:expectedValueInterval:@
writeAttributeOptionsWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOptionsWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOptionsWithValue:expectedValueInterval:params:@
writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOptionsWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOptionsWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnOffTransitionTimeWithParams:@
readAttributeOnOffTransitionTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnOffTransitionTimeWithParamsSelector = mkSelector "readAttributeOnOffTransitionTimeWithParams:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOnOffTransitionTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOnOffTransitionTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnOffTransitionTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnLevelWithParams:@
readAttributeOnLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnLevelWithParamsSelector = mkSelector "readAttributeOnLevelWithParams:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:expectedValueInterval:@
writeAttributeOnLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOnLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnLevelWithValue:expectedValueInterval:params:@
writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOnLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOnTransitionTimeWithParams:@
readAttributeOnTransitionTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOnTransitionTimeWithParamsSelector = mkSelector "readAttributeOnTransitionTimeWithParams:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOnTransitionTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOnTransitionTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOnTransitionTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeOffTransitionTimeWithParams:@
readAttributeOffTransitionTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOffTransitionTimeWithParamsSelector = mkSelector "readAttributeOffTransitionTimeWithParams:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:expectedValueInterval:@
writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOffTransitionTimeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:@
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOffTransitionTimeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOffTransitionTimeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeDefaultMoveRateWithParams:@
readAttributeDefaultMoveRateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultMoveRateWithParamsSelector = mkSelector "readAttributeDefaultMoveRateWithParams:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:expectedValueInterval:@
writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeDefaultMoveRateWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:@
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeDefaultMoveRateWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeDefaultMoveRateWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStartUpCurrentLevelWithParams:@
readAttributeStartUpCurrentLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStartUpCurrentLevelWithParamsSelector = mkSelector "readAttributeStartUpCurrentLevelWithParams:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeStartUpCurrentLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:@
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeStartUpCurrentLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeStartUpCurrentLevelWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterLevelControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterLevelControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterLevelControl)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterMoveToLevelParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToLevelWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToLevelWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterMoveParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterStepParams, Id NSArray, Id NSNumber, Ptr ()] ()
stepWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterStopParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterMoveToLevelWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToLevelWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToLevelWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterMoveWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterStepWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
stepWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stepWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterStopWithOnOffParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopWithOnOffWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopWithOnOffWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:@
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRLevelControlClusterMoveToClosestFrequencyParams, Id NSArray, Id NSNumber, Ptr ()] ()
moveToClosestFrequencyWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "moveToClosestFrequencyWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterLevelControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

