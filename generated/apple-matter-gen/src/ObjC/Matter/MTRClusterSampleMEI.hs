{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Sample MEI    The Sample MEI cluster showcases a cluster manufacturer extensions
--
-- Generated bindings for @MTRClusterSampleMEI@.
module ObjC.Matter.MTRClusterSampleMEI
  ( MTRClusterSampleMEI
  , IsMTRClusterSampleMEI(..)
  , pingWithParams_expectedValues_expectedValueInterval_completion
  , pingWithExpectedValues_expectedValueInterval_completion
  , addArgumentsWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeFlipFlopWithParams
  , writeAttributeFlipFlopWithValue_expectedValueInterval
  , writeAttributeFlipFlopWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , pingWithExpectedValues_expectedValueInterval_completionSelector
  , pingWithParams_expectedValues_expectedValueInterval_completionSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeFlipFlopWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , writeAttributeFlipFlopWithValue_expectedValueIntervalSelector
  , writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pingWithParams:expectedValues:expectedValueInterval:completion:@
pingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRSampleMEIClusterPingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
pingWithParams_expectedValues_expectedValueInterval_completion mtrClusterSampleMEI params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterSampleMEI pingWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRSampleMEIClusterPingParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- pingWithExpectedValues:expectedValueInterval:completion:@
pingWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
pingWithExpectedValues_expectedValueInterval_completion mtrClusterSampleMEI expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterSampleMEI pingWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- addArgumentsWithParams:expectedValues:expectedValueInterval:completion:@
addArgumentsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRSampleMEIClusterAddArgumentsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addArgumentsWithParams_expectedValues_expectedValueInterval_completion mtrClusterSampleMEI params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterSampleMEI addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRSampleMEIClusterAddArgumentsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeFlipFlopWithParams:@
readAttributeFlipFlopWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeFlipFlopWithParams mtrClusterSampleMEI params =
  sendMessage mtrClusterSampleMEI readAttributeFlipFlopWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeFlipFlopWithValue:expectedValueInterval:@
writeAttributeFlipFlopWithValue_expectedValueInterval :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterSampleMEI -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeFlipFlopWithValue_expectedValueInterval mtrClusterSampleMEI dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterSampleMEI writeAttributeFlipFlopWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeFlipFlopWithValue:expectedValueInterval:params:@
writeAttributeFlipFlopWithValue_expectedValueInterval_params :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterSampleMEI -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeFlipFlopWithValue_expectedValueInterval_params mtrClusterSampleMEI dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterSampleMEI writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterSampleMEI params =
  sendMessage mtrClusterSampleMEI readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterSampleMEI params =
  sendMessage mtrClusterSampleMEI readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterSampleMEI params =
  sendMessage mtrClusterSampleMEI readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterSampleMEI params =
  sendMessage mtrClusterSampleMEI readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRReadParams params) => mtrClusterSampleMEI -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterSampleMEI params =
  sendMessage mtrClusterSampleMEI readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterSampleMEI mtrClusterSampleMEI => mtrClusterSampleMEI -> IO (Id MTRClusterSampleMEI)
init_ mtrClusterSampleMEI =
  sendOwnedMessage mtrClusterSampleMEI initSelector

-- | @+ new@
new :: IO (Id MTRClusterSampleMEI)
new  =
  do
    cls' <- getRequiredClass "MTRClusterSampleMEI"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterSampleMEI mtrClusterSampleMEI, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterSampleMEI -> device -> endpointID -> queue -> IO (Id MTRClusterSampleMEI)
initWithDevice_endpointID_queue mtrClusterSampleMEI device endpointID queue =
  sendOwnedMessage mtrClusterSampleMEI initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pingWithParams:expectedValues:expectedValueInterval:completion:@
pingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRSampleMEIClusterPingParams, Id NSArray, Id NSNumber, Ptr ()] ()
pingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "pingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @pingWithExpectedValues:expectedValueInterval:completion:@
pingWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
pingWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "pingWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addArgumentsWithParams:expectedValues:expectedValueInterval:completion:@
addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRSampleMEIClusterAddArgumentsParams, Id NSArray, Id NSNumber, Ptr ()] ()
addArgumentsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addArgumentsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeFlipFlopWithParams:@
readAttributeFlipFlopWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFlipFlopWithParamsSelector = mkSelector "readAttributeFlipFlopWithParams:"

-- | @Selector@ for @writeAttributeFlipFlopWithValue:expectedValueInterval:@
writeAttributeFlipFlopWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeFlipFlopWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeFlipFlopWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeFlipFlopWithValue:expectedValueInterval:params:@
writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeFlipFlopWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeFlipFlopWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterSampleMEI)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterSampleMEI)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterSampleMEI)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

