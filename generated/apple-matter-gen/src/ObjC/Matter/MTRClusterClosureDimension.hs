{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Closure Dimension    This cluster provides an interface to reflect and control a closure's range of movement, usually involving a panel, by using 6-axis framework.
--
-- Generated bindings for @MTRClusterClosureDimension@.
module ObjC.Matter.MTRClusterClosureDimension
  ( MTRClusterClosureDimension
  , IsMTRClusterClosureDimension(..)
  , setTargetWithParams_expectedValues_expectedValueInterval_completion
  , setTargetWithExpectedValues_expectedValueInterval_completion
  , stepWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCurrentStateWithParams
  , readAttributeTargetStateWithParams
  , readAttributeResolutionWithParams
  , readAttributeStepValueWithParams
  , readAttributeUnitWithParams
  , readAttributeUnitRangeWithParams
  , readAttributeLimitRangeWithParams
  , readAttributeTranslationDirectionWithParams
  , readAttributeRotationAxisWithParams
  , readAttributeOverflowWithParams
  , readAttributeModulationTypeWithParams
  , readAttributeLatchControlModesWithParams
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
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentStateWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLatchControlModesWithParamsSelector
  , readAttributeLimitRangeWithParamsSelector
  , readAttributeModulationTypeWithParamsSelector
  , readAttributeOverflowWithParamsSelector
  , readAttributeResolutionWithParamsSelector
  , readAttributeRotationAxisWithParamsSelector
  , readAttributeStepValueWithParamsSelector
  , readAttributeTargetStateWithParamsSelector
  , readAttributeTranslationDirectionWithParamsSelector
  , readAttributeUnitRangeWithParamsSelector
  , readAttributeUnitWithParamsSelector
  , setTargetWithExpectedValues_expectedValueInterval_completionSelector
  , setTargetWithParams_expectedValues_expectedValueInterval_completionSelector
  , stepWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setTargetWithParams:expectedValues:expectedValueInterval:completion:@
setTargetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRClosureDimensionClusterSetTargetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureDimension -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTargetWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureDimension params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureDimension setTargetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRClosureDimensionClusterSetTargetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTargetWithExpectedValues:expectedValueInterval:completion:@
setTargetWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureDimension -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setTargetWithExpectedValues_expectedValueInterval_completion mtrClusterClosureDimension expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureDimension setTargetWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRClosureDimensionClusterStepParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterClosureDimension -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stepWithParams_expectedValues_expectedValueInterval_completion mtrClusterClosureDimension params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterClosureDimension stepWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRClosureDimensionClusterStepParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeCurrentStateWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeCurrentStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeTargetStateWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeTargetStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeResolutionWithParams:@
readAttributeResolutionWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeResolutionWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeResolutionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStepValueWithParams:@
readAttributeStepValueWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeStepValueWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeStepValueWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUnitWithParams:@
readAttributeUnitWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeUnitWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeUnitWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUnitRangeWithParams:@
readAttributeUnitRangeWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeUnitRangeWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeUnitRangeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLimitRangeWithParams:@
readAttributeLimitRangeWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeLimitRangeWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeLimitRangeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTranslationDirectionWithParams:@
readAttributeTranslationDirectionWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeTranslationDirectionWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeTranslationDirectionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRotationAxisWithParams:@
readAttributeRotationAxisWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeRotationAxisWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeRotationAxisWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOverflowWithParams:@
readAttributeOverflowWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeOverflowWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeOverflowWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeModulationTypeWithParams:@
readAttributeModulationTypeWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeModulationTypeWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeModulationTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeLatchControlModesWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeLatchControlModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRReadParams params) => mtrClusterClosureDimension -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterClosureDimension params =
  sendMessage mtrClusterClosureDimension readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterClosureDimension mtrClusterClosureDimension => mtrClusterClosureDimension -> IO (Id MTRClusterClosureDimension)
init_ mtrClusterClosureDimension =
  sendOwnedMessage mtrClusterClosureDimension initSelector

-- | @+ new@
new :: IO (Id MTRClusterClosureDimension)
new  =
  do
    cls' <- getRequiredClass "MTRClusterClosureDimension"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterClosureDimension mtrClusterClosureDimension, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterClosureDimension -> device -> endpointID -> queue -> IO (Id MTRClusterClosureDimension)
initWithDevice_endpointID_queue mtrClusterClosureDimension device endpointID queue =
  sendOwnedMessage mtrClusterClosureDimension initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTargetWithParams:expectedValues:expectedValueInterval:completion:@
setTargetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRClosureDimensionClusterSetTargetParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTargetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTargetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTargetWithExpectedValues:expectedValueInterval:completion:@
setTargetWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
setTargetWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setTargetWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stepWithParams:expectedValues:expectedValueInterval:completion:@
stepWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRClosureDimensionClusterStepParams, Id NSArray, Id NSNumber, Ptr ()] ()
stepWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stepWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCurrentStateWithParams:@
readAttributeCurrentStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentStateWithParamsSelector = mkSelector "readAttributeCurrentStateWithParams:"

-- | @Selector@ for @readAttributeTargetStateWithParams:@
readAttributeTargetStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTargetStateWithParamsSelector = mkSelector "readAttributeTargetStateWithParams:"

-- | @Selector@ for @readAttributeResolutionWithParams:@
readAttributeResolutionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeResolutionWithParamsSelector = mkSelector "readAttributeResolutionWithParams:"

-- | @Selector@ for @readAttributeStepValueWithParams:@
readAttributeStepValueWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStepValueWithParamsSelector = mkSelector "readAttributeStepValueWithParams:"

-- | @Selector@ for @readAttributeUnitWithParams:@
readAttributeUnitWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUnitWithParamsSelector = mkSelector "readAttributeUnitWithParams:"

-- | @Selector@ for @readAttributeUnitRangeWithParams:@
readAttributeUnitRangeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUnitRangeWithParamsSelector = mkSelector "readAttributeUnitRangeWithParams:"

-- | @Selector@ for @readAttributeLimitRangeWithParams:@
readAttributeLimitRangeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLimitRangeWithParamsSelector = mkSelector "readAttributeLimitRangeWithParams:"

-- | @Selector@ for @readAttributeTranslationDirectionWithParams:@
readAttributeTranslationDirectionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTranslationDirectionWithParamsSelector = mkSelector "readAttributeTranslationDirectionWithParams:"

-- | @Selector@ for @readAttributeRotationAxisWithParams:@
readAttributeRotationAxisWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRotationAxisWithParamsSelector = mkSelector "readAttributeRotationAxisWithParams:"

-- | @Selector@ for @readAttributeOverflowWithParams:@
readAttributeOverflowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOverflowWithParamsSelector = mkSelector "readAttributeOverflowWithParams:"

-- | @Selector@ for @readAttributeModulationTypeWithParams:@
readAttributeModulationTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeModulationTypeWithParamsSelector = mkSelector "readAttributeModulationTypeWithParams:"

-- | @Selector@ for @readAttributeLatchControlModesWithParams:@
readAttributeLatchControlModesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLatchControlModesWithParamsSelector = mkSelector "readAttributeLatchControlModesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterClosureDimension)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterClosureDimension)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterClosureDimension)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

