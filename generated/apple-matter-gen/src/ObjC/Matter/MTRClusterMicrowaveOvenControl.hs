{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Microwave Oven Control    Attributes and commands for configuring the microwave oven control, and reporting cooking stats.
--
-- Generated bindings for @MTRClusterMicrowaveOvenControl@.
module ObjC.Matter.MTRClusterMicrowaveOvenControl
  ( MTRClusterMicrowaveOvenControl
  , IsMTRClusterMicrowaveOvenControl(..)
  , setCookingParametersWithParams_expectedValues_expectedValueInterval_completion
  , setCookingParametersWithExpectedValues_expectedValueInterval_completion
  , addMoreTimeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeCookTimeWithParams
  , readAttributeMaxCookTimeWithParams
  , readAttributePowerSettingWithParams
  , readAttributeMinPowerWithParams
  , readAttributeMaxPowerWithParams
  , readAttributePowerStepWithParams
  , readAttributeSupportedWattsWithParams
  , readAttributeSelectedWattIndexWithParams
  , readAttributeWattRatingWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCookTimeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaxCookTimeWithParamsSelector
  , readAttributeMaxPowerWithParamsSelector
  , readAttributeMinPowerWithParamsSelector
  , readAttributePowerSettingWithParamsSelector
  , readAttributePowerStepWithParamsSelector
  , readAttributeSelectedWattIndexWithParamsSelector
  , readAttributeSupportedWattsWithParamsSelector
  , readAttributeWattRatingWithParamsSelector
  , setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector
  , setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:@
setCookingParametersWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterSetCookingParametersParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMicrowaveOvenControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setCookingParametersWithParams_expectedValues_expectedValueInterval_completion mtrClusterMicrowaveOvenControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMicrowaveOvenControl setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMicrowaveOvenControlClusterSetCookingParametersParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setCookingParametersWithExpectedValues:expectedValueInterval:completion:@
setCookingParametersWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterMicrowaveOvenControl -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
setCookingParametersWithExpectedValues_expectedValueInterval_completion mtrClusterMicrowaveOvenControl expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterMicrowaveOvenControl setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:@
addMoreTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRMicrowaveOvenControlClusterAddMoreTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterMicrowaveOvenControl -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
addMoreTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterMicrowaveOvenControl params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterMicrowaveOvenControl addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRMicrowaveOvenControlClusterAddMoreTimeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeCookTimeWithParams:@
readAttributeCookTimeWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeCookTimeWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeCookTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxCookTimeWithParams:@
readAttributeMaxCookTimeWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeMaxCookTimeWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeMaxCookTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerSettingWithParams:@
readAttributePowerSettingWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributePowerSettingWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributePowerSettingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinPowerWithParams:@
readAttributeMinPowerWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeMinPowerWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeMinPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxPowerWithParams:@
readAttributeMaxPowerWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeMaxPowerWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeMaxPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerStepWithParams:@
readAttributePowerStepWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributePowerStepWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributePowerStepWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedWattsWithParams:@
readAttributeSupportedWattsWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeSupportedWattsWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeSupportedWattsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSelectedWattIndexWithParams:@
readAttributeSelectedWattIndexWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeSelectedWattIndexWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeSelectedWattIndexWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWattRatingWithParams:@
readAttributeWattRatingWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeWattRatingWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeWattRatingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRReadParams params) => mtrClusterMicrowaveOvenControl -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterMicrowaveOvenControl params =
  sendMessage mtrClusterMicrowaveOvenControl readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl => mtrClusterMicrowaveOvenControl -> IO (Id MTRClusterMicrowaveOvenControl)
init_ mtrClusterMicrowaveOvenControl =
  sendOwnedMessage mtrClusterMicrowaveOvenControl initSelector

-- | @+ new@
new :: IO (Id MTRClusterMicrowaveOvenControl)
new  =
  do
    cls' <- getRequiredClass "MTRClusterMicrowaveOvenControl"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterMicrowaveOvenControl mtrClusterMicrowaveOvenControl, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterMicrowaveOvenControl -> device -> endpointID -> queue -> IO (Id MTRClusterMicrowaveOvenControl)
initWithDevice_endpointID_queue mtrClusterMicrowaveOvenControl device endpointID queue =
  sendOwnedMessage mtrClusterMicrowaveOvenControl initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:@
setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMicrowaveOvenControlClusterSetCookingParametersParams, Id NSArray, Id NSNumber, Ptr ()] ()
setCookingParametersWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setCookingParametersWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setCookingParametersWithExpectedValues:expectedValueInterval:completion:@
setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
setCookingParametersWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "setCookingParametersWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:@
addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRMicrowaveOvenControlClusterAddMoreTimeParams, Id NSArray, Id NSNumber, Ptr ()] ()
addMoreTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "addMoreTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeCookTimeWithParams:@
readAttributeCookTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCookTimeWithParamsSelector = mkSelector "readAttributeCookTimeWithParams:"

-- | @Selector@ for @readAttributeMaxCookTimeWithParams:@
readAttributeMaxCookTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxCookTimeWithParamsSelector = mkSelector "readAttributeMaxCookTimeWithParams:"

-- | @Selector@ for @readAttributePowerSettingWithParams:@
readAttributePowerSettingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerSettingWithParamsSelector = mkSelector "readAttributePowerSettingWithParams:"

-- | @Selector@ for @readAttributeMinPowerWithParams:@
readAttributeMinPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinPowerWithParamsSelector = mkSelector "readAttributeMinPowerWithParams:"

-- | @Selector@ for @readAttributeMaxPowerWithParams:@
readAttributeMaxPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxPowerWithParamsSelector = mkSelector "readAttributeMaxPowerWithParams:"

-- | @Selector@ for @readAttributePowerStepWithParams:@
readAttributePowerStepWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerStepWithParamsSelector = mkSelector "readAttributePowerStepWithParams:"

-- | @Selector@ for @readAttributeSupportedWattsWithParams:@
readAttributeSupportedWattsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedWattsWithParamsSelector = mkSelector "readAttributeSupportedWattsWithParams:"

-- | @Selector@ for @readAttributeSelectedWattIndexWithParams:@
readAttributeSelectedWattIndexWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSelectedWattIndexWithParamsSelector = mkSelector "readAttributeSelectedWattIndexWithParams:"

-- | @Selector@ for @readAttributeWattRatingWithParams:@
readAttributeWattRatingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWattRatingWithParamsSelector = mkSelector "readAttributeWattRatingWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterMicrowaveOvenControl)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterMicrowaveOvenControl)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterMicrowaveOvenControl)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

