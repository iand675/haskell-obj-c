{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy EVSE    Electric Vehicle Supply Equipment (EVSE) is equipment used to charge an Electric Vehicle (EV) or Plug-In Hybrid Electric Vehicle. This cluster provides an interface to the functionality of Electric Vehicle Supply Equipment (EVSE) management.
--
-- Generated bindings for @MTRClusterEnergyEVSE@.
module ObjC.Matter.MTRClusterEnergyEVSE
  ( MTRClusterEnergyEVSE
  , IsMTRClusterEnergyEVSE(..)
  , disableWithParams_expectedValues_expectedValueInterval_completion
  , disableWithExpectedValues_expectedValueInterval_completion
  , enableChargingWithParams_expectedValues_expectedValueInterval_completion
  , enableDischargingWithParams_expectedValues_expectedValueInterval_completion
  , startDiagnosticsWithParams_expectedValues_expectedValueInterval_completion
  , startDiagnosticsWithExpectedValues_expectedValueInterval_completion
  , setTargetsWithParams_expectedValues_expectedValueInterval_completion
  , getTargetsWithParams_expectedValues_expectedValueInterval_completion
  , getTargetsWithExpectedValues_expectedValueInterval_completion
  , clearTargetsWithParams_expectedValues_expectedValueInterval_completion
  , clearTargetsWithExpectedValues_expectedValueInterval_completion
  , readAttributeStateWithParams
  , readAttributeSupplyStateWithParams
  , readAttributeFaultStateWithParams
  , readAttributeChargingEnabledUntilWithParams
  , readAttributeDischargingEnabledUntilWithParams
  , readAttributeCircuitCapacityWithParams
  , readAttributeMinimumChargeCurrentWithParams
  , readAttributeMaximumChargeCurrentWithParams
  , readAttributeMaximumDischargeCurrentWithParams
  , readAttributeUserMaximumChargeCurrentWithParams
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_params
  , readAttributeRandomizationDelayWindowWithParams
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_params
  , readAttributeNextChargeStartTimeWithParams
  , readAttributeNextChargeTargetTimeWithParams
  , readAttributeNextChargeRequiredEnergyWithParams
  , readAttributeNextChargeTargetSoCWithParams
  , readAttributeApproximateEVEfficiencyWithParams
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_params
  , readAttributeStateOfChargeWithParams
  , readAttributeBatteryCapacityWithParams
  , readAttributeVehicleIDWithParams
  , readAttributeSessionIDWithParams
  , readAttributeSessionDurationWithParams
  , readAttributeSessionEnergyChargedWithParams
  , readAttributeSessionEnergyDischargedWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , clearTargetsWithExpectedValues_expectedValueInterval_completionSelector
  , clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector
  , disableWithExpectedValues_expectedValueInterval_completionSelector
  , disableWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector
  , enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector
  , getTargetsWithExpectedValues_expectedValueInterval_completionSelector
  , getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeApproximateEVEfficiencyWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBatteryCapacityWithParamsSelector
  , readAttributeChargingEnabledUntilWithParamsSelector
  , readAttributeCircuitCapacityWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDischargingEnabledUntilWithParamsSelector
  , readAttributeFaultStateWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeMaximumChargeCurrentWithParamsSelector
  , readAttributeMaximumDischargeCurrentWithParamsSelector
  , readAttributeMinimumChargeCurrentWithParamsSelector
  , readAttributeNextChargeRequiredEnergyWithParamsSelector
  , readAttributeNextChargeStartTimeWithParamsSelector
  , readAttributeNextChargeTargetSoCWithParamsSelector
  , readAttributeNextChargeTargetTimeWithParamsSelector
  , readAttributeRandomizationDelayWindowWithParamsSelector
  , readAttributeSessionDurationWithParamsSelector
  , readAttributeSessionEnergyChargedWithParamsSelector
  , readAttributeSessionEnergyDischargedWithParamsSelector
  , readAttributeSessionIDWithParamsSelector
  , readAttributeStateOfChargeWithParamsSelector
  , readAttributeStateWithParamsSelector
  , readAttributeSupplyStateWithParamsSelector
  , readAttributeUserMaximumChargeCurrentWithParamsSelector
  , readAttributeVehicleIDWithParamsSelector
  , setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector
  , startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector
  , startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector
  , writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector
  , writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector
  , writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterDisableParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE disableWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterDisableParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
disableWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE disableWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- enableChargingWithParams:expectedValues:expectedValueInterval:completion:@
enableChargingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableChargingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableChargingWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterEnableChargingParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- enableDischargingWithParams:expectedValues:expectedValueInterval:completion:@
enableDischargingWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterEnableDischargingParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
enableDischargingWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterEnableDischargingParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:@
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterStartDiagnosticsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterStartDiagnosticsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- startDiagnosticsWithExpectedValues:expectedValueInterval:completion:@
startDiagnosticsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
startDiagnosticsWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTargetsWithParams:expectedValues:expectedValueInterval:completion:@
setTargetsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterSetTargetsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTargetsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterSetTargetsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getTargetsWithParams:expectedValues:expectedValueInterval:completion:@
getTargetsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterGetTargetsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getTargetsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterGetTargetsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getTargetsWithExpectedValues:expectedValueInterval:completion:@
getTargetsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getTargetsWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE getTargetsWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- clearTargetsWithParams:expectedValues:expectedValueInterval:completion:@
clearTargetsWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTREnergyEVSEClusterClearTargetsParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
clearTargetsWithParams_expectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector (toMTREnergyEVSEClusterClearTargetsParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- clearTargetsWithExpectedValues:expectedValueInterval:completion:@
clearTargetsWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
clearTargetsWithExpectedValues_expectedValueInterval_completion mtrClusterEnergyEVSE expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterEnergyEVSE clearTargetsWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeStateWithParams:@
readAttributeStateWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeStateWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupplyStateWithParams:@
readAttributeSupplyStateWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSupplyStateWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeSupplyStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFaultStateWithParams:@
readAttributeFaultStateWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeFaultStateWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeFaultStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeChargingEnabledUntilWithParams:@
readAttributeChargingEnabledUntilWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeChargingEnabledUntilWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeChargingEnabledUntilWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDischargingEnabledUntilWithParams:@
readAttributeDischargingEnabledUntilWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeDischargingEnabledUntilWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeDischargingEnabledUntilWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCircuitCapacityWithParams:@
readAttributeCircuitCapacityWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeCircuitCapacityWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeCircuitCapacityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinimumChargeCurrentWithParams:@
readAttributeMinimumChargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeMinimumChargeCurrentWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeMinimumChargeCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaximumChargeCurrentWithParams:@
readAttributeMaximumChargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeMaximumChargeCurrentWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeMaximumChargeCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaximumDischargeCurrentWithParams:@
readAttributeMaximumDischargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeMaximumDischargeCurrentWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeMaximumDischargeCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUserMaximumChargeCurrentWithParams:@
readAttributeUserMaximumChargeCurrentWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeUserMaximumChargeCurrentWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeUserMaximumChargeCurrentWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval mtrClusterEnergyEVSE dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterEnergyEVSE writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_params mtrClusterEnergyEVSE dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterEnergyEVSE writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeRandomizationDelayWindowWithParams:@
readAttributeRandomizationDelayWindowWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeRandomizationDelayWindowWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeRandomizationDelayWindowWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval mtrClusterEnergyEVSE dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterEnergyEVSE writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_params mtrClusterEnergyEVSE dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterEnergyEVSE writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeNextChargeStartTimeWithParams:@
readAttributeNextChargeStartTimeWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeStartTimeWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeNextChargeStartTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextChargeTargetTimeWithParams:@
readAttributeNextChargeTargetTimeWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeTargetTimeWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeNextChargeTargetTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextChargeRequiredEnergyWithParams:@
readAttributeNextChargeRequiredEnergyWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeRequiredEnergyWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeNextChargeRequiredEnergyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNextChargeTargetSoCWithParams:@
readAttributeNextChargeTargetSoCWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeNextChargeTargetSoCWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeNextChargeTargetSoCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApproximateEVEfficiencyWithParams:@
readAttributeApproximateEVEfficiencyWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeApproximateEVEfficiencyWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeApproximateEVEfficiencyWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval mtrClusterEnergyEVSE dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterEnergyEVSE writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyEVSE -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_params mtrClusterEnergyEVSE dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterEnergyEVSE writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeStateOfChargeWithParams:@
readAttributeStateOfChargeWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeStateOfChargeWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeStateOfChargeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatteryCapacityWithParams:@
readAttributeBatteryCapacityWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeBatteryCapacityWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeBatteryCapacityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeVehicleIDWithParams:@
readAttributeVehicleIDWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeVehicleIDWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeVehicleIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSessionIDWithParams:@
readAttributeSessionIDWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionIDWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeSessionIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSessionDurationWithParams:@
readAttributeSessionDurationWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionDurationWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeSessionDurationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSessionEnergyChargedWithParams:@
readAttributeSessionEnergyChargedWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionEnergyChargedWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeSessionEnergyChargedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSessionEnergyDischargedWithParams:@
readAttributeSessionEnergyDischargedWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeSessionEnergyDischargedWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeSessionEnergyDischargedWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRReadParams params) => mtrClusterEnergyEVSE -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEnergyEVSE params =
  sendMessage mtrClusterEnergyEVSE readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE => mtrClusterEnergyEVSE -> IO (Id MTRClusterEnergyEVSE)
init_ mtrClusterEnergyEVSE =
  sendOwnedMessage mtrClusterEnergyEVSE initSelector

-- | @+ new@
new :: IO (Id MTRClusterEnergyEVSE)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEnergyEVSE"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEnergyEVSE mtrClusterEnergyEVSE, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEnergyEVSE -> device -> endpointID -> queue -> IO (Id MTRClusterEnergyEVSE)
initWithDevice_endpointID_queue mtrClusterEnergyEVSE device endpointID queue =
  sendOwnedMessage mtrClusterEnergyEVSE initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disableWithParams:expectedValues:expectedValueInterval:completion:@
disableWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterDisableParams, Id NSArray, Id NSNumber, Ptr ()] ()
disableWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @disableWithExpectedValues:expectedValueInterval:completion:@
disableWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
disableWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "disableWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableChargingWithParams:expectedValues:expectedValueInterval:completion:@
enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterEnableChargingParams, Id NSArray, Id NSNumber, Ptr ()] ()
enableChargingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableChargingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @enableDischargingWithParams:expectedValues:expectedValueInterval:completion:@
enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterEnableDischargingParams, Id NSArray, Id NSNumber, Ptr ()] ()
enableDischargingWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "enableDischargingWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:@
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterStartDiagnosticsParams, Id NSArray, Id NSNumber, Ptr ()] ()
startDiagnosticsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "startDiagnosticsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @startDiagnosticsWithExpectedValues:expectedValueInterval:completion:@
startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
startDiagnosticsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "startDiagnosticsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTargetsWithParams:expectedValues:expectedValueInterval:completion:@
setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterSetTargetsParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTargetsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTargetsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getTargetsWithParams:expectedValues:expectedValueInterval:completion:@
getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterGetTargetsParams, Id NSArray, Id NSNumber, Ptr ()] ()
getTargetsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getTargetsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getTargetsWithExpectedValues:expectedValueInterval:completion:@
getTargetsWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
getTargetsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getTargetsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearTargetsWithParams:expectedValues:expectedValueInterval:completion:@
clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTREnergyEVSEClusterClearTargetsParams, Id NSArray, Id NSNumber, Ptr ()] ()
clearTargetsWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "clearTargetsWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @clearTargetsWithExpectedValues:expectedValueInterval:completion:@
clearTargetsWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
clearTargetsWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "clearTargetsWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeStateWithParams:@
readAttributeStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStateWithParamsSelector = mkSelector "readAttributeStateWithParams:"

-- | @Selector@ for @readAttributeSupplyStateWithParams:@
readAttributeSupplyStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupplyStateWithParamsSelector = mkSelector "readAttributeSupplyStateWithParams:"

-- | @Selector@ for @readAttributeFaultStateWithParams:@
readAttributeFaultStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFaultStateWithParamsSelector = mkSelector "readAttributeFaultStateWithParams:"

-- | @Selector@ for @readAttributeChargingEnabledUntilWithParams:@
readAttributeChargingEnabledUntilWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeChargingEnabledUntilWithParamsSelector = mkSelector "readAttributeChargingEnabledUntilWithParams:"

-- | @Selector@ for @readAttributeDischargingEnabledUntilWithParams:@
readAttributeDischargingEnabledUntilWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDischargingEnabledUntilWithParamsSelector = mkSelector "readAttributeDischargingEnabledUntilWithParams:"

-- | @Selector@ for @readAttributeCircuitCapacityWithParams:@
readAttributeCircuitCapacityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCircuitCapacityWithParamsSelector = mkSelector "readAttributeCircuitCapacityWithParams:"

-- | @Selector@ for @readAttributeMinimumChargeCurrentWithParams:@
readAttributeMinimumChargeCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinimumChargeCurrentWithParamsSelector = mkSelector "readAttributeMinimumChargeCurrentWithParams:"

-- | @Selector@ for @readAttributeMaximumChargeCurrentWithParams:@
readAttributeMaximumChargeCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaximumChargeCurrentWithParamsSelector = mkSelector "readAttributeMaximumChargeCurrentWithParams:"

-- | @Selector@ for @readAttributeMaximumDischargeCurrentWithParams:@
readAttributeMaximumDischargeCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaximumDischargeCurrentWithParamsSelector = mkSelector "readAttributeMaximumDischargeCurrentWithParams:"

-- | @Selector@ for @readAttributeUserMaximumChargeCurrentWithParams:@
readAttributeUserMaximumChargeCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUserMaximumChargeCurrentWithParamsSelector = mkSelector "readAttributeUserMaximumChargeCurrentWithParams:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:@
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeUserMaximumChargeCurrentWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeUserMaximumChargeCurrentWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRandomizationDelayWindowWithParams:@
readAttributeRandomizationDelayWindowWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRandomizationDelayWindowWithParamsSelector = mkSelector "readAttributeRandomizationDelayWindowWithParams:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeRandomizationDelayWindowWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:@
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeRandomizationDelayWindowWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRandomizationDelayWindowWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNextChargeStartTimeWithParams:@
readAttributeNextChargeStartTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextChargeStartTimeWithParamsSelector = mkSelector "readAttributeNextChargeStartTimeWithParams:"

-- | @Selector@ for @readAttributeNextChargeTargetTimeWithParams:@
readAttributeNextChargeTargetTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextChargeTargetTimeWithParamsSelector = mkSelector "readAttributeNextChargeTargetTimeWithParams:"

-- | @Selector@ for @readAttributeNextChargeRequiredEnergyWithParams:@
readAttributeNextChargeRequiredEnergyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextChargeRequiredEnergyWithParamsSelector = mkSelector "readAttributeNextChargeRequiredEnergyWithParams:"

-- | @Selector@ for @readAttributeNextChargeTargetSoCWithParams:@
readAttributeNextChargeTargetSoCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNextChargeTargetSoCWithParamsSelector = mkSelector "readAttributeNextChargeTargetSoCWithParams:"

-- | @Selector@ for @readAttributeApproximateEVEfficiencyWithParams:@
readAttributeApproximateEVEfficiencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApproximateEVEfficiencyWithParamsSelector = mkSelector "readAttributeApproximateEVEfficiencyWithParams:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeApproximateEVEfficiencyWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:@
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeApproximateEVEfficiencyWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeApproximateEVEfficiencyWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeStateOfChargeWithParams:@
readAttributeStateOfChargeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStateOfChargeWithParamsSelector = mkSelector "readAttributeStateOfChargeWithParams:"

-- | @Selector@ for @readAttributeBatteryCapacityWithParams:@
readAttributeBatteryCapacityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatteryCapacityWithParamsSelector = mkSelector "readAttributeBatteryCapacityWithParams:"

-- | @Selector@ for @readAttributeVehicleIDWithParams:@
readAttributeVehicleIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVehicleIDWithParamsSelector = mkSelector "readAttributeVehicleIDWithParams:"

-- | @Selector@ for @readAttributeSessionIDWithParams:@
readAttributeSessionIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSessionIDWithParamsSelector = mkSelector "readAttributeSessionIDWithParams:"

-- | @Selector@ for @readAttributeSessionDurationWithParams:@
readAttributeSessionDurationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSessionDurationWithParamsSelector = mkSelector "readAttributeSessionDurationWithParams:"

-- | @Selector@ for @readAttributeSessionEnergyChargedWithParams:@
readAttributeSessionEnergyChargedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSessionEnergyChargedWithParamsSelector = mkSelector "readAttributeSessionEnergyChargedWithParams:"

-- | @Selector@ for @readAttributeSessionEnergyDischargedWithParams:@
readAttributeSessionEnergyDischargedWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSessionEnergyDischargedWithParamsSelector = mkSelector "readAttributeSessionEnergyDischargedWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterEnergyEVSE)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterEnergyEVSE)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterEnergyEVSE)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

