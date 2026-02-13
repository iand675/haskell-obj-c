{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Electrical Measurement    Attributes related to the electrical properties of a device. This cluster is used by power outlets and other devices that need to provide instantaneous data as opposed to metrology data which should be retrieved from the metering cluster..
--
-- Generated bindings for @MTRClusterElectricalMeasurement@.
module ObjC.Matter.MTRClusterElectricalMeasurement
  ( MTRClusterElectricalMeasurement
  , IsMTRClusterElectricalMeasurement(..)
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completion
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completion
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeMeasurementTypeWithParams
  , readAttributeDcVoltageWithParams
  , readAttributeDcVoltageMinWithParams
  , readAttributeDcVoltageMaxWithParams
  , readAttributeDcCurrentWithParams
  , readAttributeDcCurrentMinWithParams
  , readAttributeDcCurrentMaxWithParams
  , readAttributeDcPowerWithParams
  , readAttributeDcPowerMinWithParams
  , readAttributeDcPowerMaxWithParams
  , readAttributeDcVoltageMultiplierWithParams
  , readAttributeDcVoltageDivisorWithParams
  , readAttributeDcCurrentMultiplierWithParams
  , readAttributeDcCurrentDivisorWithParams
  , readAttributeDcPowerMultiplierWithParams
  , readAttributeDcPowerDivisorWithParams
  , readAttributeAcFrequencyWithParams
  , readAttributeAcFrequencyMinWithParams
  , readAttributeAcFrequencyMaxWithParams
  , readAttributeNeutralCurrentWithParams
  , readAttributeTotalActivePowerWithParams
  , readAttributeTotalReactivePowerWithParams
  , readAttributeTotalApparentPowerWithParams
  , readAttributeMeasured1stHarmonicCurrentWithParams
  , readAttributeMeasured3rdHarmonicCurrentWithParams
  , readAttributeMeasured5thHarmonicCurrentWithParams
  , readAttributeMeasured7thHarmonicCurrentWithParams
  , readAttributeMeasured9thHarmonicCurrentWithParams
  , readAttributeMeasured11thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase1stHarmonicCurrentWithParams
  , readAttributeMeasuredPhase3rdHarmonicCurrentWithParams
  , readAttributeMeasuredPhase5thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase7thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase9thHarmonicCurrentWithParams
  , readAttributeMeasuredPhase11thHarmonicCurrentWithParams
  , readAttributeAcFrequencyMultiplierWithParams
  , readAttributeAcFrequencyDivisorWithParams
  , readAttributePowerMultiplierWithParams
  , readAttributePowerDivisorWithParams
  , readAttributeHarmonicCurrentMultiplierWithParams
  , readAttributePhaseHarmonicCurrentMultiplierWithParams
  , readAttributeInstantaneousVoltageWithParams
  , readAttributeInstantaneousLineCurrentWithParams
  , readAttributeInstantaneousActiveCurrentWithParams
  , readAttributeInstantaneousReactiveCurrentWithParams
  , readAttributeInstantaneousPowerWithParams
  , readAttributeRmsVoltageWithParams
  , readAttributeRmsVoltageMinWithParams
  , readAttributeRmsVoltageMaxWithParams
  , readAttributeRmsCurrentWithParams
  , readAttributeRmsCurrentMinWithParams
  , readAttributeRmsCurrentMaxWithParams
  , readAttributeActivePowerWithParams
  , readAttributeActivePowerMinWithParams
  , readAttributeActivePowerMaxWithParams
  , readAttributeReactivePowerWithParams
  , readAttributeApparentPowerWithParams
  , readAttributePowerFactorWithParams
  , readAttributeAverageRmsVoltageMeasurementPeriodWithParams
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_params
  , readAttributeAverageRmsUnderVoltageCounterWithParams
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_params
  , readAttributeRmsExtremeOverVoltagePeriodWithParams
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_params
  , readAttributeRmsExtremeUnderVoltagePeriodWithParams
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_params
  , readAttributeRmsVoltageSagPeriodWithParams
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_params
  , readAttributeRmsVoltageSwellPeriodWithParams
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_params
  , readAttributeAcVoltageMultiplierWithParams
  , readAttributeAcVoltageDivisorWithParams
  , readAttributeAcCurrentMultiplierWithParams
  , readAttributeAcCurrentDivisorWithParams
  , readAttributeAcPowerMultiplierWithParams
  , readAttributeAcPowerDivisorWithParams
  , readAttributeOverloadAlarmsMaskWithParams
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_params
  , readAttributeVoltageOverloadWithParams
  , readAttributeCurrentOverloadWithParams
  , readAttributeAcOverloadAlarmsMaskWithParams
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_params
  , readAttributeAcVoltageOverloadWithParams
  , readAttributeAcCurrentOverloadWithParams
  , readAttributeAcActivePowerOverloadWithParams
  , readAttributeAcReactivePowerOverloadWithParams
  , readAttributeAverageRmsOverVoltageWithParams
  , readAttributeAverageRmsUnderVoltageWithParams
  , readAttributeRmsExtremeOverVoltageWithParams
  , readAttributeRmsExtremeUnderVoltageWithParams
  , readAttributeRmsVoltageSagWithParams
  , readAttributeRmsVoltageSwellWithParams
  , readAttributeLineCurrentPhaseBWithParams
  , readAttributeActiveCurrentPhaseBWithParams
  , readAttributeReactiveCurrentPhaseBWithParams
  , readAttributeRmsVoltagePhaseBWithParams
  , readAttributeRmsVoltageMinPhaseBWithParams
  , readAttributeRmsVoltageMaxPhaseBWithParams
  , readAttributeRmsCurrentPhaseBWithParams
  , readAttributeRmsCurrentMinPhaseBWithParams
  , readAttributeRmsCurrentMaxPhaseBWithParams
  , readAttributeActivePowerPhaseBWithParams
  , readAttributeActivePowerMinPhaseBWithParams
  , readAttributeActivePowerMaxPhaseBWithParams
  , readAttributeReactivePowerPhaseBWithParams
  , readAttributeApparentPowerPhaseBWithParams
  , readAttributePowerFactorPhaseBWithParams
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams
  , readAttributeAverageRmsOverVoltageCounterPhaseBWithParams
  , readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams
  , readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams
  , readAttributeRmsVoltageSagPeriodPhaseBWithParams
  , readAttributeRmsVoltageSwellPeriodPhaseBWithParams
  , readAttributeLineCurrentPhaseCWithParams
  , readAttributeActiveCurrentPhaseCWithParams
  , readAttributeReactiveCurrentPhaseCWithParams
  , readAttributeRmsVoltagePhaseCWithParams
  , readAttributeRmsVoltageMinPhaseCWithParams
  , readAttributeRmsVoltageMaxPhaseCWithParams
  , readAttributeRmsCurrentPhaseCWithParams
  , readAttributeRmsCurrentMinPhaseCWithParams
  , readAttributeRmsCurrentMaxPhaseCWithParams
  , readAttributeActivePowerPhaseCWithParams
  , readAttributeActivePowerMinPhaseCWithParams
  , readAttributeActivePowerMaxPhaseCWithParams
  , readAttributeReactivePowerPhaseCWithParams
  , readAttributeApparentPowerPhaseCWithParams
  , readAttributePowerFactorPhaseCWithParams
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams
  , readAttributeAverageRmsOverVoltageCounterPhaseCWithParams
  , readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams
  , readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams
  , readAttributeRmsVoltageSagPeriodPhaseCWithParams
  , readAttributeRmsVoltageSwellPeriodPhaseCWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandler
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandler
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcActivePowerOverloadWithParamsSelector
  , readAttributeAcCurrentDivisorWithParamsSelector
  , readAttributeAcCurrentMultiplierWithParamsSelector
  , readAttributeAcCurrentOverloadWithParamsSelector
  , readAttributeAcFrequencyDivisorWithParamsSelector
  , readAttributeAcFrequencyMaxWithParamsSelector
  , readAttributeAcFrequencyMinWithParamsSelector
  , readAttributeAcFrequencyMultiplierWithParamsSelector
  , readAttributeAcFrequencyWithParamsSelector
  , readAttributeAcOverloadAlarmsMaskWithParamsSelector
  , readAttributeAcPowerDivisorWithParamsSelector
  , readAttributeAcPowerMultiplierWithParamsSelector
  , readAttributeAcReactivePowerOverloadWithParamsSelector
  , readAttributeAcVoltageDivisorWithParamsSelector
  , readAttributeAcVoltageMultiplierWithParamsSelector
  , readAttributeAcVoltageOverloadWithParamsSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveCurrentPhaseBWithParamsSelector
  , readAttributeActiveCurrentPhaseCWithParamsSelector
  , readAttributeActivePowerMaxPhaseBWithParamsSelector
  , readAttributeActivePowerMaxPhaseCWithParamsSelector
  , readAttributeActivePowerMaxWithParamsSelector
  , readAttributeActivePowerMinPhaseBWithParamsSelector
  , readAttributeActivePowerMinPhaseCWithParamsSelector
  , readAttributeActivePowerMinWithParamsSelector
  , readAttributeActivePowerPhaseBWithParamsSelector
  , readAttributeActivePowerPhaseCWithParamsSelector
  , readAttributeActivePowerWithParamsSelector
  , readAttributeApparentPowerPhaseBWithParamsSelector
  , readAttributeApparentPowerPhaseCWithParamsSelector
  , readAttributeApparentPowerWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector
  , readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector
  , readAttributeAverageRmsOverVoltageWithParamsSelector
  , readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector
  , readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector
  , readAttributeAverageRmsUnderVoltageCounterWithParamsSelector
  , readAttributeAverageRmsUnderVoltageWithParamsSelector
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector
  , readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector
  , readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentOverloadWithParamsSelector
  , readAttributeDcCurrentDivisorWithParamsSelector
  , readAttributeDcCurrentMaxWithParamsSelector
  , readAttributeDcCurrentMinWithParamsSelector
  , readAttributeDcCurrentMultiplierWithParamsSelector
  , readAttributeDcCurrentWithParamsSelector
  , readAttributeDcPowerDivisorWithParamsSelector
  , readAttributeDcPowerMaxWithParamsSelector
  , readAttributeDcPowerMinWithParamsSelector
  , readAttributeDcPowerMultiplierWithParamsSelector
  , readAttributeDcPowerWithParamsSelector
  , readAttributeDcVoltageDivisorWithParamsSelector
  , readAttributeDcVoltageMaxWithParamsSelector
  , readAttributeDcVoltageMinWithParamsSelector
  , readAttributeDcVoltageMultiplierWithParamsSelector
  , readAttributeDcVoltageWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHarmonicCurrentMultiplierWithParamsSelector
  , readAttributeInstantaneousActiveCurrentWithParamsSelector
  , readAttributeInstantaneousLineCurrentWithParamsSelector
  , readAttributeInstantaneousPowerWithParamsSelector
  , readAttributeInstantaneousReactiveCurrentWithParamsSelector
  , readAttributeInstantaneousVoltageWithParamsSelector
  , readAttributeLineCurrentPhaseBWithParamsSelector
  , readAttributeLineCurrentPhaseCWithParamsSelector
  , readAttributeMeasured11thHarmonicCurrentWithParamsSelector
  , readAttributeMeasured1stHarmonicCurrentWithParamsSelector
  , readAttributeMeasured3rdHarmonicCurrentWithParamsSelector
  , readAttributeMeasured5thHarmonicCurrentWithParamsSelector
  , readAttributeMeasured7thHarmonicCurrentWithParamsSelector
  , readAttributeMeasured9thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector
  , readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector
  , readAttributeMeasurementTypeWithParamsSelector
  , readAttributeNeutralCurrentWithParamsSelector
  , readAttributeOverloadAlarmsMaskWithParamsSelector
  , readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector
  , readAttributePowerDivisorWithParamsSelector
  , readAttributePowerFactorPhaseBWithParamsSelector
  , readAttributePowerFactorPhaseCWithParamsSelector
  , readAttributePowerFactorWithParamsSelector
  , readAttributePowerMultiplierWithParamsSelector
  , readAttributeReactiveCurrentPhaseBWithParamsSelector
  , readAttributeReactiveCurrentPhaseCWithParamsSelector
  , readAttributeReactivePowerPhaseBWithParamsSelector
  , readAttributeReactivePowerPhaseCWithParamsSelector
  , readAttributeReactivePowerWithParamsSelector
  , readAttributeRmsCurrentMaxPhaseBWithParamsSelector
  , readAttributeRmsCurrentMaxPhaseCWithParamsSelector
  , readAttributeRmsCurrentMaxWithParamsSelector
  , readAttributeRmsCurrentMinPhaseBWithParamsSelector
  , readAttributeRmsCurrentMinPhaseCWithParamsSelector
  , readAttributeRmsCurrentMinWithParamsSelector
  , readAttributeRmsCurrentPhaseBWithParamsSelector
  , readAttributeRmsCurrentPhaseCWithParamsSelector
  , readAttributeRmsCurrentWithParamsSelector
  , readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector
  , readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector
  , readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector
  , readAttributeRmsExtremeOverVoltageWithParamsSelector
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector
  , readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector
  , readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector
  , readAttributeRmsExtremeUnderVoltageWithParamsSelector
  , readAttributeRmsVoltageMaxPhaseBWithParamsSelector
  , readAttributeRmsVoltageMaxPhaseCWithParamsSelector
  , readAttributeRmsVoltageMaxWithParamsSelector
  , readAttributeRmsVoltageMinPhaseBWithParamsSelector
  , readAttributeRmsVoltageMinPhaseCWithParamsSelector
  , readAttributeRmsVoltageMinWithParamsSelector
  , readAttributeRmsVoltagePhaseBWithParamsSelector
  , readAttributeRmsVoltagePhaseCWithParamsSelector
  , readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector
  , readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector
  , readAttributeRmsVoltageSagPeriodWithParamsSelector
  , readAttributeRmsVoltageSagWithParamsSelector
  , readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector
  , readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector
  , readAttributeRmsVoltageSwellPeriodWithParamsSelector
  , readAttributeRmsVoltageSwellWithParamsSelector
  , readAttributeRmsVoltageWithParamsSelector
  , readAttributeTotalActivePowerWithParamsSelector
  , readAttributeTotalApparentPowerWithParamsSelector
  , readAttributeTotalReactivePowerWithParamsSelector
  , readAttributeVoltageOverloadWithParamsSelector
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector
  , writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector
  , writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector
  , writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector
  , writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetProfileInfoCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completion mtrClusterElectricalMeasurement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterElectricalMeasurement getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRElectricalMeasurementClusterGetProfileInfoCommandParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completion mtrClusterElectricalMeasurement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterElectricalMeasurement getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completion mtrClusterElectricalMeasurement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterElectricalMeasurement getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeMeasurementTypeWithParams:@
readAttributeMeasurementTypeWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasurementTypeWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasurementTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcVoltageWithParams:@
readAttributeDcVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcVoltageMinWithParams:@
readAttributeDcVoltageMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcVoltageMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcVoltageMaxWithParams:@
readAttributeDcVoltageMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcVoltageMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcCurrentWithParams:@
readAttributeDcCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcCurrentMinWithParams:@
readAttributeDcCurrentMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcCurrentMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcCurrentMaxWithParams:@
readAttributeDcCurrentMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcCurrentMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcPowerWithParams:@
readAttributeDcPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcPowerMinWithParams:@
readAttributeDcPowerMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcPowerMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcPowerMaxWithParams:@
readAttributeDcPowerMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcPowerMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcVoltageMultiplierWithParams:@
readAttributeDcVoltageMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcVoltageMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcVoltageDivisorWithParams:@
readAttributeDcVoltageDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcVoltageDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcVoltageDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcCurrentMultiplierWithParams:@
readAttributeDcCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcCurrentMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcCurrentDivisorWithParams:@
readAttributeDcCurrentDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcCurrentDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcCurrentDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcPowerMultiplierWithParams:@
readAttributeDcPowerMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcPowerMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDcPowerDivisorWithParams:@
readAttributeDcPowerDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeDcPowerDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeDcPowerDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcFrequencyWithParams:@
readAttributeAcFrequencyWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcFrequencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcFrequencyMinWithParams:@
readAttributeAcFrequencyMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcFrequencyMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcFrequencyMaxWithParams:@
readAttributeAcFrequencyMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcFrequencyMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeNeutralCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeNeutralCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTotalActivePowerWithParams:@
readAttributeTotalActivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeTotalActivePowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeTotalActivePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTotalReactivePowerWithParams:@
readAttributeTotalReactivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeTotalReactivePowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeTotalReactivePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTotalApparentPowerWithParams:@
readAttributeTotalApparentPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeTotalApparentPowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeTotalApparentPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasured1stHarmonicCurrentWithParams:@
readAttributeMeasured1stHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured1stHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasured1stHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasured3rdHarmonicCurrentWithParams:@
readAttributeMeasured3rdHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured3rdHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasured3rdHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasured5thHarmonicCurrentWithParams:@
readAttributeMeasured5thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured5thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasured5thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasured7thHarmonicCurrentWithParams:@
readAttributeMeasured7thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured7thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasured7thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasured9thHarmonicCurrentWithParams:@
readAttributeMeasured9thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured9thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasured9thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasured11thHarmonicCurrentWithParams:@
readAttributeMeasured11thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasured11thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasured11thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasuredPhase1stHarmonicCurrentWithParams:@
readAttributeMeasuredPhase1stHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase1stHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:@
readAttributeMeasuredPhase3rdHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase3rdHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasuredPhase5thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase5thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase5thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasuredPhase7thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase7thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase7thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasuredPhase9thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase9thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase9thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMeasuredPhase11thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase11thHarmonicCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeMeasuredPhase11thHarmonicCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcFrequencyMultiplierWithParams:@
readAttributeAcFrequencyMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcFrequencyMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcFrequencyDivisorWithParams:@
readAttributeAcFrequencyDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcFrequencyDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcFrequencyDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerMultiplierWithParams:@
readAttributePowerMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributePowerMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerDivisorWithParams:@
readAttributePowerDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributePowerDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHarmonicCurrentMultiplierWithParams:@
readAttributeHarmonicCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeHarmonicCurrentMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeHarmonicCurrentMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePhaseHarmonicCurrentMultiplierWithParams:@
readAttributePhaseHarmonicCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePhaseHarmonicCurrentMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstantaneousVoltageWithParams:@
readAttributeInstantaneousVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeInstantaneousVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstantaneousLineCurrentWithParams:@
readAttributeInstantaneousLineCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousLineCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeInstantaneousLineCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstantaneousActiveCurrentWithParams:@
readAttributeInstantaneousActiveCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousActiveCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeInstantaneousActiveCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstantaneousReactiveCurrentWithParams:@
readAttributeInstantaneousReactiveCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousReactiveCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeInstantaneousReactiveCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstantaneousPowerWithParams:@
readAttributeInstantaneousPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeInstantaneousPowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeInstantaneousPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageWithParams:@
readAttributeRmsVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageMinWithParams:@
readAttributeRmsVoltageMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageMaxWithParams:@
readAttributeRmsVoltageMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentWithParams:@
readAttributeRmsCurrentWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentMinWithParams:@
readAttributeRmsCurrentMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentMaxWithParams:@
readAttributeRmsCurrentMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerMinWithParams:@
readAttributeActivePowerMinWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMinWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerMinWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerMaxWithParams:@
readAttributeActivePowerMaxWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMaxWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerMaxWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeReactivePowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeApparentPowerWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributePowerFactorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsVoltageMeasurementPeriodWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeAverageRmsUnderVoltageCounterWithParams:@
readAttributeAverageRmsUnderVoltageCounterWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsUnderVoltageCounterWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeRmsExtremeOverVoltagePeriodWithParams:@
readAttributeRmsExtremeOverVoltagePeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeRmsExtremeUnderVoltagePeriodWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeRmsVoltageSagPeriodWithParams:@
readAttributeRmsVoltageSagPeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagPeriodWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSagPeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeRmsVoltageSwellPeriodWithParams:@
readAttributeRmsVoltageSwellPeriodWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSwellPeriodWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeAcVoltageMultiplierWithParams:@
readAttributeAcVoltageMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcVoltageMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcVoltageMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcVoltageDivisorWithParams:@
readAttributeAcVoltageDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcVoltageDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcVoltageDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcCurrentMultiplierWithParams:@
readAttributeAcCurrentMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcCurrentMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcCurrentMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcCurrentDivisorWithParams:@
readAttributeAcCurrentDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcCurrentDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcCurrentDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcPowerMultiplierWithParams:@
readAttributeAcPowerMultiplierWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcPowerMultiplierWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcPowerMultiplierWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcPowerDivisorWithParams:@
readAttributeAcPowerDivisorWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcPowerDivisorWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcPowerDivisorWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOverloadAlarmsMaskWithParams:@
readAttributeOverloadAlarmsMaskWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeOverloadAlarmsMaskWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeOverloadAlarmsMaskWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeVoltageOverloadWithParams:@
readAttributeVoltageOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeVoltageOverloadWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeVoltageOverloadWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentOverloadWithParams:@
readAttributeCurrentOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeCurrentOverloadWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeCurrentOverloadWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcOverloadAlarmsMaskWithParams:@
readAttributeAcOverloadAlarmsMaskWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcOverloadAlarmsMaskWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcOverloadAlarmsMaskWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterElectricalMeasurement writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_params :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterElectricalMeasurement -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_params mtrClusterElectricalMeasurement dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterElectricalMeasurement writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeAcVoltageOverloadWithParams:@
readAttributeAcVoltageOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcVoltageOverloadWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcVoltageOverloadWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcCurrentOverloadWithParams:@
readAttributeAcCurrentOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcCurrentOverloadWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcCurrentOverloadWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcActivePowerOverloadWithParams:@
readAttributeAcActivePowerOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcActivePowerOverloadWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcActivePowerOverloadWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcReactivePowerOverloadWithParams:@
readAttributeAcReactivePowerOverloadWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcReactivePowerOverloadWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcReactivePowerOverloadWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsOverVoltageWithParams:@
readAttributeAverageRmsOverVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsOverVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsOverVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsUnderVoltageWithParams:@
readAttributeAverageRmsUnderVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsUnderVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsExtremeOverVoltageWithParams:@
readAttributeRmsExtremeOverVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeOverVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsExtremeUnderVoltageWithParams:@
readAttributeRmsExtremeUnderVoltageWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltageWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeUnderVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageSagWithParams:@
readAttributeRmsVoltageSagWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSagWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageSwellWithParams:@
readAttributeRmsVoltageSwellWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSwellWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLineCurrentPhaseBWithParams:@
readAttributeLineCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeLineCurrentPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeLineCurrentPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveCurrentPhaseBWithParams:@
readAttributeActiveCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActiveCurrentPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActiveCurrentPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactiveCurrentPhaseBWithParams:@
readAttributeReactiveCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactiveCurrentPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeReactiveCurrentPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltagePhaseBWithParams:@
readAttributeRmsVoltagePhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltagePhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltagePhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageMinPhaseBWithParams:@
readAttributeRmsVoltageMinPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMinPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageMinPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageMaxPhaseBWithParams:@
readAttributeRmsVoltageMaxPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMaxPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageMaxPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentPhaseBWithParams:@
readAttributeRmsCurrentPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentMinPhaseBWithParams:@
readAttributeRmsCurrentMinPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMinPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentMinPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentMaxPhaseBWithParams:@
readAttributeRmsCurrentMaxPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMaxPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentMaxPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerPhaseBWithParams:@
readAttributeActivePowerPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerMinPhaseBWithParams:@
readAttributeActivePowerMinPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMinPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerMinPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerMaxPhaseBWithParams:@
readAttributeActivePowerMaxPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMaxPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerMaxPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactivePowerPhaseBWithParams:@
readAttributeReactivePowerPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeReactivePowerPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApparentPowerPhaseBWithParams:@
readAttributeApparentPowerPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeApparentPowerPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerFactorPhaseBWithParams:@
readAttributePowerFactorPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributePowerFactorPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsOverVoltageCounterPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageSagPeriodPhaseBWithParams:@
readAttributeRmsVoltageSagPeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagPeriodPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageSwellPeriodPhaseBWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseBWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodPhaseBWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLineCurrentPhaseCWithParams:@
readAttributeLineCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeLineCurrentPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeLineCurrentPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveCurrentPhaseCWithParams:@
readAttributeActiveCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActiveCurrentPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActiveCurrentPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactiveCurrentPhaseCWithParams:@
readAttributeReactiveCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactiveCurrentPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeReactiveCurrentPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltagePhaseCWithParams:@
readAttributeRmsVoltagePhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltagePhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltagePhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageMinPhaseCWithParams:@
readAttributeRmsVoltageMinPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMinPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageMinPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageMaxPhaseCWithParams:@
readAttributeRmsVoltageMaxPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageMaxPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageMaxPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentPhaseCWithParams:@
readAttributeRmsCurrentPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentMinPhaseCWithParams:@
readAttributeRmsCurrentMinPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMinPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentMinPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsCurrentMaxPhaseCWithParams:@
readAttributeRmsCurrentMaxPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsCurrentMaxPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsCurrentMaxPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerPhaseCWithParams:@
readAttributeActivePowerPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerMinPhaseCWithParams:@
readAttributeActivePowerMinPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMinPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerMinPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActivePowerMaxPhaseCWithParams:@
readAttributeActivePowerMaxPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeActivePowerMaxPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeActivePowerMaxPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReactivePowerPhaseCWithParams:@
readAttributeReactivePowerPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeReactivePowerPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeReactivePowerPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApparentPowerPhaseCWithParams:@
readAttributeApparentPowerPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeApparentPowerPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeApparentPowerPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePowerFactorPhaseCWithParams:@
readAttributePowerFactorPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributePowerFactorPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributePowerFactorPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsOverVoltageCounterPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageSagPeriodPhaseCWithParams:@
readAttributeRmsVoltageSagPeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSagPeriodPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeRmsVoltageSwellPeriodPhaseCWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseCWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodPhaseCWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRReadParams params) => mtrClusterElectricalMeasurement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterElectricalMeasurement params =
  sendMessage mtrClusterElectricalMeasurement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement => mtrClusterElectricalMeasurement -> IO (Id MTRClusterElectricalMeasurement)
init_ mtrClusterElectricalMeasurement =
  sendOwnedMessage mtrClusterElectricalMeasurement initSelector

-- | @+ new@
new :: IO (Id MTRClusterElectricalMeasurement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterElectricalMeasurement"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRDevice device, IsNSObject queue) => mtrClusterElectricalMeasurement -> device -> CUShort -> queue -> IO (Id MTRClusterElectricalMeasurement)
initWithDevice_endpoint_queue mtrClusterElectricalMeasurement device endpoint queue =
  sendOwnedMessage mtrClusterElectricalMeasurement initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetProfileInfoCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterElectricalMeasurement params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterElectricalMeasurement getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRElectricalMeasurementClusterGetProfileInfoCommandParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandler mtrClusterElectricalMeasurement expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterElectricalMeasurement getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterElectricalMeasurement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterElectricalMeasurement params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterElectricalMeasurement getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterElectricalMeasurement mtrClusterElectricalMeasurement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterElectricalMeasurement -> device -> endpointID -> queue -> IO (Id MTRClusterElectricalMeasurement)
initWithDevice_endpointID_queue mtrClusterElectricalMeasurement device endpointID queue =
  sendOwnedMessage mtrClusterElectricalMeasurement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRElectricalMeasurementClusterGetProfileInfoCommandParams, Id NSArray, Id NSNumber, Ptr ()] ()
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "getProfileInfoCommandWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams, Id NSArray, Id NSNumber, Ptr ()] ()
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeMeasurementTypeWithParams:@
readAttributeMeasurementTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasurementTypeWithParamsSelector = mkSelector "readAttributeMeasurementTypeWithParams:"

-- | @Selector@ for @readAttributeDcVoltageWithParams:@
readAttributeDcVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcVoltageWithParamsSelector = mkSelector "readAttributeDcVoltageWithParams:"

-- | @Selector@ for @readAttributeDcVoltageMinWithParams:@
readAttributeDcVoltageMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcVoltageMinWithParamsSelector = mkSelector "readAttributeDcVoltageMinWithParams:"

-- | @Selector@ for @readAttributeDcVoltageMaxWithParams:@
readAttributeDcVoltageMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcVoltageMaxWithParamsSelector = mkSelector "readAttributeDcVoltageMaxWithParams:"

-- | @Selector@ for @readAttributeDcCurrentWithParams:@
readAttributeDcCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcCurrentWithParamsSelector = mkSelector "readAttributeDcCurrentWithParams:"

-- | @Selector@ for @readAttributeDcCurrentMinWithParams:@
readAttributeDcCurrentMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcCurrentMinWithParamsSelector = mkSelector "readAttributeDcCurrentMinWithParams:"

-- | @Selector@ for @readAttributeDcCurrentMaxWithParams:@
readAttributeDcCurrentMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcCurrentMaxWithParamsSelector = mkSelector "readAttributeDcCurrentMaxWithParams:"

-- | @Selector@ for @readAttributeDcPowerWithParams:@
readAttributeDcPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcPowerWithParamsSelector = mkSelector "readAttributeDcPowerWithParams:"

-- | @Selector@ for @readAttributeDcPowerMinWithParams:@
readAttributeDcPowerMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcPowerMinWithParamsSelector = mkSelector "readAttributeDcPowerMinWithParams:"

-- | @Selector@ for @readAttributeDcPowerMaxWithParams:@
readAttributeDcPowerMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcPowerMaxWithParamsSelector = mkSelector "readAttributeDcPowerMaxWithParams:"

-- | @Selector@ for @readAttributeDcVoltageMultiplierWithParams:@
readAttributeDcVoltageMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcVoltageMultiplierWithParamsSelector = mkSelector "readAttributeDcVoltageMultiplierWithParams:"

-- | @Selector@ for @readAttributeDcVoltageDivisorWithParams:@
readAttributeDcVoltageDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcVoltageDivisorWithParamsSelector = mkSelector "readAttributeDcVoltageDivisorWithParams:"

-- | @Selector@ for @readAttributeDcCurrentMultiplierWithParams:@
readAttributeDcCurrentMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcCurrentMultiplierWithParamsSelector = mkSelector "readAttributeDcCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributeDcCurrentDivisorWithParams:@
readAttributeDcCurrentDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcCurrentDivisorWithParamsSelector = mkSelector "readAttributeDcCurrentDivisorWithParams:"

-- | @Selector@ for @readAttributeDcPowerMultiplierWithParams:@
readAttributeDcPowerMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcPowerMultiplierWithParamsSelector = mkSelector "readAttributeDcPowerMultiplierWithParams:"

-- | @Selector@ for @readAttributeDcPowerDivisorWithParams:@
readAttributeDcPowerDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDcPowerDivisorWithParamsSelector = mkSelector "readAttributeDcPowerDivisorWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyWithParams:@
readAttributeAcFrequencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcFrequencyWithParamsSelector = mkSelector "readAttributeAcFrequencyWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyMinWithParams:@
readAttributeAcFrequencyMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcFrequencyMinWithParamsSelector = mkSelector "readAttributeAcFrequencyMinWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyMaxWithParams:@
readAttributeAcFrequencyMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcFrequencyMaxWithParamsSelector = mkSelector "readAttributeAcFrequencyMaxWithParams:"

-- | @Selector@ for @readAttributeNeutralCurrentWithParams:@
readAttributeNeutralCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNeutralCurrentWithParamsSelector = mkSelector "readAttributeNeutralCurrentWithParams:"

-- | @Selector@ for @readAttributeTotalActivePowerWithParams:@
readAttributeTotalActivePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTotalActivePowerWithParamsSelector = mkSelector "readAttributeTotalActivePowerWithParams:"

-- | @Selector@ for @readAttributeTotalReactivePowerWithParams:@
readAttributeTotalReactivePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTotalReactivePowerWithParamsSelector = mkSelector "readAttributeTotalReactivePowerWithParams:"

-- | @Selector@ for @readAttributeTotalApparentPowerWithParams:@
readAttributeTotalApparentPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTotalApparentPowerWithParamsSelector = mkSelector "readAttributeTotalApparentPowerWithParams:"

-- | @Selector@ for @readAttributeMeasured1stHarmonicCurrentWithParams:@
readAttributeMeasured1stHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasured1stHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured1stHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured3rdHarmonicCurrentWithParams:@
readAttributeMeasured3rdHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasured3rdHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured3rdHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured5thHarmonicCurrentWithParams:@
readAttributeMeasured5thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasured5thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured5thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured7thHarmonicCurrentWithParams:@
readAttributeMeasured7thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasured7thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured7thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured9thHarmonicCurrentWithParams:@
readAttributeMeasured9thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasured9thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured9thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasured11thHarmonicCurrentWithParams:@
readAttributeMeasured11thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasured11thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasured11thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase1stHarmonicCurrentWithParams:@
readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredPhase1stHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase1stHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:@
readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredPhase3rdHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase3rdHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase5thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredPhase5thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase5thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase7thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredPhase7thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase7thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase9thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredPhase9thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase9thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeMeasuredPhase11thHarmonicCurrentWithParams:@
readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMeasuredPhase11thHarmonicCurrentWithParamsSelector = mkSelector "readAttributeMeasuredPhase11thHarmonicCurrentWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyMultiplierWithParams:@
readAttributeAcFrequencyMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcFrequencyMultiplierWithParamsSelector = mkSelector "readAttributeAcFrequencyMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcFrequencyDivisorWithParams:@
readAttributeAcFrequencyDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcFrequencyDivisorWithParamsSelector = mkSelector "readAttributeAcFrequencyDivisorWithParams:"

-- | @Selector@ for @readAttributePowerMultiplierWithParams:@
readAttributePowerMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerMultiplierWithParamsSelector = mkSelector "readAttributePowerMultiplierWithParams:"

-- | @Selector@ for @readAttributePowerDivisorWithParams:@
readAttributePowerDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerDivisorWithParamsSelector = mkSelector "readAttributePowerDivisorWithParams:"

-- | @Selector@ for @readAttributeHarmonicCurrentMultiplierWithParams:@
readAttributeHarmonicCurrentMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHarmonicCurrentMultiplierWithParamsSelector = mkSelector "readAttributeHarmonicCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributePhaseHarmonicCurrentMultiplierWithParams:@
readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePhaseHarmonicCurrentMultiplierWithParamsSelector = mkSelector "readAttributePhaseHarmonicCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributeInstantaneousVoltageWithParams:@
readAttributeInstantaneousVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstantaneousVoltageWithParamsSelector = mkSelector "readAttributeInstantaneousVoltageWithParams:"

-- | @Selector@ for @readAttributeInstantaneousLineCurrentWithParams:@
readAttributeInstantaneousLineCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstantaneousLineCurrentWithParamsSelector = mkSelector "readAttributeInstantaneousLineCurrentWithParams:"

-- | @Selector@ for @readAttributeInstantaneousActiveCurrentWithParams:@
readAttributeInstantaneousActiveCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstantaneousActiveCurrentWithParamsSelector = mkSelector "readAttributeInstantaneousActiveCurrentWithParams:"

-- | @Selector@ for @readAttributeInstantaneousReactiveCurrentWithParams:@
readAttributeInstantaneousReactiveCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstantaneousReactiveCurrentWithParamsSelector = mkSelector "readAttributeInstantaneousReactiveCurrentWithParams:"

-- | @Selector@ for @readAttributeInstantaneousPowerWithParams:@
readAttributeInstantaneousPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstantaneousPowerWithParamsSelector = mkSelector "readAttributeInstantaneousPowerWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageWithParams:@
readAttributeRmsVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageWithParamsSelector = mkSelector "readAttributeRmsVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMinWithParams:@
readAttributeRmsVoltageMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageMinWithParamsSelector = mkSelector "readAttributeRmsVoltageMinWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMaxWithParams:@
readAttributeRmsVoltageMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageMaxWithParamsSelector = mkSelector "readAttributeRmsVoltageMaxWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentWithParams:@
readAttributeRmsCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentWithParamsSelector = mkSelector "readAttributeRmsCurrentWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMinWithParams:@
readAttributeRmsCurrentMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentMinWithParamsSelector = mkSelector "readAttributeRmsCurrentMinWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMaxWithParams:@
readAttributeRmsCurrentMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentMaxWithParamsSelector = mkSelector "readAttributeRmsCurrentMaxWithParams:"

-- | @Selector@ for @readAttributeActivePowerWithParams:@
readAttributeActivePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerWithParamsSelector = mkSelector "readAttributeActivePowerWithParams:"

-- | @Selector@ for @readAttributeActivePowerMinWithParams:@
readAttributeActivePowerMinWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerMinWithParamsSelector = mkSelector "readAttributeActivePowerMinWithParams:"

-- | @Selector@ for @readAttributeActivePowerMaxWithParams:@
readAttributeActivePowerMaxWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerMaxWithParamsSelector = mkSelector "readAttributeActivePowerMaxWithParams:"

-- | @Selector@ for @readAttributeReactivePowerWithParams:@
readAttributeReactivePowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactivePowerWithParamsSelector = mkSelector "readAttributeReactivePowerWithParams:"

-- | @Selector@ for @readAttributeApparentPowerWithParams:@
readAttributeApparentPowerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApparentPowerWithParamsSelector = mkSelector "readAttributeApparentPowerWithParams:"

-- | @Selector@ for @readAttributePowerFactorWithParams:@
readAttributePowerFactorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerFactorWithParamsSelector = mkSelector "readAttributePowerFactorWithParams:"

-- | @Selector@ for @readAttributeAverageRmsVoltageMeasurementPeriodWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodWithParamsSelector = mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodWithParams:"

-- | @Selector@ for @writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeAverageRmsVoltageMeasurementPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAverageRmsVoltageMeasurementPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageCounterWithParams:@
readAttributeAverageRmsUnderVoltageCounterWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageCounterWithParams:"

-- | @Selector@ for @writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:@
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeAverageRmsUnderVoltageCounterWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAverageRmsUnderVoltageCounterWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltagePeriodWithParams:@
readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltagePeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeRmsExtremeOverVoltagePeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsExtremeOverVoltagePeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltagePeriodWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltagePeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeRmsExtremeUnderVoltagePeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsExtremeUnderVoltagePeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsVoltageSagPeriodWithParams:@
readAttributeRmsVoltageSagPeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSagPeriodWithParamsSelector = mkSelector "readAttributeRmsVoltageSagPeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeRmsVoltageSagPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsVoltageSagPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeRmsVoltageSwellPeriodWithParams:@
readAttributeRmsVoltageSwellPeriodWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellPeriodWithParams:"

-- | @Selector@ for @writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:@
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeRmsVoltageSwellPeriodWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeRmsVoltageSwellPeriodWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAcVoltageMultiplierWithParams:@
readAttributeAcVoltageMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcVoltageMultiplierWithParamsSelector = mkSelector "readAttributeAcVoltageMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcVoltageDivisorWithParams:@
readAttributeAcVoltageDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcVoltageDivisorWithParamsSelector = mkSelector "readAttributeAcVoltageDivisorWithParams:"

-- | @Selector@ for @readAttributeAcCurrentMultiplierWithParams:@
readAttributeAcCurrentMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcCurrentMultiplierWithParamsSelector = mkSelector "readAttributeAcCurrentMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcCurrentDivisorWithParams:@
readAttributeAcCurrentDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcCurrentDivisorWithParamsSelector = mkSelector "readAttributeAcCurrentDivisorWithParams:"

-- | @Selector@ for @readAttributeAcPowerMultiplierWithParams:@
readAttributeAcPowerMultiplierWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcPowerMultiplierWithParamsSelector = mkSelector "readAttributeAcPowerMultiplierWithParams:"

-- | @Selector@ for @readAttributeAcPowerDivisorWithParams:@
readAttributeAcPowerDivisorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcPowerDivisorWithParamsSelector = mkSelector "readAttributeAcPowerDivisorWithParams:"

-- | @Selector@ for @readAttributeOverloadAlarmsMaskWithParams:@
readAttributeOverloadAlarmsMaskWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOverloadAlarmsMaskWithParamsSelector = mkSelector "readAttributeOverloadAlarmsMaskWithParams:"

-- | @Selector@ for @writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeOverloadAlarmsMaskWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeOverloadAlarmsMaskWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeVoltageOverloadWithParams:@
readAttributeVoltageOverloadWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVoltageOverloadWithParamsSelector = mkSelector "readAttributeVoltageOverloadWithParams:"

-- | @Selector@ for @readAttributeCurrentOverloadWithParams:@
readAttributeCurrentOverloadWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentOverloadWithParamsSelector = mkSelector "readAttributeCurrentOverloadWithParams:"

-- | @Selector@ for @readAttributeAcOverloadAlarmsMaskWithParams:@
readAttributeAcOverloadAlarmsMaskWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcOverloadAlarmsMaskWithParamsSelector = mkSelector "readAttributeAcOverloadAlarmsMaskWithParams:"

-- | @Selector@ for @writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:@
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeAcOverloadAlarmsMaskWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeAcOverloadAlarmsMaskWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeAcVoltageOverloadWithParams:@
readAttributeAcVoltageOverloadWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcVoltageOverloadWithParamsSelector = mkSelector "readAttributeAcVoltageOverloadWithParams:"

-- | @Selector@ for @readAttributeAcCurrentOverloadWithParams:@
readAttributeAcCurrentOverloadWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcCurrentOverloadWithParamsSelector = mkSelector "readAttributeAcCurrentOverloadWithParams:"

-- | @Selector@ for @readAttributeAcActivePowerOverloadWithParams:@
readAttributeAcActivePowerOverloadWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcActivePowerOverloadWithParamsSelector = mkSelector "readAttributeAcActivePowerOverloadWithParams:"

-- | @Selector@ for @readAttributeAcReactivePowerOverloadWithParams:@
readAttributeAcReactivePowerOverloadWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcReactivePowerOverloadWithParamsSelector = mkSelector "readAttributeAcReactivePowerOverloadWithParams:"

-- | @Selector@ for @readAttributeAverageRmsOverVoltageWithParams:@
readAttributeAverageRmsOverVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsOverVoltageWithParamsSelector = mkSelector "readAttributeAverageRmsOverVoltageWithParams:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageWithParams:@
readAttributeAverageRmsUnderVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsUnderVoltageWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltageWithParams:@
readAttributeRmsExtremeOverVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeOverVoltageWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltageWithParams:@
readAttributeRmsExtremeUnderVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeUnderVoltageWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltageWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSagWithParams:@
readAttributeRmsVoltageSagWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSagWithParamsSelector = mkSelector "readAttributeRmsVoltageSagWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSwellWithParams:@
readAttributeRmsVoltageSwellWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSwellWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellWithParams:"

-- | @Selector@ for @readAttributeLineCurrentPhaseBWithParams:@
readAttributeLineCurrentPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLineCurrentPhaseBWithParamsSelector = mkSelector "readAttributeLineCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeActiveCurrentPhaseBWithParams:@
readAttributeActiveCurrentPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveCurrentPhaseBWithParamsSelector = mkSelector "readAttributeActiveCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeReactiveCurrentPhaseBWithParams:@
readAttributeReactiveCurrentPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactiveCurrentPhaseBWithParamsSelector = mkSelector "readAttributeReactiveCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltagePhaseBWithParams:@
readAttributeRmsVoltagePhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltagePhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltagePhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMinPhaseBWithParams:@
readAttributeRmsVoltageMinPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageMinPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageMinPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMaxPhaseBWithParams:@
readAttributeRmsVoltageMaxPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageMaxPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageMaxPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentPhaseBWithParams:@
readAttributeRmsCurrentPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentPhaseBWithParamsSelector = mkSelector "readAttributeRmsCurrentPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMinPhaseBWithParams:@
readAttributeRmsCurrentMinPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentMinPhaseBWithParamsSelector = mkSelector "readAttributeRmsCurrentMinPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMaxPhaseBWithParams:@
readAttributeRmsCurrentMaxPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentMaxPhaseBWithParamsSelector = mkSelector "readAttributeRmsCurrentMaxPhaseBWithParams:"

-- | @Selector@ for @readAttributeActivePowerPhaseBWithParams:@
readAttributeActivePowerPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerPhaseBWithParamsSelector = mkSelector "readAttributeActivePowerPhaseBWithParams:"

-- | @Selector@ for @readAttributeActivePowerMinPhaseBWithParams:@
readAttributeActivePowerMinPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerMinPhaseBWithParamsSelector = mkSelector "readAttributeActivePowerMinPhaseBWithParams:"

-- | @Selector@ for @readAttributeActivePowerMaxPhaseBWithParams:@
readAttributeActivePowerMaxPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerMaxPhaseBWithParamsSelector = mkSelector "readAttributeActivePowerMaxPhaseBWithParams:"

-- | @Selector@ for @readAttributeReactivePowerPhaseBWithParams:@
readAttributeReactivePowerPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactivePowerPhaseBWithParamsSelector = mkSelector "readAttributeReactivePowerPhaseBWithParams:"

-- | @Selector@ for @readAttributeApparentPowerPhaseBWithParams:@
readAttributeApparentPowerPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApparentPowerPhaseBWithParamsSelector = mkSelector "readAttributeApparentPowerPhaseBWithParams:"

-- | @Selector@ for @readAttributePowerFactorPhaseBWithParams:@
readAttributePowerFactorPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerFactorPhaseBWithParamsSelector = mkSelector "readAttributePowerFactorPhaseBWithParams:"

-- | @Selector@ for @readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParamsSelector = mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsOverVoltageCounterPhaseBWithParamsSelector = mkSelector "readAttributeAverageRmsOverVoltageCounterPhaseBWithParams:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterPhaseBWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageCounterPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltagePeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltagePeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSagPeriodPhaseBWithParams:@
readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSagPeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageSagPeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSwellPeriodPhaseBWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodPhaseBWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellPeriodPhaseBWithParams:"

-- | @Selector@ for @readAttributeLineCurrentPhaseCWithParams:@
readAttributeLineCurrentPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLineCurrentPhaseCWithParamsSelector = mkSelector "readAttributeLineCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeActiveCurrentPhaseCWithParams:@
readAttributeActiveCurrentPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveCurrentPhaseCWithParamsSelector = mkSelector "readAttributeActiveCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeReactiveCurrentPhaseCWithParams:@
readAttributeReactiveCurrentPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactiveCurrentPhaseCWithParamsSelector = mkSelector "readAttributeReactiveCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltagePhaseCWithParams:@
readAttributeRmsVoltagePhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltagePhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltagePhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMinPhaseCWithParams:@
readAttributeRmsVoltageMinPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageMinPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageMinPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageMaxPhaseCWithParams:@
readAttributeRmsVoltageMaxPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageMaxPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageMaxPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentPhaseCWithParams:@
readAttributeRmsCurrentPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentPhaseCWithParamsSelector = mkSelector "readAttributeRmsCurrentPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMinPhaseCWithParams:@
readAttributeRmsCurrentMinPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentMinPhaseCWithParamsSelector = mkSelector "readAttributeRmsCurrentMinPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsCurrentMaxPhaseCWithParams:@
readAttributeRmsCurrentMaxPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsCurrentMaxPhaseCWithParamsSelector = mkSelector "readAttributeRmsCurrentMaxPhaseCWithParams:"

-- | @Selector@ for @readAttributeActivePowerPhaseCWithParams:@
readAttributeActivePowerPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerPhaseCWithParamsSelector = mkSelector "readAttributeActivePowerPhaseCWithParams:"

-- | @Selector@ for @readAttributeActivePowerMinPhaseCWithParams:@
readAttributeActivePowerMinPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerMinPhaseCWithParamsSelector = mkSelector "readAttributeActivePowerMinPhaseCWithParams:"

-- | @Selector@ for @readAttributeActivePowerMaxPhaseCWithParams:@
readAttributeActivePowerMaxPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActivePowerMaxPhaseCWithParamsSelector = mkSelector "readAttributeActivePowerMaxPhaseCWithParams:"

-- | @Selector@ for @readAttributeReactivePowerPhaseCWithParams:@
readAttributeReactivePowerPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReactivePowerPhaseCWithParamsSelector = mkSelector "readAttributeReactivePowerPhaseCWithParams:"

-- | @Selector@ for @readAttributeApparentPowerPhaseCWithParams:@
readAttributeApparentPowerPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApparentPowerPhaseCWithParamsSelector = mkSelector "readAttributeApparentPowerPhaseCWithParams:"

-- | @Selector@ for @readAttributePowerFactorPhaseCWithParams:@
readAttributePowerFactorPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePowerFactorPhaseCWithParamsSelector = mkSelector "readAttributePowerFactorPhaseCWithParams:"

-- | @Selector@ for @readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:@
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParamsSelector = mkSelector "readAttributeAverageRmsVoltageMeasurementPeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsOverVoltageCounterPhaseCWithParamsSelector = mkSelector "readAttributeAverageRmsOverVoltageCounterPhaseCWithParams:"

-- | @Selector@ for @readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:@
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAverageRmsUnderVoltageCounterPhaseCWithParamsSelector = mkSelector "readAttributeAverageRmsUnderVoltageCounterPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsExtremeOverVoltagePeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:@
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsExtremeUnderVoltagePeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSagPeriodPhaseCWithParams:@
readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSagPeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageSagPeriodPhaseCWithParams:"

-- | @Selector@ for @readAttributeRmsVoltageSwellPeriodPhaseCWithParams:@
readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeRmsVoltageSwellPeriodPhaseCWithParamsSelector = mkSelector "readAttributeRmsVoltageSwellPeriodPhaseCWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterElectricalMeasurement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterElectricalMeasurement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterElectricalMeasurement)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRElectricalMeasurementClusterGetProfileInfoCommandParams, Id NSArray, Id NSNumber, Ptr ()] ()
getProfileInfoCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getProfileInfoCommandWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:@
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
getProfileInfoCommandWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getProfileInfoCommandWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:@
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams, Id NSArray, Id NSNumber, Ptr ()] ()
getMeasurementProfileCommandWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "getMeasurementProfileCommandWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterElectricalMeasurement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

