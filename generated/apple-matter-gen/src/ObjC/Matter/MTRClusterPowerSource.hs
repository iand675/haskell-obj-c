{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Power Source    This cluster is used to describe the configuration and capabilities of a physical power source that provides power to the Node.
--
-- Generated bindings for @MTRClusterPowerSource@.
module ObjC.Matter.MTRClusterPowerSource
  ( MTRClusterPowerSource
  , IsMTRClusterPowerSource(..)
  , readAttributeStatusWithParams
  , readAttributeOrderWithParams
  , readAttributeDescriptionWithParams
  , readAttributeWiredAssessedInputVoltageWithParams
  , readAttributeWiredAssessedInputFrequencyWithParams
  , readAttributeWiredCurrentTypeWithParams
  , readAttributeWiredAssessedCurrentWithParams
  , readAttributeWiredNominalVoltageWithParams
  , readAttributeWiredMaximumCurrentWithParams
  , readAttributeWiredPresentWithParams
  , readAttributeActiveWiredFaultsWithParams
  , readAttributeBatVoltageWithParams
  , readAttributeBatPercentRemainingWithParams
  , readAttributeBatTimeRemainingWithParams
  , readAttributeBatChargeLevelWithParams
  , readAttributeBatReplacementNeededWithParams
  , readAttributeBatReplaceabilityWithParams
  , readAttributeBatPresentWithParams
  , readAttributeActiveBatFaultsWithParams
  , readAttributeBatReplacementDescriptionWithParams
  , readAttributeBatCommonDesignationWithParams
  , readAttributeBatANSIDesignationWithParams
  , readAttributeBatIECDesignationWithParams
  , readAttributeBatApprovedChemistryWithParams
  , readAttributeBatCapacityWithParams
  , readAttributeBatQuantityWithParams
  , readAttributeBatChargeStateWithParams
  , readAttributeBatTimeToFullChargeWithParams
  , readAttributeBatFunctionalWhileChargingWithParams
  , readAttributeBatChargingCurrentWithParams
  , readAttributeActiveBatChargeFaultsWithParams
  , readAttributeEndpointListWithParams
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
  , readAttributeActiveBatChargeFaultsWithParamsSelector
  , readAttributeActiveBatFaultsWithParamsSelector
  , readAttributeActiveWiredFaultsWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBatANSIDesignationWithParamsSelector
  , readAttributeBatApprovedChemistryWithParamsSelector
  , readAttributeBatCapacityWithParamsSelector
  , readAttributeBatChargeLevelWithParamsSelector
  , readAttributeBatChargeStateWithParamsSelector
  , readAttributeBatChargingCurrentWithParamsSelector
  , readAttributeBatCommonDesignationWithParamsSelector
  , readAttributeBatFunctionalWhileChargingWithParamsSelector
  , readAttributeBatIECDesignationWithParamsSelector
  , readAttributeBatPercentRemainingWithParamsSelector
  , readAttributeBatPresentWithParamsSelector
  , readAttributeBatQuantityWithParamsSelector
  , readAttributeBatReplaceabilityWithParamsSelector
  , readAttributeBatReplacementDescriptionWithParamsSelector
  , readAttributeBatReplacementNeededWithParamsSelector
  , readAttributeBatTimeRemainingWithParamsSelector
  , readAttributeBatTimeToFullChargeWithParamsSelector
  , readAttributeBatVoltageWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeDescriptionWithParamsSelector
  , readAttributeEndpointListWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeOrderWithParamsSelector
  , readAttributeStatusWithParamsSelector
  , readAttributeWiredAssessedCurrentWithParamsSelector
  , readAttributeWiredAssessedInputFrequencyWithParamsSelector
  , readAttributeWiredAssessedInputVoltageWithParamsSelector
  , readAttributeWiredCurrentTypeWithParamsSelector
  , readAttributeWiredMaximumCurrentWithParamsSelector
  , readAttributeWiredNominalVoltageWithParamsSelector
  , readAttributeWiredPresentWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeStatusWithParams:@
readAttributeStatusWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeStatusWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOrderWithParams:@
readAttributeOrderWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeOrderWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeOrderWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeDescriptionWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeDescriptionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredAssessedInputVoltageWithParams:@
readAttributeWiredAssessedInputVoltageWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredAssessedInputVoltageWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredAssessedInputVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredAssessedInputFrequencyWithParams:@
readAttributeWiredAssessedInputFrequencyWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredAssessedInputFrequencyWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredAssessedInputFrequencyWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredCurrentTypeWithParams:@
readAttributeWiredCurrentTypeWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredCurrentTypeWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredCurrentTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredAssessedCurrentWithParams:@
readAttributeWiredAssessedCurrentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredAssessedCurrentWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredAssessedCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredNominalVoltageWithParams:@
readAttributeWiredNominalVoltageWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredNominalVoltageWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredNominalVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredMaximumCurrentWithParams:@
readAttributeWiredMaximumCurrentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredMaximumCurrentWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredMaximumCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeWiredPresentWithParams:@
readAttributeWiredPresentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeWiredPresentWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeWiredPresentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveWiredFaultsWithParams:@
readAttributeActiveWiredFaultsWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeActiveWiredFaultsWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeActiveWiredFaultsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatVoltageWithParams:@
readAttributeBatVoltageWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatVoltageWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatVoltageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatPercentRemainingWithParams:@
readAttributeBatPercentRemainingWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatPercentRemainingWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatPercentRemainingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatTimeRemainingWithParams:@
readAttributeBatTimeRemainingWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatTimeRemainingWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatTimeRemainingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatChargeLevelWithParams:@
readAttributeBatChargeLevelWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatChargeLevelWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatChargeLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatReplacementNeededWithParams:@
readAttributeBatReplacementNeededWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatReplacementNeededWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatReplacementNeededWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatReplaceabilityWithParams:@
readAttributeBatReplaceabilityWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatReplaceabilityWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatReplaceabilityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatPresentWithParams:@
readAttributeBatPresentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatPresentWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatPresentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveBatFaultsWithParams:@
readAttributeActiveBatFaultsWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeActiveBatFaultsWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeActiveBatFaultsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatReplacementDescriptionWithParams:@
readAttributeBatReplacementDescriptionWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatReplacementDescriptionWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatReplacementDescriptionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatCommonDesignationWithParams:@
readAttributeBatCommonDesignationWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatCommonDesignationWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatCommonDesignationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatANSIDesignationWithParams:@
readAttributeBatANSIDesignationWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatANSIDesignationWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatANSIDesignationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatIECDesignationWithParams:@
readAttributeBatIECDesignationWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatIECDesignationWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatIECDesignationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatApprovedChemistryWithParams:@
readAttributeBatApprovedChemistryWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatApprovedChemistryWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatApprovedChemistryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatCapacityWithParams:@
readAttributeBatCapacityWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatCapacityWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatCapacityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatQuantityWithParams:@
readAttributeBatQuantityWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatQuantityWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatQuantityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatChargeStateWithParams:@
readAttributeBatChargeStateWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatChargeStateWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatChargeStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatTimeToFullChargeWithParams:@
readAttributeBatTimeToFullChargeWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatTimeToFullChargeWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatTimeToFullChargeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatFunctionalWhileChargingWithParams:@
readAttributeBatFunctionalWhileChargingWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatFunctionalWhileChargingWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatFunctionalWhileChargingWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBatChargingCurrentWithParams:@
readAttributeBatChargingCurrentWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeBatChargingCurrentWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeBatChargingCurrentWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeActiveBatChargeFaultsWithParams:@
readAttributeActiveBatChargeFaultsWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeActiveBatChargeFaultsWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeActiveBatChargeFaultsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEndpointListWithParams:@
readAttributeEndpointListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeEndpointListWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeEndpointListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRReadParams params) => mtrClusterPowerSource -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterPowerSource params =
  sendMessage mtrClusterPowerSource readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterPowerSource mtrClusterPowerSource => mtrClusterPowerSource -> IO (Id MTRClusterPowerSource)
init_ mtrClusterPowerSource =
  sendOwnedMessage mtrClusterPowerSource initSelector

-- | @+ new@
new :: IO (Id MTRClusterPowerSource)
new  =
  do
    cls' <- getRequiredClass "MTRClusterPowerSource"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRDevice device, IsNSObject queue) => mtrClusterPowerSource -> device -> CUShort -> queue -> IO (Id MTRClusterPowerSource)
initWithDevice_endpoint_queue mtrClusterPowerSource device endpoint queue =
  sendOwnedMessage mtrClusterPowerSource initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterPowerSource mtrClusterPowerSource, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterPowerSource -> device -> endpointID -> queue -> IO (Id MTRClusterPowerSource)
initWithDevice_endpointID_queue mtrClusterPowerSource device endpointID queue =
  sendOwnedMessage mtrClusterPowerSource initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeStatusWithParams:@
readAttributeStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStatusWithParamsSelector = mkSelector "readAttributeStatusWithParams:"

-- | @Selector@ for @readAttributeOrderWithParams:@
readAttributeOrderWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOrderWithParamsSelector = mkSelector "readAttributeOrderWithParams:"

-- | @Selector@ for @readAttributeDescriptionWithParams:@
readAttributeDescriptionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDescriptionWithParamsSelector = mkSelector "readAttributeDescriptionWithParams:"

-- | @Selector@ for @readAttributeWiredAssessedInputVoltageWithParams:@
readAttributeWiredAssessedInputVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredAssessedInputVoltageWithParamsSelector = mkSelector "readAttributeWiredAssessedInputVoltageWithParams:"

-- | @Selector@ for @readAttributeWiredAssessedInputFrequencyWithParams:@
readAttributeWiredAssessedInputFrequencyWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredAssessedInputFrequencyWithParamsSelector = mkSelector "readAttributeWiredAssessedInputFrequencyWithParams:"

-- | @Selector@ for @readAttributeWiredCurrentTypeWithParams:@
readAttributeWiredCurrentTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredCurrentTypeWithParamsSelector = mkSelector "readAttributeWiredCurrentTypeWithParams:"

-- | @Selector@ for @readAttributeWiredAssessedCurrentWithParams:@
readAttributeWiredAssessedCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredAssessedCurrentWithParamsSelector = mkSelector "readAttributeWiredAssessedCurrentWithParams:"

-- | @Selector@ for @readAttributeWiredNominalVoltageWithParams:@
readAttributeWiredNominalVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredNominalVoltageWithParamsSelector = mkSelector "readAttributeWiredNominalVoltageWithParams:"

-- | @Selector@ for @readAttributeWiredMaximumCurrentWithParams:@
readAttributeWiredMaximumCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredMaximumCurrentWithParamsSelector = mkSelector "readAttributeWiredMaximumCurrentWithParams:"

-- | @Selector@ for @readAttributeWiredPresentWithParams:@
readAttributeWiredPresentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeWiredPresentWithParamsSelector = mkSelector "readAttributeWiredPresentWithParams:"

-- | @Selector@ for @readAttributeActiveWiredFaultsWithParams:@
readAttributeActiveWiredFaultsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveWiredFaultsWithParamsSelector = mkSelector "readAttributeActiveWiredFaultsWithParams:"

-- | @Selector@ for @readAttributeBatVoltageWithParams:@
readAttributeBatVoltageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatVoltageWithParamsSelector = mkSelector "readAttributeBatVoltageWithParams:"

-- | @Selector@ for @readAttributeBatPercentRemainingWithParams:@
readAttributeBatPercentRemainingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatPercentRemainingWithParamsSelector = mkSelector "readAttributeBatPercentRemainingWithParams:"

-- | @Selector@ for @readAttributeBatTimeRemainingWithParams:@
readAttributeBatTimeRemainingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatTimeRemainingWithParamsSelector = mkSelector "readAttributeBatTimeRemainingWithParams:"

-- | @Selector@ for @readAttributeBatChargeLevelWithParams:@
readAttributeBatChargeLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatChargeLevelWithParamsSelector = mkSelector "readAttributeBatChargeLevelWithParams:"

-- | @Selector@ for @readAttributeBatReplacementNeededWithParams:@
readAttributeBatReplacementNeededWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatReplacementNeededWithParamsSelector = mkSelector "readAttributeBatReplacementNeededWithParams:"

-- | @Selector@ for @readAttributeBatReplaceabilityWithParams:@
readAttributeBatReplaceabilityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatReplaceabilityWithParamsSelector = mkSelector "readAttributeBatReplaceabilityWithParams:"

-- | @Selector@ for @readAttributeBatPresentWithParams:@
readAttributeBatPresentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatPresentWithParamsSelector = mkSelector "readAttributeBatPresentWithParams:"

-- | @Selector@ for @readAttributeActiveBatFaultsWithParams:@
readAttributeActiveBatFaultsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveBatFaultsWithParamsSelector = mkSelector "readAttributeActiveBatFaultsWithParams:"

-- | @Selector@ for @readAttributeBatReplacementDescriptionWithParams:@
readAttributeBatReplacementDescriptionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatReplacementDescriptionWithParamsSelector = mkSelector "readAttributeBatReplacementDescriptionWithParams:"

-- | @Selector@ for @readAttributeBatCommonDesignationWithParams:@
readAttributeBatCommonDesignationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatCommonDesignationWithParamsSelector = mkSelector "readAttributeBatCommonDesignationWithParams:"

-- | @Selector@ for @readAttributeBatANSIDesignationWithParams:@
readAttributeBatANSIDesignationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatANSIDesignationWithParamsSelector = mkSelector "readAttributeBatANSIDesignationWithParams:"

-- | @Selector@ for @readAttributeBatIECDesignationWithParams:@
readAttributeBatIECDesignationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatIECDesignationWithParamsSelector = mkSelector "readAttributeBatIECDesignationWithParams:"

-- | @Selector@ for @readAttributeBatApprovedChemistryWithParams:@
readAttributeBatApprovedChemistryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatApprovedChemistryWithParamsSelector = mkSelector "readAttributeBatApprovedChemistryWithParams:"

-- | @Selector@ for @readAttributeBatCapacityWithParams:@
readAttributeBatCapacityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatCapacityWithParamsSelector = mkSelector "readAttributeBatCapacityWithParams:"

-- | @Selector@ for @readAttributeBatQuantityWithParams:@
readAttributeBatQuantityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatQuantityWithParamsSelector = mkSelector "readAttributeBatQuantityWithParams:"

-- | @Selector@ for @readAttributeBatChargeStateWithParams:@
readAttributeBatChargeStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatChargeStateWithParamsSelector = mkSelector "readAttributeBatChargeStateWithParams:"

-- | @Selector@ for @readAttributeBatTimeToFullChargeWithParams:@
readAttributeBatTimeToFullChargeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatTimeToFullChargeWithParamsSelector = mkSelector "readAttributeBatTimeToFullChargeWithParams:"

-- | @Selector@ for @readAttributeBatFunctionalWhileChargingWithParams:@
readAttributeBatFunctionalWhileChargingWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatFunctionalWhileChargingWithParamsSelector = mkSelector "readAttributeBatFunctionalWhileChargingWithParams:"

-- | @Selector@ for @readAttributeBatChargingCurrentWithParams:@
readAttributeBatChargingCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBatChargingCurrentWithParamsSelector = mkSelector "readAttributeBatChargingCurrentWithParams:"

-- | @Selector@ for @readAttributeActiveBatChargeFaultsWithParams:@
readAttributeActiveBatChargeFaultsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveBatChargeFaultsWithParamsSelector = mkSelector "readAttributeActiveBatChargeFaultsWithParams:"

-- | @Selector@ for @readAttributeEndpointListWithParams:@
readAttributeEndpointListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEndpointListWithParamsSelector = mkSelector "readAttributeEndpointListWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterPowerSource)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterPowerSource)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterPowerSource)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterPowerSource)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

