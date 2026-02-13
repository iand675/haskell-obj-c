{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ballast Configuration    Attributes and commands for configuring a lighting ballast.
--
-- Generated bindings for @MTRClusterBallastConfiguration@.
module ObjC.Matter.MTRClusterBallastConfiguration
  ( MTRClusterBallastConfiguration
  , IsMTRClusterBallastConfiguration(..)
  , readAttributePhysicalMinLevelWithParams
  , readAttributePhysicalMaxLevelWithParams
  , readAttributeBallastStatusWithParams
  , readAttributeMinLevelWithParams
  , writeAttributeMinLevelWithValue_expectedValueInterval
  , writeAttributeMinLevelWithValue_expectedValueInterval_params
  , readAttributeMaxLevelWithParams
  , writeAttributeMaxLevelWithValue_expectedValueInterval
  , writeAttributeMaxLevelWithValue_expectedValueInterval_params
  , readAttributeIntrinsicBallastFactorWithParams
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_params
  , readAttributeBallastFactorAdjustmentWithParams
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_params
  , readAttributeLampQuantityWithParams
  , readAttributeLampTypeWithParams
  , writeAttributeLampTypeWithValue_expectedValueInterval
  , writeAttributeLampTypeWithValue_expectedValueInterval_params
  , readAttributeLampManufacturerWithParams
  , writeAttributeLampManufacturerWithValue_expectedValueInterval
  , writeAttributeLampManufacturerWithValue_expectedValueInterval_params
  , readAttributeLampRatedHoursWithParams
  , writeAttributeLampRatedHoursWithValue_expectedValueInterval
  , writeAttributeLampRatedHoursWithValue_expectedValueInterval_params
  , readAttributeLampBurnHoursWithParams
  , writeAttributeLampBurnHoursWithValue_expectedValueInterval
  , writeAttributeLampBurnHoursWithValue_expectedValueInterval_params
  , readAttributeLampAlarmModeWithParams
  , writeAttributeLampAlarmModeWithValue_expectedValueInterval
  , writeAttributeLampAlarmModeWithValue_expectedValueInterval_params
  , readAttributeLampBurnHoursTripPointWithParams
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_params
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , readAttributeIntrinsicBalanceFactorWithParams
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_params
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBallastFactorAdjustmentWithParamsSelector
  , readAttributeBallastStatusWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeIntrinsicBalanceFactorWithParamsSelector
  , readAttributeIntrinsicBallastFactorWithParamsSelector
  , readAttributeLampAlarmModeWithParamsSelector
  , readAttributeLampBurnHoursTripPointWithParamsSelector
  , readAttributeLampBurnHoursWithParamsSelector
  , readAttributeLampManufacturerWithParamsSelector
  , readAttributeLampQuantityWithParamsSelector
  , readAttributeLampRatedHoursWithParamsSelector
  , readAttributeLampTypeWithParamsSelector
  , readAttributeMaxLevelWithParamsSelector
  , readAttributeMinLevelWithParamsSelector
  , readAttributePhysicalMaxLevelWithParamsSelector
  , readAttributePhysicalMinLevelWithParamsSelector
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector
  , writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector
  , writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector
  , writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector
  , writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector
  , writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector
  , writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector
  , writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector
  , writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLampTypeWithValue_expectedValueIntervalSelector
  , writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeMaxLevelWithValue_expectedValueIntervalSelector
  , writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector
  , writeAttributeMinLevelWithValue_expectedValueIntervalSelector
  , writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributePhysicalMinLevelWithParams:@
readAttributePhysicalMinLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributePhysicalMinLevelWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributePhysicalMinLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePhysicalMaxLevelWithParams:@
readAttributePhysicalMaxLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributePhysicalMaxLevelWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributePhysicalMaxLevelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBallastStatusWithParams:@
readAttributeBallastStatusWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeBallastStatusWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeBallastStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeMinLevelWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeMinLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeMinLevelWithValue:expectedValueInterval:@
writeAttributeMinLevelWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMinLevelWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeMinLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeMinLevelWithValue:expectedValueInterval:params:@
writeAttributeMinLevelWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMinLevelWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeMaxLevelWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeMaxLevelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeMaxLevelWithValue:expectedValueInterval:@
writeAttributeMaxLevelWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeMaxLevelWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeMaxLevelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeMaxLevelWithValue:expectedValueInterval:params:@
writeAttributeMaxLevelWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeMaxLevelWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeIntrinsicBallastFactorWithParams:@
readAttributeIntrinsicBallastFactorWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeIntrinsicBallastFactorWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeIntrinsicBallastFactorWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeBallastFactorAdjustmentWithParams:@
readAttributeBallastFactorAdjustmentWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeBallastFactorAdjustmentWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeBallastFactorAdjustmentWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLampQuantityWithParams:@
readAttributeLampQuantityWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampQuantityWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampQuantityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLampTypeWithParams:@
readAttributeLampTypeWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampTypeWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampTypeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLampTypeWithValue:expectedValueInterval:@
writeAttributeLampTypeWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampTypeWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampTypeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLampTypeWithValue:expectedValueInterval:params:@
writeAttributeLampTypeWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampTypeWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLampManufacturerWithParams:@
readAttributeLampManufacturerWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampManufacturerWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampManufacturerWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLampManufacturerWithValue:expectedValueInterval:@
writeAttributeLampManufacturerWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampManufacturerWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLampManufacturerWithValue:expectedValueInterval:params:@
writeAttributeLampManufacturerWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampManufacturerWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLampRatedHoursWithParams:@
readAttributeLampRatedHoursWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampRatedHoursWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampRatedHoursWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLampRatedHoursWithValue:expectedValueInterval:@
writeAttributeLampRatedHoursWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampRatedHoursWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:@
writeAttributeLampRatedHoursWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampRatedHoursWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLampBurnHoursWithParams:@
readAttributeLampBurnHoursWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampBurnHoursWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampBurnHoursWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLampBurnHoursWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampBurnHoursWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampBurnHoursWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLampAlarmModeWithParams:@
readAttributeLampAlarmModeWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampAlarmModeWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampAlarmModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLampAlarmModeWithValue:expectedValueInterval:@
writeAttributeLampAlarmModeWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampAlarmModeWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:@
writeAttributeLampAlarmModeWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampAlarmModeWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLampBurnHoursTripPointWithParams:@
readAttributeLampBurnHoursTripPointWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeLampBurnHoursTripPointWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeLampBurnHoursTripPointWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration => mtrClusterBallastConfiguration -> IO (Id MTRClusterBallastConfiguration)
init_ mtrClusterBallastConfiguration =
  sendOwnedMessage mtrClusterBallastConfiguration initSelector

-- | @+ new@
new :: IO (Id MTRClusterBallastConfiguration)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBallastConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRDevice device, IsNSObject queue) => mtrClusterBallastConfiguration -> device -> CUShort -> queue -> IO (Id MTRClusterBallastConfiguration)
initWithDevice_endpoint_queue mtrClusterBallastConfiguration device endpoint queue =
  sendOwnedMessage mtrClusterBallastConfiguration initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- readAttributeIntrinsicBalanceFactorWithParams:@
readAttributeIntrinsicBalanceFactorWithParams :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRReadParams params) => mtrClusterBallastConfiguration -> params -> IO (Id NSDictionary)
readAttributeIntrinsicBalanceFactorWithParams mtrClusterBallastConfiguration params =
  sendMessage mtrClusterBallastConfiguration readAttributeIntrinsicBalanceFactorWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBallastConfiguration writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_params :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBallastConfiguration -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_params mtrClusterBallastConfiguration dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBallastConfiguration writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBallastConfiguration mtrClusterBallastConfiguration, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBallastConfiguration -> device -> endpointID -> queue -> IO (Id MTRClusterBallastConfiguration)
initWithDevice_endpointID_queue mtrClusterBallastConfiguration device endpointID queue =
  sendOwnedMessage mtrClusterBallastConfiguration initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributePhysicalMinLevelWithParams:@
readAttributePhysicalMinLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePhysicalMinLevelWithParamsSelector = mkSelector "readAttributePhysicalMinLevelWithParams:"

-- | @Selector@ for @readAttributePhysicalMaxLevelWithParams:@
readAttributePhysicalMaxLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePhysicalMaxLevelWithParamsSelector = mkSelector "readAttributePhysicalMaxLevelWithParams:"

-- | @Selector@ for @readAttributeBallastStatusWithParams:@
readAttributeBallastStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBallastStatusWithParamsSelector = mkSelector "readAttributeBallastStatusWithParams:"

-- | @Selector@ for @readAttributeMinLevelWithParams:@
readAttributeMinLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMinLevelWithParamsSelector = mkSelector "readAttributeMinLevelWithParams:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:expectedValueInterval:@
writeAttributeMinLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeMinLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMinLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMinLevelWithValue:expectedValueInterval:params:@
writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeMinLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMinLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeMaxLevelWithParams:@
readAttributeMaxLevelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxLevelWithParamsSelector = mkSelector "readAttributeMaxLevelWithParams:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:expectedValueInterval:@
writeAttributeMaxLevelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeMaxLevelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeMaxLevelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeMaxLevelWithValue:expectedValueInterval:params:@
writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeMaxLevelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeMaxLevelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeIntrinsicBallastFactorWithParams:@
readAttributeIntrinsicBallastFactorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIntrinsicBallastFactorWithParamsSelector = mkSelector "readAttributeIntrinsicBallastFactorWithParams:"

-- | @Selector@ for @writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeIntrinsicBallastFactorWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeIntrinsicBallastFactorWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIntrinsicBallastFactorWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeBallastFactorAdjustmentWithParams:@
readAttributeBallastFactorAdjustmentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBallastFactorAdjustmentWithParamsSelector = mkSelector "readAttributeBallastFactorAdjustmentWithParams:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeBallastFactorAdjustmentWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:@
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeBallastFactorAdjustmentWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeBallastFactorAdjustmentWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampQuantityWithParams:@
readAttributeLampQuantityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampQuantityWithParamsSelector = mkSelector "readAttributeLampQuantityWithParams:"

-- | @Selector@ for @readAttributeLampTypeWithParams:@
readAttributeLampTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampTypeWithParamsSelector = mkSelector "readAttributeLampTypeWithParams:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:expectedValueInterval:@
writeAttributeLampTypeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLampTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampTypeWithValue:expectedValueInterval:params:@
writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLampTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampManufacturerWithParams:@
readAttributeLampManufacturerWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampManufacturerWithParamsSelector = mkSelector "readAttributeLampManufacturerWithParams:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:expectedValueInterval:@
writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLampManufacturerWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampManufacturerWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampManufacturerWithValue:expectedValueInterval:params:@
writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLampManufacturerWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampManufacturerWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampRatedHoursWithParams:@
readAttributeLampRatedHoursWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampRatedHoursWithParamsSelector = mkSelector "readAttributeLampRatedHoursWithParams:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:expectedValueInterval:@
writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLampRatedHoursWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampRatedHoursWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:@
writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLampRatedHoursWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampRatedHoursWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampBurnHoursWithParams:@
readAttributeLampBurnHoursWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampBurnHoursWithParamsSelector = mkSelector "readAttributeLampBurnHoursWithParams:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLampBurnHoursWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampBurnHoursWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLampBurnHoursWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampBurnHoursWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampAlarmModeWithParams:@
readAttributeLampAlarmModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampAlarmModeWithParamsSelector = mkSelector "readAttributeLampAlarmModeWithParams:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:expectedValueInterval:@
writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLampAlarmModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampAlarmModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:@
writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLampAlarmModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampAlarmModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeLampBurnHoursTripPointWithParams:@
readAttributeLampBurnHoursTripPointWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLampBurnHoursTripPointWithParamsSelector = mkSelector "readAttributeLampBurnHoursTripPointWithParams:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLampBurnHoursTripPointWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:@
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLampBurnHoursTripPointWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLampBurnHoursTripPointWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterBallastConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBallastConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterBallastConfiguration)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @readAttributeIntrinsicBalanceFactorWithParams:@
readAttributeIntrinsicBalanceFactorWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIntrinsicBalanceFactorWithParamsSelector = mkSelector "readAttributeIntrinsicBalanceFactorWithParams:"

-- | @Selector@ for @writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:@
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeIntrinsicBalanceFactorWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIntrinsicBalanceFactorWithValue:expectedValueInterval:params:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBallastConfiguration)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

