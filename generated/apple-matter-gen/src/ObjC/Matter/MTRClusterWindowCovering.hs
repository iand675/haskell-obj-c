{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Window Covering    Provides an interface for controlling and adjusting automatic window coverings.
--
-- Generated bindings for @MTRClusterWindowCovering@.
module ObjC.Matter.MTRClusterWindowCovering
  ( MTRClusterWindowCovering
  , IsMTRClusterWindowCovering(..)
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completion
  , upOrOpenWithExpectedValues_expectedValueInterval_completion
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completion
  , downOrCloseWithExpectedValues_expectedValueInterval_completion
  , stopMotionWithParams_expectedValues_expectedValueInterval_completion
  , stopMotionWithExpectedValues_expectedValueInterval_completion
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completion
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completion
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completion
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeTypeWithParams
  , readAttributePhysicalClosedLimitLiftWithParams
  , readAttributePhysicalClosedLimitTiltWithParams
  , readAttributeCurrentPositionLiftWithParams
  , readAttributeCurrentPositionTiltWithParams
  , readAttributeNumberOfActuationsLiftWithParams
  , readAttributeNumberOfActuationsTiltWithParams
  , readAttributeConfigStatusWithParams
  , readAttributeCurrentPositionLiftPercentageWithParams
  , readAttributeCurrentPositionTiltPercentageWithParams
  , readAttributeOperationalStatusWithParams
  , readAttributeTargetPositionLiftPercent100thsWithParams
  , readAttributeTargetPositionTiltPercent100thsWithParams
  , readAttributeEndProductTypeWithParams
  , readAttributeCurrentPositionLiftPercent100thsWithParams
  , readAttributeCurrentPositionTiltPercent100thsWithParams
  , readAttributeInstalledOpenLimitLiftWithParams
  , readAttributeInstalledClosedLimitLiftWithParams
  , readAttributeInstalledOpenLimitTiltWithParams
  , readAttributeInstalledClosedLimitTiltWithParams
  , readAttributeModeWithParams
  , writeAttributeModeWithValue_expectedValueInterval
  , writeAttributeModeWithValue_expectedValueInterval_params
  , readAttributeSafetyStatusWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandler
  , upOrOpenWithExpectedValues_expectedValueInterval_completionHandler
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandler
  , downOrCloseWithExpectedValues_expectedValueInterval_completionHandler
  , stopMotionWithParams_expectedValues_expectedValueInterval_completionHandler
  , stopMotionWithExpectedValues_expectedValueInterval_completionHandler
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandler
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandler
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandler
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandler
  , initWithDevice_endpointID_queue
  , downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , downOrCloseWithExpectedValues_expectedValueInterval_completionSelector
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeConfigStatusWithParamsSelector
  , readAttributeCurrentPositionLiftPercent100thsWithParamsSelector
  , readAttributeCurrentPositionLiftPercentageWithParamsSelector
  , readAttributeCurrentPositionLiftWithParamsSelector
  , readAttributeCurrentPositionTiltPercent100thsWithParamsSelector
  , readAttributeCurrentPositionTiltPercentageWithParamsSelector
  , readAttributeCurrentPositionTiltWithParamsSelector
  , readAttributeEndProductTypeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeInstalledClosedLimitLiftWithParamsSelector
  , readAttributeInstalledClosedLimitTiltWithParamsSelector
  , readAttributeInstalledOpenLimitLiftWithParamsSelector
  , readAttributeInstalledOpenLimitTiltWithParamsSelector
  , readAttributeModeWithParamsSelector
  , readAttributeNumberOfActuationsLiftWithParamsSelector
  , readAttributeNumberOfActuationsTiltWithParamsSelector
  , readAttributeOperationalStatusWithParamsSelector
  , readAttributePhysicalClosedLimitLiftWithParamsSelector
  , readAttributePhysicalClosedLimitTiltWithParamsSelector
  , readAttributeSafetyStatusWithParamsSelector
  , readAttributeTargetPositionLiftPercent100thsWithParamsSelector
  , readAttributeTargetPositionTiltPercent100thsWithParamsSelector
  , readAttributeTypeWithParamsSelector
  , stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , stopMotionWithExpectedValues_expectedValueInterval_completionSelector
  , stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector
  , upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector
  , upOrOpenWithExpectedValues_expectedValueInterval_completionSelector
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector
  , upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector
  , writeAttributeModeWithValue_expectedValueIntervalSelector
  , writeAttributeModeWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- upOrOpenWithParams:expectedValues:expectedValueInterval:completion:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterUpOrOpenParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterUpOrOpenParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- upOrOpenWithExpectedValues:expectedValueInterval:completion:@
upOrOpenWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithExpectedValues_expectedValueInterval_completion mtrClusterWindowCovering expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering upOrOpenWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- downOrCloseWithParams:expectedValues:expectedValueInterval:completion:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterDownOrCloseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterDownOrCloseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- downOrCloseWithExpectedValues:expectedValueInterval:completion:@
downOrCloseWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithExpectedValues_expectedValueInterval_completion mtrClusterWindowCovering expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering downOrCloseWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopMotionWithParams:expectedValues:expectedValueInterval:completion:@
stopMotionWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterStopMotionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterStopMotionParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- stopMotionWithExpectedValues:expectedValueInterval:completion:@
stopMotionWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithExpectedValues_expectedValueInterval_completion mtrClusterWindowCovering expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering stopMotionWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftValueWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterGoToLiftValueParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterGoToLiftPercentageParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltValueWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterGoToTiltValueParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completion mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWindowCovering goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWindowCoveringClusterGoToTiltPercentageParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeTypeWithParams:@
readAttributeTypeWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeTypeWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePhysicalClosedLimitLiftWithParams:@
readAttributePhysicalClosedLimitLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributePhysicalClosedLimitLiftWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributePhysicalClosedLimitLiftWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePhysicalClosedLimitTiltWithParams:@
readAttributePhysicalClosedLimitTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributePhysicalClosedLimitTiltWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributePhysicalClosedLimitTiltWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionLiftWithParams:@
readAttributeCurrentPositionLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionLiftWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeCurrentPositionLiftWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionTiltWithParams:@
readAttributeCurrentPositionTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionTiltWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeCurrentPositionTiltWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNumberOfActuationsLiftWithParams:@
readAttributeNumberOfActuationsLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeNumberOfActuationsLiftWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeNumberOfActuationsLiftWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNumberOfActuationsTiltWithParams:@
readAttributeNumberOfActuationsTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeNumberOfActuationsTiltWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeNumberOfActuationsTiltWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeConfigStatusWithParams:@
readAttributeConfigStatusWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeConfigStatusWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeConfigStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionLiftPercentageWithParams:@
readAttributeCurrentPositionLiftPercentageWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionLiftPercentageWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeCurrentPositionLiftPercentageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionTiltPercentageWithParams:@
readAttributeCurrentPositionTiltPercentageWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionTiltPercentageWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeCurrentPositionTiltPercentageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeOperationalStatusWithParams:@
readAttributeOperationalStatusWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeOperationalStatusWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeOperationalStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTargetPositionLiftPercent100thsWithParams:@
readAttributeTargetPositionLiftPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeTargetPositionLiftPercent100thsWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeTargetPositionLiftPercent100thsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTargetPositionTiltPercent100thsWithParams:@
readAttributeTargetPositionTiltPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeTargetPositionTiltPercent100thsWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeTargetPositionTiltPercent100thsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEndProductTypeWithParams:@
readAttributeEndProductTypeWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeEndProductTypeWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeEndProductTypeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionLiftPercent100thsWithParams:@
readAttributeCurrentPositionLiftPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionLiftPercent100thsWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeCurrentPositionLiftPercent100thsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentPositionTiltPercent100thsWithParams:@
readAttributeCurrentPositionTiltPercent100thsWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeCurrentPositionTiltPercent100thsWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeCurrentPositionTiltPercent100thsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstalledOpenLimitLiftWithParams:@
readAttributeInstalledOpenLimitLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledOpenLimitLiftWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeInstalledOpenLimitLiftWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstalledClosedLimitLiftWithParams:@
readAttributeInstalledClosedLimitLiftWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledClosedLimitLiftWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeInstalledClosedLimitLiftWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstalledOpenLimitTiltWithParams:@
readAttributeInstalledOpenLimitTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledOpenLimitTiltWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeInstalledOpenLimitTiltWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeInstalledClosedLimitTiltWithParams:@
readAttributeInstalledClosedLimitTiltWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeInstalledClosedLimitTiltWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeInstalledClosedLimitTiltWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeModeWithParams:@
readAttributeModeWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeModeWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeModeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeModeWithValue:expectedValueInterval:@
writeAttributeModeWithValue_expectedValueInterval :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeModeWithValue_expectedValueInterval mtrClusterWindowCovering dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterWindowCovering writeAttributeModeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeModeWithValue:expectedValueInterval:params:@
writeAttributeModeWithValue_expectedValueInterval_params :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterWindowCovering -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeModeWithValue_expectedValueInterval_params mtrClusterWindowCovering dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterWindowCovering writeAttributeModeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSafetyStatusWithParams:@
readAttributeSafetyStatusWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeSafetyStatusWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeSafetyStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRReadParams params) => mtrClusterWindowCovering -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWindowCovering params =
  sendMessage mtrClusterWindowCovering readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWindowCovering mtrClusterWindowCovering => mtrClusterWindowCovering -> IO (Id MTRClusterWindowCovering)
init_ mtrClusterWindowCovering =
  sendOwnedMessage mtrClusterWindowCovering initSelector

-- | @+ new@
new :: IO (Id MTRClusterWindowCovering)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWindowCovering"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRDevice device, IsNSObject queue) => mtrClusterWindowCovering -> device -> CUShort -> queue -> IO (Id MTRClusterWindowCovering)
initWithDevice_endpoint_queue mtrClusterWindowCovering device endpoint queue =
  sendOwnedMessage mtrClusterWindowCovering initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | @- upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterUpOrOpenParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterUpOrOpenParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
upOrOpenWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterDownOrCloseParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterDownOrCloseParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
downOrCloseWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterStopMotionParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterStopMotionParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- stopMotionWithExpectedValues:expectedValueInterval:completionHandler:@
stopMotionWithExpectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
stopMotionWithExpectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering expectedValues expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterGoToLiftValueParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToLiftPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterGoToLiftPercentageParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltValueParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterGoToTiltValueParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | @- goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandler :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRWindowCoveringClusterGoToTiltPercentageParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWindowCovering -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandler mtrClusterWindowCovering params expectedDataValueDictionaries expectedValueIntervalMs completionHandler =
  sendMessage mtrClusterWindowCovering goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector (toMTRWindowCoveringClusterGoToTiltPercentageParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completionHandler

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWindowCovering mtrClusterWindowCovering, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWindowCovering -> device -> endpointID -> queue -> IO (Id MTRClusterWindowCovering)
initWithDevice_endpointID_queue mtrClusterWindowCovering device endpointID queue =
  sendOwnedMessage mtrClusterWindowCovering initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @upOrOpenWithParams:expectedValues:expectedValueInterval:completion:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterUpOrOpenParams, Id NSArray, Id NSNumber, Ptr ()] ()
upOrOpenWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "upOrOpenWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @upOrOpenWithExpectedValues:expectedValueInterval:completion:@
upOrOpenWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
upOrOpenWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "upOrOpenWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @downOrCloseWithParams:expectedValues:expectedValueInterval:completion:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterDownOrCloseParams, Id NSArray, Id NSNumber, Ptr ()] ()
downOrCloseWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "downOrCloseWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @downOrCloseWithExpectedValues:expectedValueInterval:completion:@
downOrCloseWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
downOrCloseWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "downOrCloseWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopMotionWithParams:expectedValues:expectedValueInterval:completion:@
stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterStopMotionParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopMotionWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "stopMotionWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @stopMotionWithExpectedValues:expectedValueInterval:completion:@
stopMotionWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopMotionWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "stopMotionWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterGoToLiftValueParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToLiftValueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterGoToLiftPercentageParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterGoToTiltValueParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToTiltValueWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWindowCoveringClusterGoToTiltPercentageParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeTypeWithParams:@
readAttributeTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTypeWithParamsSelector = mkSelector "readAttributeTypeWithParams:"

-- | @Selector@ for @readAttributePhysicalClosedLimitLiftWithParams:@
readAttributePhysicalClosedLimitLiftWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePhysicalClosedLimitLiftWithParamsSelector = mkSelector "readAttributePhysicalClosedLimitLiftWithParams:"

-- | @Selector@ for @readAttributePhysicalClosedLimitTiltWithParams:@
readAttributePhysicalClosedLimitTiltWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePhysicalClosedLimitTiltWithParamsSelector = mkSelector "readAttributePhysicalClosedLimitTiltWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionLiftWithParams:@
readAttributeCurrentPositionLiftWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionLiftWithParamsSelector = mkSelector "readAttributeCurrentPositionLiftWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionTiltWithParams:@
readAttributeCurrentPositionTiltWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionTiltWithParamsSelector = mkSelector "readAttributeCurrentPositionTiltWithParams:"

-- | @Selector@ for @readAttributeNumberOfActuationsLiftWithParams:@
readAttributeNumberOfActuationsLiftWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNumberOfActuationsLiftWithParamsSelector = mkSelector "readAttributeNumberOfActuationsLiftWithParams:"

-- | @Selector@ for @readAttributeNumberOfActuationsTiltWithParams:@
readAttributeNumberOfActuationsTiltWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNumberOfActuationsTiltWithParamsSelector = mkSelector "readAttributeNumberOfActuationsTiltWithParams:"

-- | @Selector@ for @readAttributeConfigStatusWithParams:@
readAttributeConfigStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeConfigStatusWithParamsSelector = mkSelector "readAttributeConfigStatusWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionLiftPercentageWithParams:@
readAttributeCurrentPositionLiftPercentageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionLiftPercentageWithParamsSelector = mkSelector "readAttributeCurrentPositionLiftPercentageWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionTiltPercentageWithParams:@
readAttributeCurrentPositionTiltPercentageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionTiltPercentageWithParamsSelector = mkSelector "readAttributeCurrentPositionTiltPercentageWithParams:"

-- | @Selector@ for @readAttributeOperationalStatusWithParams:@
readAttributeOperationalStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeOperationalStatusWithParamsSelector = mkSelector "readAttributeOperationalStatusWithParams:"

-- | @Selector@ for @readAttributeTargetPositionLiftPercent100thsWithParams:@
readAttributeTargetPositionLiftPercent100thsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTargetPositionLiftPercent100thsWithParamsSelector = mkSelector "readAttributeTargetPositionLiftPercent100thsWithParams:"

-- | @Selector@ for @readAttributeTargetPositionTiltPercent100thsWithParams:@
readAttributeTargetPositionTiltPercent100thsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTargetPositionTiltPercent100thsWithParamsSelector = mkSelector "readAttributeTargetPositionTiltPercent100thsWithParams:"

-- | @Selector@ for @readAttributeEndProductTypeWithParams:@
readAttributeEndProductTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEndProductTypeWithParamsSelector = mkSelector "readAttributeEndProductTypeWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionLiftPercent100thsWithParams:@
readAttributeCurrentPositionLiftPercent100thsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionLiftPercent100thsWithParamsSelector = mkSelector "readAttributeCurrentPositionLiftPercent100thsWithParams:"

-- | @Selector@ for @readAttributeCurrentPositionTiltPercent100thsWithParams:@
readAttributeCurrentPositionTiltPercent100thsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentPositionTiltPercent100thsWithParamsSelector = mkSelector "readAttributeCurrentPositionTiltPercent100thsWithParams:"

-- | @Selector@ for @readAttributeInstalledOpenLimitLiftWithParams:@
readAttributeInstalledOpenLimitLiftWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstalledOpenLimitLiftWithParamsSelector = mkSelector "readAttributeInstalledOpenLimitLiftWithParams:"

-- | @Selector@ for @readAttributeInstalledClosedLimitLiftWithParams:@
readAttributeInstalledClosedLimitLiftWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstalledClosedLimitLiftWithParamsSelector = mkSelector "readAttributeInstalledClosedLimitLiftWithParams:"

-- | @Selector@ for @readAttributeInstalledOpenLimitTiltWithParams:@
readAttributeInstalledOpenLimitTiltWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstalledOpenLimitTiltWithParamsSelector = mkSelector "readAttributeInstalledOpenLimitTiltWithParams:"

-- | @Selector@ for @readAttributeInstalledClosedLimitTiltWithParams:@
readAttributeInstalledClosedLimitTiltWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeInstalledClosedLimitTiltWithParamsSelector = mkSelector "readAttributeInstalledClosedLimitTiltWithParams:"

-- | @Selector@ for @readAttributeModeWithParams:@
readAttributeModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeModeWithParamsSelector = mkSelector "readAttributeModeWithParams:"

-- | @Selector@ for @writeAttributeModeWithValue:expectedValueInterval:@
writeAttributeModeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeModeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeModeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeModeWithValue:expectedValueInterval:params:@
writeAttributeModeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeModeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeModeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSafetyStatusWithParams:@
readAttributeSafetyStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSafetyStatusWithParamsSelector = mkSelector "readAttributeSafetyStatusWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterWindowCovering)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWindowCovering)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterWindowCovering)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterUpOrOpenParams, Id NSArray, Id NSNumber, Ptr ()] ()
upOrOpenWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "upOrOpenWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:@
upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
upOrOpenWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "upOrOpenWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterDownOrCloseParams, Id NSArray, Id NSNumber, Ptr ()] ()
downOrCloseWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "downOrCloseWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:@
downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
downOrCloseWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "downOrCloseWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:@
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterStopMotionParams, Id NSArray, Id NSNumber, Ptr ()] ()
stopMotionWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopMotionWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @stopMotionWithExpectedValues:expectedValueInterval:completionHandler:@
stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
stopMotionWithExpectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "stopMotionWithExpectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterGoToLiftValueParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToLiftValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToLiftValueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterGoToLiftPercentageParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToLiftPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToLiftPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterGoToTiltValueParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToTiltValueWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToTiltValueWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:@
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector :: Selector '[Id MTRWindowCoveringClusterGoToTiltPercentageParams, Id NSArray, Id NSNumber, Ptr ()] ()
goToTiltPercentageWithParams_expectedValues_expectedValueInterval_completionHandlerSelector = mkSelector "goToTiltPercentageWithParams:expectedValues:expectedValueInterval:completionHandler:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWindowCovering)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

