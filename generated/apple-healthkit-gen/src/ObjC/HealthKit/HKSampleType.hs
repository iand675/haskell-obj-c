{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSampleType
--
-- Represents a type of HKSample.
--
-- Generated bindings for @HKSampleType@.
module ObjC.HealthKit.HKSampleType
  ( HKSampleType
  , IsHKSampleType(..)
  , isMaximumDurationRestricted
  , maximumAllowedDuration
  , isMinimumDurationRestricted
  , minimumAllowedDuration
  , allowsRecalibrationForEstimates
  , allowsRecalibrationForEstimatesSelector
  , isMaximumDurationRestrictedSelector
  , isMinimumDurationRestrictedSelector
  , maximumAllowedDurationSelector
  , minimumAllowedDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | isMaximumDurationRestricted
--
-- Returns YES if the start and end date for samples of this type are restricted by a maximum duration.
--
-- ObjC selector: @- isMaximumDurationRestricted@
isMaximumDurationRestricted :: IsHKSampleType hkSampleType => hkSampleType -> IO Bool
isMaximumDurationRestricted hkSampleType =
  sendMessage hkSampleType isMaximumDurationRestrictedSelector

-- | maximumAllowedDuration
--
-- When the duration is restricted for samples of this type, returns the maximum duration allowed,                calculated as the difference between end and start dates.
--
-- Throws an exception if there is no maximum restriction on duration for samples of this type.
--
-- ObjC selector: @- maximumAllowedDuration@
maximumAllowedDuration :: IsHKSampleType hkSampleType => hkSampleType -> IO CDouble
maximumAllowedDuration hkSampleType =
  sendMessage hkSampleType maximumAllowedDurationSelector

-- | isMinimumDurationRestricted
--
-- Returns YES if the start and end date for samples of this type are restricted by a minimum duration.
--
-- ObjC selector: @- isMinimumDurationRestricted@
isMinimumDurationRestricted :: IsHKSampleType hkSampleType => hkSampleType -> IO Bool
isMinimumDurationRestricted hkSampleType =
  sendMessage hkSampleType isMinimumDurationRestrictedSelector

-- | minimumAllowedDuration
--
-- When the duration is restricted for samples of this type, returns the minimum duration allowed,                calculated as the difference between end and start dates.
--
-- Throws an exception if there is no minimum restriction on duration for samples of this type.
--
-- ObjC selector: @- minimumAllowedDuration@
minimumAllowedDuration :: IsHKSampleType hkSampleType => hkSampleType -> IO CDouble
minimumAllowedDuration hkSampleType =
  sendMessage hkSampleType minimumAllowedDurationSelector

-- | allowsRecalibrationForEstimates
--
-- Returns YES if first-party samples of this type are produced using a prediction algorithm, and that algorithm supports recalibration. To recalibrate the                estimates for a sample type, see -[HKHealthStore recalibrateEstimatesForSampleType:atDate:completion:]
--
-- ObjC selector: @- allowsRecalibrationForEstimates@
allowsRecalibrationForEstimates :: IsHKSampleType hkSampleType => hkSampleType -> IO Bool
allowsRecalibrationForEstimates hkSampleType =
  sendMessage hkSampleType allowsRecalibrationForEstimatesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isMaximumDurationRestricted@
isMaximumDurationRestrictedSelector :: Selector '[] Bool
isMaximumDurationRestrictedSelector = mkSelector "isMaximumDurationRestricted"

-- | @Selector@ for @maximumAllowedDuration@
maximumAllowedDurationSelector :: Selector '[] CDouble
maximumAllowedDurationSelector = mkSelector "maximumAllowedDuration"

-- | @Selector@ for @isMinimumDurationRestricted@
isMinimumDurationRestrictedSelector :: Selector '[] Bool
isMinimumDurationRestrictedSelector = mkSelector "isMinimumDurationRestricted"

-- | @Selector@ for @minimumAllowedDuration@
minimumAllowedDurationSelector :: Selector '[] CDouble
minimumAllowedDurationSelector = mkSelector "minimumAllowedDuration"

-- | @Selector@ for @allowsRecalibrationForEstimates@
allowsRecalibrationForEstimatesSelector :: Selector '[] Bool
allowsRecalibrationForEstimatesSelector = mkSelector "allowsRecalibrationForEstimates"

