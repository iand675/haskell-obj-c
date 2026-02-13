{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines the range within which an ear's sensitivity point may have been clamped, if any.
--
-- At times, it may be required to indicate that a sensitivity point has been clamped to a range. These reasons include but are not limited to user safety, hardware limitations, or algorithm features.
--
-- Generated bindings for @HKAudiogramSensitivityPointClampingRange@.
module ObjC.HealthKit.HKAudiogramSensitivityPointClampingRange
  ( HKAudiogramSensitivityPointClampingRange
  , IsHKAudiogramSensitivityPointClampingRange(..)
  , init_
  , clampingRangeWithLowerBound_upperBound_error
  , lowerBound
  , upperBound
  , clampingRangeWithLowerBound_upperBound_errorSelector
  , initSelector
  , lowerBoundSelector
  , upperBoundSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKAudiogramSensitivityPointClampingRange hkAudiogramSensitivityPointClampingRange => hkAudiogramSensitivityPointClampingRange -> IO (Id HKAudiogramSensitivityPointClampingRange)
init_ hkAudiogramSensitivityPointClampingRange =
  sendOwnedMessage hkAudiogramSensitivityPointClampingRange initSelector

-- | clampingRangeWithLowerBound:upperBound:error:
--
-- Creates a clamping range from a given lower and upper bound. At least one bound must be specified. If both bounds are provided, the lower bound must be less than the upper bound.
--
-- @lowerBound@ — The lower bound of the clamping range (if any)
--
-- @upperBound@ — The upper bound of the clamping range (if any)
--
-- @errorOut@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a clamping range or nil if there were problems                            creating the instance.  Errors may include not having any bound or lower bound is greater than the upper bound
--
-- ObjC selector: @+ clampingRangeWithLowerBound:upperBound:error:@
clampingRangeWithLowerBound_upperBound_error :: (IsNSNumber lowerBound, IsNSNumber upperBound, IsNSError errorOut) => lowerBound -> upperBound -> errorOut -> IO (Id HKAudiogramSensitivityPointClampingRange)
clampingRangeWithLowerBound_upperBound_error lowerBound upperBound errorOut =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityPointClampingRange"
    sendClassMessage cls' clampingRangeWithLowerBound_upperBound_errorSelector (toNSNumber lowerBound) (toNSNumber upperBound) (toNSError errorOut)

-- | lowerBound
--
-- The lower bound of the clamping range, if any, in dBHL.
--
-- ObjC selector: @- lowerBound@
lowerBound :: IsHKAudiogramSensitivityPointClampingRange hkAudiogramSensitivityPointClampingRange => hkAudiogramSensitivityPointClampingRange -> IO (Id HKQuantity)
lowerBound hkAudiogramSensitivityPointClampingRange =
  sendMessage hkAudiogramSensitivityPointClampingRange lowerBoundSelector

-- | upperBound
--
-- The upper bound of the clamping range, if any, in dBHL.
--
-- ObjC selector: @- upperBound@
upperBound :: IsHKAudiogramSensitivityPointClampingRange hkAudiogramSensitivityPointClampingRange => hkAudiogramSensitivityPointClampingRange -> IO (Id HKQuantity)
upperBound hkAudiogramSensitivityPointClampingRange =
  sendMessage hkAudiogramSensitivityPointClampingRange upperBoundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKAudiogramSensitivityPointClampingRange)
initSelector = mkSelector "init"

-- | @Selector@ for @clampingRangeWithLowerBound:upperBound:error:@
clampingRangeWithLowerBound_upperBound_errorSelector :: Selector '[Id NSNumber, Id NSNumber, Id NSError] (Id HKAudiogramSensitivityPointClampingRange)
clampingRangeWithLowerBound_upperBound_errorSelector = mkSelector "clampingRangeWithLowerBound:upperBound:error:"

-- | @Selector@ for @lowerBound@
lowerBoundSelector :: Selector '[] (Id HKQuantity)
lowerBoundSelector = mkSelector "lowerBound"

-- | @Selector@ for @upperBound@
upperBoundSelector :: Selector '[] (Id HKQuantity)
upperBoundSelector = mkSelector "upperBound"

