{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKAudiogramSensitivityPoint@.
module ObjC.HealthKit.HKAudiogramSensitivityPoint
  ( HKAudiogramSensitivityPoint
  , IsHKAudiogramSensitivityPoint(..)
  , sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_error
  , sensitivityPointWithFrequency_tests_error
  , init_
  , frequency
  , leftEarSensitivity
  , rightEarSensitivity
  , tests
  , frequencySelector
  , initSelector
  , leftEarSensitivitySelector
  , rightEarSensitivitySelector
  , sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector
  , sensitivityPointWithFrequency_tests_errorSelector
  , testsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:
--
-- Creates a point that can be included in a audiogram.
--
-- @frequency@ — Frequency where sensitivity was measured.
--
-- @leftEarSensitivity@ — Left ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- @rightEarSensitivity@ — Right ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- @error@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a sensitivity point or nil if there were problems                            creating the instance.  Errors may include incorrect quantity units                            or data that is out of an expected range.
--
-- ObjC selector: @+ sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:@
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_error :: (IsHKQuantity frequency, IsHKQuantity leftEarSensitivity, IsHKQuantity rightEarSensitivity, IsNSError error_) => frequency -> leftEarSensitivity -> rightEarSensitivity -> error_ -> IO (Id HKAudiogramSensitivityPoint)
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_error frequency leftEarSensitivity rightEarSensitivity error_ =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityPoint"
    sendClassMessage cls' sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector (toHKQuantity frequency) (toHKQuantity leftEarSensitivity) (toHKQuantity rightEarSensitivity) (toNSError error_)

-- | sensitivityPointWithFrequency:tests:error:
--
-- Creates a point that can be included in a audiogram.
--
-- @frequency@ — Frequency at which sensitivity was measured.
--
-- @tests@ — The tests conducted at the frequency
--
-- @errorOut@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a sensitivity point or nil if there were problems                            creating the instance.  Errors may include incorrect quantity units                            or data that is out of an expected range.
--
-- ObjC selector: @+ sensitivityPointWithFrequency:tests:error:@
sensitivityPointWithFrequency_tests_error :: (IsHKQuantity frequency, IsNSArray tests, IsNSError errorOut) => frequency -> tests -> errorOut -> IO (Id HKAudiogramSensitivityPoint)
sensitivityPointWithFrequency_tests_error frequency tests errorOut =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityPoint"
    sendClassMessage cls' sensitivityPointWithFrequency_tests_errorSelector (toHKQuantity frequency) (toNSArray tests) (toNSError errorOut)

-- | @- init@
init_ :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKAudiogramSensitivityPoint)
init_ hkAudiogramSensitivityPoint =
  sendOwnedMessage hkAudiogramSensitivityPoint initSelector

-- | frequency  Frequency where sensitivity was measured.  The unit of measurement
--
-- is [HKUnit hertzUnit] or "Hz".
--
-- ObjC selector: @- frequency@
frequency :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKQuantity)
frequency hkAudiogramSensitivityPoint =
  sendMessage hkAudiogramSensitivityPoint frequencySelector

-- | sensitivity Left ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- The unit of measurement is @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- ObjC selector: @- leftEarSensitivity@
leftEarSensitivity :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKQuantity)
leftEarSensitivity hkAudiogramSensitivityPoint =
  sendMessage hkAudiogramSensitivityPoint leftEarSensitivitySelector

-- | sensitivity Right ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB.
--
-- The unit of measurement is @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- ObjC selector: @- rightEarSensitivity@
rightEarSensitivity :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id HKQuantity)
rightEarSensitivity hkAudiogramSensitivityPoint =
  sendMessage hkAudiogramSensitivityPoint rightEarSensitivitySelector

-- | tests
--
-- The tests conducted at this frequency
--
-- ObjC selector: @- tests@
tests :: IsHKAudiogramSensitivityPoint hkAudiogramSensitivityPoint => hkAudiogramSensitivityPoint -> IO (Id NSArray)
tests hkAudiogramSensitivityPoint =
  sendMessage hkAudiogramSensitivityPoint testsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:@
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector :: Selector '[Id HKQuantity, Id HKQuantity, Id HKQuantity, Id NSError] (Id HKAudiogramSensitivityPoint)
sensitivityPointWithFrequency_leftEarSensitivity_rightEarSensitivity_errorSelector = mkSelector "sensitivityPointWithFrequency:leftEarSensitivity:rightEarSensitivity:error:"

-- | @Selector@ for @sensitivityPointWithFrequency:tests:error:@
sensitivityPointWithFrequency_tests_errorSelector :: Selector '[Id HKQuantity, Id NSArray, Id NSError] (Id HKAudiogramSensitivityPoint)
sensitivityPointWithFrequency_tests_errorSelector = mkSelector "sensitivityPointWithFrequency:tests:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKAudiogramSensitivityPoint)
initSelector = mkSelector "init"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] (Id HKQuantity)
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @leftEarSensitivity@
leftEarSensitivitySelector :: Selector '[] (Id HKQuantity)
leftEarSensitivitySelector = mkSelector "leftEarSensitivity"

-- | @Selector@ for @rightEarSensitivity@
rightEarSensitivitySelector :: Selector '[] (Id HKQuantity)
rightEarSensitivitySelector = mkSelector "rightEarSensitivity"

-- | @Selector@ for @tests@
testsSelector :: Selector '[] (Id NSArray)
testsSelector = mkSelector "tests"

