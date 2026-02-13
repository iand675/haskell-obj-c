{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKAudiogramSensitivityTest@.
module ObjC.HealthKit.HKAudiogramSensitivityTest
  ( HKAudiogramSensitivityTest
  , IsHKAudiogramSensitivityTest(..)
  , initWithSensitivity_type_masked_side_clampingRange_error
  , init_
  , new
  , sensitivity
  , type_
  , masked
  , side
  , clampingRange
  , clampingRangeSelector
  , initSelector
  , initWithSensitivity_type_masked_side_clampingRange_errorSelector
  , maskedSelector
  , newSelector
  , sensitivitySelector
  , sideSelector
  , typeSelector

  -- * Enum types
  , HKAudiogramConductionType(HKAudiogramConductionType)
  , pattern HKAudiogramConductionTypeAir
  , HKAudiogramSensitivityTestSide(HKAudiogramSensitivityTestSide)
  , pattern HKAudiogramSensitivityTestSideLeft
  , pattern HKAudiogramSensitivityTestSideRight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithSensitivity:type:masked:side:clampingRange:error:
--
-- Creates a sensitivity test which can be added to a HKAudiogramSensitivityPoint
--
-- @sensitivity@ — The ear sensitivity measured in dB from a baseline of 0 dB with unit @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- @type@ — The type of test
--
-- @masked@ — If the test was conducted with or without masking
--
-- @side@ — The test side which was tested
--
-- @clampingRange@ — The clamping range (if any)
--
-- @errorOut@ — If there was a problem creating this instance this will contain the error.
--
-- Returns: New instance of a Sensitivity Test or nil if there were problems                            creating the instance.  Errors may include incorrect quantity units or sensitivity out of range
--
-- ObjC selector: @- initWithSensitivity:type:masked:side:clampingRange:error:@
initWithSensitivity_type_masked_side_clampingRange_error :: (IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest, IsHKQuantity sensitivity, IsHKAudiogramSensitivityPointClampingRange clampingRange, IsNSError errorOut) => hkAudiogramSensitivityTest -> sensitivity -> HKAudiogramConductionType -> Bool -> HKAudiogramSensitivityTestSide -> clampingRange -> errorOut -> IO (Id HKAudiogramSensitivityTest)
initWithSensitivity_type_masked_side_clampingRange_error hkAudiogramSensitivityTest sensitivity type_ masked side clampingRange errorOut =
  sendOwnedMessage hkAudiogramSensitivityTest initWithSensitivity_type_masked_side_clampingRange_errorSelector (toHKQuantity sensitivity) type_ masked side (toHKAudiogramSensitivityPointClampingRange clampingRange) (toNSError errorOut)

-- | @- init@
init_ :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO (Id HKAudiogramSensitivityTest)
init_ hkAudiogramSensitivityTest =
  sendOwnedMessage hkAudiogramSensitivityTest initSelector

-- | @+ new@
new :: IO (Id HKAudiogramSensitivityTest)
new  =
  do
    cls' <- getRequiredClass "HKAudiogramSensitivityTest"
    sendOwnedClassMessage cls' newSelector

-- | sensitivity
--
-- Ear sensitivity measured in dB from a baseline of 0 dB. Reduced hearing sensitivity corresponds to an increase from 0 dB. The unit of measurement is @HKUnit.decibelHearingLevelUnit@ or "dBHL".
--
-- ObjC selector: @- sensitivity@
sensitivity :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO (Id HKQuantity)
sensitivity hkAudiogramSensitivityTest =
  sendMessage hkAudiogramSensitivityTest sensitivitySelector

-- | type
--
-- The conduction type
--
-- ObjC selector: @- type@
type_ :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO HKAudiogramConductionType
type_ hkAudiogramSensitivityTest =
  sendMessage hkAudiogramSensitivityTest typeSelector

-- | masked
--
-- Indicates if the test was conducted with or without masking
--
-- ObjC selector: @- masked@
masked :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO Bool
masked hkAudiogramSensitivityTest =
  sendMessage hkAudiogramSensitivityTest maskedSelector

-- | side
--
-- The test side
--
-- ObjC selector: @- side@
side :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO HKAudiogramSensitivityTestSide
side hkAudiogramSensitivityTest =
  sendMessage hkAudiogramSensitivityTest sideSelector

-- | clampingRange
--
-- If present, indicates that the range within which the sensitivity point should be clamped.
--
-- ObjC selector: @- clampingRange@
clampingRange :: IsHKAudiogramSensitivityTest hkAudiogramSensitivityTest => hkAudiogramSensitivityTest -> IO (Id HKAudiogramSensitivityPointClampingRange)
clampingRange hkAudiogramSensitivityTest =
  sendMessage hkAudiogramSensitivityTest clampingRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSensitivity:type:masked:side:clampingRange:error:@
initWithSensitivity_type_masked_side_clampingRange_errorSelector :: Selector '[Id HKQuantity, HKAudiogramConductionType, Bool, HKAudiogramSensitivityTestSide, Id HKAudiogramSensitivityPointClampingRange, Id NSError] (Id HKAudiogramSensitivityTest)
initWithSensitivity_type_masked_side_clampingRange_errorSelector = mkSelector "initWithSensitivity:type:masked:side:clampingRange:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKAudiogramSensitivityTest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id HKAudiogramSensitivityTest)
newSelector = mkSelector "new"

-- | @Selector@ for @sensitivity@
sensitivitySelector :: Selector '[] (Id HKQuantity)
sensitivitySelector = mkSelector "sensitivity"

-- | @Selector@ for @type@
typeSelector :: Selector '[] HKAudiogramConductionType
typeSelector = mkSelector "type"

-- | @Selector@ for @masked@
maskedSelector :: Selector '[] Bool
maskedSelector = mkSelector "masked"

-- | @Selector@ for @side@
sideSelector :: Selector '[] HKAudiogramSensitivityTestSide
sideSelector = mkSelector "side"

-- | @Selector@ for @clampingRange@
clampingRangeSelector :: Selector '[] (Id HKAudiogramSensitivityPointClampingRange)
clampingRangeSelector = mkSelector "clampingRange"

