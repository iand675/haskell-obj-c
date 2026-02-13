{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsAccessibility@.
module ObjC.SensorKit.SRAcousticSettingsAccessibility
  ( SRAcousticSettingsAccessibility
  , IsSRAcousticSettingsAccessibility(..)
  , leftRightBalance
  , monoAudioEnabled
  , backgroundSounds
  , headphoneAccommodations
  , backgroundSoundsSelector
  , headphoneAccommodationsSelector
  , leftRightBalanceSelector
  , monoAudioEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | leftRightBalance
--
-- Audio volume between left and right channels
--
-- ObjC selector: @- leftRightBalance@
leftRightBalance :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO CDouble
leftRightBalance srAcousticSettingsAccessibility =
  sendMessage srAcousticSettingsAccessibility leftRightBalanceSelector

-- | monoAudioEnabled
--
-- When in mono mode, audio output is the same audio from both the left and right channels
--
-- ObjC selector: @- monoAudioEnabled@
monoAudioEnabled :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO Bool
monoAudioEnabled srAcousticSettingsAccessibility =
  sendMessage srAcousticSettingsAccessibility monoAudioEnabledSelector

-- | backgroundSounds
--
-- Background Sounds Settings
--
-- ObjC selector: @- backgroundSounds@
backgroundSounds :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO (Id SRAcousticSettingsAccessibilityBackgroundSounds)
backgroundSounds srAcousticSettingsAccessibility =
  sendMessage srAcousticSettingsAccessibility backgroundSoundsSelector

-- | headphoneAccommodations
--
-- Headphone Accommodations Settings
--
-- ObjC selector: @- headphoneAccommodations@
headphoneAccommodations :: IsSRAcousticSettingsAccessibility srAcousticSettingsAccessibility => srAcousticSettingsAccessibility -> IO (Id SRAcousticSettingsAccessibilityHeadphoneAccommodations)
headphoneAccommodations srAcousticSettingsAccessibility =
  sendMessage srAcousticSettingsAccessibility headphoneAccommodationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @leftRightBalance@
leftRightBalanceSelector :: Selector '[] CDouble
leftRightBalanceSelector = mkSelector "leftRightBalance"

-- | @Selector@ for @monoAudioEnabled@
monoAudioEnabledSelector :: Selector '[] Bool
monoAudioEnabledSelector = mkSelector "monoAudioEnabled"

-- | @Selector@ for @backgroundSounds@
backgroundSoundsSelector :: Selector '[] (Id SRAcousticSettingsAccessibilityBackgroundSounds)
backgroundSoundsSelector = mkSelector "backgroundSounds"

-- | @Selector@ for @headphoneAccommodations@
headphoneAccommodationsSelector :: Selector '[] (Id SRAcousticSettingsAccessibilityHeadphoneAccommodations)
headphoneAccommodationsSelector = mkSelector "headphoneAccommodations"

