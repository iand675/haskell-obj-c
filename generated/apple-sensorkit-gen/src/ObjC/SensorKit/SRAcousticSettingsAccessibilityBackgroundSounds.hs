{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsAccessibilityBackgroundSounds@.
module ObjC.SensorKit.SRAcousticSettingsAccessibilityBackgroundSounds
  ( SRAcousticSettingsAccessibilityBackgroundSounds
  , IsSRAcousticSettingsAccessibilityBackgroundSounds(..)
  , enabled
  , soundName
  , relativeVolume
  , playWithMediaEnabled
  , relativeVolumeWithMedia
  , stopOnLockEnabled
  , enabledSelector
  , playWithMediaEnabledSelector
  , relativeVolumeSelector
  , relativeVolumeWithMediaSelector
  , soundNameSelector
  , stopOnLockEnabledSelector

  -- * Enum types
  , SRAcousticSettingsAccessibilityBackgroundSoundsName(SRAcousticSettingsAccessibilityBackgroundSoundsName)
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBalancedNoise
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBrightNoise
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameDarkNoise
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameOcean
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameRain
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameStream
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameNight
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameFire
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBabble
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameSteam
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameAirplane
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBoat
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameBus
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameTrain
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameRainOnRoof
  , pattern SRAcousticSettingsAccessibilityBackgroundSoundsNameQuietNight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.SensorKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | enabled
--
-- Background Sounds is turned on/off
--
-- Plays background sounds to mask unwanted environmental noise.
--
-- ObjC selector: @- enabled@
enabled :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO Bool
enabled srAcousticSettingsAccessibilityBackgroundSounds =
  sendMessage srAcousticSettingsAccessibilityBackgroundSounds enabledSelector

-- | soundName
--
-- Accessibility Background sounds name
--
-- ObjC selector: @- soundName@
soundName :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO SRAcousticSettingsAccessibilityBackgroundSoundsName
soundName srAcousticSettingsAccessibilityBackgroundSounds =
  sendMessage srAcousticSettingsAccessibilityBackgroundSounds soundNameSelector

-- | relativeVolume
--
-- Accessibility Background sounds volume
--
-- Background sounds volume relative to system volume. Units is a percentage.
--
-- ObjC selector: @- relativeVolume@
relativeVolume :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO CDouble
relativeVolume srAcousticSettingsAccessibilityBackgroundSounds =
  sendMessage srAcousticSettingsAccessibilityBackgroundSounds relativeVolumeSelector

-- | playWithMediaEnabled
--
-- Background sounds is to be played while media is also playing
--
-- ObjC selector: @- playWithMediaEnabled@
playWithMediaEnabled :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO Bool
playWithMediaEnabled srAcousticSettingsAccessibilityBackgroundSounds =
  sendMessage srAcousticSettingsAccessibilityBackgroundSounds playWithMediaEnabledSelector

-- | relativeVolumeWithMedia
--
-- Accessibility Background sounds volume with media
--
-- Background sounds volume while media is playing. Units is a percentage.
--
-- ObjC selector: @- relativeVolumeWithMedia@
relativeVolumeWithMedia :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO CDouble
relativeVolumeWithMedia srAcousticSettingsAccessibilityBackgroundSounds =
  sendMessage srAcousticSettingsAccessibilityBackgroundSounds relativeVolumeWithMediaSelector

-- | stopOnLockEnabled
--
-- Stop background sounds when iPhone is locked
--
-- ObjC selector: @- stopOnLockEnabled@
stopOnLockEnabled :: IsSRAcousticSettingsAccessibilityBackgroundSounds srAcousticSettingsAccessibilityBackgroundSounds => srAcousticSettingsAccessibilityBackgroundSounds -> IO Bool
stopOnLockEnabled srAcousticSettingsAccessibilityBackgroundSounds =
  sendMessage srAcousticSettingsAccessibilityBackgroundSounds stopOnLockEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @soundName@
soundNameSelector :: Selector '[] SRAcousticSettingsAccessibilityBackgroundSoundsName
soundNameSelector = mkSelector "soundName"

-- | @Selector@ for @relativeVolume@
relativeVolumeSelector :: Selector '[] CDouble
relativeVolumeSelector = mkSelector "relativeVolume"

-- | @Selector@ for @playWithMediaEnabled@
playWithMediaEnabledSelector :: Selector '[] Bool
playWithMediaEnabledSelector = mkSelector "playWithMediaEnabled"

-- | @Selector@ for @relativeVolumeWithMedia@
relativeVolumeWithMediaSelector :: Selector '[] CDouble
relativeVolumeWithMediaSelector = mkSelector "relativeVolumeWithMedia"

-- | @Selector@ for @stopOnLockEnabled@
stopOnLockEnabledSelector :: Selector '[] Bool
stopOnLockEnabledSelector = mkSelector "stopOnLockEnabled"

