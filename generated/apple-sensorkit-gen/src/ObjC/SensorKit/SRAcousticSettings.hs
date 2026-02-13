{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettings@.
module ObjC.SensorKit.SRAcousticSettings
  ( SRAcousticSettings
  , IsSRAcousticSettings(..)
  , init_
  , new
  , environmentalSoundMeasurementsEnabled
  , audioExposureSampleLifetime
  , headphoneSafetyAudioLevel
  , musicEQSettings
  , accessibilitySettings
  , accessibilitySettingsSelector
  , audioExposureSampleLifetimeSelector
  , environmentalSoundMeasurementsEnabledSelector
  , headphoneSafetyAudioLevelSelector
  , initSelector
  , musicEQSettingsSelector
  , newSelector

  -- * Enum types
  , SRAcousticSettingsSampleLifetime(SRAcousticSettingsSampleLifetime)
  , pattern SRAcousticSettingsSampleLifetimeEightDays
  , pattern SRAcousticSettingsSampleLifetimeUntilUserDeletes

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

-- | @- init@
init_ :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id SRAcousticSettings)
init_ srAcousticSettings =
  sendOwnedMessage srAcousticSettings initSelector

-- | @+ new@
new :: IO (Id SRAcousticSettings)
new  =
  do
    cls' <- getRequiredClass "SRAcousticSettings"
    sendOwnedClassMessage cls' newSelector

-- | environmentalSoundMeasurementsEnabled
--
-- Environmental Sound Measurements
--
-- Setting for Apple Watch Environmental Sound Measurements.
--
-- ObjC selector: @- environmentalSoundMeasurementsEnabled@
environmentalSoundMeasurementsEnabled :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO Bool
environmentalSoundMeasurementsEnabled srAcousticSettings =
  sendMessage srAcousticSettings environmentalSoundMeasurementsEnabledSelector

-- | audioExposureSampleLifetime
--
-- Expected lifetime of headphone audio exposure samples in HealthKit
--
-- ObjC selector: @- audioExposureSampleLifetime@
audioExposureSampleLifetime :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO SRAcousticSettingsSampleLifetime
audioExposureSampleLifetime srAcousticSettings =
  sendMessage srAcousticSettings audioExposureSampleLifetimeSelector

-- | headphoneSafetyAudioLevel
--
-- Reduce Loud Audio
--
-- iPhone can analyze headphone audio and reduce any sound that is over a set decibel level. A nil value  means the setting is disabled. If the setting is enabled, the property will hold the decibel value  that headphone audio sound volume is not to exceed.
--
-- ObjC selector: @- headphoneSafetyAudioLevel@
headphoneSafetyAudioLevel :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id NSNumber)
headphoneSafetyAudioLevel srAcousticSettings =
  sendMessage srAcousticSettings headphoneSafetyAudioLevelSelector

-- | musicEQSettings
--
-- Music EQ Settings
--
-- ObjC selector: @- musicEQSettings@
musicEQSettings :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id SRAcousticSettingsMusicEQ)
musicEQSettings srAcousticSettings =
  sendMessage srAcousticSettings musicEQSettingsSelector

-- | accessibilitySettings
--
-- Accessibility Settings
--
-- ObjC selector: @- accessibilitySettings@
accessibilitySettings :: IsSRAcousticSettings srAcousticSettings => srAcousticSettings -> IO (Id SRAcousticSettingsAccessibility)
accessibilitySettings srAcousticSettings =
  sendMessage srAcousticSettings accessibilitySettingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SRAcousticSettings)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SRAcousticSettings)
newSelector = mkSelector "new"

-- | @Selector@ for @environmentalSoundMeasurementsEnabled@
environmentalSoundMeasurementsEnabledSelector :: Selector '[] Bool
environmentalSoundMeasurementsEnabledSelector = mkSelector "environmentalSoundMeasurementsEnabled"

-- | @Selector@ for @audioExposureSampleLifetime@
audioExposureSampleLifetimeSelector :: Selector '[] SRAcousticSettingsSampleLifetime
audioExposureSampleLifetimeSelector = mkSelector "audioExposureSampleLifetime"

-- | @Selector@ for @headphoneSafetyAudioLevel@
headphoneSafetyAudioLevelSelector :: Selector '[] (Id NSNumber)
headphoneSafetyAudioLevelSelector = mkSelector "headphoneSafetyAudioLevel"

-- | @Selector@ for @musicEQSettings@
musicEQSettingsSelector :: Selector '[] (Id SRAcousticSettingsMusicEQ)
musicEQSettingsSelector = mkSelector "musicEQSettings"

-- | @Selector@ for @accessibilitySettings@
accessibilitySettingsSelector :: Selector '[] (Id SRAcousticSettingsAccessibility)
accessibilitySettingsSelector = mkSelector "accessibilitySettings"

