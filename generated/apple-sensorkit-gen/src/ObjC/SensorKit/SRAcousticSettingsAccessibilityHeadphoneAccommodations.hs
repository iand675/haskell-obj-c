{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsAccessibilityHeadphoneAccommodations@.
module ObjC.SensorKit.SRAcousticSettingsAccessibilityHeadphoneAccommodations
  ( SRAcousticSettingsAccessibilityHeadphoneAccommodations
  , IsSRAcousticSettingsAccessibilityHeadphoneAccommodations(..)
  , enabled
  , mediaEnhanceTuning
  , mediaEnhanceBoosting
  , mediaEnhanceApplication
  , enabledSelector
  , mediaEnhanceApplicationSelector
  , mediaEnhanceBoostingSelector
  , mediaEnhanceTuningSelector

  -- * Enum types
  , SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication(SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication)
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationNone
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationPhone
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationMedia
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplicationPhoneAndMedia
  , SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting(SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting)
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoostingSlight
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoostingModerate
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoostingStrong
  , SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning(SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning)
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuningBalancedTone
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuningVocalRange
  , pattern SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuningBrightness

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
-- Headphone Accommodations is turned on/off
--
-- ObjC selector: @- enabled@
enabled :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO Bool
enabled srAcousticSettingsAccessibilityHeadphoneAccommodations =
  sendMessage srAcousticSettingsAccessibilityHeadphoneAccommodations enabledSelector

-- | mediaEnhanceTuning
--
-- Tune for different range of frequencies
--
-- Optimize for providing audio tuning for different ranges of frequencies.
--
-- ObjC selector: @- mediaEnhanceTuning@
mediaEnhanceTuning :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning
mediaEnhanceTuning srAcousticSettingsAccessibilityHeadphoneAccommodations =
  sendMessage srAcousticSettingsAccessibilityHeadphoneAccommodations mediaEnhanceTuningSelector

-- | mediaEnhanceBoosting
--
-- Soft Sounds boost level
--
-- Soft sounds will be boosted slightly, moderately, or strongly.
--
-- ObjC selector: @- mediaEnhanceBoosting@
mediaEnhanceBoosting :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting
mediaEnhanceBoosting srAcousticSettingsAccessibilityHeadphoneAccommodations =
  sendMessage srAcousticSettingsAccessibilityHeadphoneAccommodations mediaEnhanceBoostingSelector

-- | mediaEnhanceApplication
--
-- Headphone Accommodations Application
--
-- Headphone Accommodations Apply to phone, media, or both.
--
-- ObjC selector: @- mediaEnhanceApplication@
mediaEnhanceApplication :: IsSRAcousticSettingsAccessibilityHeadphoneAccommodations srAcousticSettingsAccessibilityHeadphoneAccommodations => srAcousticSettingsAccessibilityHeadphoneAccommodations -> IO SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication
mediaEnhanceApplication srAcousticSettingsAccessibilityHeadphoneAccommodations =
  sendMessage srAcousticSettingsAccessibilityHeadphoneAccommodations mediaEnhanceApplicationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @mediaEnhanceTuning@
mediaEnhanceTuningSelector :: Selector '[] SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceTuning
mediaEnhanceTuningSelector = mkSelector "mediaEnhanceTuning"

-- | @Selector@ for @mediaEnhanceBoosting@
mediaEnhanceBoostingSelector :: Selector '[] SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceBoosting
mediaEnhanceBoostingSelector = mkSelector "mediaEnhanceBoosting"

-- | @Selector@ for @mediaEnhanceApplication@
mediaEnhanceApplicationSelector :: Selector '[] SRAcousticSettingsAccessibilityHeadphoneAccommodationsMediaEnhanceApplication
mediaEnhanceApplicationSelector = mkSelector "mediaEnhanceApplication"

