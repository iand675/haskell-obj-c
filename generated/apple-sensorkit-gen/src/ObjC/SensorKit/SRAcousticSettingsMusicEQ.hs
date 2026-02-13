{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SRAcousticSettingsMusicEQ@.
module ObjC.SensorKit.SRAcousticSettingsMusicEQ
  ( SRAcousticSettingsMusicEQ
  , IsSRAcousticSettingsMusicEQ(..)
  , soundCheckEnabled
  , lateNightModeEnabled
  , lateNightModeEnabledSelector
  , soundCheckEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensorKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | soundCheckEnabled
--
-- Sound Check is turned on/off
--
-- ObjC selector: @- soundCheckEnabled@
soundCheckEnabled :: IsSRAcousticSettingsMusicEQ srAcousticSettingsMusicEQ => srAcousticSettingsMusicEQ -> IO Bool
soundCheckEnabled srAcousticSettingsMusicEQ =
  sendMessage srAcousticSettingsMusicEQ soundCheckEnabledSelector

-- | lateNightModeEnabled
--
-- Late Night Mode is turned on/off
--
-- Music EQ Setting to dynamically compress system level audio
--
-- ObjC selector: @- lateNightModeEnabled@
lateNightModeEnabled :: IsSRAcousticSettingsMusicEQ srAcousticSettingsMusicEQ => srAcousticSettingsMusicEQ -> IO Bool
lateNightModeEnabled srAcousticSettingsMusicEQ =
  sendMessage srAcousticSettingsMusicEQ lateNightModeEnabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @soundCheckEnabled@
soundCheckEnabledSelector :: Selector '[] Bool
soundCheckEnabledSelector = mkSelector "soundCheckEnabled"

-- | @Selector@ for @lateNightModeEnabled@
lateNightModeEnabledSelector :: Selector '[] Bool
lateNightModeEnabledSelector = mkSelector "lateNightModeEnabled"

