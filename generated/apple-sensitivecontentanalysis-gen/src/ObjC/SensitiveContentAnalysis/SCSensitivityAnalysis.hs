{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Sensitive Analysis Results object is returned after sensitivity analysis is performed on media
--
-- Generated bindings for @SCSensitivityAnalysis@.
module ObjC.SensitiveContentAnalysis.SCSensitivityAnalysis
  ( SCSensitivityAnalysis
  , IsSCSensitivityAnalysis(..)
  , sensitive
  , shouldInterruptVideo
  , shouldIndicateSensitivity
  , shouldMuteAudio
  , sensitiveSelector
  , shouldIndicateSensitivitySelector
  , shouldInterruptVideoSelector
  , shouldMuteAudioSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensitiveContentAnalysis.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Set to YES if analyzed media contains sensitive content
--
-- ObjC selector: @- sensitive@
sensitive :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
sensitive scSensitivityAnalysis =
  sendMessage scSensitivityAnalysis sensitiveSelector

-- | Intervention guidance that suggests the app interrupt the video stream.
--
-- ObjC selector: @- shouldInterruptVideo@
shouldInterruptVideo :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
shouldInterruptVideo scSensitivityAnalysis =
  sendMessage scSensitivityAnalysis shouldInterruptVideoSelector

-- | Intervention guidance that suggests the app indicate the presence of sensitive content.
--
-- ObjC selector: @- shouldIndicateSensitivity@
shouldIndicateSensitivity :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
shouldIndicateSensitivity scSensitivityAnalysis =
  sendMessage scSensitivityAnalysis shouldIndicateSensitivitySelector

-- | Intervention guidance that suggests the app mute the audio of the current video stream.
--
-- ObjC selector: @- shouldMuteAudio@
shouldMuteAudio :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
shouldMuteAudio scSensitivityAnalysis =
  sendMessage scSensitivityAnalysis shouldMuteAudioSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensitive@
sensitiveSelector :: Selector '[] Bool
sensitiveSelector = mkSelector "sensitive"

-- | @Selector@ for @shouldInterruptVideo@
shouldInterruptVideoSelector :: Selector '[] Bool
shouldInterruptVideoSelector = mkSelector "shouldInterruptVideo"

-- | @Selector@ for @shouldIndicateSensitivity@
shouldIndicateSensitivitySelector :: Selector '[] Bool
shouldIndicateSensitivitySelector = mkSelector "shouldIndicateSensitivity"

-- | @Selector@ for @shouldMuteAudio@
shouldMuteAudioSelector :: Selector '[] Bool
shouldMuteAudioSelector = mkSelector "shouldMuteAudio"

