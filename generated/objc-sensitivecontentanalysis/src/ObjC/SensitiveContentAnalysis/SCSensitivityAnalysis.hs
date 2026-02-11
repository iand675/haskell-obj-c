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
  , shouldInterruptVideoSelector
  , shouldIndicateSensitivitySelector
  , shouldMuteAudioSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensitiveContentAnalysis.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Set to YES if analyzed media contains sensitive content
--
-- ObjC selector: @- sensitive@
sensitive :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
sensitive scSensitivityAnalysis  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scSensitivityAnalysis (mkSelector "sensitive") retCULong []

-- | Intervention guidance that suggests the app interrupt the video stream.
--
-- ObjC selector: @- shouldInterruptVideo@
shouldInterruptVideo :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
shouldInterruptVideo scSensitivityAnalysis  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scSensitivityAnalysis (mkSelector "shouldInterruptVideo") retCULong []

-- | Intervention guidance that suggests the app indicate the presence of sensitive content.
--
-- ObjC selector: @- shouldIndicateSensitivity@
shouldIndicateSensitivity :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
shouldIndicateSensitivity scSensitivityAnalysis  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scSensitivityAnalysis (mkSelector "shouldIndicateSensitivity") retCULong []

-- | Intervention guidance that suggests the app mute the audio of the current video stream.
--
-- ObjC selector: @- shouldMuteAudio@
shouldMuteAudio :: IsSCSensitivityAnalysis scSensitivityAnalysis => scSensitivityAnalysis -> IO Bool
shouldMuteAudio scSensitivityAnalysis  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scSensitivityAnalysis (mkSelector "shouldMuteAudio") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensitive@
sensitiveSelector :: Selector
sensitiveSelector = mkSelector "sensitive"

-- | @Selector@ for @shouldInterruptVideo@
shouldInterruptVideoSelector :: Selector
shouldInterruptVideoSelector = mkSelector "shouldInterruptVideo"

-- | @Selector@ for @shouldIndicateSensitivity@
shouldIndicateSensitivitySelector :: Selector
shouldIndicateSensitivitySelector = mkSelector "shouldIndicateSensitivity"

-- | @Selector@ for @shouldMuteAudio@
shouldMuteAudioSelector :: Selector
shouldMuteAudioSelector = mkSelector "shouldMuteAudio"

