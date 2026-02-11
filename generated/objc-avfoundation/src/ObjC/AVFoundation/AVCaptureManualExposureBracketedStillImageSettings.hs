{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureManualExposureBracketedStillImageSettings
--
-- AVCaptureManualExposureBracketedStillImageSettings is a concrete subclass of AVCaptureBracketedStillImageSettings to be used when bracketing exposure duration and ISO.
--
-- An AVCaptureManualExposureBracketedStillImageSettings instance defines the exposure duration and ISO settings that should be applied to one image in a bracket. An array of settings objects is passed to -[AVCaptureStillImageOutput captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:]. Min and max duration and ISO values are queryable properties of the AVCaptureDevice supplying data to an AVCaptureStillImageOutput instance. If you wish to leave exposureDuration unchanged for this bracketed still image, you may pass the special value AVCaptureExposureDurationCurrent. To keep ISO unchanged, you may pass AVCaptureISOCurrent (see AVCaptureDevice.h).
--
-- Generated bindings for @AVCaptureManualExposureBracketedStillImageSettings@.
module ObjC.AVFoundation.AVCaptureManualExposureBracketedStillImageSettings
  ( AVCaptureManualExposureBracketedStillImageSettings
  , IsAVCaptureManualExposureBracketedStillImageSettings(..)
  , iso
  , isoSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | ISO
--
-- The ISO for the still image.
--
-- ObjC selector: @- ISO@
iso :: IsAVCaptureManualExposureBracketedStillImageSettings avCaptureManualExposureBracketedStillImageSettings => avCaptureManualExposureBracketedStillImageSettings -> IO CFloat
iso avCaptureManualExposureBracketedStillImageSettings  =
  sendMsg avCaptureManualExposureBracketedStillImageSettings (mkSelector "ISO") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ISO@
isoSelector :: Selector
isoSelector = mkSelector "ISO"

