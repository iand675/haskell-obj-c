{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
iso avCaptureManualExposureBracketedStillImageSettings =
  sendMessage avCaptureManualExposureBracketedStillImageSettings isoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ISO@
isoSelector :: Selector '[] CFloat
isoSelector = mkSelector "ISO"

