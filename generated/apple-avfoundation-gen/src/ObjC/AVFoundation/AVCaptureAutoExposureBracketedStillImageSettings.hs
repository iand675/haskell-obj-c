{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureAutoExposureBracketedStillImageSettings
--
-- AVCaptureAutoExposureBracketedStillImageSettings is a concrete subclass of AVCaptureBracketedStillImageSettings to be used when bracketing exposure target bias.
--
-- An AVCaptureAutoExposureBracketedStillImageSettings instance defines the exposure target bias setting that should be applied to one image in a bracket. An array of settings objects is passed to -[AVCaptureStillImageOutput captureStillImageBracketAsynchronouslyFromConnection:withSettingsArray:completionHandler:]. Min and max exposure target bias are queryable properties of the AVCaptureDevice supplying data to an AVCaptureStillImageOutput instance. If you wish to leave exposureTargetBias unchanged for this bracketed still image, you may pass the special value AVCaptureExposureTargetBiasCurrent (see AVCaptureDevice.h).
--
-- Generated bindings for @AVCaptureAutoExposureBracketedStillImageSettings@.
module ObjC.AVFoundation.AVCaptureAutoExposureBracketedStillImageSettings
  ( AVCaptureAutoExposureBracketedStillImageSettings
  , IsAVCaptureAutoExposureBracketedStillImageSettings(..)
  , autoExposureSettingsWithExposureTargetBias
  , exposureTargetBias
  , autoExposureSettingsWithExposureTargetBiasSelector
  , exposureTargetBiasSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | autoExposureSettingsWithExposureTargetBias
--
-- Creates an AVCaptureAutoExposureBracketedStillImageSettings using the specified exposure target bias.
--
-- @exposureTargetBias@ — The exposure target bias. Pass AVCaptureExposureTargetBiasCurrent to leave the exposureTargetBias unchanged for this image.
--
-- Returns: An initialized AVCaptureAutoExposureBracketedStillImageSettings instance.
--
-- ObjC selector: @+ autoExposureSettingsWithExposureTargetBias:@
autoExposureSettingsWithExposureTargetBias :: CFloat -> IO (Id AVCaptureAutoExposureBracketedStillImageSettings)
autoExposureSettingsWithExposureTargetBias exposureTargetBias =
  do
    cls' <- getRequiredClass "AVCaptureAutoExposureBracketedStillImageSettings"
    sendClassMessage cls' autoExposureSettingsWithExposureTargetBiasSelector exposureTargetBias

-- | exposureTargetBias
--
-- The exposure bias for the auto exposure bracketed settings
--
-- ObjC selector: @- exposureTargetBias@
exposureTargetBias :: IsAVCaptureAutoExposureBracketedStillImageSettings avCaptureAutoExposureBracketedStillImageSettings => avCaptureAutoExposureBracketedStillImageSettings -> IO CFloat
exposureTargetBias avCaptureAutoExposureBracketedStillImageSettings =
  sendMessage avCaptureAutoExposureBracketedStillImageSettings exposureTargetBiasSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @autoExposureSettingsWithExposureTargetBias:@
autoExposureSettingsWithExposureTargetBiasSelector :: Selector '[CFloat] (Id AVCaptureAutoExposureBracketedStillImageSettings)
autoExposureSettingsWithExposureTargetBiasSelector = mkSelector "autoExposureSettingsWithExposureTargetBias:"

-- | @Selector@ for @exposureTargetBias@
exposureTargetBiasSelector :: Selector '[] CFloat
exposureTargetBiasSelector = mkSelector "exposureTargetBias"

