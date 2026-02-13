{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class you use to specify a configuration to your external display configurator.
--
-- Using an ``AVCaptureExternalDisplayConfiguration``, you direct your ``AVCaptureExternalDisplayConfigurator`` how to configure an external display to match your device's active video format.
--
-- Generated bindings for @AVCaptureExternalDisplayConfiguration@.
module ObjC.AVFoundation.AVCaptureExternalDisplayConfiguration
  ( AVCaptureExternalDisplayConfiguration
  , IsAVCaptureExternalDisplayConfiguration(..)
  , shouldMatchFrameRate
  , setShouldMatchFrameRate
  , bypassColorSpaceConversion
  , setBypassColorSpaceConversion
  , bypassColorSpaceConversionSelector
  , setBypassColorSpaceConversionSelector
  , setShouldMatchFrameRateSelector
  , shouldMatchFrameRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A property indicating whether the frame rate of the external display should be configured to match the camera's frame rate.
--
-- If you want to configure your ``AVCaptureVideoPreviewLayer`` to match its source ``AVCaptureDevice/activeVideoMinFrameDuration``, set ``shouldMatchFrameRate`` to @true@. The default value is @false@.
--
-- ObjC selector: @- shouldMatchFrameRate@
shouldMatchFrameRate :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> IO Bool
shouldMatchFrameRate avCaptureExternalDisplayConfiguration =
  sendMessage avCaptureExternalDisplayConfiguration shouldMatchFrameRateSelector

-- | A property indicating whether the frame rate of the external display should be configured to match the camera's frame rate.
--
-- If you want to configure your ``AVCaptureVideoPreviewLayer`` to match its source ``AVCaptureDevice/activeVideoMinFrameDuration``, set ``shouldMatchFrameRate`` to @true@. The default value is @false@.
--
-- ObjC selector: @- setShouldMatchFrameRate:@
setShouldMatchFrameRate :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> Bool -> IO ()
setShouldMatchFrameRate avCaptureExternalDisplayConfiguration value =
  sendMessage avCaptureExternalDisplayConfiguration setShouldMatchFrameRateSelector value

-- | A property indicating whether the color space of the configurator's preview layer should be preserved on the output display by avoiding color space conversions.
--
-- Set ``bypassColorSpaceConversion`` to @true@ if you would like the configurator's  ``AVCaptureVideoPreviewLayer`` color space preserved on the output display. This is accomplished by setting the working color space to match the color space of the external display. The color properties of the ``CALayer`` remain untouched. The default value is @false@.
--
-- ObjC selector: @- bypassColorSpaceConversion@
bypassColorSpaceConversion :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> IO Bool
bypassColorSpaceConversion avCaptureExternalDisplayConfiguration =
  sendMessage avCaptureExternalDisplayConfiguration bypassColorSpaceConversionSelector

-- | A property indicating whether the color space of the configurator's preview layer should be preserved on the output display by avoiding color space conversions.
--
-- Set ``bypassColorSpaceConversion`` to @true@ if you would like the configurator's  ``AVCaptureVideoPreviewLayer`` color space preserved on the output display. This is accomplished by setting the working color space to match the color space of the external display. The color properties of the ``CALayer`` remain untouched. The default value is @false@.
--
-- ObjC selector: @- setBypassColorSpaceConversion:@
setBypassColorSpaceConversion :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> Bool -> IO ()
setBypassColorSpaceConversion avCaptureExternalDisplayConfiguration value =
  sendMessage avCaptureExternalDisplayConfiguration setBypassColorSpaceConversionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldMatchFrameRate@
shouldMatchFrameRateSelector :: Selector '[] Bool
shouldMatchFrameRateSelector = mkSelector "shouldMatchFrameRate"

-- | @Selector@ for @setShouldMatchFrameRate:@
setShouldMatchFrameRateSelector :: Selector '[Bool] ()
setShouldMatchFrameRateSelector = mkSelector "setShouldMatchFrameRate:"

-- | @Selector@ for @bypassColorSpaceConversion@
bypassColorSpaceConversionSelector :: Selector '[] Bool
bypassColorSpaceConversionSelector = mkSelector "bypassColorSpaceConversion"

-- | @Selector@ for @setBypassColorSpaceConversion:@
setBypassColorSpaceConversionSelector :: Selector '[Bool] ()
setBypassColorSpaceConversionSelector = mkSelector "setBypassColorSpaceConversion:"

