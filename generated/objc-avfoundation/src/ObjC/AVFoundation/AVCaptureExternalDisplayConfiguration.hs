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
  , shouldMatchFrameRateSelector
  , setShouldMatchFrameRateSelector
  , bypassColorSpaceConversionSelector
  , setBypassColorSpaceConversionSelector


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

-- | A property indicating whether the frame rate of the external display should be configured to match the camera's frame rate.
--
-- If you want to configure your ``AVCaptureVideoPreviewLayer`` to match its source ``AVCaptureDevice/activeVideoMinFrameDuration``, set ``shouldMatchFrameRate`` to @true@. The default value is @false@.
--
-- ObjC selector: @- shouldMatchFrameRate@
shouldMatchFrameRate :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> IO Bool
shouldMatchFrameRate avCaptureExternalDisplayConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureExternalDisplayConfiguration (mkSelector "shouldMatchFrameRate") retCULong []

-- | A property indicating whether the frame rate of the external display should be configured to match the camera's frame rate.
--
-- If you want to configure your ``AVCaptureVideoPreviewLayer`` to match its source ``AVCaptureDevice/activeVideoMinFrameDuration``, set ``shouldMatchFrameRate`` to @true@. The default value is @false@.
--
-- ObjC selector: @- setShouldMatchFrameRate:@
setShouldMatchFrameRate :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> Bool -> IO ()
setShouldMatchFrameRate avCaptureExternalDisplayConfiguration  value =
  sendMsg avCaptureExternalDisplayConfiguration (mkSelector "setShouldMatchFrameRate:") retVoid [argCULong (if value then 1 else 0)]

-- | A property indicating whether the color space of the configurator's preview layer should be preserved on the output display by avoiding color space conversions.
--
-- Set ``bypassColorSpaceConversion`` to @true@ if you would like the configurator's  ``AVCaptureVideoPreviewLayer`` color space preserved on the output display. This is accomplished by setting the working color space to match the color space of the external display. The color properties of the ``CALayer`` remain untouched. The default value is @false@.
--
-- ObjC selector: @- bypassColorSpaceConversion@
bypassColorSpaceConversion :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> IO Bool
bypassColorSpaceConversion avCaptureExternalDisplayConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureExternalDisplayConfiguration (mkSelector "bypassColorSpaceConversion") retCULong []

-- | A property indicating whether the color space of the configurator's preview layer should be preserved on the output display by avoiding color space conversions.
--
-- Set ``bypassColorSpaceConversion`` to @true@ if you would like the configurator's  ``AVCaptureVideoPreviewLayer`` color space preserved on the output display. This is accomplished by setting the working color space to match the color space of the external display. The color properties of the ``CALayer`` remain untouched. The default value is @false@.
--
-- ObjC selector: @- setBypassColorSpaceConversion:@
setBypassColorSpaceConversion :: IsAVCaptureExternalDisplayConfiguration avCaptureExternalDisplayConfiguration => avCaptureExternalDisplayConfiguration -> Bool -> IO ()
setBypassColorSpaceConversion avCaptureExternalDisplayConfiguration  value =
  sendMsg avCaptureExternalDisplayConfiguration (mkSelector "setBypassColorSpaceConversion:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldMatchFrameRate@
shouldMatchFrameRateSelector :: Selector
shouldMatchFrameRateSelector = mkSelector "shouldMatchFrameRate"

-- | @Selector@ for @setShouldMatchFrameRate:@
setShouldMatchFrameRateSelector :: Selector
setShouldMatchFrameRateSelector = mkSelector "setShouldMatchFrameRate:"

-- | @Selector@ for @bypassColorSpaceConversion@
bypassColorSpaceConversionSelector :: Selector
bypassColorSpaceConversionSelector = mkSelector "bypassColorSpaceConversion"

-- | @Selector@ for @setBypassColorSpaceConversion:@
setBypassColorSpaceConversionSelector :: Selector
setBypassColorSpaceConversionSelector = mkSelector "setBypassColorSpaceConversion:"

