{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A configurator class allowing you to configure properties of an external display to match the camera's active video format.
--
-- An ``AVCaptureExternalDisplayConfigurator`` allows you to configure a connected external display to output a clean feed using a ``CALayer``. Using the configurator, you can opt into automatic adjustment of the external display’s color space and / or frame rate to match your device’s capture configuration. These adjustments are only applied to the external display, not to the device.
--
-- - Note: Not all displays support the same configuration options as the device’s capture formats. Your adjustments to the external display are applied with utmost effort to accurately represent the capture device. When your capture device's ``AVCaptureDevice/activeFormat`` is unavailable on the external display, the configurator automatically chooses the closest available format.
--
-- Generated bindings for @AVCaptureExternalDisplayConfigurator@.
module ObjC.AVFoundation.AVCaptureExternalDisplayConfigurator
  ( AVCaptureExternalDisplayConfigurator
  , IsAVCaptureExternalDisplayConfigurator(..)
  , init_
  , new
  , initWithDevice_previewLayer_configuration
  , stop
  , device
  , previewLayer
  , active
  , activeExternalDisplayFrameRate
  , shouldMatchFrameRateSupported
  , supportsBypassingColorSpaceConversion
  , supportsPreferredResolution
  , initSelector
  , newSelector
  , initWithDevice_previewLayer_configurationSelector
  , stopSelector
  , deviceSelector
  , previewLayerSelector
  , activeSelector
  , activeExternalDisplayFrameRateSelector
  , shouldMatchFrameRateSupportedSelector
  , supportsBypassingColorSpaceConversionSelector
  , supportsPreferredResolutionSelector


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
import ObjC.QuartzCore.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator => avCaptureExternalDisplayConfigurator -> IO (Id AVCaptureExternalDisplayConfigurator)
init_ avCaptureExternalDisplayConfigurator  =
  sendMsg avCaptureExternalDisplayConfigurator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureExternalDisplayConfigurator)
new  =
  do
    cls' <- getRequiredClass "AVCaptureExternalDisplayConfigurator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An external display configurator instance that attempts to synchronize the preview layer configuration with the device capture configuration.
--
-- - Parameter device: The device for which to monitor the configuration. - Parameter previewLayer: The layer that is being used on an external display for displaying the camera preview. - Parameter configuration: A configuration specifying which aspects of the camera's active format to monitor and configure on the external display. - Returns: an ``AVCaptureExternalDisplayConfigurator`` instance.
--
-- An ``AVCaptureExternalDisplayConfigurator`` is only applicable to external displays. It determines which properties to configure on the external display based on your provided configuration (see ``AVCaptureExternalDisplayConfiguration``). The configurator observes changes to your camera''s configuration, and when changes are observed, it modifies the external display's properties to match.
--
-- If multiple configurators are linked to the same external display ,the last one created becomes the active configurator for the external display (see ``active``).
--
-- - Important: An @NSInvalidArgumentException@ is thrown if any of the ``AVCaptureExternalDisplayConfiguration`` options are not supported.
--
-- ObjC selector: @- initWithDevice:previewLayer:configuration:@
initWithDevice_previewLayer_configuration :: (IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator, IsAVCaptureDevice device, IsCALayer previewLayer, IsAVCaptureExternalDisplayConfiguration configuration) => avCaptureExternalDisplayConfigurator -> device -> previewLayer -> configuration -> IO (Id AVCaptureExternalDisplayConfigurator)
initWithDevice_previewLayer_configuration avCaptureExternalDisplayConfigurator  device previewLayer configuration =
withObjCPtr device $ \raw_device ->
  withObjCPtr previewLayer $ \raw_previewLayer ->
    withObjCPtr configuration $ \raw_configuration ->
        sendMsg avCaptureExternalDisplayConfigurator (mkSelector "initWithDevice:previewLayer:configuration:") (retPtr retVoid) [argPtr (castPtr raw_device :: Ptr ()), argPtr (castPtr raw_previewLayer :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | Forces the external display configurator to asynchronously stop configuring the external display.
--
-- Call ``stop`` to force the ``AVCaptureExternalDisplayConfigurator`` to asynchronously stop configuring the external display. Once stopped, the ``active`` property changes to @false@ and the ``activeExternalDisplayFrameRate`` becomes 0.
--
-- ObjC selector: @- stop@
stop :: IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator => avCaptureExternalDisplayConfigurator -> IO ()
stop avCaptureExternalDisplayConfigurator  =
  sendMsg avCaptureExternalDisplayConfigurator (mkSelector "stop") retVoid []

-- | The device for which the coordinator configures the preview layer.
--
-- The value of this property is the ``AVCaptureDevice`` instance you provided when instantiating the configurator. ``AVCaptureExternalDisplayConfigurator`` holds a weak reference to the device. If the device is released, this property returns @nil@.
--
-- ObjC selector: @- device@
device :: IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator => avCaptureExternalDisplayConfigurator -> IO (Id AVCaptureDevice)
device avCaptureExternalDisplayConfigurator  =
  sendMsg avCaptureExternalDisplayConfigurator (mkSelector "device") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The layer for which the configurator adjusts display properties to match the device's state.
--
-- The value of this property is the ``CALayer`` instance that you provided when instantiating the configurator. You may specify either an ``AVCaptureVideoPreviewLayer`` or another ``CALayer`` instance that displays a camera's video preview. ``AVCaptureExternalDisplayConfigurator``holds a weak reference to the layer. If the layer is released, this property returns @nil@.
--
-- ObjC selector: @- previewLayer@
previewLayer :: IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator => avCaptureExternalDisplayConfigurator -> IO (Id CALayer)
previewLayer avCaptureExternalDisplayConfigurator  =
  sendMsg avCaptureExternalDisplayConfigurator (mkSelector "previewLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property tells you whether the configurator is actively configuring the external display.
--
-- When this property returns @true@, the external display is successfully configured to match the device. If it returns@false@, the configurator is not making any configuration changes to the external display. If another ``AVCaptureExternalDisplayConfigurator`` instance takes over the configuration of the external display, this property returns @false@.
--
-- ObjC selector: @- active@
active :: IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator => avCaptureExternalDisplayConfigurator -> IO Bool
active avCaptureExternalDisplayConfigurator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureExternalDisplayConfigurator (mkSelector "active") retCULong []

-- | The currently configured frame rate on the external display that's displaying the preview layer.
--
-- Observe this property to determine if the configured frame rate matches the max frame rate (``AVCaptureDevice/activeVideoMinFrameDuration``) of the device. When the ``active`` property becomes @false@, this property changes to 0.
--
-- ObjC selector: @- activeExternalDisplayFrameRate@
activeExternalDisplayFrameRate :: IsAVCaptureExternalDisplayConfigurator avCaptureExternalDisplayConfigurator => avCaptureExternalDisplayConfigurator -> IO CDouble
activeExternalDisplayFrameRate avCaptureExternalDisplayConfigurator  =
  sendMsg avCaptureExternalDisplayConfigurator (mkSelector "activeExternalDisplayFrameRate") retCDouble []

-- | Whether the external display supports matching frame rate to a capture device.
--
-- If @true@, you may instantiate a configurator with a configuration specifying ``AVCaptureExternalDisplayConfiguration/shouldMatchFrameRate`` set to @true@.
--
-- ObjC selector: @+ shouldMatchFrameRateSupported@
shouldMatchFrameRateSupported :: IO Bool
shouldMatchFrameRateSupported  =
  do
    cls' <- getRequiredClass "AVCaptureExternalDisplayConfigurator"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "shouldMatchFrameRateSupported") retCULong []

-- | Whether the external display supports bypassing color space conversion.
--
-- If @true@, you may instantiate a configurator with a configuration specifying ``AVCaptureExternalDisplayConfiguration/bypassColorSpaceConversion`` set to @true@.
--
-- ObjC selector: @+ supportsBypassingColorSpaceConversion@
supportsBypassingColorSpaceConversion :: IO Bool
supportsBypassingColorSpaceConversion  =
  do
    cls' <- getRequiredClass "AVCaptureExternalDisplayConfigurator"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsBypassingColorSpaceConversion") retCULong []

-- | Whether the external display supports configuration to your preferred resolution.
--
-- If @true@, you may instantiate a configurator with a configuration specifying ``AVCaptureExternalDisplayConfiguration/preferredResolution`` set to @true@.
--
-- ObjC selector: @+ supportsPreferredResolution@
supportsPreferredResolution :: IO Bool
supportsPreferredResolution  =
  do
    cls' <- getRequiredClass "AVCaptureExternalDisplayConfigurator"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supportsPreferredResolution") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:previewLayer:configuration:@
initWithDevice_previewLayer_configurationSelector :: Selector
initWithDevice_previewLayer_configurationSelector = mkSelector "initWithDevice:previewLayer:configuration:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @device@
deviceSelector :: Selector
deviceSelector = mkSelector "device"

-- | @Selector@ for @previewLayer@
previewLayerSelector :: Selector
previewLayerSelector = mkSelector "previewLayer"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @activeExternalDisplayFrameRate@
activeExternalDisplayFrameRateSelector :: Selector
activeExternalDisplayFrameRateSelector = mkSelector "activeExternalDisplayFrameRate"

-- | @Selector@ for @shouldMatchFrameRateSupported@
shouldMatchFrameRateSupportedSelector :: Selector
shouldMatchFrameRateSupportedSelector = mkSelector "shouldMatchFrameRateSupported"

-- | @Selector@ for @supportsBypassingColorSpaceConversion@
supportsBypassingColorSpaceConversionSelector :: Selector
supportsBypassingColorSpaceConversionSelector = mkSelector "supportsBypassingColorSpaceConversion"

-- | @Selector@ for @supportsPreferredResolution@
supportsPreferredResolutionSelector :: Selector
supportsPreferredResolutionSelector = mkSelector "supportsPreferredResolution"

