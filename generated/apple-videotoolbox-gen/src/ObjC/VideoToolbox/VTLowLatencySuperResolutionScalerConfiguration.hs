{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object you use to configure frame processor for low-latency super-resolution scaler processing.
--
-- Use this object to configure a ``VTFrameProcessor``. Query this interface also for important operating details, like the pixel buffer attributes required for frames you submit to the processor.
--
-- > Important: When calling ``VTFrameProcessor/startSessionWithConfiguration:error:`` to create a @VTLowLatencySuperResolutionScaler@ session, ML model loading may take longer than a frame time. Avoid blocking the UI thread or stalling frame rendering pipelines during this call.
--
-- Generated bindings for @VTLowLatencySuperResolutionScalerConfiguration@.
module ObjC.VideoToolbox.VTLowLatencySuperResolutionScalerConfiguration
  ( VTLowLatencySuperResolutionScalerConfiguration
  , IsVTLowLatencySuperResolutionScalerConfiguration(..)
  , initWithFrameWidth_frameHeight_scaleFactor
  , init_
  , new
  , supportedScaleFactorsForFrameWidth_frameHeight
  , frameWidth
  , frameHeight
  , frameSupportedPixelFormats
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , scaleFactor
  , supported
  , destinationPixelBufferAttributesSelector
  , frameHeightSelector
  , frameSupportedPixelFormatsSelector
  , frameWidthSelector
  , initSelector
  , initWithFrameWidth_frameHeight_scaleFactorSelector
  , newSelector
  , scaleFactorSelector
  , sourcePixelBufferAttributesSelector
  , supportedScaleFactorsForFrameWidth_frameHeightSelector
  , supportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new low-latency super-resolution scaler configuration with specified frame width and height.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels.   - frameHeight: Height of source frame in pixels.   - scaleFactor: The scale factor to apply. This must be a supported value that ``supportedScaleFactorsForFrameWidth:frameHeight:`` returns.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:scaleFactor:@
initWithFrameWidth_frameHeight_scaleFactor :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> CLong -> CLong -> CFloat -> IO (Id VTLowLatencySuperResolutionScalerConfiguration)
initWithFrameWidth_frameHeight_scaleFactor vtLowLatencySuperResolutionScalerConfiguration frameWidth frameHeight scaleFactor =
  sendOwnedMessage vtLowLatencySuperResolutionScalerConfiguration initWithFrameWidth_frameHeight_scaleFactorSelector frameWidth frameHeight scaleFactor

-- | @- init@
init_ :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id VTLowLatencySuperResolutionScalerConfiguration)
init_ vtLowLatencySuperResolutionScalerConfiguration =
  sendOwnedMessage vtLowLatencySuperResolutionScalerConfiguration initSelector

-- | @+ new@
new :: IO (Id VTLowLatencySuperResolutionScalerConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Returns an array of supported scale factors values, or an empty list if the processor doesn't support the dimensions.
--
-- ObjC selector: @+ supportedScaleFactorsForFrameWidth:frameHeight:@
supportedScaleFactorsForFrameWidth_frameHeight :: CLong -> CLong -> IO (Id NSArray)
supportedScaleFactorsForFrameWidth_frameHeight frameWidth frameHeight =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"
    sendClassMessage cls' supportedScaleFactorsForFrameWidth_frameHeightSelector frameWidth frameHeight

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO CLong
frameWidth vtLowLatencySuperResolutionScalerConfiguration =
  sendMessage vtLowLatencySuperResolutionScalerConfiguration frameWidthSelector

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO CLong
frameHeight vtLowLatencySuperResolutionScalerConfiguration =
  sendMessage vtLowLatencySuperResolutionScalerConfiguration frameHeightSelector

-- | Available supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtLowLatencySuperResolutionScalerConfiguration =
  sendMessage vtLowLatencySuperResolutionScalerConfiguration frameSupportedPixelFormatsSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtLowLatencySuperResolutionScalerConfiguration =
  sendMessage vtLowLatencySuperResolutionScalerConfiguration sourcePixelBufferAttributesSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtLowLatencySuperResolutionScalerConfiguration =
  sendMessage vtLowLatencySuperResolutionScalerConfiguration destinationPixelBufferAttributesSelector

-- | Scale factor with which you initialized the configuration.
--
-- ObjC selector: @- scaleFactor@
scaleFactor :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO CFloat
scaleFactor vtLowLatencySuperResolutionScalerConfiguration =
  sendMessage vtLowLatencySuperResolutionScalerConfiguration scaleFactorSelector

-- | Reports whether the system supports this processor on the current configuration.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"
    sendClassMessage cls' supportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:scaleFactor:@
initWithFrameWidth_frameHeight_scaleFactorSelector :: Selector '[CLong, CLong, CFloat] (Id VTLowLatencySuperResolutionScalerConfiguration)
initWithFrameWidth_frameHeight_scaleFactorSelector = mkSelector "initWithFrameWidth:frameHeight:scaleFactor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTLowLatencySuperResolutionScalerConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTLowLatencySuperResolutionScalerConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @supportedScaleFactorsForFrameWidth:frameHeight:@
supportedScaleFactorsForFrameWidth_frameHeightSelector :: Selector '[CLong, CLong] (Id NSArray)
supportedScaleFactorsForFrameWidth_frameHeightSelector = mkSelector "supportedScaleFactorsForFrameWidth:frameHeight:"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector '[] CLong
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector '[] CLong
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @frameSupportedPixelFormats@
frameSupportedPixelFormatsSelector :: Selector '[] (Id NSArray)
frameSupportedPixelFormatsSelector = mkSelector "frameSupportedPixelFormats"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @destinationPixelBufferAttributes@
destinationPixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
destinationPixelBufferAttributesSelector = mkSelector "destinationPixelBufferAttributes"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector '[] CFloat
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

