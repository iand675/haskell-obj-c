{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration that you use to program Video Toolbox frame processor for low-latency frame interpolation.
--
-- This configuration can do either purely temporal interpolation (frame-rate conversion) or temporal and spatial interpolation (scaling and frame-rate conversion). This processor requires a source frame and a previous frame. It does temporal scaling, which interpolates frames between the previous frame and the source frame. When performing both temporal and spatial interpolation, the processor can only perform 2x upscaling, and a single frame of temporal interpolation. When performing spatial scaling, the processor produces upscaled intermediate frames and an upscaled @sourceFrame@, but it does not upscale the previous reference frame you provided.
--
-- > Important: When calling ``VTFrameProcessor/startSessionWithConfiguration:error:`` to create a @VTLowLatencyFrameInterpolation@ session, ML model loading may take longer than a frame time. Avoid blocking the UI thread or stalling frame rendering pipelines during this call.
--
-- Generated bindings for @VTLowLatencyFrameInterpolationConfiguration@.
module ObjC.VideoToolbox.VTLowLatencyFrameInterpolationConfiguration
  ( VTLowLatencyFrameInterpolationConfiguration
  , IsVTLowLatencyFrameInterpolationConfiguration(..)
  , initWithFrameWidth_frameHeight_numberOfInterpolatedFrames
  , initWithFrameWidth_frameHeight_spatialScaleFactor
  , init_
  , new
  , frameWidth
  , frameHeight
  , spatialScaleFactor
  , numberOfInterpolatedFrames
  , frameSupportedPixelFormats
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , supported
  , destinationPixelBufferAttributesSelector
  , frameHeightSelector
  , frameSupportedPixelFormatsSelector
  , frameWidthSelector
  , initSelector
  , initWithFrameWidth_frameHeight_numberOfInterpolatedFramesSelector
  , initWithFrameWidth_frameHeight_spatialScaleFactorSelector
  , newSelector
  , numberOfInterpolatedFramesSelector
  , sourcePixelBufferAttributesSelector
  , spatialScaleFactorSelector
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

-- | Creates a new low-latency frame interpolation configuration for frame-rate conversion.
--
-- The available interpolation points are the equal to the value of (2^x - 1), where x is equal to @numberOfInterpolatedFrames@. For example, - If you request 1 interpolated frame, 1 interpolation point at 0.5 is available. - If you request 2 interpolated frames, 3 interpolation points at 0.25, 0.5 and 0.75 are available. You don't need to use all available interpolation points. Setting a higher @numberOfInterpolatedFrames@ increases the resolution of interpolation in some cases, but also increases latency.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels.   - frameHeight: Height of source frame in pixels.   - numberOfInterpolatedFrames: The number of uniformly spaced frames that you want to be used for interpolation.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:numberOfInterpolatedFrames:@
initWithFrameWidth_frameHeight_numberOfInterpolatedFrames :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> CLong -> CLong -> CLong -> IO (Id VTLowLatencyFrameInterpolationConfiguration)
initWithFrameWidth_frameHeight_numberOfInterpolatedFrames vtLowLatencyFrameInterpolationConfiguration frameWidth frameHeight numberOfInterpolatedFrames =
  sendOwnedMessage vtLowLatencyFrameInterpolationConfiguration initWithFrameWidth_frameHeight_numberOfInterpolatedFramesSelector frameWidth frameHeight numberOfInterpolatedFrames

-- | Creates a new low-latency frame interpolation configuration for spatial scaling and temporal scaling.
--
-- When you configure the processor for spatial scaling, the low-latency frame interpolation processor only supports 2x spatial upscaling and a single frame of temporal interpolation at a 0.5 interpolation phase.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels.   - frameHeight: Height of source frame in pixels.   - spatialScaleFactor: The requested spatial scale factor as an integer. Currently, the processor supports only 2x spatial scaling.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:spatialScaleFactor:@
initWithFrameWidth_frameHeight_spatialScaleFactor :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> CLong -> CLong -> CLong -> IO (Id VTLowLatencyFrameInterpolationConfiguration)
initWithFrameWidth_frameHeight_spatialScaleFactor vtLowLatencyFrameInterpolationConfiguration frameWidth frameHeight spatialScaleFactor =
  sendOwnedMessage vtLowLatencyFrameInterpolationConfiguration initWithFrameWidth_frameHeight_spatialScaleFactorSelector frameWidth frameHeight spatialScaleFactor

-- | @- init@
init_ :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO (Id VTLowLatencyFrameInterpolationConfiguration)
init_ vtLowLatencyFrameInterpolationConfiguration =
  sendOwnedMessage vtLowLatencyFrameInterpolationConfiguration initSelector

-- | @+ new@
new :: IO (Id VTLowLatencyFrameInterpolationConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencyFrameInterpolationConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Width of source frames in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO CLong
frameWidth vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration frameWidthSelector

-- | Height of source frames in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO CLong
frameHeight vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration frameHeightSelector

-- | Configured spatial scale factor as an integer.
--
-- ObjC selector: @- spatialScaleFactor@
spatialScaleFactor :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO CLong
spatialScaleFactor vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration spatialScaleFactorSelector

-- | Number of uniformly spaced frames for which you configured the processor.
--
-- ObjC selector: @- numberOfInterpolatedFrames@
numberOfInterpolatedFrames :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO CLong
numberOfInterpolatedFrames vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration numberOfInterpolatedFramesSelector

-- | Available supported pixel formats for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration frameSupportedPixelFormatsSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration sourcePixelBufferAttributesSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTLowLatencyFrameInterpolationConfiguration vtLowLatencyFrameInterpolationConfiguration => vtLowLatencyFrameInterpolationConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtLowLatencyFrameInterpolationConfiguration =
  sendMessage vtLowLatencyFrameInterpolationConfiguration destinationPixelBufferAttributesSelector

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTLowLatencyFrameInterpolationConfiguration"
    sendClassMessage cls' supportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:numberOfInterpolatedFrames:@
initWithFrameWidth_frameHeight_numberOfInterpolatedFramesSelector :: Selector '[CLong, CLong, CLong] (Id VTLowLatencyFrameInterpolationConfiguration)
initWithFrameWidth_frameHeight_numberOfInterpolatedFramesSelector = mkSelector "initWithFrameWidth:frameHeight:numberOfInterpolatedFrames:"

-- | @Selector@ for @initWithFrameWidth:frameHeight:spatialScaleFactor:@
initWithFrameWidth_frameHeight_spatialScaleFactorSelector :: Selector '[CLong, CLong, CLong] (Id VTLowLatencyFrameInterpolationConfiguration)
initWithFrameWidth_frameHeight_spatialScaleFactorSelector = mkSelector "initWithFrameWidth:frameHeight:spatialScaleFactor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTLowLatencyFrameInterpolationConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTLowLatencyFrameInterpolationConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector '[] CLong
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector '[] CLong
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @spatialScaleFactor@
spatialScaleFactorSelector :: Selector '[] CLong
spatialScaleFactorSelector = mkSelector "spatialScaleFactor"

-- | @Selector@ for @numberOfInterpolatedFrames@
numberOfInterpolatedFramesSelector :: Selector '[] CLong
numberOfInterpolatedFramesSelector = mkSelector "numberOfInterpolatedFrames"

-- | @Selector@ for @frameSupportedPixelFormats@
frameSupportedPixelFormatsSelector :: Selector '[] (Id NSArray)
frameSupportedPixelFormatsSelector = mkSelector "frameSupportedPixelFormats"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @destinationPixelBufferAttributes@
destinationPixelBufferAttributesSelector :: Selector '[] (Id NSDictionary)
destinationPixelBufferAttributesSelector = mkSelector "destinationPixelBufferAttributes"

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

