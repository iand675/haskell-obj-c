{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A configuration object to initiate a frame processor and use temporal noise-filter processor.
--
-- The class properties of @VTTemporalNoiseFilterConfiguration@ help to identify the capabilities of temporal noise filter processor on the current platform, prior to initiating a session. You can confirm the availability of temporal noise-filter processor in the current platform by checking the ``isSupported`` class property. Verify the processor's capability to process source frames by ensuring that the dimensions are no less than ``minimumDimensions`` and no greater than ``maximumDimensions``. Use the instance properties such as ``frameSupportedPixelFormats``, ``sourcePixelBufferAttributes``, and ``destinationPixelBufferAttributes`` to ensure that the input and output pixel buffer formats and attributes of the processor align with the client's specific requirements. The properties ``previousFrameCount`` and ``nextFrameCount`` represent the maximum number of preceding and subsequent reference frames, used in the processing of a source frame, to achieve optimum noise-reduction quality.
--
-- Generated bindings for @VTTemporalNoiseFilterConfiguration@.
module ObjC.VideoToolbox.VTTemporalNoiseFilterConfiguration
  ( VTTemporalNoiseFilterConfiguration
  , IsVTTemporalNoiseFilterConfiguration(..)
  , initWithFrameWidth_frameHeight_sourcePixelFormat
  , init_
  , new
  , frameWidth
  , frameHeight
  , frameSupportedPixelFormats
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , nextFrameCount
  , previousFrameCount
  , supportedSourcePixelFormats
  , supported
  , destinationPixelBufferAttributesSelector
  , frameHeightSelector
  , frameSupportedPixelFormatsSelector
  , frameWidthSelector
  , initSelector
  , initWithFrameWidth_frameHeight_sourcePixelFormatSelector
  , newSelector
  , nextFrameCountSelector
  , previousFrameCountSelector
  , sourcePixelBufferAttributesSelector
  , supportedSelector
  , supportedSourcePixelFormatsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.VideoToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new temporal noise-processor configuration.
--
-- Returns nil if frameWidth, frameHeight, or sourcePixelFormat is unsupported.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels.   - frameHeight: Height of source frame in pixels.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:sourcePixelFormat:@
initWithFrameWidth_frameHeight_sourcePixelFormat :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> CLong -> CLong -> CUInt -> IO (Id VTTemporalNoiseFilterConfiguration)
initWithFrameWidth_frameHeight_sourcePixelFormat vtTemporalNoiseFilterConfiguration frameWidth frameHeight sourcePixelFormat =
  sendOwnedMessage vtTemporalNoiseFilterConfiguration initWithFrameWidth_frameHeight_sourcePixelFormatSelector frameWidth frameHeight sourcePixelFormat

-- | @- init@
init_ :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id VTTemporalNoiseFilterConfiguration)
init_ vtTemporalNoiseFilterConfiguration =
  sendOwnedMessage vtTemporalNoiseFilterConfiguration initSelector

-- | @+ new@
new :: IO (Id VTTemporalNoiseFilterConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTTemporalNoiseFilterConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
frameWidth vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration frameWidthSelector

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
frameHeight vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration frameHeightSelector

-- | Supported pixel formats for source frames for current configuration.
--
-- ObjC selector: @- frameSupportedPixelFormats@
frameSupportedPixelFormats :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id NSArray)
frameSupportedPixelFormats vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration frameSupportedPixelFormatsSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration sourcePixelBufferAttributesSelector

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration destinationPixelBufferAttributesSelector

-- | Maximum number of future reference frames that the processor can use to process a source frame.
--
-- ObjC selector: @- nextFrameCount@
nextFrameCount :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
nextFrameCount vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration nextFrameCountSelector

-- | Maximum number of past reference frames that the processor can use to process a source frame.
--
-- ObjC selector: @- previousFrameCount@
previousFrameCount :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
previousFrameCount vtTemporalNoiseFilterConfiguration =
  sendMessage vtTemporalNoiseFilterConfiguration previousFrameCountSelector

-- | List of all supported pixel formats for source frames.
--
-- ObjC selector: @+ supportedSourcePixelFormats@
supportedSourcePixelFormats :: IO (Id NSArray)
supportedSourcePixelFormats  =
  do
    cls' <- getRequiredClass "VTTemporalNoiseFilterConfiguration"
    sendClassMessage cls' supportedSourcePixelFormatsSelector

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTTemporalNoiseFilterConfiguration"
    sendClassMessage cls' supportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:sourcePixelFormat:@
initWithFrameWidth_frameHeight_sourcePixelFormatSelector :: Selector '[CLong, CLong, CUInt] (Id VTTemporalNoiseFilterConfiguration)
initWithFrameWidth_frameHeight_sourcePixelFormatSelector = mkSelector "initWithFrameWidth:frameHeight:sourcePixelFormat:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VTTemporalNoiseFilterConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VTTemporalNoiseFilterConfiguration)
newSelector = mkSelector "new"

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

-- | @Selector@ for @nextFrameCount@
nextFrameCountSelector :: Selector '[] CLong
nextFrameCountSelector = mkSelector "nextFrameCount"

-- | @Selector@ for @previousFrameCount@
previousFrameCountSelector :: Selector '[] CLong
previousFrameCountSelector = mkSelector "previousFrameCount"

-- | @Selector@ for @supportedSourcePixelFormats@
supportedSourcePixelFormatsSelector :: Selector '[] (Id NSArray)
supportedSourcePixelFormatsSelector = mkSelector "supportedSourcePixelFormats"

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

