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
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , nextFrameCount
  , previousFrameCount
  , supported
  , initWithFrameWidth_frameHeight_sourcePixelFormatSelector
  , initSelector
  , newSelector
  , frameWidthSelector
  , frameHeightSelector
  , sourcePixelBufferAttributesSelector
  , destinationPixelBufferAttributesSelector
  , nextFrameCountSelector
  , previousFrameCountSelector
  , supportedSelector


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
initWithFrameWidth_frameHeight_sourcePixelFormat vtTemporalNoiseFilterConfiguration  frameWidth frameHeight sourcePixelFormat =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "initWithFrameWidth:frameHeight:sourcePixelFormat:") (retPtr retVoid) [argCLong (fromIntegral frameWidth), argCLong (fromIntegral frameHeight), argCUInt (fromIntegral sourcePixelFormat)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id VTTemporalNoiseFilterConfiguration)
init_ vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTTemporalNoiseFilterConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTTemporalNoiseFilterConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
frameWidth vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "frameWidth") retCLong []

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
frameHeight vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "frameHeight") retCLong []

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "destinationPixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Maximum number of future reference frames that the processor can use to process a source frame.
--
-- ObjC selector: @- nextFrameCount@
nextFrameCount :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
nextFrameCount vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "nextFrameCount") retCLong []

-- | Maximum number of past reference frames that the processor can use to process a source frame.
--
-- ObjC selector: @- previousFrameCount@
previousFrameCount :: IsVTTemporalNoiseFilterConfiguration vtTemporalNoiseFilterConfiguration => vtTemporalNoiseFilterConfiguration -> IO CLong
previousFrameCount vtTemporalNoiseFilterConfiguration  =
  sendMsg vtTemporalNoiseFilterConfiguration (mkSelector "previousFrameCount") retCLong []

-- | Reports whether the system supports this processor.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTTemporalNoiseFilterConfiguration"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:sourcePixelFormat:@
initWithFrameWidth_frameHeight_sourcePixelFormatSelector :: Selector
initWithFrameWidth_frameHeight_sourcePixelFormatSelector = mkSelector "initWithFrameWidth:frameHeight:sourcePixelFormat:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @frameWidth@
frameWidthSelector :: Selector
frameWidthSelector = mkSelector "frameWidth"

-- | @Selector@ for @frameHeight@
frameHeightSelector :: Selector
frameHeightSelector = mkSelector "frameHeight"

-- | @Selector@ for @sourcePixelBufferAttributes@
sourcePixelBufferAttributesSelector :: Selector
sourcePixelBufferAttributesSelector = mkSelector "sourcePixelBufferAttributes"

-- | @Selector@ for @destinationPixelBufferAttributes@
destinationPixelBufferAttributesSelector :: Selector
destinationPixelBufferAttributesSelector = mkSelector "destinationPixelBufferAttributes"

-- | @Selector@ for @nextFrameCount@
nextFrameCountSelector :: Selector
nextFrameCountSelector = mkSelector "nextFrameCount"

-- | @Selector@ for @previousFrameCount@
previousFrameCountSelector :: Selector
previousFrameCountSelector = mkSelector "previousFrameCount"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

