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
  , sourcePixelBufferAttributes
  , destinationPixelBufferAttributes
  , scaleFactor
  , supported
  , initWithFrameWidth_frameHeight_scaleFactorSelector
  , initSelector
  , newSelector
  , supportedScaleFactorsForFrameWidth_frameHeightSelector
  , frameWidthSelector
  , frameHeightSelector
  , sourcePixelBufferAttributesSelector
  , destinationPixelBufferAttributesSelector
  , scaleFactorSelector
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

-- | Creates a new low-latency super-resolution scaler configuration with specified frame width and height.
--
-- - Parameters:   - frameWidth: Width of source frame in pixels.   - frameHeight: Height of source frame in pixels.   - scaleFactor: The scale factor to apply. This must be a supported value that ``supportedScaleFactorsForFrameWidth:frameHeight:`` returns.
--
-- ObjC selector: @- initWithFrameWidth:frameHeight:scaleFactor:@
initWithFrameWidth_frameHeight_scaleFactor :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> CLong -> CLong -> CFloat -> IO (Id VTLowLatencySuperResolutionScalerConfiguration)
initWithFrameWidth_frameHeight_scaleFactor vtLowLatencySuperResolutionScalerConfiguration  frameWidth frameHeight scaleFactor =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "initWithFrameWidth:frameHeight:scaleFactor:") (retPtr retVoid) [argCLong (fromIntegral frameWidth), argCLong (fromIntegral frameHeight), argCFloat (fromIntegral scaleFactor)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id VTLowLatencySuperResolutionScalerConfiguration)
init_ vtLowLatencySuperResolutionScalerConfiguration  =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id VTLowLatencySuperResolutionScalerConfiguration)
new  =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns an array of supported scale factors values, or an empty list if the processor doesn't support the dimensions.
--
-- ObjC selector: @+ supportedScaleFactorsForFrameWidth:frameHeight:@
supportedScaleFactorsForFrameWidth_frameHeight :: CLong -> CLong -> IO (Id NSArray)
supportedScaleFactorsForFrameWidth_frameHeight frameWidth frameHeight =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"
    sendClassMsg cls' (mkSelector "supportedScaleFactorsForFrameWidth:frameHeight:") (retPtr retVoid) [argCLong (fromIntegral frameWidth), argCLong (fromIntegral frameHeight)] >>= retainedObject . castPtr

-- | Width of source frame in pixels.
--
-- ObjC selector: @- frameWidth@
frameWidth :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO CLong
frameWidth vtLowLatencySuperResolutionScalerConfiguration  =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "frameWidth") retCLong []

-- | Height of source frame in pixels.
--
-- ObjC selector: @- frameHeight@
frameHeight :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO CLong
frameHeight vtLowLatencySuperResolutionScalerConfiguration  =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "frameHeight") retCLong []

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent source frames and reference frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- sourcePixelBufferAttributes@
sourcePixelBufferAttributes :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id NSDictionary)
sourcePixelBufferAttributes vtLowLatencySuperResolutionScalerConfiguration  =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "sourcePixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Pixel buffer attributes dictionary that describes requirements for pixel buffers which represent destination frames.
--
-- Use ``CVPixelBufferCreateResolvedAttributesDictionary`` to combine this dictionary with your pixel buffer attributes dictionary.
--
-- ObjC selector: @- destinationPixelBufferAttributes@
destinationPixelBufferAttributes :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO (Id NSDictionary)
destinationPixelBufferAttributes vtLowLatencySuperResolutionScalerConfiguration  =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "destinationPixelBufferAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Scale factor with which you initialized the configuration.
--
-- ObjC selector: @- scaleFactor@
scaleFactor :: IsVTLowLatencySuperResolutionScalerConfiguration vtLowLatencySuperResolutionScalerConfiguration => vtLowLatencySuperResolutionScalerConfiguration -> IO CFloat
scaleFactor vtLowLatencySuperResolutionScalerConfiguration  =
  sendMsg vtLowLatencySuperResolutionScalerConfiguration (mkSelector "scaleFactor") retCFloat []

-- | Reports whether the system supports this processor on the current configuration.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "VTLowLatencySuperResolutionScalerConfiguration"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrameWidth:frameHeight:scaleFactor:@
initWithFrameWidth_frameHeight_scaleFactorSelector :: Selector
initWithFrameWidth_frameHeight_scaleFactorSelector = mkSelector "initWithFrameWidth:frameHeight:scaleFactor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @supportedScaleFactorsForFrameWidth:frameHeight:@
supportedScaleFactorsForFrameWidth_frameHeightSelector :: Selector
supportedScaleFactorsForFrameWidth_frameHeightSelector = mkSelector "supportedScaleFactorsForFrameWidth:frameHeight:"

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

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

