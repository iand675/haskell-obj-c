{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVRenderedCaptionImage
--
-- AVRenderedCaptionImage is a wrapper class vended out to the client for reading a rendered caption image (CVPixelBuffer) and its associated position (in pixels). The position is relative to the videoDisplaySize (in pixels) provided by the client during the initialization of AVPlayerItemRenderedLegibleOutput, and accordinging to the upper-left-origin coordinate system (ULO). The CVPixelBuffer will be backed by an IOSurface enabling it to be converted to MTLTexture using CVMetalTextureCache.
--
-- Display scale is a property of the screen on which the client UI elements are displayed. This value defines the mapping between the logical coordinate space (measured in points) and the physical coordinate space (measured in pixels). Higher scale factors indicate that each point is represented by more than one pixel at render time. For example, if the display scale factor is 2.0 and the bounds of caption rectangle are 50 x 50 points, the size of the CVPixelBufferRef for the caption is 100 x 100 pixels. The client shall provide videoDisplaySize value in pixels only and the position value of the caption image shall also be returned in pixels only.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVRenderedCaptionImage@.
module ObjC.AVFoundation.AVRenderedCaptionImage
  ( AVRenderedCaptionImage
  , IsAVRenderedCaptionImage(..)
  , init_
  , new
  , pixelBuffer
  , initSelector
  , newSelector
  , pixelBufferSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVRenderedCaptionImage avRenderedCaptionImage => avRenderedCaptionImage -> IO (Id AVRenderedCaptionImage)
init_ avRenderedCaptionImage =
  sendOwnedMessage avRenderedCaptionImage initSelector

-- | @+ new@
new :: IO (Id AVRenderedCaptionImage)
new  =
  do
    cls' <- getRequiredClass "AVRenderedCaptionImage"
    sendOwnedClassMessage cls' newSelector

-- | pixelBuffer
--
-- A CVPixelBufferRef that contains pixel data for the rendered caption.
--
-- If the client reads a pixelBuffer and wants to use it longer than AVRenderedCaptionImage, it must retain the pixelBuffer. The pixel buffer can be converted to MTLTexture using CVMetalTextureCache. The pixel format is fixed to kCVPixelFormatType_32BGRA defined in <CoreVideo/CVPixelBuffer.h>.
--
-- ObjC selector: @- pixelBuffer@
pixelBuffer :: IsAVRenderedCaptionImage avRenderedCaptionImage => avRenderedCaptionImage -> IO (Ptr ())
pixelBuffer avRenderedCaptionImage =
  sendMessage avRenderedCaptionImage pixelBufferSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVRenderedCaptionImage)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVRenderedCaptionImage)
newSelector = mkSelector "new"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector '[] (Ptr ())
pixelBufferSelector = mkSelector "pixelBuffer"

