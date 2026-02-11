{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The AVVideoCompositionRenderContext class defines the context within which custom compositors render new output pixels buffers.
--
-- An instance of AVVideoCompositionRenderContext provides size and scaling information and offers a service for efficiently providing pixel buffers from a managed pool of buffers.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVVideoCompositionRenderContext@.
module ObjC.AVFoundation.AVVideoCompositionRenderContext
  ( AVVideoCompositionRenderContext
  , IsAVVideoCompositionRenderContext(..)
  , newPixelBuffer
  , renderScale
  , highQualityRendering
  , videoComposition
  , newPixelBufferSelector
  , renderScaleSelector
  , highQualityRenderingSelector
  , videoCompositionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Vends a CVPixelBuffer to use for rendering
--
-- The buffer will have its kCVImageBufferCleanApertureKey and kCVImageBufferPixelAspectRatioKey attachments set to match the current composition processor properties.
--
-- ObjC selector: @- newPixelBuffer@
newPixelBuffer :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO (Ptr ())
newPixelBuffer avVideoCompositionRenderContext  =
  fmap castPtr $ sendMsg avVideoCompositionRenderContext (mkSelector "newPixelBuffer") (retPtr retVoid) []

-- | Indicates a scaling ratio that should be applied when rendering frames.
--
-- ObjC selector: @- renderScale@
renderScale :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO CFloat
renderScale avVideoCompositionRenderContext  =
  sendMsg avVideoCompositionRenderContext (mkSelector "renderScale") retCFloat []

-- | Hints the custom compositor that it may use higher quality, potentially slower algorithms. Generally true for non real time use cases.
--
-- ObjC selector: @- highQualityRendering@
highQualityRendering :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO Bool
highQualityRendering avVideoCompositionRenderContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avVideoCompositionRenderContext (mkSelector "highQualityRendering") retCULong []

-- | The AVVideoComposition being rendered.
--
-- ObjC selector: @- videoComposition@
videoComposition :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO (Id AVVideoComposition)
videoComposition avVideoCompositionRenderContext  =
  sendMsg avVideoCompositionRenderContext (mkSelector "videoComposition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPixelBuffer@
newPixelBufferSelector :: Selector
newPixelBufferSelector = mkSelector "newPixelBuffer"

-- | @Selector@ for @renderScale@
renderScaleSelector :: Selector
renderScaleSelector = mkSelector "renderScale"

-- | @Selector@ for @highQualityRendering@
highQualityRenderingSelector :: Selector
highQualityRenderingSelector = mkSelector "highQualityRendering"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector
videoCompositionSelector = mkSelector "videoComposition"

