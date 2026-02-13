{-# LANGUAGE DataKinds #-}
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
  , highQualityRenderingSelector
  , newPixelBufferSelector
  , renderScaleSelector
  , videoCompositionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
newPixelBuffer avVideoCompositionRenderContext =
  sendOwnedMessage avVideoCompositionRenderContext newPixelBufferSelector

-- | Indicates a scaling ratio that should be applied when rendering frames.
--
-- ObjC selector: @- renderScale@
renderScale :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO CFloat
renderScale avVideoCompositionRenderContext =
  sendMessage avVideoCompositionRenderContext renderScaleSelector

-- | Hints the custom compositor that it may use higher quality, potentially slower algorithms. Generally true for non real time use cases.
--
-- ObjC selector: @- highQualityRendering@
highQualityRendering :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO Bool
highQualityRendering avVideoCompositionRenderContext =
  sendMessage avVideoCompositionRenderContext highQualityRenderingSelector

-- | The AVVideoComposition being rendered.
--
-- ObjC selector: @- videoComposition@
videoComposition :: IsAVVideoCompositionRenderContext avVideoCompositionRenderContext => avVideoCompositionRenderContext -> IO (Id AVVideoComposition)
videoComposition avVideoCompositionRenderContext =
  sendMessage avVideoCompositionRenderContext videoCompositionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newPixelBuffer@
newPixelBufferSelector :: Selector '[] (Ptr ())
newPixelBufferSelector = mkSelector "newPixelBuffer"

-- | @Selector@ for @renderScale@
renderScaleSelector :: Selector '[] CFloat
renderScaleSelector = mkSelector "renderScale"

-- | @Selector@ for @highQualityRendering@
highQualityRenderingSelector :: Selector '[] Bool
highQualityRenderingSelector = mkSelector "highQualityRendering"

-- | @Selector@ for @videoComposition@
videoCompositionSelector :: Selector '[] (Id AVVideoComposition)
videoCompositionSelector = mkSelector "videoComposition"

