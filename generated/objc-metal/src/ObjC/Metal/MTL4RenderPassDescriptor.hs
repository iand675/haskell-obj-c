{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a render pass.
--
-- You use render pass descriptors to create instances of ``MTL4RenderCommandEncoder`` and encode draw commands into instances of ``MTL4CommandBuffer``.
--
-- To create render command encoders, you typically call ``MTL4CommandBuffer/renderCommandEncoderWithDescriptor:``. The ``MTL4CommandBuffer/renderCommandEncoderWithDescriptor:options:`` variant of this method allows you to specify additional options to encode a render pass in parallel from multiple CPU cores by creating *suspending* and *resuming* render passes.
--
-- Generated bindings for @MTL4RenderPassDescriptor@.
module ObjC.Metal.MTL4RenderPassDescriptor
  ( MTL4RenderPassDescriptor
  , IsMTL4RenderPassDescriptor(..)
  , colorAttachments
  , depthAttachment
  , setDepthAttachment
  , stencilAttachment
  , setStencilAttachment
  , renderTargetArrayLength
  , setRenderTargetArrayLength
  , imageblockSampleLength
  , setImageblockSampleLength
  , threadgroupMemoryLength
  , setThreadgroupMemoryLength
  , tileWidth
  , setTileWidth
  , tileHeight
  , setTileHeight
  , defaultRasterSampleCount
  , setDefaultRasterSampleCount
  , renderTargetWidth
  , setRenderTargetWidth
  , renderTargetHeight
  , setRenderTargetHeight
  , visibilityResultType
  , setVisibilityResultType
  , supportColorAttachmentMapping
  , setSupportColorAttachmentMapping
  , colorAttachmentsSelector
  , depthAttachmentSelector
  , setDepthAttachmentSelector
  , stencilAttachmentSelector
  , setStencilAttachmentSelector
  , renderTargetArrayLengthSelector
  , setRenderTargetArrayLengthSelector
  , imageblockSampleLengthSelector
  , setImageblockSampleLengthSelector
  , threadgroupMemoryLengthSelector
  , setThreadgroupMemoryLengthSelector
  , tileWidthSelector
  , setTileWidthSelector
  , tileHeightSelector
  , setTileHeightSelector
  , defaultRasterSampleCountSelector
  , setDefaultRasterSampleCountSelector
  , renderTargetWidthSelector
  , setRenderTargetWidthSelector
  , renderTargetHeightSelector
  , setRenderTargetHeightSelector
  , visibilityResultTypeSelector
  , setVisibilityResultTypeSelector
  , supportColorAttachmentMappingSelector
  , setSupportColorAttachmentMappingSelector

  -- * Enum types
  , MTLVisibilityResultType(MTLVisibilityResultType)
  , pattern MTLVisibilityResultTypeReset
  , pattern MTLVisibilityResultTypeAccumulate

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Accesses the array of state information for render attachments that store color data.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO (Id MTLRenderPassColorAttachmentDescriptorArray)
colorAttachments mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Accesses state information for a render attachment that stores depth data.
--
-- ObjC selector: @- depthAttachment@
depthAttachment :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO (Id MTLRenderPassDepthAttachmentDescriptor)
depthAttachment mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "depthAttachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Accesses state information for a render attachment that stores depth data.
--
-- ObjC selector: @- setDepthAttachment:@
setDepthAttachment :: (IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor, IsMTLRenderPassDepthAttachmentDescriptor value) => mtL4RenderPassDescriptor -> value -> IO ()
setDepthAttachment mtL4RenderPassDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPassDescriptor (mkSelector "setDepthAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Accesses state information for a render attachment that stores stencil data.
--
-- ObjC selector: @- stencilAttachment@
stencilAttachment :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO (Id MTLRenderPassStencilAttachmentDescriptor)
stencilAttachment mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "stencilAttachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Accesses state information for a render attachment that stores stencil data.
--
-- ObjC selector: @- setStencilAttachment:@
setStencilAttachment :: (IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor, IsMTLRenderPassStencilAttachmentDescriptor value) => mtL4RenderPassDescriptor -> value -> IO ()
setStencilAttachment mtL4RenderPassDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4RenderPassDescriptor (mkSelector "setStencilAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns the number of layers that all attachments this descriptor references have.
--
-- ObjC selector: @- renderTargetArrayLength@
renderTargetArrayLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
renderTargetArrayLength mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "renderTargetArrayLength") retCULong []

-- | Assigns the number of layers that all attachments this descriptor references have.
--
-- ObjC selector: @- setRenderTargetArrayLength:@
setRenderTargetArrayLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setRenderTargetArrayLength mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setRenderTargetArrayLength:") retVoid [argCULong (fromIntegral value)]

-- | Assigns the per-sample size, in bytes, of the largest explicit imageblock layout in the render pass.
--
-- ObjC selector: @- imageblockSampleLength@
imageblockSampleLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
imageblockSampleLength mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "imageblockSampleLength") retCULong []

-- | Assigns the per-sample size, in bytes, of the largest explicit imageblock layout in the render pass.
--
-- ObjC selector: @- setImageblockSampleLength:@
setImageblockSampleLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setImageblockSampleLength mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setImageblockSampleLength:") retVoid [argCULong (fromIntegral value)]

-- | Assigns the per-tile size, in bytes, of the persistent threadgroup memory allocation of this render pass.
--
-- ObjC selector: @- threadgroupMemoryLength@
threadgroupMemoryLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
threadgroupMemoryLength mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "threadgroupMemoryLength") retCULong []

-- | Assigns the per-tile size, in bytes, of the persistent threadgroup memory allocation of this render pass.
--
-- ObjC selector: @- setThreadgroupMemoryLength:@
setThreadgroupMemoryLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setThreadgroupMemoryLength mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setThreadgroupMemoryLength:") retVoid [argCULong (fromIntegral value)]

-- | The width of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- tileWidth@
tileWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
tileWidth mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "tileWidth") retCULong []

-- | The width of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- setTileWidth:@
setTileWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setTileWidth mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setTileWidth:") retVoid [argCULong (fromIntegral value)]

-- | The height of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- tileHeight@
tileHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
tileHeight mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "tileHeight") retCULong []

-- | The height of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- setTileHeight:@
setTileHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setTileHeight mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setTileHeight:") retVoid [argCULong (fromIntegral value)]

-- | Sets the default raster sample count for the render pass when it references no attachments.
--
-- ObjC selector: @- defaultRasterSampleCount@
defaultRasterSampleCount :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
defaultRasterSampleCount mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "defaultRasterSampleCount") retCULong []

-- | Sets the default raster sample count for the render pass when it references no attachments.
--
-- ObjC selector: @- setDefaultRasterSampleCount:@
setDefaultRasterSampleCount :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setDefaultRasterSampleCount mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setDefaultRasterSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | Sets the width, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum width of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- renderTargetWidth@
renderTargetWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
renderTargetWidth mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "renderTargetWidth") retCULong []

-- | Sets the width, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum width of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- setRenderTargetWidth:@
setRenderTargetWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setRenderTargetWidth mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setRenderTargetWidth:") retVoid [argCULong (fromIntegral value)]

-- | Sets the height, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum height of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- renderTargetHeight@
renderTargetHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
renderTargetHeight mtL4RenderPassDescriptor  =
  sendMsg mtL4RenderPassDescriptor (mkSelector "renderTargetHeight") retCULong []

-- | Sets the height, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum height of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- setRenderTargetHeight:@
setRenderTargetHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setRenderTargetHeight mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setRenderTargetHeight:") retVoid [argCULong (fromIntegral value)]

-- | Determines if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- visibilityResultType@
visibilityResultType :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO MTLVisibilityResultType
visibilityResultType mtL4RenderPassDescriptor  =
  fmap (coerce :: CLong -> MTLVisibilityResultType) $ sendMsg mtL4RenderPassDescriptor (mkSelector "visibilityResultType") retCLong []

-- | Determines if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- setVisibilityResultType:@
setVisibilityResultType :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> MTLVisibilityResultType -> IO ()
setVisibilityResultType mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setVisibilityResultType:") retVoid [argCLong (coerce value)]

-- | Controls if the render pass supports color attachment mapping.
--
-- ObjC selector: @- supportColorAttachmentMapping@
supportColorAttachmentMapping :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO Bool
supportColorAttachmentMapping mtL4RenderPassDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4RenderPassDescriptor (mkSelector "supportColorAttachmentMapping") retCULong []

-- | Controls if the render pass supports color attachment mapping.
--
-- ObjC selector: @- setSupportColorAttachmentMapping:@
setSupportColorAttachmentMapping :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> Bool -> IO ()
setSupportColorAttachmentMapping mtL4RenderPassDescriptor  value =
  sendMsg mtL4RenderPassDescriptor (mkSelector "setSupportColorAttachmentMapping:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @depthAttachment@
depthAttachmentSelector :: Selector
depthAttachmentSelector = mkSelector "depthAttachment"

-- | @Selector@ for @setDepthAttachment:@
setDepthAttachmentSelector :: Selector
setDepthAttachmentSelector = mkSelector "setDepthAttachment:"

-- | @Selector@ for @stencilAttachment@
stencilAttachmentSelector :: Selector
stencilAttachmentSelector = mkSelector "stencilAttachment"

-- | @Selector@ for @setStencilAttachment:@
setStencilAttachmentSelector :: Selector
setStencilAttachmentSelector = mkSelector "setStencilAttachment:"

-- | @Selector@ for @renderTargetArrayLength@
renderTargetArrayLengthSelector :: Selector
renderTargetArrayLengthSelector = mkSelector "renderTargetArrayLength"

-- | @Selector@ for @setRenderTargetArrayLength:@
setRenderTargetArrayLengthSelector :: Selector
setRenderTargetArrayLengthSelector = mkSelector "setRenderTargetArrayLength:"

-- | @Selector@ for @imageblockSampleLength@
imageblockSampleLengthSelector :: Selector
imageblockSampleLengthSelector = mkSelector "imageblockSampleLength"

-- | @Selector@ for @setImageblockSampleLength:@
setImageblockSampleLengthSelector :: Selector
setImageblockSampleLengthSelector = mkSelector "setImageblockSampleLength:"

-- | @Selector@ for @threadgroupMemoryLength@
threadgroupMemoryLengthSelector :: Selector
threadgroupMemoryLengthSelector = mkSelector "threadgroupMemoryLength"

-- | @Selector@ for @setThreadgroupMemoryLength:@
setThreadgroupMemoryLengthSelector :: Selector
setThreadgroupMemoryLengthSelector = mkSelector "setThreadgroupMemoryLength:"

-- | @Selector@ for @tileWidth@
tileWidthSelector :: Selector
tileWidthSelector = mkSelector "tileWidth"

-- | @Selector@ for @setTileWidth:@
setTileWidthSelector :: Selector
setTileWidthSelector = mkSelector "setTileWidth:"

-- | @Selector@ for @tileHeight@
tileHeightSelector :: Selector
tileHeightSelector = mkSelector "tileHeight"

-- | @Selector@ for @setTileHeight:@
setTileHeightSelector :: Selector
setTileHeightSelector = mkSelector "setTileHeight:"

-- | @Selector@ for @defaultRasterSampleCount@
defaultRasterSampleCountSelector :: Selector
defaultRasterSampleCountSelector = mkSelector "defaultRasterSampleCount"

-- | @Selector@ for @setDefaultRasterSampleCount:@
setDefaultRasterSampleCountSelector :: Selector
setDefaultRasterSampleCountSelector = mkSelector "setDefaultRasterSampleCount:"

-- | @Selector@ for @renderTargetWidth@
renderTargetWidthSelector :: Selector
renderTargetWidthSelector = mkSelector "renderTargetWidth"

-- | @Selector@ for @setRenderTargetWidth:@
setRenderTargetWidthSelector :: Selector
setRenderTargetWidthSelector = mkSelector "setRenderTargetWidth:"

-- | @Selector@ for @renderTargetHeight@
renderTargetHeightSelector :: Selector
renderTargetHeightSelector = mkSelector "renderTargetHeight"

-- | @Selector@ for @setRenderTargetHeight:@
setRenderTargetHeightSelector :: Selector
setRenderTargetHeightSelector = mkSelector "setRenderTargetHeight:"

-- | @Selector@ for @visibilityResultType@
visibilityResultTypeSelector :: Selector
visibilityResultTypeSelector = mkSelector "visibilityResultType"

-- | @Selector@ for @setVisibilityResultType:@
setVisibilityResultTypeSelector :: Selector
setVisibilityResultTypeSelector = mkSelector "setVisibilityResultType:"

-- | @Selector@ for @supportColorAttachmentMapping@
supportColorAttachmentMappingSelector :: Selector
supportColorAttachmentMappingSelector = mkSelector "supportColorAttachmentMapping"

-- | @Selector@ for @setSupportColorAttachmentMapping:@
setSupportColorAttachmentMappingSelector :: Selector
setSupportColorAttachmentMappingSelector = mkSelector "setSupportColorAttachmentMapping:"

