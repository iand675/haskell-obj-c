{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , setSamplePositions_count
  , getSamplePositions_count
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
  , rasterizationRateMap
  , setRasterizationRateMap
  , visibilityResultBuffer
  , setVisibilityResultBuffer
  , visibilityResultType
  , setVisibilityResultType
  , supportColorAttachmentMapping
  , setSupportColorAttachmentMapping
  , colorAttachmentsSelector
  , defaultRasterSampleCountSelector
  , depthAttachmentSelector
  , getSamplePositions_countSelector
  , imageblockSampleLengthSelector
  , rasterizationRateMapSelector
  , renderTargetArrayLengthSelector
  , renderTargetHeightSelector
  , renderTargetWidthSelector
  , setDefaultRasterSampleCountSelector
  , setDepthAttachmentSelector
  , setImageblockSampleLengthSelector
  , setRasterizationRateMapSelector
  , setRenderTargetArrayLengthSelector
  , setRenderTargetHeightSelector
  , setRenderTargetWidthSelector
  , setSamplePositions_countSelector
  , setStencilAttachmentSelector
  , setSupportColorAttachmentMappingSelector
  , setThreadgroupMemoryLengthSelector
  , setTileHeightSelector
  , setTileWidthSelector
  , setVisibilityResultBufferSelector
  , setVisibilityResultTypeSelector
  , stencilAttachmentSelector
  , supportColorAttachmentMappingSelector
  , threadgroupMemoryLengthSelector
  , tileHeightSelector
  , tileWidthSelector
  , visibilityResultBufferSelector
  , visibilityResultTypeSelector

  -- * Enum types
  , MTLVisibilityResultType(MTLVisibilityResultType)
  , pattern MTLVisibilityResultTypeReset
  , pattern MTLVisibilityResultTypeAccumulate

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Configures the custom sample positions to use in MSAA rendering.
--
-- - Parameters:   - positions: Array of ``MTLSamplePosition`` instances.   - count:     Number of ``MTLSamplePosition`` instances in the array. This value                needs to be a valid sample count, or @0@ to disable custom sample positions.
--
-- ObjC selector: @- setSamplePositions:count:@
setSamplePositions_count :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> Const RawId -> CULong -> IO ()
setSamplePositions_count mtL4RenderPassDescriptor positions count =
  sendMessage mtL4RenderPassDescriptor setSamplePositions_countSelector positions count

-- | Retrieves the previously-configured custom sample positions.
--
-- This method stores the app's last set custom sample positions into an output array. Metal only modifies the array when the @count@ parameter consists of a length sufficient to store the number of sample positions.
--
-- - Parameters:   - positions: The destination array where Metal stores ``MTLSamplePosition`` instances.   - count:     Number of ``MTLSamplePosition`` instances in the array. This array                needs to be large enough to store all sample positions.
--
-- - Returns: The number of previously-configured custom sample positions.
--
-- ObjC selector: @- getSamplePositions:count:@
getSamplePositions_count :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> RawId -> CULong -> IO CULong
getSamplePositions_count mtL4RenderPassDescriptor positions count =
  sendMessage mtL4RenderPassDescriptor getSamplePositions_countSelector positions count

-- | Accesses the array of state information for render attachments that store color data.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO (Id MTLRenderPassColorAttachmentDescriptorArray)
colorAttachments mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor colorAttachmentsSelector

-- | Accesses state information for a render attachment that stores depth data.
--
-- ObjC selector: @- depthAttachment@
depthAttachment :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO (Id MTLRenderPassDepthAttachmentDescriptor)
depthAttachment mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor depthAttachmentSelector

-- | Accesses state information for a render attachment that stores depth data.
--
-- ObjC selector: @- setDepthAttachment:@
setDepthAttachment :: (IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor, IsMTLRenderPassDepthAttachmentDescriptor value) => mtL4RenderPassDescriptor -> value -> IO ()
setDepthAttachment mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setDepthAttachmentSelector (toMTLRenderPassDepthAttachmentDescriptor value)

-- | Accesses state information for a render attachment that stores stencil data.
--
-- ObjC selector: @- stencilAttachment@
stencilAttachment :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO (Id MTLRenderPassStencilAttachmentDescriptor)
stencilAttachment mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor stencilAttachmentSelector

-- | Accesses state information for a render attachment that stores stencil data.
--
-- ObjC selector: @- setStencilAttachment:@
setStencilAttachment :: (IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor, IsMTLRenderPassStencilAttachmentDescriptor value) => mtL4RenderPassDescriptor -> value -> IO ()
setStencilAttachment mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setStencilAttachmentSelector (toMTLRenderPassStencilAttachmentDescriptor value)

-- | Assigns the number of layers that all attachments this descriptor references have.
--
-- ObjC selector: @- renderTargetArrayLength@
renderTargetArrayLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
renderTargetArrayLength mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor renderTargetArrayLengthSelector

-- | Assigns the number of layers that all attachments this descriptor references have.
--
-- ObjC selector: @- setRenderTargetArrayLength:@
setRenderTargetArrayLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setRenderTargetArrayLength mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setRenderTargetArrayLengthSelector value

-- | Assigns the per-sample size, in bytes, of the largest explicit imageblock layout in the render pass.
--
-- ObjC selector: @- imageblockSampleLength@
imageblockSampleLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
imageblockSampleLength mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor imageblockSampleLengthSelector

-- | Assigns the per-sample size, in bytes, of the largest explicit imageblock layout in the render pass.
--
-- ObjC selector: @- setImageblockSampleLength:@
setImageblockSampleLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setImageblockSampleLength mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setImageblockSampleLengthSelector value

-- | Assigns the per-tile size, in bytes, of the persistent threadgroup memory allocation of this render pass.
--
-- ObjC selector: @- threadgroupMemoryLength@
threadgroupMemoryLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
threadgroupMemoryLength mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor threadgroupMemoryLengthSelector

-- | Assigns the per-tile size, in bytes, of the persistent threadgroup memory allocation of this render pass.
--
-- ObjC selector: @- setThreadgroupMemoryLength:@
setThreadgroupMemoryLength :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setThreadgroupMemoryLength mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setThreadgroupMemoryLengthSelector value

-- | The width of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- tileWidth@
tileWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
tileWidth mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor tileWidthSelector

-- | The width of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- setTileWidth:@
setTileWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setTileWidth mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setTileWidthSelector value

-- | The height of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- tileHeight@
tileHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
tileHeight mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor tileHeightSelector

-- | The height of the tiles, in pixels, a render pass you create with this descriptor applies to its attachments.
--
-- For tile-based rendering, Metal divides each render attachment into smaller regions, or _tiles_. The property's default is @0@, which tells Metal to select a size that fits in tile memory.
--
-- See <doc:tailor-your-apps-for-apple-gpus-and-tile-based-deferred-rendering> for more information about tiles, tile memory, and deferred rendering.
--
-- ObjC selector: @- setTileHeight:@
setTileHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setTileHeight mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setTileHeightSelector value

-- | Sets the default raster sample count for the render pass when it references no attachments.
--
-- ObjC selector: @- defaultRasterSampleCount@
defaultRasterSampleCount :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
defaultRasterSampleCount mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor defaultRasterSampleCountSelector

-- | Sets the default raster sample count for the render pass when it references no attachments.
--
-- ObjC selector: @- setDefaultRasterSampleCount:@
setDefaultRasterSampleCount :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setDefaultRasterSampleCount mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setDefaultRasterSampleCountSelector value

-- | Sets the width, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum width of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- renderTargetWidth@
renderTargetWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
renderTargetWidth mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor renderTargetWidthSelector

-- | Sets the width, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum width of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- setRenderTargetWidth:@
setRenderTargetWidth :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setRenderTargetWidth mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setRenderTargetWidthSelector value

-- | Sets the height, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum height of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- renderTargetHeight@
renderTargetHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO CULong
renderTargetHeight mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor renderTargetHeightSelector

-- | Sets the height, in pixels, to which Metal constrains the render target.
--
-- When this value is non-zero, you need to assign it to be smaller than or equal to the minimum height of all attachments.
--
-- The default value of this property is @0@.
--
-- ObjC selector: @- setRenderTargetHeight:@
setRenderTargetHeight :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> CULong -> IO ()
setRenderTargetHeight mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setRenderTargetHeightSelector value

-- | Assigns an optional variable rasterization rate map that Metal uses in the render pass.
--
-- Enabling variable rasterization rate allows Metal to decrease the rasterization rate, typically in unimportant regions of color attachments, to accelerate processing.
--
-- When set to @nil@, the default, Metal doesn't use variable rasterization rate.
--
-- ObjC selector: @- rasterizationRateMap@
rasterizationRateMap :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO RawId
rasterizationRateMap mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor rasterizationRateMapSelector

-- | Assigns an optional variable rasterization rate map that Metal uses in the render pass.
--
-- Enabling variable rasterization rate allows Metal to decrease the rasterization rate, typically in unimportant regions of color attachments, to accelerate processing.
--
-- When set to @nil@, the default, Metal doesn't use variable rasterization rate.
--
-- ObjC selector: @- setRasterizationRateMap:@
setRasterizationRateMap :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> RawId -> IO ()
setRasterizationRateMap mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setRasterizationRateMapSelector value

-- | Configures a buffer into which Metal writes counts of fragments (pixels) passing the depth and stencil tests.
--
-- ObjC selector: @- visibilityResultBuffer@
visibilityResultBuffer :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO RawId
visibilityResultBuffer mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor visibilityResultBufferSelector

-- | Configures a buffer into which Metal writes counts of fragments (pixels) passing the depth and stencil tests.
--
-- ObjC selector: @- setVisibilityResultBuffer:@
setVisibilityResultBuffer :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> RawId -> IO ()
setVisibilityResultBuffer mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setVisibilityResultBufferSelector value

-- | Determines if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- visibilityResultType@
visibilityResultType :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO MTLVisibilityResultType
visibilityResultType mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor visibilityResultTypeSelector

-- | Determines if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- setVisibilityResultType:@
setVisibilityResultType :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> MTLVisibilityResultType -> IO ()
setVisibilityResultType mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setVisibilityResultTypeSelector value

-- | Controls if the render pass supports color attachment mapping.
--
-- ObjC selector: @- supportColorAttachmentMapping@
supportColorAttachmentMapping :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> IO Bool
supportColorAttachmentMapping mtL4RenderPassDescriptor =
  sendMessage mtL4RenderPassDescriptor supportColorAttachmentMappingSelector

-- | Controls if the render pass supports color attachment mapping.
--
-- ObjC selector: @- setSupportColorAttachmentMapping:@
setSupportColorAttachmentMapping :: IsMTL4RenderPassDescriptor mtL4RenderPassDescriptor => mtL4RenderPassDescriptor -> Bool -> IO ()
setSupportColorAttachmentMapping mtL4RenderPassDescriptor value =
  sendMessage mtL4RenderPassDescriptor setSupportColorAttachmentMappingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSamplePositions:count:@
setSamplePositions_countSelector :: Selector '[Const RawId, CULong] ()
setSamplePositions_countSelector = mkSelector "setSamplePositions:count:"

-- | @Selector@ for @getSamplePositions:count:@
getSamplePositions_countSelector :: Selector '[RawId, CULong] CULong
getSamplePositions_countSelector = mkSelector "getSamplePositions:count:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector '[] (Id MTLRenderPassColorAttachmentDescriptorArray)
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @depthAttachment@
depthAttachmentSelector :: Selector '[] (Id MTLRenderPassDepthAttachmentDescriptor)
depthAttachmentSelector = mkSelector "depthAttachment"

-- | @Selector@ for @setDepthAttachment:@
setDepthAttachmentSelector :: Selector '[Id MTLRenderPassDepthAttachmentDescriptor] ()
setDepthAttachmentSelector = mkSelector "setDepthAttachment:"

-- | @Selector@ for @stencilAttachment@
stencilAttachmentSelector :: Selector '[] (Id MTLRenderPassStencilAttachmentDescriptor)
stencilAttachmentSelector = mkSelector "stencilAttachment"

-- | @Selector@ for @setStencilAttachment:@
setStencilAttachmentSelector :: Selector '[Id MTLRenderPassStencilAttachmentDescriptor] ()
setStencilAttachmentSelector = mkSelector "setStencilAttachment:"

-- | @Selector@ for @renderTargetArrayLength@
renderTargetArrayLengthSelector :: Selector '[] CULong
renderTargetArrayLengthSelector = mkSelector "renderTargetArrayLength"

-- | @Selector@ for @setRenderTargetArrayLength:@
setRenderTargetArrayLengthSelector :: Selector '[CULong] ()
setRenderTargetArrayLengthSelector = mkSelector "setRenderTargetArrayLength:"

-- | @Selector@ for @imageblockSampleLength@
imageblockSampleLengthSelector :: Selector '[] CULong
imageblockSampleLengthSelector = mkSelector "imageblockSampleLength"

-- | @Selector@ for @setImageblockSampleLength:@
setImageblockSampleLengthSelector :: Selector '[CULong] ()
setImageblockSampleLengthSelector = mkSelector "setImageblockSampleLength:"

-- | @Selector@ for @threadgroupMemoryLength@
threadgroupMemoryLengthSelector :: Selector '[] CULong
threadgroupMemoryLengthSelector = mkSelector "threadgroupMemoryLength"

-- | @Selector@ for @setThreadgroupMemoryLength:@
setThreadgroupMemoryLengthSelector :: Selector '[CULong] ()
setThreadgroupMemoryLengthSelector = mkSelector "setThreadgroupMemoryLength:"

-- | @Selector@ for @tileWidth@
tileWidthSelector :: Selector '[] CULong
tileWidthSelector = mkSelector "tileWidth"

-- | @Selector@ for @setTileWidth:@
setTileWidthSelector :: Selector '[CULong] ()
setTileWidthSelector = mkSelector "setTileWidth:"

-- | @Selector@ for @tileHeight@
tileHeightSelector :: Selector '[] CULong
tileHeightSelector = mkSelector "tileHeight"

-- | @Selector@ for @setTileHeight:@
setTileHeightSelector :: Selector '[CULong] ()
setTileHeightSelector = mkSelector "setTileHeight:"

-- | @Selector@ for @defaultRasterSampleCount@
defaultRasterSampleCountSelector :: Selector '[] CULong
defaultRasterSampleCountSelector = mkSelector "defaultRasterSampleCount"

-- | @Selector@ for @setDefaultRasterSampleCount:@
setDefaultRasterSampleCountSelector :: Selector '[CULong] ()
setDefaultRasterSampleCountSelector = mkSelector "setDefaultRasterSampleCount:"

-- | @Selector@ for @renderTargetWidth@
renderTargetWidthSelector :: Selector '[] CULong
renderTargetWidthSelector = mkSelector "renderTargetWidth"

-- | @Selector@ for @setRenderTargetWidth:@
setRenderTargetWidthSelector :: Selector '[CULong] ()
setRenderTargetWidthSelector = mkSelector "setRenderTargetWidth:"

-- | @Selector@ for @renderTargetHeight@
renderTargetHeightSelector :: Selector '[] CULong
renderTargetHeightSelector = mkSelector "renderTargetHeight"

-- | @Selector@ for @setRenderTargetHeight:@
setRenderTargetHeightSelector :: Selector '[CULong] ()
setRenderTargetHeightSelector = mkSelector "setRenderTargetHeight:"

-- | @Selector@ for @rasterizationRateMap@
rasterizationRateMapSelector :: Selector '[] RawId
rasterizationRateMapSelector = mkSelector "rasterizationRateMap"

-- | @Selector@ for @setRasterizationRateMap:@
setRasterizationRateMapSelector :: Selector '[RawId] ()
setRasterizationRateMapSelector = mkSelector "setRasterizationRateMap:"

-- | @Selector@ for @visibilityResultBuffer@
visibilityResultBufferSelector :: Selector '[] RawId
visibilityResultBufferSelector = mkSelector "visibilityResultBuffer"

-- | @Selector@ for @setVisibilityResultBuffer:@
setVisibilityResultBufferSelector :: Selector '[RawId] ()
setVisibilityResultBufferSelector = mkSelector "setVisibilityResultBuffer:"

-- | @Selector@ for @visibilityResultType@
visibilityResultTypeSelector :: Selector '[] MTLVisibilityResultType
visibilityResultTypeSelector = mkSelector "visibilityResultType"

-- | @Selector@ for @setVisibilityResultType:@
setVisibilityResultTypeSelector :: Selector '[MTLVisibilityResultType] ()
setVisibilityResultTypeSelector = mkSelector "setVisibilityResultType:"

-- | @Selector@ for @supportColorAttachmentMapping@
supportColorAttachmentMappingSelector :: Selector '[] Bool
supportColorAttachmentMappingSelector = mkSelector "supportColorAttachmentMapping"

-- | @Selector@ for @setSupportColorAttachmentMapping:@
setSupportColorAttachmentMappingSelector :: Selector '[Bool] ()
setSupportColorAttachmentMappingSelector = mkSelector "setSupportColorAttachmentMapping:"

