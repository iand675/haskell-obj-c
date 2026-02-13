{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLRenderPassDescriptor
--
-- MTLRenderPassDescriptor represents a collection of attachments to be used to create a concrete render command encoder
--
-- Generated bindings for @MTLRenderPassDescriptor@.
module ObjC.Metal.MTLRenderPassDescriptor
  ( MTLRenderPassDescriptor
  , IsMTLRenderPassDescriptor(..)
  , renderPassDescriptor
  , setSamplePositions_count
  , getSamplePositions_count
  , colorAttachments
  , depthAttachment
  , setDepthAttachment
  , stencilAttachment
  , setStencilAttachment
  , visibilityResultBuffer
  , setVisibilityResultBuffer
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
  , sampleBufferAttachments
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
  , renderPassDescriptorSelector
  , renderTargetArrayLengthSelector
  , renderTargetHeightSelector
  , renderTargetWidthSelector
  , sampleBufferAttachmentsSelector
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

-- | renderPassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ renderPassDescriptor@
renderPassDescriptor :: IO (Id MTLRenderPassDescriptor)
renderPassDescriptor  =
  do
    cls' <- getRequiredClass "MTLRenderPassDescriptor"
    sendClassMessage cls' renderPassDescriptorSelector

-- | setSamplePositions:count:
--
-- Configure the custom sample positions, to be used in MSAA rendering (i.e. when sample count > 1).
--
-- @positions@ — The source array for custom sample position data.
--
-- @count@ — Specifies the length of the positions array, and must be a valid sample count or 0 (to disable custom sample positions).
--
-- ObjC selector: @- setSamplePositions:count:@
setSamplePositions_count :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> Const RawId -> CULong -> IO ()
setSamplePositions_count mtlRenderPassDescriptor positions count =
  sendMessage mtlRenderPassDescriptor setSamplePositions_countSelector positions count

-- | getSamplePositions:count:
--
-- Retrieve the previously configured custom sample positions. The positions input array will only be modified when count specifies a length sufficient for the number of previously configured positions.
--
-- @positions@ — The destination array for custom sample position data.
--
-- @count@ — Specifies the length of the positions array, which must be large enough to hold all configured sample positions.
--
-- Returns: The number of previously configured custom sample positions.
--
-- ObjC selector: @- getSamplePositions:count:@
getSamplePositions_count :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> RawId -> CULong -> IO CULong
getSamplePositions_count mtlRenderPassDescriptor positions count =
  sendMessage mtlRenderPassDescriptor getSamplePositions_countSelector positions count

-- | @- colorAttachments@
colorAttachments :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassColorAttachmentDescriptorArray)
colorAttachments mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor colorAttachmentsSelector

-- | @- depthAttachment@
depthAttachment :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassDepthAttachmentDescriptor)
depthAttachment mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor depthAttachmentSelector

-- | @- setDepthAttachment:@
setDepthAttachment :: (IsMTLRenderPassDescriptor mtlRenderPassDescriptor, IsMTLRenderPassDepthAttachmentDescriptor value) => mtlRenderPassDescriptor -> value -> IO ()
setDepthAttachment mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setDepthAttachmentSelector (toMTLRenderPassDepthAttachmentDescriptor value)

-- | @- stencilAttachment@
stencilAttachment :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassStencilAttachmentDescriptor)
stencilAttachment mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor stencilAttachmentSelector

-- | @- setStencilAttachment:@
setStencilAttachment :: (IsMTLRenderPassDescriptor mtlRenderPassDescriptor, IsMTLRenderPassStencilAttachmentDescriptor value) => mtlRenderPassDescriptor -> value -> IO ()
setStencilAttachment mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setStencilAttachmentSelector (toMTLRenderPassStencilAttachmentDescriptor value)

-- | visibilityResultBuffer:
--
-- Buffer into which samples passing the depth and stencil tests are counted.
--
-- ObjC selector: @- visibilityResultBuffer@
visibilityResultBuffer :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO RawId
visibilityResultBuffer mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor visibilityResultBufferSelector

-- | visibilityResultBuffer:
--
-- Buffer into which samples passing the depth and stencil tests are counted.
--
-- ObjC selector: @- setVisibilityResultBuffer:@
setVisibilityResultBuffer :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> RawId -> IO ()
setVisibilityResultBuffer mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setVisibilityResultBufferSelector value

-- | renderTargetArrayLength:
--
-- The number of active layers
--
-- ObjC selector: @- renderTargetArrayLength@
renderTargetArrayLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
renderTargetArrayLength mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor renderTargetArrayLengthSelector

-- | renderTargetArrayLength:
--
-- The number of active layers
--
-- ObjC selector: @- setRenderTargetArrayLength:@
setRenderTargetArrayLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setRenderTargetArrayLength mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setRenderTargetArrayLengthSelector value

-- | imageblockSampleLength:
--
-- The per sample size in bytes of the largest explicit imageblock layout in the renderPass.
--
-- ObjC selector: @- imageblockSampleLength@
imageblockSampleLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
imageblockSampleLength mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor imageblockSampleLengthSelector

-- | imageblockSampleLength:
--
-- The per sample size in bytes of the largest explicit imageblock layout in the renderPass.
--
-- ObjC selector: @- setImageblockSampleLength:@
setImageblockSampleLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setImageblockSampleLength mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setImageblockSampleLengthSelector value

-- | threadgroupMemoryLength:
--
-- The per tile size in bytes of the persistent threadgroup memory allocation.
--
-- ObjC selector: @- threadgroupMemoryLength@
threadgroupMemoryLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
threadgroupMemoryLength mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor threadgroupMemoryLengthSelector

-- | threadgroupMemoryLength:
--
-- The per tile size in bytes of the persistent threadgroup memory allocation.
--
-- ObjC selector: @- setThreadgroupMemoryLength:@
setThreadgroupMemoryLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setThreadgroupMemoryLength mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setThreadgroupMemoryLengthSelector value

-- | tileWidth:
--
-- The width in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a width that fits within the local memory.
--
-- ObjC selector: @- tileWidth@
tileWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
tileWidth mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor tileWidthSelector

-- | tileWidth:
--
-- The width in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a width that fits within the local memory.
--
-- ObjC selector: @- setTileWidth:@
setTileWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setTileWidth mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setTileWidthSelector value

-- | tileHeight:
--
-- The height in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a height that fits within the local memory.
--
-- ObjC selector: @- tileHeight@
tileHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
tileHeight mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor tileHeightSelector

-- | tileHeight:
--
-- The height in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a height that fits within the local memory.
--
-- ObjC selector: @- setTileHeight:@
setTileHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setTileHeight mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setTileHeightSelector value

-- | defaultRasterSampleCount:
--
-- The raster sample count for the render pass when no attachments are given.
--
-- ObjC selector: @- defaultRasterSampleCount@
defaultRasterSampleCount :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
defaultRasterSampleCount mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor defaultRasterSampleCountSelector

-- | defaultRasterSampleCount:
--
-- The raster sample count for the render pass when no attachments are given.
--
-- ObjC selector: @- setDefaultRasterSampleCount:@
setDefaultRasterSampleCount :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setDefaultRasterSampleCount mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setDefaultRasterSampleCountSelector value

-- | renderTargetWidth:
--
-- The width in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum width of all attachments.
--
-- ObjC selector: @- renderTargetWidth@
renderTargetWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
renderTargetWidth mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor renderTargetWidthSelector

-- | renderTargetWidth:
--
-- The width in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum width of all attachments.
--
-- ObjC selector: @- setRenderTargetWidth:@
setRenderTargetWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setRenderTargetWidth mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setRenderTargetWidthSelector value

-- | renderTargetHeight:
--
-- The height in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum height of all attachments.
--
-- ObjC selector: @- renderTargetHeight@
renderTargetHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
renderTargetHeight mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor renderTargetHeightSelector

-- | renderTargetHeight:
--
-- The height in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum height of all attachments.
--
-- ObjC selector: @- setRenderTargetHeight:@
setRenderTargetHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setRenderTargetHeight mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setRenderTargetHeightSelector value

-- | rasterizationRateMap
--
-- The variable rasterization rate map to use when rendering this pass, or nil to not use variable rasterization rate.
--
-- The default value is nil. Enabling variable rasterization rate allows for decreasing the rasterization rate in unimportant regions of screen space.
--
-- ObjC selector: @- rasterizationRateMap@
rasterizationRateMap :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO RawId
rasterizationRateMap mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor rasterizationRateMapSelector

-- | rasterizationRateMap
--
-- The variable rasterization rate map to use when rendering this pass, or nil to not use variable rasterization rate.
--
-- The default value is nil. Enabling variable rasterization rate allows for decreasing the rasterization rate in unimportant regions of screen space.
--
-- ObjC selector: @- setRasterizationRateMap:@
setRasterizationRateMap :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> RawId -> IO ()
setRasterizationRateMap mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setRasterizationRateMapSelector value

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor sampleBufferAttachmentsSelector

-- | Specifies if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- visibilityResultType@
visibilityResultType :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO MTLVisibilityResultType
visibilityResultType mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor visibilityResultTypeSelector

-- | Specifies if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- setVisibilityResultType:@
setVisibilityResultType :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> MTLVisibilityResultType -> IO ()
setVisibilityResultType mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setVisibilityResultTypeSelector value

-- | Specifies if the render pass should support color attachment mapping.
--
-- ObjC selector: @- supportColorAttachmentMapping@
supportColorAttachmentMapping :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO Bool
supportColorAttachmentMapping mtlRenderPassDescriptor =
  sendMessage mtlRenderPassDescriptor supportColorAttachmentMappingSelector

-- | Specifies if the render pass should support color attachment mapping.
--
-- ObjC selector: @- setSupportColorAttachmentMapping:@
setSupportColorAttachmentMapping :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> Bool -> IO ()
setSupportColorAttachmentMapping mtlRenderPassDescriptor value =
  sendMessage mtlRenderPassDescriptor setSupportColorAttachmentMappingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @renderPassDescriptor@
renderPassDescriptorSelector :: Selector '[] (Id MTLRenderPassDescriptor)
renderPassDescriptorSelector = mkSelector "renderPassDescriptor"

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

-- | @Selector@ for @visibilityResultBuffer@
visibilityResultBufferSelector :: Selector '[] RawId
visibilityResultBufferSelector = mkSelector "visibilityResultBuffer"

-- | @Selector@ for @setVisibilityResultBuffer:@
setVisibilityResultBufferSelector :: Selector '[RawId] ()
setVisibilityResultBufferSelector = mkSelector "setVisibilityResultBuffer:"

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

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector '[] (Id MTLRenderPassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

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

