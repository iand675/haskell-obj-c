{-# LANGUAGE PatternSynonyms #-}
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
  , renderPassDescriptorSelector
  , setSamplePositions_countSelector
  , getSamplePositions_countSelector
  , colorAttachmentsSelector
  , depthAttachmentSelector
  , setDepthAttachmentSelector
  , stencilAttachmentSelector
  , setStencilAttachmentSelector
  , visibilityResultBufferSelector
  , setVisibilityResultBufferSelector
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
  , rasterizationRateMapSelector
  , setRasterizationRateMapSelector
  , sampleBufferAttachmentsSelector
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

-- | renderPassDescriptor
--
-- Create an autoreleased default frame buffer descriptor
--
-- ObjC selector: @+ renderPassDescriptor@
renderPassDescriptor :: IO (Id MTLRenderPassDescriptor)
renderPassDescriptor  =
  do
    cls' <- getRequiredClass "MTLRenderPassDescriptor"
    sendClassMsg cls' (mkSelector "renderPassDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setSamplePositions_count mtlRenderPassDescriptor  positions count =
    sendMsg mtlRenderPassDescriptor (mkSelector "setSamplePositions:count:") retVoid [argPtr (castPtr (unRawId (unConst positions)) :: Ptr ()), argCULong count]

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
getSamplePositions_count mtlRenderPassDescriptor  positions count =
    sendMsg mtlRenderPassDescriptor (mkSelector "getSamplePositions:count:") retCULong [argPtr (castPtr (unRawId positions) :: Ptr ()), argCULong count]

-- | @- colorAttachments@
colorAttachments :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassColorAttachmentDescriptorArray)
colorAttachments mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- depthAttachment@
depthAttachment :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassDepthAttachmentDescriptor)
depthAttachment mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "depthAttachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDepthAttachment:@
setDepthAttachment :: (IsMTLRenderPassDescriptor mtlRenderPassDescriptor, IsMTLRenderPassDepthAttachmentDescriptor value) => mtlRenderPassDescriptor -> value -> IO ()
setDepthAttachment mtlRenderPassDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlRenderPassDescriptor (mkSelector "setDepthAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- stencilAttachment@
stencilAttachment :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassStencilAttachmentDescriptor)
stencilAttachment mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "stencilAttachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStencilAttachment:@
setStencilAttachment :: (IsMTLRenderPassDescriptor mtlRenderPassDescriptor, IsMTLRenderPassStencilAttachmentDescriptor value) => mtlRenderPassDescriptor -> value -> IO ()
setStencilAttachment mtlRenderPassDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlRenderPassDescriptor (mkSelector "setStencilAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | visibilityResultBuffer:
--
-- Buffer into which samples passing the depth and stencil tests are counted.
--
-- ObjC selector: @- visibilityResultBuffer@
visibilityResultBuffer :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO RawId
visibilityResultBuffer mtlRenderPassDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlRenderPassDescriptor (mkSelector "visibilityResultBuffer") (retPtr retVoid) []

-- | visibilityResultBuffer:
--
-- Buffer into which samples passing the depth and stencil tests are counted.
--
-- ObjC selector: @- setVisibilityResultBuffer:@
setVisibilityResultBuffer :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> RawId -> IO ()
setVisibilityResultBuffer mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setVisibilityResultBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | renderTargetArrayLength:
--
-- The number of active layers
--
-- ObjC selector: @- renderTargetArrayLength@
renderTargetArrayLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
renderTargetArrayLength mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "renderTargetArrayLength") retCULong []

-- | renderTargetArrayLength:
--
-- The number of active layers
--
-- ObjC selector: @- setRenderTargetArrayLength:@
setRenderTargetArrayLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setRenderTargetArrayLength mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setRenderTargetArrayLength:") retVoid [argCULong value]

-- | imageblockSampleLength:
--
-- The per sample size in bytes of the largest explicit imageblock layout in the renderPass.
--
-- ObjC selector: @- imageblockSampleLength@
imageblockSampleLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
imageblockSampleLength mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "imageblockSampleLength") retCULong []

-- | imageblockSampleLength:
--
-- The per sample size in bytes of the largest explicit imageblock layout in the renderPass.
--
-- ObjC selector: @- setImageblockSampleLength:@
setImageblockSampleLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setImageblockSampleLength mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setImageblockSampleLength:") retVoid [argCULong value]

-- | threadgroupMemoryLength:
--
-- The per tile size in bytes of the persistent threadgroup memory allocation.
--
-- ObjC selector: @- threadgroupMemoryLength@
threadgroupMemoryLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
threadgroupMemoryLength mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "threadgroupMemoryLength") retCULong []

-- | threadgroupMemoryLength:
--
-- The per tile size in bytes of the persistent threadgroup memory allocation.
--
-- ObjC selector: @- setThreadgroupMemoryLength:@
setThreadgroupMemoryLength :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setThreadgroupMemoryLength mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setThreadgroupMemoryLength:") retVoid [argCULong value]

-- | tileWidth:
--
-- The width in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a width that fits within the local memory.
--
-- ObjC selector: @- tileWidth@
tileWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
tileWidth mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "tileWidth") retCULong []

-- | tileWidth:
--
-- The width in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a width that fits within the local memory.
--
-- ObjC selector: @- setTileWidth:@
setTileWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setTileWidth mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setTileWidth:") retVoid [argCULong value]

-- | tileHeight:
--
-- The height in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a height that fits within the local memory.
--
-- ObjC selector: @- tileHeight@
tileHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
tileHeight mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "tileHeight") retCULong []

-- | tileHeight:
--
-- The height in pixels of the tile.
--
-- Defaults to 0. Zero means Metal chooses a height that fits within the local memory.
--
-- ObjC selector: @- setTileHeight:@
setTileHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setTileHeight mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setTileHeight:") retVoid [argCULong value]

-- | defaultRasterSampleCount:
--
-- The raster sample count for the render pass when no attachments are given.
--
-- ObjC selector: @- defaultRasterSampleCount@
defaultRasterSampleCount :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
defaultRasterSampleCount mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "defaultRasterSampleCount") retCULong []

-- | defaultRasterSampleCount:
--
-- The raster sample count for the render pass when no attachments are given.
--
-- ObjC selector: @- setDefaultRasterSampleCount:@
setDefaultRasterSampleCount :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setDefaultRasterSampleCount mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setDefaultRasterSampleCount:") retVoid [argCULong value]

-- | renderTargetWidth:
--
-- The width in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum width of all attachments.
--
-- ObjC selector: @- renderTargetWidth@
renderTargetWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
renderTargetWidth mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "renderTargetWidth") retCULong []

-- | renderTargetWidth:
--
-- The width in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum width of all attachments.
--
-- ObjC selector: @- setRenderTargetWidth:@
setRenderTargetWidth :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setRenderTargetWidth mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setRenderTargetWidth:") retVoid [argCULong value]

-- | renderTargetHeight:
--
-- The height in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum height of all attachments.
--
-- ObjC selector: @- renderTargetHeight@
renderTargetHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO CULong
renderTargetHeight mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "renderTargetHeight") retCULong []

-- | renderTargetHeight:
--
-- The height in pixels to constrain the render target to.
--
-- Defaults to 0. If non-zero the value must be smaller than or equal to the minimum height of all attachments.
--
-- ObjC selector: @- setRenderTargetHeight:@
setRenderTargetHeight :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> CULong -> IO ()
setRenderTargetHeight mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setRenderTargetHeight:") retVoid [argCULong value]

-- | rasterizationRateMap
--
-- The variable rasterization rate map to use when rendering this pass, or nil to not use variable rasterization rate.
--
-- The default value is nil. Enabling variable rasterization rate allows for decreasing the rasterization rate in unimportant regions of screen space.
--
-- ObjC selector: @- rasterizationRateMap@
rasterizationRateMap :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO RawId
rasterizationRateMap mtlRenderPassDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlRenderPassDescriptor (mkSelector "rasterizationRateMap") (retPtr retVoid) []

-- | rasterizationRateMap
--
-- The variable rasterization rate map to use when rendering this pass, or nil to not use variable rasterization rate.
--
-- The default value is nil. Enabling variable rasterization rate allows for decreasing the rasterization rate in unimportant regions of screen space.
--
-- ObjC selector: @- setRasterizationRateMap:@
setRasterizationRateMap :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> RawId -> IO ()
setRasterizationRateMap mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setRasterizationRateMap:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | sampleBufferAttachments
--
-- An array of sample buffers and associated sample indices.
--
-- ObjC selector: @- sampleBufferAttachments@
sampleBufferAttachments :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO (Id MTLRenderPassSampleBufferAttachmentDescriptorArray)
sampleBufferAttachments mtlRenderPassDescriptor  =
    sendMsg mtlRenderPassDescriptor (mkSelector "sampleBufferAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- visibilityResultType@
visibilityResultType :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO MTLVisibilityResultType
visibilityResultType mtlRenderPassDescriptor  =
    fmap (coerce :: CLong -> MTLVisibilityResultType) $ sendMsg mtlRenderPassDescriptor (mkSelector "visibilityResultType") retCLong []

-- | Specifies if Metal accumulates visibility results between render encoders or resets them.
--
-- ObjC selector: @- setVisibilityResultType:@
setVisibilityResultType :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> MTLVisibilityResultType -> IO ()
setVisibilityResultType mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setVisibilityResultType:") retVoid [argCLong (coerce value)]

-- | Specifies if the render pass should support color attachment mapping.
--
-- ObjC selector: @- supportColorAttachmentMapping@
supportColorAttachmentMapping :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> IO Bool
supportColorAttachmentMapping mtlRenderPassDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlRenderPassDescriptor (mkSelector "supportColorAttachmentMapping") retCULong []

-- | Specifies if the render pass should support color attachment mapping.
--
-- ObjC selector: @- setSupportColorAttachmentMapping:@
setSupportColorAttachmentMapping :: IsMTLRenderPassDescriptor mtlRenderPassDescriptor => mtlRenderPassDescriptor -> Bool -> IO ()
setSupportColorAttachmentMapping mtlRenderPassDescriptor  value =
    sendMsg mtlRenderPassDescriptor (mkSelector "setSupportColorAttachmentMapping:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @renderPassDescriptor@
renderPassDescriptorSelector :: Selector
renderPassDescriptorSelector = mkSelector "renderPassDescriptor"

-- | @Selector@ for @setSamplePositions:count:@
setSamplePositions_countSelector :: Selector
setSamplePositions_countSelector = mkSelector "setSamplePositions:count:"

-- | @Selector@ for @getSamplePositions:count:@
getSamplePositions_countSelector :: Selector
getSamplePositions_countSelector = mkSelector "getSamplePositions:count:"

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

-- | @Selector@ for @visibilityResultBuffer@
visibilityResultBufferSelector :: Selector
visibilityResultBufferSelector = mkSelector "visibilityResultBuffer"

-- | @Selector@ for @setVisibilityResultBuffer:@
setVisibilityResultBufferSelector :: Selector
setVisibilityResultBufferSelector = mkSelector "setVisibilityResultBuffer:"

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

-- | @Selector@ for @rasterizationRateMap@
rasterizationRateMapSelector :: Selector
rasterizationRateMapSelector = mkSelector "rasterizationRateMap"

-- | @Selector@ for @setRasterizationRateMap:@
setRasterizationRateMapSelector :: Selector
setRasterizationRateMapSelector = mkSelector "setRasterizationRateMap:"

-- | @Selector@ for @sampleBufferAttachments@
sampleBufferAttachmentsSelector :: Selector
sampleBufferAttachmentsSelector = mkSelector "sampleBufferAttachments"

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

