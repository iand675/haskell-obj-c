{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties you use to create a tile render pipeline state object.
--
-- Generated bindings for @MTL4TileRenderPipelineDescriptor@.
module ObjC.Metal.MTL4TileRenderPipelineDescriptor
  ( MTL4TileRenderPipelineDescriptor
  , IsMTL4TileRenderPipelineDescriptor(..)
  , reset
  , tileFunctionDescriptor
  , setTileFunctionDescriptor
  , rasterSampleCount
  , setRasterSampleCount
  , colorAttachments
  , threadgroupSizeMatchesTileSize
  , setThreadgroupSizeMatchesTileSize
  , maxTotalThreadsPerThreadgroup
  , setMaxTotalThreadsPerThreadgroup
  , staticLinkingDescriptor
  , setStaticLinkingDescriptor
  , supportBinaryLinking
  , setSupportBinaryLinking
  , resetSelector
  , tileFunctionDescriptorSelector
  , setTileFunctionDescriptorSelector
  , rasterSampleCountSelector
  , setRasterSampleCountSelector
  , colorAttachmentsSelector
  , threadgroupSizeMatchesTileSizeSelector
  , setThreadgroupSizeMatchesTileSizeSelector
  , maxTotalThreadsPerThreadgroupSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , staticLinkingDescriptorSelector
  , setStaticLinkingDescriptorSelector
  , supportBinaryLinkingSelector
  , setSupportBinaryLinkingSelector


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
import ObjC.Foundation.Internal.Classes

-- | Resets the descriptor to the default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO ()
reset mtL4TileRenderPipelineDescriptor  =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "reset") retVoid []

-- | Configures the tile function that the render pipeline executes for each tile in the tile shader stage.
--
-- ObjC selector: @- tileFunctionDescriptor@
tileFunctionDescriptor :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
tileFunctionDescriptor mtL4TileRenderPipelineDescriptor  =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "tileFunctionDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures the tile function that the render pipeline executes for each tile in the tile shader stage.
--
-- ObjC selector: @- setTileFunctionDescriptor:@
setTileFunctionDescriptor :: (IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4TileRenderPipelineDescriptor -> value -> IO ()
setTileFunctionDescriptor mtL4TileRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "setTileFunctionDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configures the number of samples per pixel used for multisampling.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtL4TileRenderPipelineDescriptor  =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "rasterSampleCount") retCULong []

-- | Configures the number of samples per pixel used for multisampling.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtL4TileRenderPipelineDescriptor  value =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "setRasterSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | Access an array of descriptors that configure the properties of each color attachment in the tile render pipeline.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO (Id MTLTileRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtL4TileRenderPipelineDescriptor  =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "colorAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicating whether the size of the threadgroup matches the size of a tile in the render pipeline.
--
-- ObjC selector: @- threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSize :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO Bool
threadgroupSizeMatchesTileSize mtL4TileRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "threadgroupSizeMatchesTileSize") retCULong []

-- | Indicating whether the size of the threadgroup matches the size of a tile in the render pipeline.
--
-- ObjC selector: @- setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSize :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> Bool -> IO ()
setThreadgroupSizeMatchesTileSize mtL4TileRenderPipelineDescriptor  value =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "setThreadgroupSizeMatchesTileSize:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets the maximum number of threads that the GPU can execute simultaneously within a single threadgroup in the tile render pipeline.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtL4TileRenderPipelineDescriptor  =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "maxTotalThreadsPerThreadgroup") retCULong []

-- | Sets the maximum number of threads that the GPU can execute simultaneously within a single threadgroup in the tile render pipeline.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtL4TileRenderPipelineDescriptor  value =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "setMaxTotalThreadsPerThreadgroup:") retVoid [argCULong (fromIntegral value)]

-- | Configures an object that contains information about functions to link to the tile render pipeline when Metal builds it.
--
-- ObjC selector: @- staticLinkingDescriptor@
staticLinkingDescriptor :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
staticLinkingDescriptor mtL4TileRenderPipelineDescriptor  =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "staticLinkingDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Configures an object that contains information about functions to link to the tile render pipeline when Metal builds it.
--
-- ObjC selector: @- setStaticLinkingDescriptor:@
setStaticLinkingDescriptor :: (IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4TileRenderPipelineDescriptor -> value -> IO ()
setStaticLinkingDescriptor mtL4TileRenderPipelineDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "setStaticLinkingDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether the pipeline supports linking binary functions.
--
-- ObjC selector: @- supportBinaryLinking@
supportBinaryLinking :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO Bool
supportBinaryLinking mtL4TileRenderPipelineDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "supportBinaryLinking") retCULong []

-- | Indicates whether the pipeline supports linking binary functions.
--
-- ObjC selector: @- setSupportBinaryLinking:@
setSupportBinaryLinking :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> Bool -> IO ()
setSupportBinaryLinking mtL4TileRenderPipelineDescriptor  value =
  sendMsg mtL4TileRenderPipelineDescriptor (mkSelector "setSupportBinaryLinking:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @tileFunctionDescriptor@
tileFunctionDescriptorSelector :: Selector
tileFunctionDescriptorSelector = mkSelector "tileFunctionDescriptor"

-- | @Selector@ for @setTileFunctionDescriptor:@
setTileFunctionDescriptorSelector :: Selector
setTileFunctionDescriptorSelector = mkSelector "setTileFunctionDescriptor:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSizeSelector :: Selector
threadgroupSizeMatchesTileSizeSelector = mkSelector "threadgroupSizeMatchesTileSize"

-- | @Selector@ for @setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSizeSelector :: Selector
setThreadgroupSizeMatchesTileSizeSelector = mkSelector "setThreadgroupSizeMatchesTileSize:"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @staticLinkingDescriptor@
staticLinkingDescriptorSelector :: Selector
staticLinkingDescriptorSelector = mkSelector "staticLinkingDescriptor"

-- | @Selector@ for @setStaticLinkingDescriptor:@
setStaticLinkingDescriptorSelector :: Selector
setStaticLinkingDescriptorSelector = mkSelector "setStaticLinkingDescriptor:"

-- | @Selector@ for @supportBinaryLinking@
supportBinaryLinkingSelector :: Selector
supportBinaryLinkingSelector = mkSelector "supportBinaryLinking"

-- | @Selector@ for @setSupportBinaryLinking:@
setSupportBinaryLinkingSelector :: Selector
setSupportBinaryLinkingSelector = mkSelector "setSupportBinaryLinking:"

