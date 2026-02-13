{-# LANGUAGE DataKinds #-}
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
  , colorAttachmentsSelector
  , maxTotalThreadsPerThreadgroupSelector
  , rasterSampleCountSelector
  , resetSelector
  , setMaxTotalThreadsPerThreadgroupSelector
  , setRasterSampleCountSelector
  , setStaticLinkingDescriptorSelector
  , setSupportBinaryLinkingSelector
  , setThreadgroupSizeMatchesTileSizeSelector
  , setTileFunctionDescriptorSelector
  , staticLinkingDescriptorSelector
  , supportBinaryLinkingSelector
  , threadgroupSizeMatchesTileSizeSelector
  , tileFunctionDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Resets the descriptor to the default state.
--
-- ObjC selector: @- reset@
reset :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO ()
reset mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor resetSelector

-- | Configures the tile function that the render pipeline executes for each tile in the tile shader stage.
--
-- ObjC selector: @- tileFunctionDescriptor@
tileFunctionDescriptor :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
tileFunctionDescriptor mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor tileFunctionDescriptorSelector

-- | Configures the tile function that the render pipeline executes for each tile in the tile shader stage.
--
-- ObjC selector: @- setTileFunctionDescriptor:@
setTileFunctionDescriptor :: (IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4TileRenderPipelineDescriptor -> value -> IO ()
setTileFunctionDescriptor mtL4TileRenderPipelineDescriptor value =
  sendMessage mtL4TileRenderPipelineDescriptor setTileFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- | Configures the number of samples per pixel used for multisampling.
--
-- ObjC selector: @- rasterSampleCount@
rasterSampleCount :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO CULong
rasterSampleCount mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor rasterSampleCountSelector

-- | Configures the number of samples per pixel used for multisampling.
--
-- ObjC selector: @- setRasterSampleCount:@
setRasterSampleCount :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> CULong -> IO ()
setRasterSampleCount mtL4TileRenderPipelineDescriptor value =
  sendMessage mtL4TileRenderPipelineDescriptor setRasterSampleCountSelector value

-- | Access an array of descriptors that configure the properties of each color attachment in the tile render pipeline.
--
-- ObjC selector: @- colorAttachments@
colorAttachments :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO (Id MTLTileRenderPipelineColorAttachmentDescriptorArray)
colorAttachments mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor colorAttachmentsSelector

-- | Indicating whether the size of the threadgroup matches the size of a tile in the render pipeline.
--
-- ObjC selector: @- threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSize :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO Bool
threadgroupSizeMatchesTileSize mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor threadgroupSizeMatchesTileSizeSelector

-- | Indicating whether the size of the threadgroup matches the size of a tile in the render pipeline.
--
-- ObjC selector: @- setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSize :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> Bool -> IO ()
setThreadgroupSizeMatchesTileSize mtL4TileRenderPipelineDescriptor value =
  sendMessage mtL4TileRenderPipelineDescriptor setThreadgroupSizeMatchesTileSizeSelector value

-- | Sets the maximum number of threads that the GPU can execute simultaneously within a single threadgroup in the tile render pipeline.
--
-- ObjC selector: @- maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroup :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO CULong
maxTotalThreadsPerThreadgroup mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor maxTotalThreadsPerThreadgroupSelector

-- | Sets the maximum number of threads that the GPU can execute simultaneously within a single threadgroup in the tile render pipeline.
--
-- ObjC selector: @- setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroup :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> CULong -> IO ()
setMaxTotalThreadsPerThreadgroup mtL4TileRenderPipelineDescriptor value =
  sendMessage mtL4TileRenderPipelineDescriptor setMaxTotalThreadsPerThreadgroupSelector value

-- | Configures an object that contains information about functions to link to the tile render pipeline when Metal builds it.
--
-- ObjC selector: @- staticLinkingDescriptor@
staticLinkingDescriptor :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO (Id MTL4StaticLinkingDescriptor)
staticLinkingDescriptor mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor staticLinkingDescriptorSelector

-- | Configures an object that contains information about functions to link to the tile render pipeline when Metal builds it.
--
-- ObjC selector: @- setStaticLinkingDescriptor:@
setStaticLinkingDescriptor :: (IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor, IsMTL4StaticLinkingDescriptor value) => mtL4TileRenderPipelineDescriptor -> value -> IO ()
setStaticLinkingDescriptor mtL4TileRenderPipelineDescriptor value =
  sendMessage mtL4TileRenderPipelineDescriptor setStaticLinkingDescriptorSelector (toMTL4StaticLinkingDescriptor value)

-- | Indicates whether the pipeline supports linking binary functions.
--
-- ObjC selector: @- supportBinaryLinking@
supportBinaryLinking :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> IO Bool
supportBinaryLinking mtL4TileRenderPipelineDescriptor =
  sendMessage mtL4TileRenderPipelineDescriptor supportBinaryLinkingSelector

-- | Indicates whether the pipeline supports linking binary functions.
--
-- ObjC selector: @- setSupportBinaryLinking:@
setSupportBinaryLinking :: IsMTL4TileRenderPipelineDescriptor mtL4TileRenderPipelineDescriptor => mtL4TileRenderPipelineDescriptor -> Bool -> IO ()
setSupportBinaryLinking mtL4TileRenderPipelineDescriptor value =
  sendMessage mtL4TileRenderPipelineDescriptor setSupportBinaryLinkingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @tileFunctionDescriptor@
tileFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
tileFunctionDescriptorSelector = mkSelector "tileFunctionDescriptor"

-- | @Selector@ for @setTileFunctionDescriptor:@
setTileFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setTileFunctionDescriptorSelector = mkSelector "setTileFunctionDescriptor:"

-- | @Selector@ for @rasterSampleCount@
rasterSampleCountSelector :: Selector '[] CULong
rasterSampleCountSelector = mkSelector "rasterSampleCount"

-- | @Selector@ for @setRasterSampleCount:@
setRasterSampleCountSelector :: Selector '[CULong] ()
setRasterSampleCountSelector = mkSelector "setRasterSampleCount:"

-- | @Selector@ for @colorAttachments@
colorAttachmentsSelector :: Selector '[] (Id MTLTileRenderPipelineColorAttachmentDescriptorArray)
colorAttachmentsSelector = mkSelector "colorAttachments"

-- | @Selector@ for @threadgroupSizeMatchesTileSize@
threadgroupSizeMatchesTileSizeSelector :: Selector '[] Bool
threadgroupSizeMatchesTileSizeSelector = mkSelector "threadgroupSizeMatchesTileSize"

-- | @Selector@ for @setThreadgroupSizeMatchesTileSize:@
setThreadgroupSizeMatchesTileSizeSelector :: Selector '[Bool] ()
setThreadgroupSizeMatchesTileSizeSelector = mkSelector "setThreadgroupSizeMatchesTileSize:"

-- | @Selector@ for @maxTotalThreadsPerThreadgroup@
maxTotalThreadsPerThreadgroupSelector :: Selector '[] CULong
maxTotalThreadsPerThreadgroupSelector = mkSelector "maxTotalThreadsPerThreadgroup"

-- | @Selector@ for @setMaxTotalThreadsPerThreadgroup:@
setMaxTotalThreadsPerThreadgroupSelector :: Selector '[CULong] ()
setMaxTotalThreadsPerThreadgroupSelector = mkSelector "setMaxTotalThreadsPerThreadgroup:"

-- | @Selector@ for @staticLinkingDescriptor@
staticLinkingDescriptorSelector :: Selector '[] (Id MTL4StaticLinkingDescriptor)
staticLinkingDescriptorSelector = mkSelector "staticLinkingDescriptor"

-- | @Selector@ for @setStaticLinkingDescriptor:@
setStaticLinkingDescriptorSelector :: Selector '[Id MTL4StaticLinkingDescriptor] ()
setStaticLinkingDescriptorSelector = mkSelector "setStaticLinkingDescriptor:"

-- | @Selector@ for @supportBinaryLinking@
supportBinaryLinkingSelector :: Selector '[] Bool
supportBinaryLinkingSelector = mkSelector "supportBinaryLinking"

-- | @Selector@ for @setSupportBinaryLinking:@
setSupportBinaryLinkingSelector :: Selector '[Bool] ()
setSupportBinaryLinkingSelector = mkSelector "setSupportBinaryLinking:"

