{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node for a MPSCNNDilatedPooling kernel
--
-- This class corresponds to the MPSCNNDilatedPooling class.
--
-- Generated bindings for @MPSCNNDilatedPoolingMaxNode@.
module ObjC.MetalPerformanceShaders.MPSCNNDilatedPoolingMaxNode
  ( MPSCNNDilatedPoolingMaxNode
  , IsMPSCNNDilatedPoolingMaxNode(..)
  , nodeWithSource_filterSize
  , nodeWithSource_filterSize_stride_dilationRate
  , initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY
  , initWithSource_filterSize_stride_dilationRate
  , initWithSource_filterSize
  , dilationRateX
  , dilationRateY
  , nodeWithSource_filterSizeSelector
  , nodeWithSource_filterSize_stride_dilationRateSelector
  , initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector
  , initWithSource_filterSize_stride_dilationRateSelector
  , initWithSource_filterSizeSelector
  , dilationRateXSelector
  , dilationRateYSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Convenience initializer for MPSCNNDilatedPooling nodes with square non-overlapping kernels
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = strideInPixelsX = strideInPixelsY = dilationRateX = dilationRateY = size
--
-- Returns: A new MPSNNFilter node for a MPSCNNDilatedPooling kernel.
--
-- ObjC selector: @+ nodeWithSource:filterSize:@
nodeWithSource_filterSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNDilatedPoolingMaxNode)
nodeWithSource_filterSize sourceNode size =
  do
    cls' <- getRequiredClass "MPSCNNDilatedPoolingMaxNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:filterSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size)] >>= retainedObject . castPtr

-- | Convenience initializer for MPSCNNDilatedPooling nodes with square kernels and equal dilation factors
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = size
--
-- @stride@ — strideInPixelsX = strideInPixelsY = stride
--
-- @dilationRate@ — dilationRateX = dilationRateY = stride
--
-- Returns: A new MPSNNFilter node for a MPSCNNDilatedPooling kernel.
--
-- ObjC selector: @+ nodeWithSource:filterSize:stride:dilationRate:@
nodeWithSource_filterSize_stride_dilationRate :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxNode)
nodeWithSource_filterSize_stride_dilationRate sourceNode size stride dilationRate =
  do
    cls' <- getRequiredClass "MPSCNNDilatedPoolingMaxNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:filterSize:stride:dilationRate:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size), argCULong (fromIntegral stride), argCULong (fromIntegral dilationRate)] >>= retainedObject . castPtr

-- | Init a node representing a MPSCNNPooling kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @kernelWidth@ — The width of the max filter window
--
-- @kernelHeight@ — The height of the max filter window
--
-- @strideInPixelsX@ — The output stride (downsampling factor) in the x dimension.
--
-- @strideInPixelsY@ — The output stride (downsampling factor) in the y dimension.
--
-- @dilationRateX@ — The dilation factor in the x dimension.
--
-- @dilationRateY@ — The dilation factor in the y dimension.
--
-- Returns: A new MPSNNFilter node for a MPSCNNPooling kernel.
--
-- ObjC selector: @- initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY :: (IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode, IsMPSNNImageNode sourceNode) => mpscnnDilatedPoolingMaxNode -> sourceNode -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxNode)
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY mpscnnDilatedPoolingMaxNode  sourceNode kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnDilatedPoolingMaxNode (mkSelector "initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY), argCULong (fromIntegral dilationRateX), argCULong (fromIntegral dilationRateY)] >>= ownedObject . castPtr

-- | Convenience initializer for MPSCNNDilatedPooling nodes with square kernels and equal dilation factors
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = size
--
-- @stride@ — strideInPixelsX = strideInPixelsY = stride
--
-- @dilationRate@ — dilationRateX = dilationRateY = stride
--
-- Returns: A new MPSNNFilter node for a MPSCNNDilatedPooling kernel.
--
-- ObjC selector: @- initWithSource:filterSize:stride:dilationRate:@
initWithSource_filterSize_stride_dilationRate :: (IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode, IsMPSNNImageNode sourceNode) => mpscnnDilatedPoolingMaxNode -> sourceNode -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxNode)
initWithSource_filterSize_stride_dilationRate mpscnnDilatedPoolingMaxNode  sourceNode size stride dilationRate =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnDilatedPoolingMaxNode (mkSelector "initWithSource:filterSize:stride:dilationRate:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size), argCULong (fromIntegral stride), argCULong (fromIntegral dilationRate)] >>= ownedObject . castPtr

-- | Convenience initializer for MPSCNNDilatedPooling nodes with square non-overlapping kernels
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = strideInPixelsX = strideInPixelsY = dilationRateX = dilationRateY = size
--
-- Returns: A new MPSNNFilter node for a MPSCNNDilatedPooling kernel.
--
-- ObjC selector: @- initWithSource:filterSize:@
initWithSource_filterSize :: (IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode, IsMPSNNImageNode sourceNode) => mpscnnDilatedPoolingMaxNode -> sourceNode -> CULong -> IO (Id MPSCNNDilatedPoolingMaxNode)
initWithSource_filterSize mpscnnDilatedPoolingMaxNode  sourceNode size =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnDilatedPoolingMaxNode (mkSelector "initWithSource:filterSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size)] >>= ownedObject . castPtr

-- | @- dilationRateX@
dilationRateX :: IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode => mpscnnDilatedPoolingMaxNode -> IO CULong
dilationRateX mpscnnDilatedPoolingMaxNode  =
  sendMsg mpscnnDilatedPoolingMaxNode (mkSelector "dilationRateX") retCULong []

-- | @- dilationRateY@
dilationRateY :: IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode => mpscnnDilatedPoolingMaxNode -> IO CULong
dilationRateY mpscnnDilatedPoolingMaxNode  =
  sendMsg mpscnnDilatedPoolingMaxNode (mkSelector "dilationRateY") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:filterSize:@
nodeWithSource_filterSizeSelector :: Selector
nodeWithSource_filterSizeSelector = mkSelector "nodeWithSource:filterSize:"

-- | @Selector@ for @nodeWithSource:filterSize:stride:dilationRate:@
nodeWithSource_filterSize_stride_dilationRateSelector :: Selector
nodeWithSource_filterSize_stride_dilationRateSelector = mkSelector "nodeWithSource:filterSize:stride:dilationRate:"

-- | @Selector@ for @initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector :: Selector
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector = mkSelector "initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:"

-- | @Selector@ for @initWithSource:filterSize:stride:dilationRate:@
initWithSource_filterSize_stride_dilationRateSelector :: Selector
initWithSource_filterSize_stride_dilationRateSelector = mkSelector "initWithSource:filterSize:stride:dilationRate:"

-- | @Selector@ for @initWithSource:filterSize:@
initWithSource_filterSizeSelector :: Selector
initWithSource_filterSizeSelector = mkSelector "initWithSource:filterSize:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector
dilationRateYSelector = mkSelector "dilationRateY"

