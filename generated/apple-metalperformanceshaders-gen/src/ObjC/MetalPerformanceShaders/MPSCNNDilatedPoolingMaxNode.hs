{-# LANGUAGE DataKinds #-}
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
  , dilationRateXSelector
  , dilationRateYSelector
  , initWithSource_filterSizeSelector
  , initWithSource_filterSize_stride_dilationRateSelector
  , initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector
  , nodeWithSource_filterSizeSelector
  , nodeWithSource_filterSize_stride_dilationRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' nodeWithSource_filterSizeSelector (toMPSNNImageNode sourceNode) size

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
    sendClassMessage cls' nodeWithSource_filterSize_stride_dilationRateSelector (toMPSNNImageNode sourceNode) size stride dilationRate

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
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateY mpscnnDilatedPoolingMaxNode sourceNode kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY =
  sendOwnedMessage mpscnnDilatedPoolingMaxNode initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector (toMPSNNImageNode sourceNode) kernelWidth kernelHeight strideInPixelsX strideInPixelsY dilationRateX dilationRateY

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
initWithSource_filterSize_stride_dilationRate mpscnnDilatedPoolingMaxNode sourceNode size stride dilationRate =
  sendOwnedMessage mpscnnDilatedPoolingMaxNode initWithSource_filterSize_stride_dilationRateSelector (toMPSNNImageNode sourceNode) size stride dilationRate

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
initWithSource_filterSize mpscnnDilatedPoolingMaxNode sourceNode size =
  sendOwnedMessage mpscnnDilatedPoolingMaxNode initWithSource_filterSizeSelector (toMPSNNImageNode sourceNode) size

-- | @- dilationRateX@
dilationRateX :: IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode => mpscnnDilatedPoolingMaxNode -> IO CULong
dilationRateX mpscnnDilatedPoolingMaxNode =
  sendMessage mpscnnDilatedPoolingMaxNode dilationRateXSelector

-- | @- dilationRateY@
dilationRateY :: IsMPSCNNDilatedPoolingMaxNode mpscnnDilatedPoolingMaxNode => mpscnnDilatedPoolingMaxNode -> IO CULong
dilationRateY mpscnnDilatedPoolingMaxNode =
  sendMessage mpscnnDilatedPoolingMaxNode dilationRateYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:filterSize:@
nodeWithSource_filterSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNDilatedPoolingMaxNode)
nodeWithSource_filterSizeSelector = mkSelector "nodeWithSource:filterSize:"

-- | @Selector@ for @nodeWithSource:filterSize:stride:dilationRate:@
nodeWithSource_filterSize_stride_dilationRateSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxNode)
nodeWithSource_filterSize_stride_dilationRateSelector = mkSelector "nodeWithSource:filterSize:stride:dilationRate:"

-- | @Selector@ for @initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:@
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, CULong, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxNode)
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY_dilationRateX_dilationRateYSelector = mkSelector "initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:dilationRateX:dilationRateY:"

-- | @Selector@ for @initWithSource:filterSize:stride:dilationRate:@
initWithSource_filterSize_stride_dilationRateSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, CULong] (Id MPSCNNDilatedPoolingMaxNode)
initWithSource_filterSize_stride_dilationRateSelector = mkSelector "initWithSource:filterSize:stride:dilationRate:"

-- | @Selector@ for @initWithSource:filterSize:@
initWithSource_filterSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNDilatedPoolingMaxNode)
initWithSource_filterSizeSelector = mkSelector "initWithSource:filterSize:"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector '[] CULong
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector '[] CULong
dilationRateYSelector = mkSelector "dilationRateY"

