{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node for a MPSCNNPooling kernel
--
-- This is an abstract base class that does not correspond with any              particular MPSCNNKernel. Please make one of the MPSCNNPooling              subclasses instead.
--
-- Generated bindings for @MPSCNNPoolingNode@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingNode
  ( MPSCNNPoolingNode
  , IsMPSCNNPoolingNode(..)
  , nodeWithSource_filterSize
  , nodeWithSource_filterSize_stride
  , initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithSource_filterSize_stride
  , initWithSource_filterSize
  , kernelWidth
  , kernelHeight
  , strideInPixelsX
  , strideInPixelsY
  , initWithSource_filterSizeSelector
  , initWithSource_filterSize_strideSelector
  , initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , nodeWithSource_filterSizeSelector
  , nodeWithSource_filterSize_strideSelector
  , strideInPixelsXSelector
  , strideInPixelsYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Convenience initializer for MPSCNNPooling nodes with square non-overlapping kernels
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = strideInPixelsX = strideInPixelsY = size
--
-- Returns: A new MPSNNFilter node for a MPSCNNPooling kernel.
--
-- ObjC selector: @+ nodeWithSource:filterSize:@
nodeWithSource_filterSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNPoolingNode)
nodeWithSource_filterSize sourceNode size =
  do
    cls' <- getRequiredClass "MPSCNNPoolingNode"
    sendClassMessage cls' nodeWithSource_filterSizeSelector (toMPSNNImageNode sourceNode) size

-- | Convenience initializer for MPSCNNPooling nodes with square non-overlapping kernels and a different stride
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = size
--
-- @stride@ — strideInPixelsX = strideInPixelsY = stride
--
-- Returns: A new MPSNNFilter node for a MPSCNNPooling kernel.
--
-- ObjC selector: @+ nodeWithSource:filterSize:stride:@
nodeWithSource_filterSize_stride :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> CULong -> IO (Id MPSCNNPoolingNode)
nodeWithSource_filterSize_stride sourceNode size stride =
  do
    cls' <- getRequiredClass "MPSCNNPoolingNode"
    sendClassMessage cls' nodeWithSource_filterSize_strideSelector (toMPSNNImageNode sourceNode) size stride

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
-- Returns: A new MPSNNFilter node for a MPSCNNPooling kernel.
--
-- ObjC selector: @- initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: (IsMPSCNNPoolingNode mpscnnPoolingNode, IsMPSNNImageNode sourceNode) => mpscnnPoolingNode -> sourceNode -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingNode)
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingNode sourceNode kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnPoolingNode initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector (toMPSNNImageNode sourceNode) kernelWidth kernelHeight strideInPixelsX strideInPixelsY

-- | Convenience initializer for MPSCNNPooling nodes with square kernels
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = size
--
-- @stride@ — strideInPixelsX = strideInPixelsY = stride
--
-- Returns: A new MPSNNFilter node for a MPSCNNPooling kernel.
--
-- ObjC selector: @- initWithSource:filterSize:stride:@
initWithSource_filterSize_stride :: (IsMPSCNNPoolingNode mpscnnPoolingNode, IsMPSNNImageNode sourceNode) => mpscnnPoolingNode -> sourceNode -> CULong -> CULong -> IO (Id MPSCNNPoolingNode)
initWithSource_filterSize_stride mpscnnPoolingNode sourceNode size stride =
  sendOwnedMessage mpscnnPoolingNode initWithSource_filterSize_strideSelector (toMPSNNImageNode sourceNode) size stride

-- | Convenience initializer for MPSCNNPooling nodes with square non-overlapping kernels
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @size@ — kernelWidth = kernelHeight = strideInPixelsX = strideInPixelsY = size
--
-- Returns: A new MPSNNFilter node for a MPSCNNPooling kernel.
--
-- ObjC selector: @- initWithSource:filterSize:@
initWithSource_filterSize :: (IsMPSCNNPoolingNode mpscnnPoolingNode, IsMPSNNImageNode sourceNode) => mpscnnPoolingNode -> sourceNode -> CULong -> IO (Id MPSCNNPoolingNode)
initWithSource_filterSize mpscnnPoolingNode sourceNode size =
  sendOwnedMessage mpscnnPoolingNode initWithSource_filterSizeSelector (toMPSNNImageNode sourceNode) size

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
kernelWidth mpscnnPoolingNode =
  sendMessage mpscnnPoolingNode kernelWidthSelector

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
kernelHeight mpscnnPoolingNode =
  sendMessage mpscnnPoolingNode kernelHeightSelector

-- | @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
strideInPixelsX mpscnnPoolingNode =
  sendMessage mpscnnPoolingNode strideInPixelsXSelector

-- | @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
strideInPixelsY mpscnnPoolingNode =
  sendMessage mpscnnPoolingNode strideInPixelsYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:filterSize:@
nodeWithSource_filterSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNPoolingNode)
nodeWithSource_filterSizeSelector = mkSelector "nodeWithSource:filterSize:"

-- | @Selector@ for @nodeWithSource:filterSize:stride:@
nodeWithSource_filterSize_strideSelector :: Selector '[Id MPSNNImageNode, CULong, CULong] (Id MPSCNNPoolingNode)
nodeWithSource_filterSize_strideSelector = mkSelector "nodeWithSource:filterSize:stride:"

-- | @Selector@ for @initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, CULong, CULong] (Id MPSCNNPoolingNode)
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithSource:filterSize:stride:@
initWithSource_filterSize_strideSelector :: Selector '[Id MPSNNImageNode, CULong, CULong] (Id MPSCNNPoolingNode)
initWithSource_filterSize_strideSelector = mkSelector "initWithSource:filterSize:stride:"

-- | @Selector@ for @initWithSource:filterSize:@
initWithSource_filterSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNPoolingNode)
initWithSource_filterSizeSelector = mkSelector "initWithSource:filterSize:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector '[] CULong
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector '[] CULong
strideInPixelsYSelector = mkSelector "strideInPixelsY"

