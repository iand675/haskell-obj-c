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
  , nodeWithSource_filterSizeSelector
  , nodeWithSource_filterSize_strideSelector
  , initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
  , initWithSource_filterSize_strideSelector
  , initWithSource_filterSizeSelector
  , kernelWidthSelector
  , kernelHeightSelector
  , strideInPixelsXSelector
  , strideInPixelsYSelector


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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:filterSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size)] >>= retainedObject . castPtr

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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:filterSize:stride:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size), argCULong (fromIntegral stride)] >>= retainedObject . castPtr

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
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingNode  sourceNode kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnPoolingNode (mkSelector "initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

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
initWithSource_filterSize_stride mpscnnPoolingNode  sourceNode size stride =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnPoolingNode (mkSelector "initWithSource:filterSize:stride:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size), argCULong (fromIntegral stride)] >>= ownedObject . castPtr

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
initWithSource_filterSize mpscnnPoolingNode  sourceNode size =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnPoolingNode (mkSelector "initWithSource:filterSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral size)] >>= ownedObject . castPtr

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
kernelWidth mpscnnPoolingNode  =
  sendMsg mpscnnPoolingNode (mkSelector "kernelWidth") retCULong []

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
kernelHeight mpscnnPoolingNode  =
  sendMsg mpscnnPoolingNode (mkSelector "kernelHeight") retCULong []

-- | @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
strideInPixelsX mpscnnPoolingNode  =
  sendMsg mpscnnPoolingNode (mkSelector "strideInPixelsX") retCULong []

-- | @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNPoolingNode mpscnnPoolingNode => mpscnnPoolingNode -> IO CULong
strideInPixelsY mpscnnPoolingNode  =
  sendMsg mpscnnPoolingNode (mkSelector "strideInPixelsY") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:filterSize:@
nodeWithSource_filterSizeSelector :: Selector
nodeWithSource_filterSizeSelector = mkSelector "nodeWithSource:filterSize:"

-- | @Selector@ for @nodeWithSource:filterSize:stride:@
nodeWithSource_filterSize_strideSelector :: Selector
nodeWithSource_filterSize_strideSelector = mkSelector "nodeWithSource:filterSize:stride:"

-- | @Selector@ for @initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithSource_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithSource:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithSource:filterSize:stride:@
initWithSource_filterSize_strideSelector :: Selector
initWithSource_filterSize_strideSelector = mkSelector "initWithSource:filterSize:stride:"

-- | @Selector@ for @initWithSource:filterSize:@
initWithSource_filterSizeSelector :: Selector
initWithSource_filterSizeSelector = mkSelector "initWithSource:filterSize:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector
strideInPixelsYSelector = mkSelector "strideInPixelsY"

