{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing MPSCNNSpatialNormalization
--
-- For each feature channel, the function computes the sum of squares of X inside each rectangle, N2(i,j).               It then divides each element of X as follows:                  Y(i,j) = X(i,j) / (delta + alpha/(kw*kh) * N2(i,j))^beta,               where kw and kh are the kernelWidth and the kernelHeight.
--
-- Defaults:
-- alpha = 1.0f
-- beta  = 5.0f
-- delta = 1.0f
-- kernelHeight = kernelWidth = kernelSize
--
-- Generated bindings for @MPSCNNSpatialNormalizationNode@.
module ObjC.MetalPerformanceShaders.MPSCNNSpatialNormalizationNode
  ( MPSCNNSpatialNormalizationNode
  , IsMPSCNNSpatialNormalizationNode(..)
  , nodeWithSource_kernelSize
  , initWithSource_kernelSize
  , initWithSource
  , kernelWidth
  , setKernelWidth
  , kernelHeight
  , setKernelHeight
  , initWithSourceSelector
  , initWithSource_kernelSizeSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , nodeWithSource_kernelSizeSelector
  , setKernelHeightSelector
  , setKernelWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSource:kernelSize:@
nodeWithSource_kernelSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNSpatialNormalizationNode)
nodeWithSource_kernelSize sourceNode kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNSpatialNormalizationNode"
    sendClassMessage cls' nodeWithSource_kernelSizeSelector (toMPSNNImageNode sourceNode) kernelSize

-- | @- initWithSource:kernelSize:@
initWithSource_kernelSize :: (IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnSpatialNormalizationNode -> sourceNode -> CULong -> IO (Id MPSCNNSpatialNormalizationNode)
initWithSource_kernelSize mpscnnSpatialNormalizationNode sourceNode kernelSize =
  sendOwnedMessage mpscnnSpatialNormalizationNode initWithSource_kernelSizeSelector (toMPSNNImageNode sourceNode) kernelSize

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnSpatialNormalizationNode -> sourceNode -> IO (Id MPSCNNSpatialNormalizationNode)
initWithSource mpscnnSpatialNormalizationNode sourceNode =
  sendOwnedMessage mpscnnSpatialNormalizationNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> IO CULong
kernelWidth mpscnnSpatialNormalizationNode =
  sendMessage mpscnnSpatialNormalizationNode kernelWidthSelector

-- | @- setKernelWidth:@
setKernelWidth :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> CULong -> IO ()
setKernelWidth mpscnnSpatialNormalizationNode value =
  sendMessage mpscnnSpatialNormalizationNode setKernelWidthSelector value

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> IO CULong
kernelHeight mpscnnSpatialNormalizationNode =
  sendMessage mpscnnSpatialNormalizationNode kernelHeightSelector

-- | @- setKernelHeight:@
setKernelHeight :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> CULong -> IO ()
setKernelHeight mpscnnSpatialNormalizationNode value =
  sendMessage mpscnnSpatialNormalizationNode setKernelHeightSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:kernelSize:@
nodeWithSource_kernelSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNSpatialNormalizationNode)
nodeWithSource_kernelSizeSelector = mkSelector "nodeWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:kernelSize:@
initWithSource_kernelSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNSpatialNormalizationNode)
initWithSource_kernelSizeSelector = mkSelector "initWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNSpatialNormalizationNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector '[CULong] ()
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector '[CULong] ()
setKernelHeightSelector = mkSelector "setKernelHeight:"

