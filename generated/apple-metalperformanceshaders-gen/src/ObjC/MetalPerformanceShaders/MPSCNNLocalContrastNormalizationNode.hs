{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing MPSCNNLocalContrastNormalization
--
-- The result is computed for each element of X as follows:
--
-- Y(i,j) = pm + ps * ( X(i,j) - p0 * M(i,j)) / pow((delta + alpha * variance(i,j)), beta),
--
-- where kw and kh are the kernelWidth and the kernelHeight and pm, ps and p0 are parameters that              can be used to offset and scale the result in various ways. *
--
-- Defaults:
-- alpha = 1.0f
-- beta  = 0.5f
-- delta = 2^-10
-- pm = 0
-- ps = 1
-- p0 = 1
-- kernelHeight = kernelWidth = kernelSize
--
-- Generated bindings for @MPSCNNLocalContrastNormalizationNode@.
module ObjC.MetalPerformanceShaders.MPSCNNLocalContrastNormalizationNode
  ( MPSCNNLocalContrastNormalizationNode
  , IsMPSCNNLocalContrastNormalizationNode(..)
  , nodeWithSource_kernelSize
  , initWithSource_kernelSize
  , initWithSource
  , pm
  , setPm
  , ps
  , setPs
  , p0
  , setP0
  , kernelWidth
  , setKernelWidth
  , kernelHeight
  , setKernelHeight
  , initWithSourceSelector
  , initWithSource_kernelSizeSelector
  , kernelHeightSelector
  , kernelWidthSelector
  , nodeWithSource_kernelSizeSelector
  , p0Selector
  , pmSelector
  , psSelector
  , setKernelHeightSelector
  , setKernelWidthSelector
  , setP0Selector
  , setPmSelector
  , setPsSelector


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
nodeWithSource_kernelSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNLocalContrastNormalizationNode)
nodeWithSource_kernelSize sourceNode kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNLocalContrastNormalizationNode"
    sendClassMessage cls' nodeWithSource_kernelSizeSelector (toMPSNNImageNode sourceNode) kernelSize

-- | @- initWithSource:kernelSize:@
initWithSource_kernelSize :: (IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnLocalContrastNormalizationNode -> sourceNode -> CULong -> IO (Id MPSCNNLocalContrastNormalizationNode)
initWithSource_kernelSize mpscnnLocalContrastNormalizationNode sourceNode kernelSize =
  sendOwnedMessage mpscnnLocalContrastNormalizationNode initWithSource_kernelSizeSelector (toMPSNNImageNode sourceNode) kernelSize

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnLocalContrastNormalizationNode -> sourceNode -> IO (Id MPSCNNLocalContrastNormalizationNode)
initWithSource mpscnnLocalContrastNormalizationNode sourceNode =
  sendOwnedMessage mpscnnLocalContrastNormalizationNode initWithSourceSelector (toMPSNNImageNode sourceNode)

-- | @- pm@
pm :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CFloat
pm mpscnnLocalContrastNormalizationNode =
  sendMessage mpscnnLocalContrastNormalizationNode pmSelector

-- | @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalizationNode value =
  sendMessage mpscnnLocalContrastNormalizationNode setPmSelector value

-- | @- ps@
ps :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CFloat
ps mpscnnLocalContrastNormalizationNode =
  sendMessage mpscnnLocalContrastNormalizationNode psSelector

-- | @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalizationNode value =
  sendMessage mpscnnLocalContrastNormalizationNode setPsSelector value

-- | @- p0@
p0 :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CFloat
p0 mpscnnLocalContrastNormalizationNode =
  sendMessage mpscnnLocalContrastNormalizationNode p0Selector

-- | @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalizationNode value =
  sendMessage mpscnnLocalContrastNormalizationNode setP0Selector value

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CULong
kernelWidth mpscnnLocalContrastNormalizationNode =
  sendMessage mpscnnLocalContrastNormalizationNode kernelWidthSelector

-- | @- setKernelWidth:@
setKernelWidth :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CULong -> IO ()
setKernelWidth mpscnnLocalContrastNormalizationNode value =
  sendMessage mpscnnLocalContrastNormalizationNode setKernelWidthSelector value

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CULong
kernelHeight mpscnnLocalContrastNormalizationNode =
  sendMessage mpscnnLocalContrastNormalizationNode kernelHeightSelector

-- | @- setKernelHeight:@
setKernelHeight :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CULong -> IO ()
setKernelHeight mpscnnLocalContrastNormalizationNode value =
  sendMessage mpscnnLocalContrastNormalizationNode setKernelHeightSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:kernelSize:@
nodeWithSource_kernelSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNLocalContrastNormalizationNode)
nodeWithSource_kernelSizeSelector = mkSelector "nodeWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:kernelSize:@
initWithSource_kernelSizeSelector :: Selector '[Id MPSNNImageNode, CULong] (Id MPSCNNLocalContrastNormalizationNode)
initWithSource_kernelSizeSelector = mkSelector "initWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSCNNLocalContrastNormalizationNode)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @pm@
pmSelector :: Selector '[] CFloat
pmSelector = mkSelector "pm"

-- | @Selector@ for @setPm:@
setPmSelector :: Selector '[CFloat] ()
setPmSelector = mkSelector "setPm:"

-- | @Selector@ for @ps@
psSelector :: Selector '[] CFloat
psSelector = mkSelector "ps"

-- | @Selector@ for @setPs:@
setPsSelector :: Selector '[CFloat] ()
setPsSelector = mkSelector "setPs:"

-- | @Selector@ for @p0@
p0Selector :: Selector '[] CFloat
p0Selector = mkSelector "p0"

-- | @Selector@ for @setP0:@
setP0Selector :: Selector '[CFloat] ()
setP0Selector = mkSelector "setP0:"

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

