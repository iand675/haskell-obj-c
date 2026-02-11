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
  , nodeWithSource_kernelSizeSelector
  , initWithSource_kernelSizeSelector
  , initWithSourceSelector
  , pmSelector
  , setPmSelector
  , psSelector
  , setPsSelector
  , p0Selector
  , setP0Selector
  , kernelWidthSelector
  , setKernelWidthSelector
  , kernelHeightSelector
  , setKernelHeightSelector


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

-- | @+ nodeWithSource:kernelSize:@
nodeWithSource_kernelSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNLocalContrastNormalizationNode)
nodeWithSource_kernelSize sourceNode kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNLocalContrastNormalizationNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= retainedObject . castPtr

-- | @- initWithSource:kernelSize:@
initWithSource_kernelSize :: (IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnLocalContrastNormalizationNode -> sourceNode -> CULong -> IO (Id MPSCNNLocalContrastNormalizationNode)
initWithSource_kernelSize mpscnnLocalContrastNormalizationNode  sourceNode kernelSize =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "initWithSource:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= ownedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnLocalContrastNormalizationNode -> sourceNode -> IO (Id MPSCNNLocalContrastNormalizationNode)
initWithSource mpscnnLocalContrastNormalizationNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | @- pm@
pm :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CFloat
pm mpscnnLocalContrastNormalizationNode  =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "pm") retCFloat []

-- | @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalizationNode  value =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "setPm:") retVoid [argCFloat (fromIntegral value)]

-- | @- ps@
ps :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CFloat
ps mpscnnLocalContrastNormalizationNode  =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "ps") retCFloat []

-- | @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalizationNode  value =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "setPs:") retVoid [argCFloat (fromIntegral value)]

-- | @- p0@
p0 :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CFloat
p0 mpscnnLocalContrastNormalizationNode  =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "p0") retCFloat []

-- | @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalizationNode  value =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "setP0:") retVoid [argCFloat (fromIntegral value)]

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CULong
kernelWidth mpscnnLocalContrastNormalizationNode  =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "kernelWidth") retCULong []

-- | @- setKernelWidth:@
setKernelWidth :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CULong -> IO ()
setKernelWidth mpscnnLocalContrastNormalizationNode  value =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "setKernelWidth:") retVoid [argCULong (fromIntegral value)]

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> IO CULong
kernelHeight mpscnnLocalContrastNormalizationNode  =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "kernelHeight") retCULong []

-- | @- setKernelHeight:@
setKernelHeight :: IsMPSCNNLocalContrastNormalizationNode mpscnnLocalContrastNormalizationNode => mpscnnLocalContrastNormalizationNode -> CULong -> IO ()
setKernelHeight mpscnnLocalContrastNormalizationNode  value =
  sendMsg mpscnnLocalContrastNormalizationNode (mkSelector "setKernelHeight:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:kernelSize:@
nodeWithSource_kernelSizeSelector :: Selector
nodeWithSource_kernelSizeSelector = mkSelector "nodeWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:kernelSize:@
initWithSource_kernelSizeSelector :: Selector
initWithSource_kernelSizeSelector = mkSelector "initWithSource:kernelSize:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @pm@
pmSelector :: Selector
pmSelector = mkSelector "pm"

-- | @Selector@ for @setPm:@
setPmSelector :: Selector
setPmSelector = mkSelector "setPm:"

-- | @Selector@ for @ps@
psSelector :: Selector
psSelector = mkSelector "ps"

-- | @Selector@ for @setPs:@
setPsSelector :: Selector
setPsSelector = mkSelector "setPs:"

-- | @Selector@ for @p0@
p0Selector :: Selector
p0Selector = mkSelector "p0"

-- | @Selector@ for @setP0:@
setP0Selector :: Selector
setP0Selector = mkSelector "setP0:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @setKernelWidth:@
setKernelWidthSelector :: Selector
setKernelWidthSelector = mkSelector "setKernelWidth:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @setKernelHeight:@
setKernelHeightSelector :: Selector
setKernelHeightSelector = mkSelector "setKernelHeight:"

