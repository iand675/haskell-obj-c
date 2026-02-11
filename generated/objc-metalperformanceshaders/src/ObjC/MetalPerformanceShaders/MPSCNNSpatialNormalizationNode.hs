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
  , nodeWithSource_kernelSizeSelector
  , initWithSource_kernelSizeSelector
  , initWithSourceSelector
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
nodeWithSource_kernelSize :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> IO (Id MPSCNNSpatialNormalizationNode)
nodeWithSource_kernelSize sourceNode kernelSize =
  do
    cls' <- getRequiredClass "MPSCNNSpatialNormalizationNode"
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= retainedObject . castPtr

-- | @- initWithSource:kernelSize:@
initWithSource_kernelSize :: (IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnSpatialNormalizationNode -> sourceNode -> CULong -> IO (Id MPSCNNSpatialNormalizationNode)
initWithSource_kernelSize mpscnnSpatialNormalizationNode  sourceNode kernelSize =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnSpatialNormalizationNode (mkSelector "initWithSource:kernelSize:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= ownedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode, IsMPSNNImageNode sourceNode) => mpscnnSpatialNormalizationNode -> sourceNode -> IO (Id MPSCNNSpatialNormalizationNode)
initWithSource mpscnnSpatialNormalizationNode  sourceNode =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnSpatialNormalizationNode (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ())] >>= ownedObject . castPtr

-- | @- kernelWidth@
kernelWidth :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> IO CULong
kernelWidth mpscnnSpatialNormalizationNode  =
  sendMsg mpscnnSpatialNormalizationNode (mkSelector "kernelWidth") retCULong []

-- | @- setKernelWidth:@
setKernelWidth :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> CULong -> IO ()
setKernelWidth mpscnnSpatialNormalizationNode  value =
  sendMsg mpscnnSpatialNormalizationNode (mkSelector "setKernelWidth:") retVoid [argCULong (fromIntegral value)]

-- | @- kernelHeight@
kernelHeight :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> IO CULong
kernelHeight mpscnnSpatialNormalizationNode  =
  sendMsg mpscnnSpatialNormalizationNode (mkSelector "kernelHeight") retCULong []

-- | @- setKernelHeight:@
setKernelHeight :: IsMPSCNNSpatialNormalizationNode mpscnnSpatialNormalizationNode => mpscnnSpatialNormalizationNode -> CULong -> IO ()
setKernelHeight mpscnnSpatialNormalizationNode  value =
  sendMsg mpscnnSpatialNormalizationNode (mkSelector "setKernelHeight:") retVoid [argCULong (fromIntegral value)]

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

