{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSCNNUpsamplingNearest kernel
--
-- Generated bindings for @MPSCNNUpsamplingNearestNode@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingNearestNode
  ( MPSCNNUpsamplingNearestNode
  , IsMPSCNNUpsamplingNearestNode(..)
  , nodeWithSource_integerScaleFactorX_integerScaleFactorY
  , initWithSource_integerScaleFactorX_integerScaleFactorY
  , scaleFactorX
  , scaleFactorY
  , initWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , scaleFactorXSelector
  , scaleFactorYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Convenience initializer for an autoreleased MPSCNNUpsamplingNearest nodes
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- Returns: A new MPSNNFilter node for a MPSCNNUpsamplingNearest kernel.
--
-- ObjC selector: @+ nodeWithSource:integerScaleFactorX:integerScaleFactorY:@
nodeWithSource_integerScaleFactorX_integerScaleFactorY :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> CULong -> IO (Id MPSCNNUpsamplingNearestNode)
nodeWithSource_integerScaleFactorX_integerScaleFactorY sourceNode integerScaleFactorX integerScaleFactorY =
  do
    cls' <- getRequiredClass "MPSCNNUpsamplingNearestNode"
    sendClassMessage cls' nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector (toMPSNNImageNode sourceNode) integerScaleFactorX integerScaleFactorY

-- | Init a node representing a MPSCNNUpsamplingNearest kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- Returns: A new MPSNNFilter node for a MPSCNNUpsamplingNearest kernel.
--
-- ObjC selector: @- initWithSource:integerScaleFactorX:integerScaleFactorY:@
initWithSource_integerScaleFactorX_integerScaleFactorY :: (IsMPSCNNUpsamplingNearestNode mpscnnUpsamplingNearestNode, IsMPSNNImageNode sourceNode) => mpscnnUpsamplingNearestNode -> sourceNode -> CULong -> CULong -> IO (Id MPSCNNUpsamplingNearestNode)
initWithSource_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingNearestNode sourceNode integerScaleFactorX integerScaleFactorY =
  sendOwnedMessage mpscnnUpsamplingNearestNode initWithSource_integerScaleFactorX_integerScaleFactorYSelector (toMPSNNImageNode sourceNode) integerScaleFactorX integerScaleFactorY

-- | @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingNearestNode mpscnnUpsamplingNearestNode => mpscnnUpsamplingNearestNode -> IO CDouble
scaleFactorX mpscnnUpsamplingNearestNode =
  sendMessage mpscnnUpsamplingNearestNode scaleFactorXSelector

-- | @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingNearestNode mpscnnUpsamplingNearestNode => mpscnnUpsamplingNearestNode -> IO CDouble
scaleFactorY mpscnnUpsamplingNearestNode =
  sendMessage mpscnnUpsamplingNearestNode scaleFactorYSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:integerScaleFactorX:integerScaleFactorY:@
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[Id MPSNNImageNode, CULong, CULong] (Id MPSCNNUpsamplingNearestNode)
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @initWithSource:integerScaleFactorX:integerScaleFactorY:@
initWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[Id MPSNNImageNode, CULong, CULong] (Id MPSCNNUpsamplingNearestNode)
initWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector '[] CDouble
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector '[] CDouble
scaleFactorYSelector = mkSelector "scaleFactorY"

