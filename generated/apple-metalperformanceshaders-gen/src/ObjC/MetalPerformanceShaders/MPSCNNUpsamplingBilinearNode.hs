{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSCNNUpsamplingBilinear kernel
--
-- Generated bindings for @MPSCNNUpsamplingBilinearNode@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingBilinearNode
  ( MPSCNNUpsamplingBilinearNode
  , IsMPSCNNUpsamplingBilinearNode(..)
  , nodeWithSource_integerScaleFactorX_integerScaleFactorY
  , nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners
  , initWithSource_integerScaleFactorX_integerScaleFactorY
  , initWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners
  , scaleFactorX
  , scaleFactorY
  , alignCorners
  , alignCornersSelector
  , initWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector
  , nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector
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

-- | Init a autoreleased node representing a MPSCNNUpsamplingBilinear kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- Returns: A new MPSNNFilter node for a MPSCNNUpsamplingBilinear kernel.
--
-- ObjC selector: @+ nodeWithSource:integerScaleFactorX:integerScaleFactorY:@
nodeWithSource_integerScaleFactorX_integerScaleFactorY :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> CULong -> IO (Id MPSCNNUpsamplingBilinearNode)
nodeWithSource_integerScaleFactorX_integerScaleFactorY sourceNode integerScaleFactorX integerScaleFactorY =
  do
    cls' <- getRequiredClass "MPSCNNUpsamplingBilinearNode"
    sendClassMessage cls' nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector (toMPSNNImageNode sourceNode) integerScaleFactorX integerScaleFactorY

-- | Init a autoreleased node representing a MPSCNNUpsamplingBilinear kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- @alignCorners@ — Specifier whether the centers of the 4 corner pixels of the input and output regions are aligned,
--
-- Returns: A new MPSNNFilter node for a MPSCNNUpsamplingBilinear kernel.
--
-- ObjC selector: @+ nodeWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:@
nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners :: IsMPSNNImageNode sourceNode => sourceNode -> CULong -> CULong -> Bool -> IO (Id MPSCNNUpsamplingBilinearNode)
nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners sourceNode integerScaleFactorX integerScaleFactorY alignCorners =
  do
    cls' <- getRequiredClass "MPSCNNUpsamplingBilinearNode"
    sendClassMessage cls' nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector (toMPSNNImageNode sourceNode) integerScaleFactorX integerScaleFactorY alignCorners

-- | Init a node representing a MPSCNNUpsamplingBilinear kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- Returns: A new MPSNNFilter node for a MPSCNNUpsamplingBilinear kernel.
--
-- ObjC selector: @- initWithSource:integerScaleFactorX:integerScaleFactorY:@
initWithSource_integerScaleFactorX_integerScaleFactorY :: (IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode, IsMPSNNImageNode sourceNode) => mpscnnUpsamplingBilinearNode -> sourceNode -> CULong -> CULong -> IO (Id MPSCNNUpsamplingBilinearNode)
initWithSource_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingBilinearNode sourceNode integerScaleFactorX integerScaleFactorY =
  sendOwnedMessage mpscnnUpsamplingBilinearNode initWithSource_integerScaleFactorX_integerScaleFactorYSelector (toMPSNNImageNode sourceNode) integerScaleFactorX integerScaleFactorY

-- | Init a node representing a MPSCNNUpsamplingBilinear kernel
--
-- @sourceNode@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- @alignCorners@ — Specifier whether the centers of the 4 corner pixels of the input and output regions are aligned,
--
-- Returns: A new MPSNNFilter node for a MPSCNNUpsamplingBilinear kernel.
--
-- ObjC selector: @- initWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:@
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners :: (IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode, IsMPSNNImageNode sourceNode) => mpscnnUpsamplingBilinearNode -> sourceNode -> CULong -> CULong -> Bool -> IO (Id MPSCNNUpsamplingBilinearNode)
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners mpscnnUpsamplingBilinearNode sourceNode integerScaleFactorX integerScaleFactorY alignCorners =
  sendOwnedMessage mpscnnUpsamplingBilinearNode initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector (toMPSNNImageNode sourceNode) integerScaleFactorX integerScaleFactorY alignCorners

-- | @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode => mpscnnUpsamplingBilinearNode -> IO CDouble
scaleFactorX mpscnnUpsamplingBilinearNode =
  sendMessage mpscnnUpsamplingBilinearNode scaleFactorXSelector

-- | @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode => mpscnnUpsamplingBilinearNode -> IO CDouble
scaleFactorY mpscnnUpsamplingBilinearNode =
  sendMessage mpscnnUpsamplingBilinearNode scaleFactorYSelector

-- | @- alignCorners@
alignCorners :: IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode => mpscnnUpsamplingBilinearNode -> IO Bool
alignCorners mpscnnUpsamplingBilinearNode =
  sendMessage mpscnnUpsamplingBilinearNode alignCornersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:integerScaleFactorX:integerScaleFactorY:@
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[Id MPSNNImageNode, CULong, CULong] (Id MPSCNNUpsamplingBilinearNode)
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @nodeWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:@
nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, Bool] (Id MPSCNNUpsamplingBilinearNode)
nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector = mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:"

-- | @Selector@ for @initWithSource:integerScaleFactorX:integerScaleFactorY:@
initWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[Id MPSNNImageNode, CULong, CULong] (Id MPSCNNUpsamplingBilinearNode)
initWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @initWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:@
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector :: Selector '[Id MPSNNImageNode, CULong, CULong, Bool] (Id MPSCNNUpsamplingBilinearNode)
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector = mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector '[] CDouble
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector '[] CDouble
scaleFactorYSelector = mkSelector "scaleFactorY"

-- | @Selector@ for @alignCorners@
alignCornersSelector :: Selector '[] Bool
alignCornersSelector = mkSelector "alignCorners"

