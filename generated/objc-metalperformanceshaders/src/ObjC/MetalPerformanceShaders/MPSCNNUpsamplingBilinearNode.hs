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
  , nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector
  , initWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector
  , scaleFactorXSelector
  , scaleFactorYSelector
  , alignCornersSelector


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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY)] >>= retainedObject . castPtr

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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY), argCULong (if alignCorners then 1 else 0)] >>= retainedObject . castPtr

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
initWithSource_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingBilinearNode  sourceNode integerScaleFactorX integerScaleFactorY =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnUpsamplingBilinearNode (mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY)] >>= ownedObject . castPtr

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
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCorners mpscnnUpsamplingBilinearNode  sourceNode integerScaleFactorX integerScaleFactorY alignCorners =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnUpsamplingBilinearNode (mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY), argCULong (if alignCorners then 1 else 0)] >>= ownedObject . castPtr

-- | @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode => mpscnnUpsamplingBilinearNode -> IO CDouble
scaleFactorX mpscnnUpsamplingBilinearNode  =
  sendMsg mpscnnUpsamplingBilinearNode (mkSelector "scaleFactorX") retCDouble []

-- | @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode => mpscnnUpsamplingBilinearNode -> IO CDouble
scaleFactorY mpscnnUpsamplingBilinearNode  =
  sendMsg mpscnnUpsamplingBilinearNode (mkSelector "scaleFactorY") retCDouble []

-- | @- alignCorners@
alignCorners :: IsMPSCNNUpsamplingBilinearNode mpscnnUpsamplingBilinearNode => mpscnnUpsamplingBilinearNode -> IO Bool
alignCorners mpscnnUpsamplingBilinearNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnUpsamplingBilinearNode (mkSelector "alignCorners") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:integerScaleFactorX:integerScaleFactorY:@
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @nodeWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:@
nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector :: Selector
nodeWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector = mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:"

-- | @Selector@ for @initWithSource:integerScaleFactorX:integerScaleFactorY:@
initWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector
initWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @initWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:@
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector :: Selector
initWithSource_integerScaleFactorX_integerScaleFactorY_alignCornersSelector = mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:alignCorners:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector
scaleFactorYSelector = mkSelector "scaleFactorY"

-- | @Selector@ for @alignCorners@
alignCornersSelector :: Selector
alignCornersSelector = mkSelector "alignCorners"

