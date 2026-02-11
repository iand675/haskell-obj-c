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
  , nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , initWithSource_integerScaleFactorX_integerScaleFactorYSelector
  , scaleFactorXSelector
  , scaleFactorYSelector


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
    withObjCPtr sourceNode $ \raw_sourceNode ->
      sendClassMsg cls' (mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY)] >>= retainedObject . castPtr

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
initWithSource_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingNearestNode  sourceNode integerScaleFactorX integerScaleFactorY =
withObjCPtr sourceNode $ \raw_sourceNode ->
    sendMsg mpscnnUpsamplingNearestNode (mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:") (retPtr retVoid) [argPtr (castPtr raw_sourceNode :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY)] >>= ownedObject . castPtr

-- | @- scaleFactorX@
scaleFactorX :: IsMPSCNNUpsamplingNearestNode mpscnnUpsamplingNearestNode => mpscnnUpsamplingNearestNode -> IO CDouble
scaleFactorX mpscnnUpsamplingNearestNode  =
  sendMsg mpscnnUpsamplingNearestNode (mkSelector "scaleFactorX") retCDouble []

-- | @- scaleFactorY@
scaleFactorY :: IsMPSCNNUpsamplingNearestNode mpscnnUpsamplingNearestNode => mpscnnUpsamplingNearestNode -> IO CDouble
scaleFactorY mpscnnUpsamplingNearestNode  =
  sendMsg mpscnnUpsamplingNearestNode (mkSelector "scaleFactorY") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:integerScaleFactorX:integerScaleFactorY:@
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector
nodeWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "nodeWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @initWithSource:integerScaleFactorX:integerScaleFactorY:@
initWithSource_integerScaleFactorX_integerScaleFactorYSelector :: Selector
initWithSource_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithSource:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @scaleFactorX@
scaleFactorXSelector :: Selector
scaleFactorXSelector = mkSelector "scaleFactorX"

-- | @Selector@ for @scaleFactorY@
scaleFactorYSelector :: Selector
scaleFactorYSelector = mkSelector "scaleFactorY"

