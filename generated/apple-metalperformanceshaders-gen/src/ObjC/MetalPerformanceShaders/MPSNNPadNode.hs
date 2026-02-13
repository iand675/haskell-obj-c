{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNPadNode
--
-- A node for a MPSNNPad kernel
--
-- You should not use this node to zero pad your data in the XY-plane.                  This node copies the input image and therefore should only be used in                  special circumstances where the normal padding operation, defined for most                  filters and nodes through MPSNNPadding, cannot achieve the necessary padding.                  Therefore use this node only when you need one of the special edge modes:                  MPSImageEdgeModeConstant, MPSImageEdgeModeMirror,                  MPSImageEdgeModeMirrorWithEdge or, if you need padding in the                  feature-channel dimesion.                  In other cases use to MPSNNPadding to get best performance.
--
-- Generated bindings for @MPSNNPadNode@.
module ObjC.MetalPerformanceShaders.MPSNNPadNode
  ( MPSNNPadNode
  , IsMPSNNPadNode(..)
  , nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeMode
  , initWithSource_paddingSizeBefore_paddingSizeAfter_edgeMode
  , fillValue
  , setFillValue
  , fillValueSelector
  , initWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector
  , nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector
  , setFillValueSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Init a node representing a autoreleased MPSNNPad kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @paddingSizeBefore@ — The amount of padding to apply before the image in each dimension.
--
-- @paddingSizeAfter@ — The amount of padding to apply after the image in each dimension.
--
-- @edgeMode@ — The MPSImageEdgeMode for the padding node - Note that for now                                      the pad-node and its gradient are the only nodes that support                                      the extended edge-modes, ie. the ones beyond MPSImageEdgeModeClamp.
--
-- Returns: A new MPSNNFilter node for a MPSNNPad kernel.
--
-- ObjC selector: @+ nodeWithSource:paddingSizeBefore:paddingSizeAfter:edgeMode:@
nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeMode :: IsMPSNNImageNode source => source -> MPSImageCoordinate -> MPSImageCoordinate -> MPSImageEdgeMode -> IO (Id MPSNNPadNode)
nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeMode source paddingSizeBefore paddingSizeAfter edgeMode =
  do
    cls' <- getRequiredClass "MPSNNPadNode"
    sendClassMessage cls' nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector (toMPSNNImageNode source) paddingSizeBefore paddingSizeAfter edgeMode

-- | Init a node representing a MPSNNPad kernel
--
-- @source@ — The MPSNNImageNode representing the source MPSImage for the filter
--
-- @paddingSizeBefore@ — The amount of padding to apply before the image in each dimension.
--
-- @paddingSizeAfter@ — The amount of padding to apply after the image in each dimension.
--
-- @edgeMode@ — The MPSImageEdgeMode for the padding node - Note that for now                                      the pad-node and its gradient are the only nodes that support                                      the extended edge-modes, ie. the ones beyond MPSImageEdgeModeClamp.
--
-- Returns: A new MPSNNFilter node for a MPSNNPad kernel.
--
-- ObjC selector: @- initWithSource:paddingSizeBefore:paddingSizeAfter:edgeMode:@
initWithSource_paddingSizeBefore_paddingSizeAfter_edgeMode :: (IsMPSNNPadNode mpsnnPadNode, IsMPSNNImageNode source) => mpsnnPadNode -> source -> MPSImageCoordinate -> MPSImageCoordinate -> MPSImageEdgeMode -> IO (Id MPSNNPadNode)
initWithSource_paddingSizeBefore_paddingSizeAfter_edgeMode mpsnnPadNode source paddingSizeBefore paddingSizeAfter edgeMode =
  sendOwnedMessage mpsnnPadNode initWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector (toMPSNNImageNode source) paddingSizeBefore paddingSizeAfter edgeMode

-- | fillValue
--
-- Determines the constant value to apply when using MPSImageEdgeModeConstant. Default: 0.0f.
--
-- ObjC selector: @- fillValue@
fillValue :: IsMPSNNPadNode mpsnnPadNode => mpsnnPadNode -> IO CFloat
fillValue mpsnnPadNode =
  sendMessage mpsnnPadNode fillValueSelector

-- | fillValue
--
-- Determines the constant value to apply when using MPSImageEdgeModeConstant. Default: 0.0f.
--
-- ObjC selector: @- setFillValue:@
setFillValue :: IsMPSNNPadNode mpsnnPadNode => mpsnnPadNode -> CFloat -> IO ()
setFillValue mpsnnPadNode value =
  sendMessage mpsnnPadNode setFillValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:paddingSizeBefore:paddingSizeAfter:edgeMode:@
nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector :: Selector '[Id MPSNNImageNode, MPSImageCoordinate, MPSImageCoordinate, MPSImageEdgeMode] (Id MPSNNPadNode)
nodeWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector = mkSelector "nodeWithSource:paddingSizeBefore:paddingSizeAfter:edgeMode:"

-- | @Selector@ for @initWithSource:paddingSizeBefore:paddingSizeAfter:edgeMode:@
initWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector :: Selector '[Id MPSNNImageNode, MPSImageCoordinate, MPSImageCoordinate, MPSImageEdgeMode] (Id MPSNNPadNode)
initWithSource_paddingSizeBefore_paddingSizeAfter_edgeModeSelector = mkSelector "initWithSource:paddingSizeBefore:paddingSizeAfter:edgeMode:"

-- | @Selector@ for @fillValue@
fillValueSelector :: Selector '[] CFloat
fillValueSelector = mkSelector "fillValue"

-- | @Selector@ for @setFillValue:@
setFillValueSelector :: Selector '[CFloat] ()
setFillValueSelector = mkSelector "setFillValue:"

