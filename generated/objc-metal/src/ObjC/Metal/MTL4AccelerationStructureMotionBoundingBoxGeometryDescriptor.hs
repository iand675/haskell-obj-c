{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes motion bounding box geometry, suitable for motion ray tracing.
--
-- You use bounding boxes to implement procedural geometry for ray tracing, such as spheres or any other shape you define by using intersection functions.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor
  ( MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor
  , IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor(..)
  , boundingBoxBuffers
  , setBoundingBoxBuffers
  , boundingBoxStride
  , setBoundingBoxStride
  , boundingBoxCount
  , setBoundingBoxCount
  , boundingBoxBuffersSelector
  , setBoundingBoxBuffersSelector
  , boundingBoxStrideSelector
  , setBoundingBoxStrideSelector
  , boundingBoxCountSelector
  , setBoundingBoxCountSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Configures a reference to a buffer where each entry contains a reference to a buffer of bounding boxes.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a vertex buffer containing the bounding box data for the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- boundingBoxBuffers@
boundingBoxBuffers :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO MTL4BufferRange
boundingBoxBuffers mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "boundingBoxBuffers") retMTL4BufferRange []

-- | Configures a reference to a buffer where each entry contains a reference to a buffer of bounding boxes.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a vertex buffer containing the bounding box data for the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- setBoundingBoxBuffers:@
setBoundingBoxBuffers :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> MTL4BufferRange -> IO ()
setBoundingBoxBuffers mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxBuffers:") retVoid [argMTL4BufferRange value]

-- | Declares the stride, in bytes, between bounding boxes in the bounding box buffers each entry in @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box stride. You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "boundingBoxStride") retCULong []

-- | Declares the stride, in bytes, between bounding boxes in the bounding box buffers each entry in @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box stride. You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxStride:") retVoid [argCULong (fromIntegral value)]

-- | Declares the number of bounding boxes in each buffer that @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box count.
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "boundingBoxCount") retCULong []

-- | Declares the number of bounding boxes in each buffer that @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box count.
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundingBoxBuffers@
boundingBoxBuffersSelector :: Selector
boundingBoxBuffersSelector = mkSelector "boundingBoxBuffers"

-- | @Selector@ for @setBoundingBoxBuffers:@
setBoundingBoxBuffersSelector :: Selector
setBoundingBoxBuffersSelector = mkSelector "setBoundingBoxBuffers:"

-- | @Selector@ for @boundingBoxStride@
boundingBoxStrideSelector :: Selector
boundingBoxStrideSelector = mkSelector "boundingBoxStride"

-- | @Selector@ for @setBoundingBoxStride:@
setBoundingBoxStrideSelector :: Selector
setBoundingBoxStrideSelector = mkSelector "setBoundingBoxStride:"

-- | @Selector@ for @boundingBoxCount@
boundingBoxCountSelector :: Selector
boundingBoxCountSelector = mkSelector "boundingBoxCount"

-- | @Selector@ for @setBoundingBoxCount:@
setBoundingBoxCountSelector :: Selector
setBoundingBoxCountSelector = mkSelector "setBoundingBoxCount:"

