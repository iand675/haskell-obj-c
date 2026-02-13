{-# LANGUAGE DataKinds #-}
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
  , boundingBoxCountSelector
  , boundingBoxStrideSelector
  , setBoundingBoxBuffersSelector
  , setBoundingBoxCountSelector
  , setBoundingBoxStrideSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
boundingBoxBuffers mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor boundingBoxBuffersSelector

-- | Configures a reference to a buffer where each entry contains a reference to a buffer of bounding boxes.
--
-- This property references a buffer that conceptually represents an array with one entry for each keyframe in the motion animation. Each one of these entries consists of a ``MTL4BufferRange`` that, in turn, references a vertex buffer containing the bounding box data for the keyframe.
--
-- You are responsible for ensuring the buffer address is not zero for the top-level buffer, as well as for all the vertex buffers it references.
--
-- ObjC selector: @- setBoundingBoxBuffers:@
setBoundingBoxBuffers :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> MTL4BufferRange -> IO ()
setBoundingBoxBuffers mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor setBoundingBoxBuffersSelector value

-- | Declares the stride, in bytes, between bounding boxes in the bounding box buffers each entry in @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box stride. You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor boundingBoxStrideSelector

-- | Declares the stride, in bytes, between bounding boxes in the bounding box buffers each entry in @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box stride. You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor setBoundingBoxStrideSelector value

-- | Declares the number of bounding boxes in each buffer that @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box count.
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor =
  sendMessage mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor boundingBoxCountSelector

-- | Declares the number of bounding boxes in each buffer that @boundingBoxBuffer@ references.
--
-- All keyframes share the same bounding box count.
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor => mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureMotionBoundingBoxGeometryDescriptor setBoundingBoxCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundingBoxBuffers@
boundingBoxBuffersSelector :: Selector '[] MTL4BufferRange
boundingBoxBuffersSelector = mkSelector "boundingBoxBuffers"

-- | @Selector@ for @setBoundingBoxBuffers:@
setBoundingBoxBuffersSelector :: Selector '[MTL4BufferRange] ()
setBoundingBoxBuffersSelector = mkSelector "setBoundingBoxBuffers:"

-- | @Selector@ for @boundingBoxStride@
boundingBoxStrideSelector :: Selector '[] CULong
boundingBoxStrideSelector = mkSelector "boundingBoxStride"

-- | @Selector@ for @setBoundingBoxStride:@
setBoundingBoxStrideSelector :: Selector '[CULong] ()
setBoundingBoxStrideSelector = mkSelector "setBoundingBoxStride:"

-- | @Selector@ for @boundingBoxCount@
boundingBoxCountSelector :: Selector '[] CULong
boundingBoxCountSelector = mkSelector "boundingBoxCount"

-- | @Selector@ for @setBoundingBoxCount:@
setBoundingBoxCountSelector :: Selector '[CULong] ()
setBoundingBoxCountSelector = mkSelector "setBoundingBoxCount:"

