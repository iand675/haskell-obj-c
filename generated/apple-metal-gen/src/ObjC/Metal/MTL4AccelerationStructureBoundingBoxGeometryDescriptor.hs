{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes bounding-box geometry suitable for ray tracing.
--
-- You use bounding boxes to implement procedural geometry for ray tracing, such as spheres or any other shape you define by using intersection functions.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4AccelerationStructureBoundingBoxGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureBoundingBoxGeometryDescriptor
  ( MTL4AccelerationStructureBoundingBoxGeometryDescriptor
  , IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor(..)
  , boundingBoxBuffer
  , setBoundingBoxBuffer
  , boundingBoxStride
  , setBoundingBoxStride
  , boundingBoxCount
  , setBoundingBoxCount
  , boundingBoxBufferSelector
  , boundingBoxCountSelector
  , boundingBoxStrideSelector
  , setBoundingBoxBufferSelector
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

-- | References a buffer containing bounding box data in @MTLAxisAlignedBoundingBoxes@ format.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- boundingBoxBuffer@
boundingBoxBuffer :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> IO MTL4BufferRange
boundingBoxBuffer mtL4AccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtL4AccelerationStructureBoundingBoxGeometryDescriptor boundingBoxBufferSelector

-- | References a buffer containing bounding box data in @MTLAxisAlignedBoundingBoxes@ format.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- setBoundingBoxBuffer:@
setBoundingBoxBuffer :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> MTL4BufferRange -> IO ()
setBoundingBoxBuffer mtL4AccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxBufferSelector value

-- | Assigns the stride, in bytes, between bounding boxes in the bounding box buffer @boundingBoxBuffer@ references.
--
-- You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtL4AccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtL4AccelerationStructureBoundingBoxGeometryDescriptor boundingBoxStrideSelector

-- | Assigns the stride, in bytes, between bounding boxes in the bounding box buffer @boundingBoxBuffer@ references.
--
-- You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtL4AccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxStrideSelector value

-- | Describes the number of bounding boxes the @boundingBoxBuffer@ contains.
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtL4AccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtL4AccelerationStructureBoundingBoxGeometryDescriptor boundingBoxCountSelector

-- | Describes the number of bounding boxes the @boundingBoxBuffer@ contains.
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtL4AccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundingBoxBuffer@
boundingBoxBufferSelector :: Selector '[] MTL4BufferRange
boundingBoxBufferSelector = mkSelector "boundingBoxBuffer"

-- | @Selector@ for @setBoundingBoxBuffer:@
setBoundingBoxBufferSelector :: Selector '[MTL4BufferRange] ()
setBoundingBoxBufferSelector = mkSelector "setBoundingBoxBuffer:"

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

