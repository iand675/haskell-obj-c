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
  , setBoundingBoxBufferSelector
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

-- | References a buffer containing bounding box data in @MTLAxisAlignedBoundingBoxes@ format.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- boundingBoxBuffer@
boundingBoxBuffer :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> IO MTL4BufferRange
boundingBoxBuffer mtL4AccelerationStructureBoundingBoxGeometryDescriptor  =
  sendMsgStret mtL4AccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "boundingBoxBuffer") retMTL4BufferRange []

-- | References a buffer containing bounding box data in @MTLAxisAlignedBoundingBoxes@ format.
--
-- You are responsible for ensuring the buffer address of the range is not zero.
--
-- ObjC selector: @- setBoundingBoxBuffer:@
setBoundingBoxBuffer :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> MTL4BufferRange -> IO ()
setBoundingBoxBuffer mtL4AccelerationStructureBoundingBoxGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxBuffer:") retVoid [argMTL4BufferRange value]

-- | Assigns the stride, in bytes, between bounding boxes in the bounding box buffer @boundingBoxBuffer@ references.
--
-- You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtL4AccelerationStructureBoundingBoxGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "boundingBoxStride") retCULong []

-- | Assigns the stride, in bytes, between bounding boxes in the bounding box buffer @boundingBoxBuffer@ references.
--
-- You are responsible for ensuring this stride is at least 24 bytes and a multiple of 4 bytes.
--
-- This property defaults to @24@ bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtL4AccelerationStructureBoundingBoxGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxStride:") retVoid [argCULong (fromIntegral value)]

-- | Describes the number of bounding boxes the @boundingBoxBuffer@ contains.
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtL4AccelerationStructureBoundingBoxGeometryDescriptor  =
  sendMsg mtL4AccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "boundingBoxCount") retCULong []

-- | Describes the number of bounding boxes the @boundingBoxBuffer@ contains.
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTL4AccelerationStructureBoundingBoxGeometryDescriptor mtL4AccelerationStructureBoundingBoxGeometryDescriptor => mtL4AccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtL4AccelerationStructureBoundingBoxGeometryDescriptor  value =
  sendMsg mtL4AccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boundingBoxBuffer@
boundingBoxBufferSelector :: Selector
boundingBoxBufferSelector = mkSelector "boundingBoxBuffer"

-- | @Selector@ for @setBoundingBoxBuffer:@
setBoundingBoxBufferSelector :: Selector
setBoundingBoxBufferSelector = mkSelector "setBoundingBoxBuffer:"

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

