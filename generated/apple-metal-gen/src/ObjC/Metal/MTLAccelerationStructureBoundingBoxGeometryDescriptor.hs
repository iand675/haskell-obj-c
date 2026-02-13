{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for bounding box geometry
--
-- Generated bindings for @MTLAccelerationStructureBoundingBoxGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureBoundingBoxGeometryDescriptor
  ( MTLAccelerationStructureBoundingBoxGeometryDescriptor
  , IsMTLAccelerationStructureBoundingBoxGeometryDescriptor(..)
  , descriptor
  , boundingBoxBuffer
  , setBoundingBoxBuffer
  , boundingBoxBufferOffset
  , setBoundingBoxBufferOffset
  , boundingBoxStride
  , setBoundingBoxStride
  , boundingBoxCount
  , setBoundingBoxCount
  , boundingBoxBufferOffsetSelector
  , boundingBoxBufferSelector
  , boundingBoxCountSelector
  , boundingBoxStrideSelector
  , descriptorSelector
  , setBoundingBoxBufferOffsetSelector
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
import ObjC.Foundation.Internal.Classes

-- | @+ descriptor@
descriptor :: IO (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureBoundingBoxGeometryDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Bounding box buffer containing MTLAxisAlignedBoundingBoxes. Must not be nil.
--
-- ObjC selector: @- boundingBoxBuffer@
boundingBoxBuffer :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO RawId
boundingBoxBuffer mtlAccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor boundingBoxBufferSelector

-- | Bounding box buffer containing MTLAxisAlignedBoundingBoxes. Must not be nil.
--
-- ObjC selector: @- setBoundingBoxBuffer:@
setBoundingBoxBuffer :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> RawId -> IO ()
setBoundingBoxBuffer mtlAccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxBufferSelector value

-- | Bounding box buffer offset. Must be a multiple of the bounding box stride and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- boundingBoxBufferOffset@
boundingBoxBufferOffset :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxBufferOffset mtlAccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor boundingBoxBufferOffsetSelector

-- | Bounding box buffer offset. Must be a multiple of the bounding box stride and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setBoundingBoxBufferOffset:@
setBoundingBoxBufferOffset :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxBufferOffset mtlAccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxBufferOffsetSelector value

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtlAccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor boundingBoxStrideSelector

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtlAccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxStrideSelector value

-- | Number of bounding boxes
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtlAccelerationStructureBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor boundingBoxCountSelector

-- | Number of bounding boxes
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtlAccelerationStructureBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureBoundingBoxGeometryDescriptor setBoundingBoxCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @boundingBoxBuffer@
boundingBoxBufferSelector :: Selector '[] RawId
boundingBoxBufferSelector = mkSelector "boundingBoxBuffer"

-- | @Selector@ for @setBoundingBoxBuffer:@
setBoundingBoxBufferSelector :: Selector '[RawId] ()
setBoundingBoxBufferSelector = mkSelector "setBoundingBoxBuffer:"

-- | @Selector@ for @boundingBoxBufferOffset@
boundingBoxBufferOffsetSelector :: Selector '[] CULong
boundingBoxBufferOffsetSelector = mkSelector "boundingBoxBufferOffset"

-- | @Selector@ for @setBoundingBoxBufferOffset:@
setBoundingBoxBufferOffsetSelector :: Selector '[CULong] ()
setBoundingBoxBufferOffsetSelector = mkSelector "setBoundingBoxBufferOffset:"

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

