{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for motion bounding box geometry
--
-- Generated bindings for @MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor
  ( MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor
  , IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor(..)
  , descriptor
  , boundingBoxBuffers
  , setBoundingBoxBuffers
  , boundingBoxStride
  , setBoundingBoxStride
  , boundingBoxCount
  , setBoundingBoxCount
  , boundingBoxBuffersSelector
  , boundingBoxCountSelector
  , boundingBoxStrideSelector
  , descriptorSelector
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
import ObjC.Foundation.Internal.Classes

-- | @+ descriptor@
descriptor :: IO (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Bounding box buffer containing MTLAxisAlignedBoundingBoxes similar to what MTLAccelerationStructureBoundingBoxGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- boundingBoxBuffers@
boundingBoxBuffers :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO (Id NSArray)
boundingBoxBuffers mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor boundingBoxBuffersSelector

-- | Bounding box buffer containing MTLAxisAlignedBoundingBoxes similar to what MTLAccelerationStructureBoundingBoxGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- setBoundingBoxBuffers:@
setBoundingBoxBuffers :: (IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> value -> IO ()
setBoundingBoxBuffers mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor setBoundingBoxBuffersSelector (toNSArray value)

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor boundingBoxStrideSelector

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor setBoundingBoxStrideSelector value

-- | Number of bounding boxes
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor =
  sendMessage mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor boundingBoxCountSelector

-- | Number of bounding boxes
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor value =
  sendMessage mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor setBoundingBoxCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @boundingBoxBuffers@
boundingBoxBuffersSelector :: Selector '[] (Id NSArray)
boundingBoxBuffersSelector = mkSelector "boundingBoxBuffers"

-- | @Selector@ for @setBoundingBoxBuffers:@
setBoundingBoxBuffersSelector :: Selector '[Id NSArray] ()
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

