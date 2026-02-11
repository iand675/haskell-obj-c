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
  , descriptorSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ descriptor@
descriptor :: IO (Id MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureMotionBoundingBoxGeometryDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Bounding box buffer containing MTLAxisAlignedBoundingBoxes similar to what MTLAccelerationStructureBoundingBoxGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- boundingBoxBuffers@
boundingBoxBuffers :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO (Id NSArray)
boundingBoxBuffers mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "boundingBoxBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Bounding box buffer containing MTLAxisAlignedBoundingBoxes similar to what MTLAccelerationStructureBoundingBoxGeometryDescriptor has but array of the values.
--
-- ObjC selector: @- setBoundingBoxBuffers:@
setBoundingBoxBuffers :: (IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor, IsNSArray value) => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> value -> IO ()
setBoundingBoxBuffers mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxBuffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "boundingBoxStride") retCULong []

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxStride:") retVoid [argCULong (fromIntegral value)]

-- | Number of bounding boxes
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor  =
  sendMsg mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "boundingBoxCount") retCULong []

-- | Number of bounding boxes
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTLAccelerationStructureMotionBoundingBoxGeometryDescriptor mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor => mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureMotionBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

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

