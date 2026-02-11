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
  , boundingBoxBufferOffset
  , setBoundingBoxBufferOffset
  , boundingBoxStride
  , setBoundingBoxStride
  , boundingBoxCount
  , setBoundingBoxCount
  , descriptorSelector
  , boundingBoxBufferOffsetSelector
  , setBoundingBoxBufferOffsetSelector
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
descriptor :: IO (Id MTLAccelerationStructureBoundingBoxGeometryDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLAccelerationStructureBoundingBoxGeometryDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Bounding box buffer offset. Must be a multiple of the bounding box stride and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- boundingBoxBufferOffset@
boundingBoxBufferOffset :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxBufferOffset mtlAccelerationStructureBoundingBoxGeometryDescriptor  =
  sendMsg mtlAccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "boundingBoxBufferOffset") retCULong []

-- | Bounding box buffer offset. Must be a multiple of the bounding box stride and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setBoundingBoxBufferOffset:@
setBoundingBoxBufferOffset :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxBufferOffset mtlAccelerationStructureBoundingBoxGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxBufferOffset:") retVoid [argCULong (fromIntegral value)]

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- boundingBoxStride@
boundingBoxStride :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxStride mtlAccelerationStructureBoundingBoxGeometryDescriptor  =
  sendMsg mtlAccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "boundingBoxStride") retCULong []

-- | Stride, in bytes, between bounding boxes in the bounding box buffer. Must be at least 24 bytes and must be a multiple of 4 bytes. Defaults to 24 bytes.
--
-- ObjC selector: @- setBoundingBoxStride:@
setBoundingBoxStride :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxStride mtlAccelerationStructureBoundingBoxGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxStride:") retVoid [argCULong (fromIntegral value)]

-- | Number of bounding boxes
--
-- ObjC selector: @- boundingBoxCount@
boundingBoxCount :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> IO CULong
boundingBoxCount mtlAccelerationStructureBoundingBoxGeometryDescriptor  =
  sendMsg mtlAccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "boundingBoxCount") retCULong []

-- | Number of bounding boxes
--
-- ObjC selector: @- setBoundingBoxCount:@
setBoundingBoxCount :: IsMTLAccelerationStructureBoundingBoxGeometryDescriptor mtlAccelerationStructureBoundingBoxGeometryDescriptor => mtlAccelerationStructureBoundingBoxGeometryDescriptor -> CULong -> IO ()
setBoundingBoxCount mtlAccelerationStructureBoundingBoxGeometryDescriptor  value =
  sendMsg mtlAccelerationStructureBoundingBoxGeometryDescriptor (mkSelector "setBoundingBoxCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @boundingBoxBufferOffset@
boundingBoxBufferOffsetSelector :: Selector
boundingBoxBufferOffsetSelector = mkSelector "boundingBoxBufferOffset"

-- | @Selector@ for @setBoundingBoxBufferOffset:@
setBoundingBoxBufferOffsetSelector :: Selector
setBoundingBoxBufferOffsetSelector = mkSelector "setBoundingBoxBufferOffset:"

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

