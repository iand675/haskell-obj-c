{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for an instance acceleration structure
--
-- Generated bindings for @MTLInstanceAccelerationStructureDescriptor@.
module ObjC.Metal.MTLInstanceAccelerationStructureDescriptor
  ( MTLInstanceAccelerationStructureDescriptor
  , IsMTLInstanceAccelerationStructureDescriptor(..)
  , descriptor
  , instanceDescriptorBufferOffset
  , setInstanceDescriptorBufferOffset
  , instanceDescriptorStride
  , setInstanceDescriptorStride
  , instanceCount
  , setInstanceCount
  , instanceDescriptorType
  , setInstanceDescriptorType
  , motionTransformBufferOffset
  , setMotionTransformBufferOffset
  , motionTransformCount
  , setMotionTransformCount
  , instanceTransformationMatrixLayout
  , setInstanceTransformationMatrixLayout
  , motionTransformType
  , setMotionTransformType
  , motionTransformStride
  , setMotionTransformStride
  , descriptorSelector
  , instanceDescriptorBufferOffsetSelector
  , setInstanceDescriptorBufferOffsetSelector
  , instanceDescriptorStrideSelector
  , setInstanceDescriptorStrideSelector
  , instanceCountSelector
  , setInstanceCountSelector
  , instanceDescriptorTypeSelector
  , setInstanceDescriptorTypeSelector
  , motionTransformBufferOffsetSelector
  , setMotionTransformBufferOffsetSelector
  , motionTransformCountSelector
  , setMotionTransformCountSelector
  , instanceTransformationMatrixLayoutSelector
  , setInstanceTransformationMatrixLayoutSelector
  , motionTransformTypeSelector
  , setMotionTransformTypeSelector
  , motionTransformStrideSelector
  , setMotionTransformStrideSelector

  -- * Enum types
  , MTLAccelerationStructureInstanceDescriptorType(MTLAccelerationStructureInstanceDescriptorType)
  , pattern MTLAccelerationStructureInstanceDescriptorTypeDefault
  , pattern MTLAccelerationStructureInstanceDescriptorTypeUserID
  , pattern MTLAccelerationStructureInstanceDescriptorTypeMotion
  , pattern MTLAccelerationStructureInstanceDescriptorTypeIndirect
  , pattern MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion
  , MTLMatrixLayout(MTLMatrixLayout)
  , pattern MTLMatrixLayoutColumnMajor
  , pattern MTLMatrixLayoutRowMajor
  , MTLTransformType(MTLTransformType)
  , pattern MTLTransformTypePackedFloat4x3
  , pattern MTLTransformTypeComponent

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
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ descriptor@
descriptor :: IO (Id MTLInstanceAccelerationStructureDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLInstanceAccelerationStructureDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- instanceDescriptorBufferOffset@
instanceDescriptorBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorBufferOffset mtlInstanceAccelerationStructureDescriptor  =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorBufferOffset") retCULong []

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setInstanceDescriptorBufferOffset:@
setInstanceDescriptorBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorBufferOffset mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorBufferOffset:") retVoid [argCULong (fromIntegral value)]

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtlInstanceAccelerationStructureDescriptor  =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorStride") retCULong []

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorStride:") retVoid [argCULong (fromIntegral value)]

-- | Number of instance descriptors
--
-- ObjC selector: @- instanceCount@
instanceCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
instanceCount mtlInstanceAccelerationStructureDescriptor  =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "instanceCount") retCULong []

-- | Number of instance descriptors
--
-- ObjC selector: @- setInstanceCount:@
setInstanceCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceCount mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setInstanceCount:") retVoid [argCULong (fromIntegral value)]

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeDefault.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtlInstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CULong -> MTLAccelerationStructureInstanceDescriptorType) $ sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorType") retCULong []

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeDefault.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorType:") retVoid [argCULong (coerce value)]

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- motionTransformBufferOffset@
motionTransformBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformBufferOffset mtlInstanceAccelerationStructureDescriptor  =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "motionTransformBufferOffset") retCULong []

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setMotionTransformBufferOffset:@
setMotionTransformBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformBufferOffset mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformBufferOffset:") retVoid [argCULong (fromIntegral value)]

-- | Number of motion transforms
--
-- ObjC selector: @- motionTransformCount@
motionTransformCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformCount mtlInstanceAccelerationStructureDescriptor  =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "motionTransformCount") retCULong []

-- | Number of motion transforms
--
-- ObjC selector: @- setMotionTransformCount:@
setMotionTransformCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformCount mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformCount:") retVoid [argCULong (fromIntegral value)]

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtlInstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CLong -> MTLMatrixLayout) $ sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "instanceTransformationMatrixLayout") retCLong []

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setInstanceTransformationMatrixLayout:") retVoid [argCLong (coerce value)]

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtlInstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CLong -> MTLTransformType) $ sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "motionTransformType") retCLong []

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformType:") retVoid [argCLong (coerce value)]

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtlInstanceAccelerationStructureDescriptor  =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "motionTransformStride") retCULong []

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtlInstanceAccelerationStructureDescriptor  value =
  sendMsg mtlInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformStride:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @instanceDescriptorBufferOffset@
instanceDescriptorBufferOffsetSelector :: Selector
instanceDescriptorBufferOffsetSelector = mkSelector "instanceDescriptorBufferOffset"

-- | @Selector@ for @setInstanceDescriptorBufferOffset:@
setInstanceDescriptorBufferOffsetSelector :: Selector
setInstanceDescriptorBufferOffsetSelector = mkSelector "setInstanceDescriptorBufferOffset:"

-- | @Selector@ for @instanceDescriptorStride@
instanceDescriptorStrideSelector :: Selector
instanceDescriptorStrideSelector = mkSelector "instanceDescriptorStride"

-- | @Selector@ for @setInstanceDescriptorStride:@
setInstanceDescriptorStrideSelector :: Selector
setInstanceDescriptorStrideSelector = mkSelector "setInstanceDescriptorStride:"

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector
setInstanceCountSelector = mkSelector "setInstanceCount:"

-- | @Selector@ for @instanceDescriptorType@
instanceDescriptorTypeSelector :: Selector
instanceDescriptorTypeSelector = mkSelector "instanceDescriptorType"

-- | @Selector@ for @setInstanceDescriptorType:@
setInstanceDescriptorTypeSelector :: Selector
setInstanceDescriptorTypeSelector = mkSelector "setInstanceDescriptorType:"

-- | @Selector@ for @motionTransformBufferOffset@
motionTransformBufferOffsetSelector :: Selector
motionTransformBufferOffsetSelector = mkSelector "motionTransformBufferOffset"

-- | @Selector@ for @setMotionTransformBufferOffset:@
setMotionTransformBufferOffsetSelector :: Selector
setMotionTransformBufferOffsetSelector = mkSelector "setMotionTransformBufferOffset:"

-- | @Selector@ for @motionTransformCount@
motionTransformCountSelector :: Selector
motionTransformCountSelector = mkSelector "motionTransformCount"

-- | @Selector@ for @setMotionTransformCount:@
setMotionTransformCountSelector :: Selector
setMotionTransformCountSelector = mkSelector "setMotionTransformCount:"

-- | @Selector@ for @instanceTransformationMatrixLayout@
instanceTransformationMatrixLayoutSelector :: Selector
instanceTransformationMatrixLayoutSelector = mkSelector "instanceTransformationMatrixLayout"

-- | @Selector@ for @setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayoutSelector :: Selector
setInstanceTransformationMatrixLayoutSelector = mkSelector "setInstanceTransformationMatrixLayout:"

-- | @Selector@ for @motionTransformType@
motionTransformTypeSelector :: Selector
motionTransformTypeSelector = mkSelector "motionTransformType"

-- | @Selector@ for @setMotionTransformType:@
setMotionTransformTypeSelector :: Selector
setMotionTransformTypeSelector = mkSelector "setMotionTransformType:"

-- | @Selector@ for @motionTransformStride@
motionTransformStrideSelector :: Selector
motionTransformStrideSelector = mkSelector "motionTransformStride"

-- | @Selector@ for @setMotionTransformStride:@
setMotionTransformStrideSelector :: Selector
setMotionTransformStrideSelector = mkSelector "setMotionTransformStride:"

