{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , instanceDescriptorBuffer
  , setInstanceDescriptorBuffer
  , instanceDescriptorBufferOffset
  , setInstanceDescriptorBufferOffset
  , instanceDescriptorStride
  , setInstanceDescriptorStride
  , instanceCount
  , setInstanceCount
  , instancedAccelerationStructures
  , setInstancedAccelerationStructures
  , instanceDescriptorType
  , setInstanceDescriptorType
  , motionTransformBuffer
  , setMotionTransformBuffer
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
  , instanceCountSelector
  , instanceDescriptorBufferOffsetSelector
  , instanceDescriptorBufferSelector
  , instanceDescriptorStrideSelector
  , instanceDescriptorTypeSelector
  , instanceTransformationMatrixLayoutSelector
  , instancedAccelerationStructuresSelector
  , motionTransformBufferOffsetSelector
  , motionTransformBufferSelector
  , motionTransformCountSelector
  , motionTransformStrideSelector
  , motionTransformTypeSelector
  , setInstanceCountSelector
  , setInstanceDescriptorBufferOffsetSelector
  , setInstanceDescriptorBufferSelector
  , setInstanceDescriptorStrideSelector
  , setInstanceDescriptorTypeSelector
  , setInstanceTransformationMatrixLayoutSelector
  , setInstancedAccelerationStructuresSelector
  , setMotionTransformBufferOffsetSelector
  , setMotionTransformBufferSelector
  , setMotionTransformCountSelector
  , setMotionTransformStrideSelector
  , setMotionTransformTypeSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' descriptorSelector

-- | Buffer containing instance descriptors of the type specified by the instanceDescriptorType property
--
-- ObjC selector: @- instanceDescriptorBuffer@
instanceDescriptorBuffer :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO RawId
instanceDescriptorBuffer mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instanceDescriptorBufferSelector

-- | Buffer containing instance descriptors of the type specified by the instanceDescriptorType property
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setInstanceDescriptorBuffer mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstanceDescriptorBufferSelector value

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- instanceDescriptorBufferOffset@
instanceDescriptorBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorBufferOffset mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instanceDescriptorBufferOffsetSelector

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setInstanceDescriptorBufferOffset:@
setInstanceDescriptorBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorBufferOffset mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstanceDescriptorBufferOffsetSelector value

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instanceDescriptorStrideSelector

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstanceDescriptorStrideSelector value

-- | Number of instance descriptors
--
-- ObjC selector: @- instanceCount@
instanceCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
instanceCount mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instanceCountSelector

-- | Number of instance descriptors
--
-- ObjC selector: @- setInstanceCount:@
setInstanceCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceCount mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstanceCountSelector value

-- | Acceleration structures to be instanced
--
-- ObjC selector: @- instancedAccelerationStructures@
instancedAccelerationStructures :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO (Id NSArray)
instancedAccelerationStructures mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instancedAccelerationStructuresSelector

-- | Acceleration structures to be instanced
--
-- ObjC selector: @- setInstancedAccelerationStructures:@
setInstancedAccelerationStructures :: (IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor, IsNSArray value) => mtlInstanceAccelerationStructureDescriptor -> value -> IO ()
setInstancedAccelerationStructures mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstancedAccelerationStructuresSelector (toNSArray value)

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeDefault.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instanceDescriptorTypeSelector

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeDefault.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstanceDescriptorTypeSelector value

-- | Buffer containing transformation information for motion
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO RawId
motionTransformBuffer mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor motionTransformBufferSelector

-- | Buffer containing transformation information for motion
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setMotionTransformBuffer mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setMotionTransformBufferSelector value

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- motionTransformBufferOffset@
motionTransformBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformBufferOffset mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor motionTransformBufferOffsetSelector

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setMotionTransformBufferOffset:@
setMotionTransformBufferOffset :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformBufferOffset mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setMotionTransformBufferOffsetSelector value

-- | Number of motion transforms
--
-- ObjC selector: @- motionTransformCount@
motionTransformCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformCount mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor motionTransformCountSelector

-- | Number of motion transforms
--
-- ObjC selector: @- setMotionTransformCount:@
setMotionTransformCount :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformCount mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setMotionTransformCountSelector value

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor instanceTransformationMatrixLayoutSelector

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setInstanceTransformationMatrixLayoutSelector value

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor motionTransformTypeSelector

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setMotionTransformTypeSelector value

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtlInstanceAccelerationStructureDescriptor =
  sendMessage mtlInstanceAccelerationStructureDescriptor motionTransformStrideSelector

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTLInstanceAccelerationStructureDescriptor mtlInstanceAccelerationStructureDescriptor => mtlInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtlInstanceAccelerationStructureDescriptor value =
  sendMessage mtlInstanceAccelerationStructureDescriptor setMotionTransformStrideSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLInstanceAccelerationStructureDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @instanceDescriptorBuffer@
instanceDescriptorBufferSelector :: Selector '[] RawId
instanceDescriptorBufferSelector = mkSelector "instanceDescriptorBuffer"

-- | @Selector@ for @setInstanceDescriptorBuffer:@
setInstanceDescriptorBufferSelector :: Selector '[RawId] ()
setInstanceDescriptorBufferSelector = mkSelector "setInstanceDescriptorBuffer:"

-- | @Selector@ for @instanceDescriptorBufferOffset@
instanceDescriptorBufferOffsetSelector :: Selector '[] CULong
instanceDescriptorBufferOffsetSelector = mkSelector "instanceDescriptorBufferOffset"

-- | @Selector@ for @setInstanceDescriptorBufferOffset:@
setInstanceDescriptorBufferOffsetSelector :: Selector '[CULong] ()
setInstanceDescriptorBufferOffsetSelector = mkSelector "setInstanceDescriptorBufferOffset:"

-- | @Selector@ for @instanceDescriptorStride@
instanceDescriptorStrideSelector :: Selector '[] CULong
instanceDescriptorStrideSelector = mkSelector "instanceDescriptorStride"

-- | @Selector@ for @setInstanceDescriptorStride:@
setInstanceDescriptorStrideSelector :: Selector '[CULong] ()
setInstanceDescriptorStrideSelector = mkSelector "setInstanceDescriptorStride:"

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector '[] CULong
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector '[CULong] ()
setInstanceCountSelector = mkSelector "setInstanceCount:"

-- | @Selector@ for @instancedAccelerationStructures@
instancedAccelerationStructuresSelector :: Selector '[] (Id NSArray)
instancedAccelerationStructuresSelector = mkSelector "instancedAccelerationStructures"

-- | @Selector@ for @setInstancedAccelerationStructures:@
setInstancedAccelerationStructuresSelector :: Selector '[Id NSArray] ()
setInstancedAccelerationStructuresSelector = mkSelector "setInstancedAccelerationStructures:"

-- | @Selector@ for @instanceDescriptorType@
instanceDescriptorTypeSelector :: Selector '[] MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorTypeSelector = mkSelector "instanceDescriptorType"

-- | @Selector@ for @setInstanceDescriptorType:@
setInstanceDescriptorTypeSelector :: Selector '[MTLAccelerationStructureInstanceDescriptorType] ()
setInstanceDescriptorTypeSelector = mkSelector "setInstanceDescriptorType:"

-- | @Selector@ for @motionTransformBuffer@
motionTransformBufferSelector :: Selector '[] RawId
motionTransformBufferSelector = mkSelector "motionTransformBuffer"

-- | @Selector@ for @setMotionTransformBuffer:@
setMotionTransformBufferSelector :: Selector '[RawId] ()
setMotionTransformBufferSelector = mkSelector "setMotionTransformBuffer:"

-- | @Selector@ for @motionTransformBufferOffset@
motionTransformBufferOffsetSelector :: Selector '[] CULong
motionTransformBufferOffsetSelector = mkSelector "motionTransformBufferOffset"

-- | @Selector@ for @setMotionTransformBufferOffset:@
setMotionTransformBufferOffsetSelector :: Selector '[CULong] ()
setMotionTransformBufferOffsetSelector = mkSelector "setMotionTransformBufferOffset:"

-- | @Selector@ for @motionTransformCount@
motionTransformCountSelector :: Selector '[] CULong
motionTransformCountSelector = mkSelector "motionTransformCount"

-- | @Selector@ for @setMotionTransformCount:@
setMotionTransformCountSelector :: Selector '[CULong] ()
setMotionTransformCountSelector = mkSelector "setMotionTransformCount:"

-- | @Selector@ for @instanceTransformationMatrixLayout@
instanceTransformationMatrixLayoutSelector :: Selector '[] MTLMatrixLayout
instanceTransformationMatrixLayoutSelector = mkSelector "instanceTransformationMatrixLayout"

-- | @Selector@ for @setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayoutSelector :: Selector '[MTLMatrixLayout] ()
setInstanceTransformationMatrixLayoutSelector = mkSelector "setInstanceTransformationMatrixLayout:"

-- | @Selector@ for @motionTransformType@
motionTransformTypeSelector :: Selector '[] MTLTransformType
motionTransformTypeSelector = mkSelector "motionTransformType"

-- | @Selector@ for @setMotionTransformType:@
setMotionTransformTypeSelector :: Selector '[MTLTransformType] ()
setMotionTransformTypeSelector = mkSelector "setMotionTransformType:"

-- | @Selector@ for @motionTransformStride@
motionTransformStrideSelector :: Selector '[] CULong
motionTransformStrideSelector = mkSelector "motionTransformStride"

-- | @Selector@ for @setMotionTransformStride:@
setMotionTransformStrideSelector :: Selector '[CULong] ()
setMotionTransformStrideSelector = mkSelector "setMotionTransformStride:"

