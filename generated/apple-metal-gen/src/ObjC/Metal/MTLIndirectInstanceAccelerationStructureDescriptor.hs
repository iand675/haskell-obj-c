{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for an instance acceleration structure built with an indirected buffer of instances.
--
-- Generated bindings for @MTLIndirectInstanceAccelerationStructureDescriptor@.
module ObjC.Metal.MTLIndirectInstanceAccelerationStructureDescriptor
  ( MTLIndirectInstanceAccelerationStructureDescriptor
  , IsMTLIndirectInstanceAccelerationStructureDescriptor(..)
  , descriptor
  , instanceDescriptorBuffer
  , setInstanceDescriptorBuffer
  , instanceDescriptorBufferOffset
  , setInstanceDescriptorBufferOffset
  , instanceDescriptorStride
  , setInstanceDescriptorStride
  , maxInstanceCount
  , setMaxInstanceCount
  , instanceCountBuffer
  , setInstanceCountBuffer
  , instanceCountBufferOffset
  , setInstanceCountBufferOffset
  , instanceDescriptorType
  , setInstanceDescriptorType
  , motionTransformBuffer
  , setMotionTransformBuffer
  , motionTransformBufferOffset
  , setMotionTransformBufferOffset
  , maxMotionTransformCount
  , setMaxMotionTransformCount
  , motionTransformCountBuffer
  , setMotionTransformCountBuffer
  , motionTransformCountBufferOffset
  , setMotionTransformCountBufferOffset
  , instanceTransformationMatrixLayout
  , setInstanceTransformationMatrixLayout
  , motionTransformType
  , setMotionTransformType
  , motionTransformStride
  , setMotionTransformStride
  , descriptorSelector
  , instanceCountBufferOffsetSelector
  , instanceCountBufferSelector
  , instanceDescriptorBufferOffsetSelector
  , instanceDescriptorBufferSelector
  , instanceDescriptorStrideSelector
  , instanceDescriptorTypeSelector
  , instanceTransformationMatrixLayoutSelector
  , maxInstanceCountSelector
  , maxMotionTransformCountSelector
  , motionTransformBufferOffsetSelector
  , motionTransformBufferSelector
  , motionTransformCountBufferOffsetSelector
  , motionTransformCountBufferSelector
  , motionTransformStrideSelector
  , motionTransformTypeSelector
  , setInstanceCountBufferOffsetSelector
  , setInstanceCountBufferSelector
  , setInstanceDescriptorBufferOffsetSelector
  , setInstanceDescriptorBufferSelector
  , setInstanceDescriptorStrideSelector
  , setInstanceDescriptorTypeSelector
  , setInstanceTransformationMatrixLayoutSelector
  , setMaxInstanceCountSelector
  , setMaxMotionTransformCountSelector
  , setMotionTransformBufferOffsetSelector
  , setMotionTransformBufferSelector
  , setMotionTransformCountBufferOffsetSelector
  , setMotionTransformCountBufferSelector
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
descriptor :: IO (Id MTLIndirectInstanceAccelerationStructureDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLIndirectInstanceAccelerationStructureDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Buffer containing instance descriptors of the type specified by the instanceDescriptorType property
--
-- ObjC selector: @- instanceDescriptorBuffer@
instanceDescriptorBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
instanceDescriptorBuffer mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceDescriptorBufferSelector

-- | Buffer containing instance descriptors of the type specified by the instanceDescriptorType property
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setInstanceDescriptorBuffer mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorBufferSelector value

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- instanceDescriptorBufferOffset@
instanceDescriptorBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceDescriptorBufferOffsetSelector

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setInstanceDescriptorBufferOffset:@
setInstanceDescriptorBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorBufferOffsetSelector value

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceDescriptorStrideSelector

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorStrideSelector value

-- | Maximum number of instance descriptors
--
-- ObjC selector: @- maxInstanceCount@
maxInstanceCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxInstanceCount mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor maxInstanceCountSelector

-- | Maximum number of instance descriptors
--
-- ObjC selector: @- setMaxInstanceCount:@
setMaxInstanceCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxInstanceCount mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMaxInstanceCountSelector value

-- | Buffer containing the instance count as a uint32_t value. Value at build time must be less than or equal to maxInstanceCount.
--
-- ObjC selector: @- instanceCountBuffer@
instanceCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
instanceCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceCountBufferSelector

-- | Buffer containing the instance count as a uint32_t value. Value at build time must be less than or equal to maxInstanceCount.
--
-- ObjC selector: @- setInstanceCountBuffer:@
setInstanceCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setInstanceCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceCountBufferSelector value

-- | Offset into the instance count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- instanceCountBufferOffset@
instanceCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceCountBufferOffsetSelector

-- | Offset into the instance count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setInstanceCountBufferOffset:@
setInstanceCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceCountBufferOffsetSelector value

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeIndirect. Must be MTLAccelerationStructureInstanceDescriptorTypeIndirect or MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceDescriptorTypeSelector

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeIndirect. Must be MTLAccelerationStructureInstanceDescriptorTypeIndirect or MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorTypeSelector value

-- | Buffer containing transformation information for motion
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
motionTransformBuffer mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor motionTransformBufferSelector

-- | Buffer containing transformation information for motion
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setMotionTransformBuffer mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMotionTransformBufferSelector value

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- motionTransformBufferOffset@
motionTransformBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor motionTransformBufferOffsetSelector

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setMotionTransformBufferOffset:@
setMotionTransformBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMotionTransformBufferOffsetSelector value

-- | Maximum number of motion transforms
--
-- ObjC selector: @- maxMotionTransformCount@
maxMotionTransformCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxMotionTransformCount mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor maxMotionTransformCountSelector

-- | Maximum number of motion transforms
--
-- ObjC selector: @- setMaxMotionTransformCount:@
setMaxMotionTransformCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxMotionTransformCount mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMaxMotionTransformCountSelector value

-- | Buffer containing the motion transform count as a uint32_t value. Value at build time must be less than or equal to maxMotionTransformCount.
--
-- ObjC selector: @- motionTransformCountBuffer@
motionTransformCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
motionTransformCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor motionTransformCountBufferSelector

-- | Buffer containing the motion transform count as a uint32_t value. Value at build time must be less than or equal to maxMotionTransformCount.
--
-- ObjC selector: @- setMotionTransformCountBuffer:@
setMotionTransformCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setMotionTransformCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMotionTransformCountBufferSelector value

-- | Offset into the motion transform count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- motionTransformCountBufferOffset@
motionTransformCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor motionTransformCountBufferOffsetSelector

-- | Offset into the motion transform count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setMotionTransformCountBufferOffset:@
setMotionTransformCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMotionTransformCountBufferOffsetSelector value

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor instanceTransformationMatrixLayoutSelector

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setInstanceTransformationMatrixLayoutSelector value

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor motionTransformTypeSelector

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMotionTransformTypeSelector value

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtlIndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor motionTransformStrideSelector

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtlIndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtlIndirectInstanceAccelerationStructureDescriptor setMotionTransformStrideSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLIndirectInstanceAccelerationStructureDescriptor)
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

-- | @Selector@ for @maxInstanceCount@
maxInstanceCountSelector :: Selector '[] CULong
maxInstanceCountSelector = mkSelector "maxInstanceCount"

-- | @Selector@ for @setMaxInstanceCount:@
setMaxInstanceCountSelector :: Selector '[CULong] ()
setMaxInstanceCountSelector = mkSelector "setMaxInstanceCount:"

-- | @Selector@ for @instanceCountBuffer@
instanceCountBufferSelector :: Selector '[] RawId
instanceCountBufferSelector = mkSelector "instanceCountBuffer"

-- | @Selector@ for @setInstanceCountBuffer:@
setInstanceCountBufferSelector :: Selector '[RawId] ()
setInstanceCountBufferSelector = mkSelector "setInstanceCountBuffer:"

-- | @Selector@ for @instanceCountBufferOffset@
instanceCountBufferOffsetSelector :: Selector '[] CULong
instanceCountBufferOffsetSelector = mkSelector "instanceCountBufferOffset"

-- | @Selector@ for @setInstanceCountBufferOffset:@
setInstanceCountBufferOffsetSelector :: Selector '[CULong] ()
setInstanceCountBufferOffsetSelector = mkSelector "setInstanceCountBufferOffset:"

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

-- | @Selector@ for @maxMotionTransformCount@
maxMotionTransformCountSelector :: Selector '[] CULong
maxMotionTransformCountSelector = mkSelector "maxMotionTransformCount"

-- | @Selector@ for @setMaxMotionTransformCount:@
setMaxMotionTransformCountSelector :: Selector '[CULong] ()
setMaxMotionTransformCountSelector = mkSelector "setMaxMotionTransformCount:"

-- | @Selector@ for @motionTransformCountBuffer@
motionTransformCountBufferSelector :: Selector '[] RawId
motionTransformCountBufferSelector = mkSelector "motionTransformCountBuffer"

-- | @Selector@ for @setMotionTransformCountBuffer:@
setMotionTransformCountBufferSelector :: Selector '[RawId] ()
setMotionTransformCountBufferSelector = mkSelector "setMotionTransformCountBuffer:"

-- | @Selector@ for @motionTransformCountBufferOffset@
motionTransformCountBufferOffsetSelector :: Selector '[] CULong
motionTransformCountBufferOffsetSelector = mkSelector "motionTransformCountBufferOffset"

-- | @Selector@ for @setMotionTransformCountBufferOffset:@
setMotionTransformCountBufferOffsetSelector :: Selector '[CULong] ()
setMotionTransformCountBufferOffsetSelector = mkSelector "setMotionTransformCountBufferOffset:"

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

