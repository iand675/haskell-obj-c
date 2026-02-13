{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for an "indirect" instance acceleration structure that allows providing the instance count and motion transform count indirectly, through buffer references.
--
-- An instance acceleration structure references other acceleration structures, and provides the ability to "instantiate" them multiple times, each one with potentially a different transformation matrix.
--
-- You specify the properties of the instances in the acceleration structure this descriptor builds by providing a buffer of @structs@ via its ``instanceDescriptorBuffer`` property.
--
-- Compared to ``MTL4InstanceAccelerationStructureDescriptor``, this descriptor allows you to provide the number of instances it references indirectly through a buffer reference, as well as the number of motion transforms.
--
-- This enables you to determine these counts indirectly in the GPU timeline via a compute pipeline. Metal needs only to know the maximum possible number of instances and motion transforms to support, which you specify via the ``maxInstanceCount`` and ``maxMotionTransformCount`` properties.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers and acceleration structures this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4IndirectInstanceAccelerationStructureDescriptor@.
module ObjC.Metal.MTL4IndirectInstanceAccelerationStructureDescriptor
  ( MTL4IndirectInstanceAccelerationStructureDescriptor
  , IsMTL4IndirectInstanceAccelerationStructureDescriptor(..)
  , instanceDescriptorBuffer
  , setInstanceDescriptorBuffer
  , instanceDescriptorStride
  , setInstanceDescriptorStride
  , maxInstanceCount
  , setMaxInstanceCount
  , instanceCountBuffer
  , setInstanceCountBuffer
  , instanceDescriptorType
  , setInstanceDescriptorType
  , motionTransformBuffer
  , setMotionTransformBuffer
  , maxMotionTransformCount
  , setMaxMotionTransformCount
  , motionTransformCountBuffer
  , setMotionTransformCountBuffer
  , instanceTransformationMatrixLayout
  , setInstanceTransformationMatrixLayout
  , motionTransformType
  , setMotionTransformType
  , motionTransformStride
  , setMotionTransformStride
  , instanceCountBufferSelector
  , instanceDescriptorBufferSelector
  , instanceDescriptorStrideSelector
  , instanceDescriptorTypeSelector
  , instanceTransformationMatrixLayoutSelector
  , maxInstanceCountSelector
  , maxMotionTransformCountSelector
  , motionTransformBufferSelector
  , motionTransformCountBufferSelector
  , motionTransformStrideSelector
  , motionTransformTypeSelector
  , setInstanceCountBufferSelector
  , setInstanceDescriptorBufferSelector
  , setInstanceDescriptorStrideSelector
  , setInstanceDescriptorTypeSelector
  , setInstanceTransformationMatrixLayoutSelector
  , setMaxInstanceCountSelector
  , setMaxMotionTransformCountSelector
  , setMotionTransformBufferSelector
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
import ObjC.Metal.Internal.Structs
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Assigns a reference to a buffer containing instance descriptors for acceleration structures to reference.
--
-- This buffer conceptually represents an array of instance data. The specific format for the structs that comprise each entry depends on the value of the  ``instanceDescriptorType`` property.
--
-- You are responsible for ensuring the buffer address the range contains is not zero.
--
-- ObjC selector: @- instanceDescriptorBuffer@
instanceDescriptorBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
instanceDescriptorBuffer mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor instanceDescriptorBufferSelector

-- | Assigns a reference to a buffer containing instance descriptors for acceleration structures to reference.
--
-- This buffer conceptually represents an array of instance data. The specific format for the structs that comprise each entry depends on the value of the  ``instanceDescriptorType`` property.
--
-- You are responsible for ensuring the buffer address the range contains is not zero.
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setInstanceDescriptorBuffer mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorBufferSelector value

-- | Sets the stride, in bytes, between instance descriptors in the instance descriptor buffer.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor instanceDescriptorStrideSelector

-- | Sets the stride, in bytes, between instance descriptors in the instance descriptor buffer.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorStrideSelector value

-- | Controls the maximum number of instance descriptors the instance descriptor buffer can reference.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via a buffer reference in ``instanceCountBuffer``, is less than or equal to this number.
--
-- ObjC selector: @- maxInstanceCount@
maxInstanceCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxInstanceCount mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor maxInstanceCountSelector

-- | Controls the maximum number of instance descriptors the instance descriptor buffer can reference.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via a buffer reference in ``instanceCountBuffer``, is less than or equal to this number.
--
-- ObjC selector: @- setMaxInstanceCount:@
setMaxInstanceCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxInstanceCount mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setMaxInstanceCountSelector value

-- | Provides a reference to a buffer containing the number of instances in the instance descriptor buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via this buffer reference , is less than or equal to the value of property ``maxInstanceCount``.
--
-- ObjC selector: @- instanceCountBuffer@
instanceCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
instanceCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor instanceCountBufferSelector

-- | Provides a reference to a buffer containing the number of instances in the instance descriptor buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via this buffer reference , is less than or equal to the value of property ``maxInstanceCount``.
--
-- ObjC selector: @- setInstanceCountBuffer:@
setInstanceCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setInstanceCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setInstanceCountBufferSelector value

-- | Controls the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor instanceDescriptorTypeSelector

-- | Controls the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setInstanceDescriptorTypeSelector value

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
motionTransformBuffer mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor motionTransformBufferSelector

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setMotionTransformBuffer mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setMotionTransformBufferSelector value

-- | Controls the maximum number of motion transforms in the motion transform buffer.
--
-- You are responsible for ensuring that final number of motion transforms at build time that the buffer ``motionTransformCountBuffer`` references is less than or equal to this number.
--
-- ObjC selector: @- maxMotionTransformCount@
maxMotionTransformCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxMotionTransformCount mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor maxMotionTransformCountSelector

-- | Controls the maximum number of motion transforms in the motion transform buffer.
--
-- You are responsible for ensuring that final number of motion transforms at build time that the buffer ``motionTransformCountBuffer`` references is less than or equal to this number.
--
-- ObjC selector: @- setMaxMotionTransformCount:@
setMaxMotionTransformCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxMotionTransformCount mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setMaxMotionTransformCountSelector value

-- | Associates a buffer reference containing the number of motion transforms in the motion transform buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of motion transforms at build time in the buffer this property references is less than or equal to the value of property ``maxMotionTransformCount``.
--
-- ObjC selector: @- motionTransformCountBuffer@
motionTransformCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
motionTransformCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor motionTransformCountBufferSelector

-- | Associates a buffer reference containing the number of motion transforms in the motion transform buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of motion transforms at build time in the buffer this property references is less than or equal to the value of property ``maxMotionTransformCount``.
--
-- ObjC selector: @- setMotionTransformCountBuffer:@
setMotionTransformCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setMotionTransformCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setMotionTransformCountBufferSelector value

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor instanceTransformationMatrixLayoutSelector

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setInstanceTransformationMatrixLayoutSelector value

-- | Sets the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor motionTransformTypeSelector

-- | Sets the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setMotionTransformTypeSelector value

-- | Sets the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtL4IndirectInstanceAccelerationStructureDescriptor =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor motionTransformStrideSelector

-- | Sets the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtL4IndirectInstanceAccelerationStructureDescriptor value =
  sendMessage mtL4IndirectInstanceAccelerationStructureDescriptor setMotionTransformStrideSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instanceDescriptorBuffer@
instanceDescriptorBufferSelector :: Selector '[] MTL4BufferRange
instanceDescriptorBufferSelector = mkSelector "instanceDescriptorBuffer"

-- | @Selector@ for @setInstanceDescriptorBuffer:@
setInstanceDescriptorBufferSelector :: Selector '[MTL4BufferRange] ()
setInstanceDescriptorBufferSelector = mkSelector "setInstanceDescriptorBuffer:"

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
instanceCountBufferSelector :: Selector '[] MTL4BufferRange
instanceCountBufferSelector = mkSelector "instanceCountBuffer"

-- | @Selector@ for @setInstanceCountBuffer:@
setInstanceCountBufferSelector :: Selector '[MTL4BufferRange] ()
setInstanceCountBufferSelector = mkSelector "setInstanceCountBuffer:"

-- | @Selector@ for @instanceDescriptorType@
instanceDescriptorTypeSelector :: Selector '[] MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorTypeSelector = mkSelector "instanceDescriptorType"

-- | @Selector@ for @setInstanceDescriptorType:@
setInstanceDescriptorTypeSelector :: Selector '[MTLAccelerationStructureInstanceDescriptorType] ()
setInstanceDescriptorTypeSelector = mkSelector "setInstanceDescriptorType:"

-- | @Selector@ for @motionTransformBuffer@
motionTransformBufferSelector :: Selector '[] MTL4BufferRange
motionTransformBufferSelector = mkSelector "motionTransformBuffer"

-- | @Selector@ for @setMotionTransformBuffer:@
setMotionTransformBufferSelector :: Selector '[MTL4BufferRange] ()
setMotionTransformBufferSelector = mkSelector "setMotionTransformBuffer:"

-- | @Selector@ for @maxMotionTransformCount@
maxMotionTransformCountSelector :: Selector '[] CULong
maxMotionTransformCountSelector = mkSelector "maxMotionTransformCount"

-- | @Selector@ for @setMaxMotionTransformCount:@
setMaxMotionTransformCountSelector :: Selector '[CULong] ()
setMaxMotionTransformCountSelector = mkSelector "setMaxMotionTransformCount:"

-- | @Selector@ for @motionTransformCountBuffer@
motionTransformCountBufferSelector :: Selector '[] MTL4BufferRange
motionTransformCountBufferSelector = mkSelector "motionTransformCountBuffer"

-- | @Selector@ for @setMotionTransformCountBuffer:@
setMotionTransformCountBufferSelector :: Selector '[MTL4BufferRange] ()
setMotionTransformCountBufferSelector = mkSelector "setMotionTransformCountBuffer:"

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

