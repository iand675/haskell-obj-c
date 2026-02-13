{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for an instance acceleration structure.
--
-- An instance acceleration structure references other acceleration structures, and provides the ability to "instantiate" them multiple times, each one with potentially a different transformation matrix.
--
-- You specify the properties of the instances in the acceleration structure this descriptor builds by providing a buffer of @structs@ via its ``instanceDescriptorBuffer`` property.
--
-- Use a ``MTLResidencySet`` to mark residency of all buffers and acceleration structures this descriptor references when you build this acceleration structure.
--
-- Generated bindings for @MTL4InstanceAccelerationStructureDescriptor@.
module ObjC.Metal.MTL4InstanceAccelerationStructureDescriptor
  ( MTL4InstanceAccelerationStructureDescriptor
  , IsMTL4InstanceAccelerationStructureDescriptor(..)
  , instanceDescriptorBuffer
  , setInstanceDescriptorBuffer
  , instanceDescriptorStride
  , setInstanceDescriptorStride
  , instanceCount
  , setInstanceCount
  , instanceDescriptorType
  , setInstanceDescriptorType
  , motionTransformBuffer
  , setMotionTransformBuffer
  , motionTransformCount
  , setMotionTransformCount
  , instanceTransformationMatrixLayout
  , setInstanceTransformationMatrixLayout
  , motionTransformType
  , setMotionTransformType
  , motionTransformStride
  , setMotionTransformStride
  , instanceCountSelector
  , instanceDescriptorBufferSelector
  , instanceDescriptorStrideSelector
  , instanceDescriptorTypeSelector
  , instanceTransformationMatrixLayoutSelector
  , motionTransformBufferSelector
  , motionTransformCountSelector
  , motionTransformStrideSelector
  , motionTransformTypeSelector
  , setInstanceCountSelector
  , setInstanceDescriptorBufferSelector
  , setInstanceDescriptorStrideSelector
  , setInstanceDescriptorTypeSelector
  , setInstanceTransformationMatrixLayoutSelector
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
instanceDescriptorBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
instanceDescriptorBuffer mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor instanceDescriptorBufferSelector

-- | Assigns a reference to a buffer containing instance descriptors for acceleration structures to reference.
--
-- This buffer conceptually represents an array of instance data. The specific format for the structs that comprise each entry depends on the value of the  ``instanceDescriptorType`` property.
--
-- You are responsible for ensuring the buffer address the range contains is not zero.
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setInstanceDescriptorBuffer mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setInstanceDescriptorBufferSelector value

-- | Sets the stride, in bytes, between instance descriptors the instance descriptor buffer references.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor instanceDescriptorStrideSelector

-- | Sets the stride, in bytes, between instance descriptors the instance descriptor buffer references.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setInstanceDescriptorStrideSelector value

-- | Controls the number of instance descriptors in the instance descriptor buffer references.
--
-- ObjC selector: @- instanceCount@
instanceCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
instanceCount mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor instanceCountSelector

-- | Controls the number of instance descriptors in the instance descriptor buffer references.
--
-- ObjC selector: @- setInstanceCount:@
setInstanceCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceCount mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setInstanceCountSelector value

-- | Sets the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor instanceDescriptorTypeSelector

-- | Sets the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setInstanceDescriptorTypeSelector value

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
motionTransformBuffer mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor motionTransformBufferSelector

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setMotionTransformBuffer mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setMotionTransformBufferSelector value

-- | Controls the total number of motion transforms in the motion transform buffer.
--
-- ObjC selector: @- motionTransformCount@
motionTransformCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
motionTransformCount mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor motionTransformCountSelector

-- | Controls the total number of motion transforms in the motion transform buffer.
--
-- ObjC selector: @- setMotionTransformCount:@
setMotionTransformCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformCount mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setMotionTransformCountSelector value

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor instanceTransformationMatrixLayoutSelector

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setInstanceTransformationMatrixLayoutSelector value

-- | Controls the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor motionTransformTypeSelector

-- | Controls the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setMotionTransformTypeSelector value

-- | Specify the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtL4InstanceAccelerationStructureDescriptor =
  sendMessage mtL4InstanceAccelerationStructureDescriptor motionTransformStrideSelector

-- | Specify the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtL4InstanceAccelerationStructureDescriptor value =
  sendMessage mtL4InstanceAccelerationStructureDescriptor setMotionTransformStrideSelector value

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

-- | @Selector@ for @instanceCount@
instanceCountSelector :: Selector '[] CULong
instanceCountSelector = mkSelector "instanceCount"

-- | @Selector@ for @setInstanceCount:@
setInstanceCountSelector :: Selector '[CULong] ()
setInstanceCountSelector = mkSelector "setInstanceCount:"

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

