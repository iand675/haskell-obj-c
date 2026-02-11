{-# LANGUAGE PatternSynonyms #-}
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
  , instanceDescriptorBufferSelector
  , setInstanceDescriptorBufferSelector
  , instanceDescriptorStrideSelector
  , setInstanceDescriptorStrideSelector
  , maxInstanceCountSelector
  , setMaxInstanceCountSelector
  , instanceCountBufferSelector
  , setInstanceCountBufferSelector
  , instanceDescriptorTypeSelector
  , setInstanceDescriptorTypeSelector
  , motionTransformBufferSelector
  , setMotionTransformBufferSelector
  , maxMotionTransformCountSelector
  , setMaxMotionTransformCountSelector
  , motionTransformCountBufferSelector
  , setMotionTransformCountBufferSelector
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
instanceDescriptorBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsgStret mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorBuffer") retMTL4BufferRange []

-- | Assigns a reference to a buffer containing instance descriptors for acceleration structures to reference.
--
-- This buffer conceptually represents an array of instance data. The specific format for the structs that comprise each entry depends on the value of the  ``instanceDescriptorType`` property.
--
-- You are responsible for ensuring the buffer address the range contains is not zero.
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setInstanceDescriptorBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorBuffer:") retVoid [argMTL4BufferRange value]

-- | Sets the stride, in bytes, between instance descriptors in the instance descriptor buffer.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorStride") retCULong []

-- | Sets the stride, in bytes, between instance descriptors in the instance descriptor buffer.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorStride:") retVoid [argCULong (fromIntegral value)]

-- | Controls the maximum number of instance descriptors the instance descriptor buffer can reference.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via a buffer reference in ``instanceCountBuffer``, is less than or equal to this number.
--
-- ObjC selector: @- maxInstanceCount@
maxInstanceCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxInstanceCount mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "maxInstanceCount") retCULong []

-- | Controls the maximum number of instance descriptors the instance descriptor buffer can reference.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via a buffer reference in ``instanceCountBuffer``, is less than or equal to this number.
--
-- ObjC selector: @- setMaxInstanceCount:@
setMaxInstanceCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxInstanceCount mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setMaxInstanceCount:") retVoid [argCULong (fromIntegral value)]

-- | Provides a reference to a buffer containing the number of instances in the instance descriptor buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via this buffer reference , is less than or equal to the value of property ``maxInstanceCount``.
--
-- ObjC selector: @- instanceCountBuffer@
instanceCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
instanceCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsgStret mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceCountBuffer") retMTL4BufferRange []

-- | Provides a reference to a buffer containing the number of instances in the instance descriptor buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of instances at build time, which you provide indirectly via this buffer reference , is less than or equal to the value of property ``maxInstanceCount``.
--
-- ObjC selector: @- setInstanceCountBuffer:@
setInstanceCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setInstanceCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceCountBuffer:") retVoid [argMTL4BufferRange value]

-- | Controls the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtL4IndirectInstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CULong -> MTLAccelerationStructureInstanceDescriptorType) $ sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorType") retCULong []

-- | Controls the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorType:") retVoid [argCULong (coerce value)]

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
motionTransformBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsgStret mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformBuffer") retMTL4BufferRange []

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setMotionTransformBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformBuffer:") retVoid [argMTL4BufferRange value]

-- | Controls the maximum number of motion transforms in the motion transform buffer.
--
-- You are responsible for ensuring that final number of motion transforms at build time that the buffer ``motionTransformCountBuffer`` references is less than or equal to this number.
--
-- ObjC selector: @- maxMotionTransformCount@
maxMotionTransformCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxMotionTransformCount mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "maxMotionTransformCount") retCULong []

-- | Controls the maximum number of motion transforms in the motion transform buffer.
--
-- You are responsible for ensuring that final number of motion transforms at build time that the buffer ``motionTransformCountBuffer`` references is less than or equal to this number.
--
-- ObjC selector: @- setMaxMotionTransformCount:@
setMaxMotionTransformCount :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxMotionTransformCount mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setMaxMotionTransformCount:") retVoid [argCULong (fromIntegral value)]

-- | Associates a buffer reference containing the number of motion transforms in the motion transform buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of motion transforms at build time in the buffer this property references is less than or equal to the value of property ``maxMotionTransformCount``.
--
-- ObjC selector: @- motionTransformCountBuffer@
motionTransformCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
motionTransformCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsgStret mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformCountBuffer") retMTL4BufferRange []

-- | Associates a buffer reference containing the number of motion transforms in the motion transform buffer, formatted as a 32-bit unsigned integer.
--
-- You are responsible for ensuring that the final number of motion transforms at build time in the buffer this property references is less than or equal to the value of property ``maxMotionTransformCount``.
--
-- ObjC selector: @- setMotionTransformCountBuffer:@
setMotionTransformCountBuffer :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setMotionTransformCountBuffer mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformCountBuffer:") retVoid [argMTL4BufferRange value]

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtL4IndirectInstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CLong -> MTLMatrixLayout) $ sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceTransformationMatrixLayout") retCLong []

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceTransformationMatrixLayout:") retVoid [argCLong (coerce value)]

-- | Sets the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtL4IndirectInstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CLong -> MTLTransformType) $ sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformType") retCLong []

-- | Sets the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformType:") retVoid [argCLong (coerce value)]

-- | Sets the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtL4IndirectInstanceAccelerationStructureDescriptor  =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformStride") retCULong []

-- | Sets the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTL4IndirectInstanceAccelerationStructureDescriptor mtL4IndirectInstanceAccelerationStructureDescriptor => mtL4IndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtL4IndirectInstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4IndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformStride:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instanceDescriptorBuffer@
instanceDescriptorBufferSelector :: Selector
instanceDescriptorBufferSelector = mkSelector "instanceDescriptorBuffer"

-- | @Selector@ for @setInstanceDescriptorBuffer:@
setInstanceDescriptorBufferSelector :: Selector
setInstanceDescriptorBufferSelector = mkSelector "setInstanceDescriptorBuffer:"

-- | @Selector@ for @instanceDescriptorStride@
instanceDescriptorStrideSelector :: Selector
instanceDescriptorStrideSelector = mkSelector "instanceDescriptorStride"

-- | @Selector@ for @setInstanceDescriptorStride:@
setInstanceDescriptorStrideSelector :: Selector
setInstanceDescriptorStrideSelector = mkSelector "setInstanceDescriptorStride:"

-- | @Selector@ for @maxInstanceCount@
maxInstanceCountSelector :: Selector
maxInstanceCountSelector = mkSelector "maxInstanceCount"

-- | @Selector@ for @setMaxInstanceCount:@
setMaxInstanceCountSelector :: Selector
setMaxInstanceCountSelector = mkSelector "setMaxInstanceCount:"

-- | @Selector@ for @instanceCountBuffer@
instanceCountBufferSelector :: Selector
instanceCountBufferSelector = mkSelector "instanceCountBuffer"

-- | @Selector@ for @setInstanceCountBuffer:@
setInstanceCountBufferSelector :: Selector
setInstanceCountBufferSelector = mkSelector "setInstanceCountBuffer:"

-- | @Selector@ for @instanceDescriptorType@
instanceDescriptorTypeSelector :: Selector
instanceDescriptorTypeSelector = mkSelector "instanceDescriptorType"

-- | @Selector@ for @setInstanceDescriptorType:@
setInstanceDescriptorTypeSelector :: Selector
setInstanceDescriptorTypeSelector = mkSelector "setInstanceDescriptorType:"

-- | @Selector@ for @motionTransformBuffer@
motionTransformBufferSelector :: Selector
motionTransformBufferSelector = mkSelector "motionTransformBuffer"

-- | @Selector@ for @setMotionTransformBuffer:@
setMotionTransformBufferSelector :: Selector
setMotionTransformBufferSelector = mkSelector "setMotionTransformBuffer:"

-- | @Selector@ for @maxMotionTransformCount@
maxMotionTransformCountSelector :: Selector
maxMotionTransformCountSelector = mkSelector "maxMotionTransformCount"

-- | @Selector@ for @setMaxMotionTransformCount:@
setMaxMotionTransformCountSelector :: Selector
setMaxMotionTransformCountSelector = mkSelector "setMaxMotionTransformCount:"

-- | @Selector@ for @motionTransformCountBuffer@
motionTransformCountBufferSelector :: Selector
motionTransformCountBufferSelector = mkSelector "motionTransformCountBuffer"

-- | @Selector@ for @setMotionTransformCountBuffer:@
setMotionTransformCountBufferSelector :: Selector
setMotionTransformCountBufferSelector = mkSelector "setMotionTransformCountBuffer:"

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

