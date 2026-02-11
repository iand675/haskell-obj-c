{-# LANGUAGE PatternSynonyms #-}
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
  , instanceDescriptorBufferSelector
  , setInstanceDescriptorBufferSelector
  , instanceDescriptorStrideSelector
  , setInstanceDescriptorStrideSelector
  , instanceCountSelector
  , setInstanceCountSelector
  , instanceDescriptorTypeSelector
  , setInstanceDescriptorTypeSelector
  , motionTransformBufferSelector
  , setMotionTransformBufferSelector
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
instanceDescriptorBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
instanceDescriptorBuffer mtL4InstanceAccelerationStructureDescriptor  =
  sendMsgStret mtL4InstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorBuffer") retMTL4BufferRange []

-- | Assigns a reference to a buffer containing instance descriptors for acceleration structures to reference.
--
-- This buffer conceptually represents an array of instance data. The specific format for the structs that comprise each entry depends on the value of the  ``instanceDescriptorType`` property.
--
-- You are responsible for ensuring the buffer address the range contains is not zero.
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setInstanceDescriptorBuffer mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorBuffer:") retVoid [argMTL4BufferRange value]

-- | Sets the stride, in bytes, between instance descriptors the instance descriptor buffer references.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtL4InstanceAccelerationStructureDescriptor  =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorStride") retCULong []

-- | Sets the stride, in bytes, between instance descriptors the instance descriptor buffer references.
--
-- You are responsible for ensuring this stride is at least the size of the structure type corresponding to the instance descriptor type and a multiple of 4 bytes.
--
-- Defaults to @0@, indicating the instance descriptors are tightly packed.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorStride:") retVoid [argCULong (fromIntegral value)]

-- | Controls the number of instance descriptors in the instance descriptor buffer references.
--
-- ObjC selector: @- instanceCount@
instanceCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
instanceCount mtL4InstanceAccelerationStructureDescriptor  =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "instanceCount") retCULong []

-- | Controls the number of instance descriptors in the instance descriptor buffer references.
--
-- ObjC selector: @- setInstanceCount:@
setInstanceCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceCount mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setInstanceCount:") retVoid [argCULong (fromIntegral value)]

-- | Sets the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtL4InstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CULong -> MTLAccelerationStructureInstanceDescriptorType) $ sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorType") retCULong []

-- | Sets the type of instance descriptor that the instance descriptor buffer references.
--
-- This value determines the layout Metal expects for the structs the instance descriptor buffer contains.
--
-- Defaults to @MTLAccelerationStructureInstanceDescriptorTypeIndirect@. Valid values for this property are @MTLAccelerationStructureInstanceDescriptorTypeIndirect@ or @MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion@.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorType:") retVoid [argCULong (coerce value)]

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTL4BufferRange
motionTransformBuffer mtL4InstanceAccelerationStructureDescriptor  =
  sendMsgStret mtL4InstanceAccelerationStructureDescriptor (mkSelector "motionTransformBuffer") retMTL4BufferRange []

-- | A buffer containing transformation information for instance motion keyframes, formatted according to the motion transform type.
--
-- Each instance can have a different number of keyframes that you configure via individual instance descriptors.
--
-- You are responsible for ensuring the buffer address the range references is not zero when using motion instance descriptors.
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTL4BufferRange -> IO ()
setMotionTransformBuffer mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformBuffer:") retVoid [argMTL4BufferRange value]

-- | Controls the total number of motion transforms in the motion transform buffer.
--
-- ObjC selector: @- motionTransformCount@
motionTransformCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
motionTransformCount mtL4InstanceAccelerationStructureDescriptor  =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "motionTransformCount") retCULong []

-- | Controls the total number of motion transforms in the motion transform buffer.
--
-- ObjC selector: @- setMotionTransformCount:@
setMotionTransformCount :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformCount mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformCount:") retVoid [argCULong (fromIntegral value)]

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtL4InstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CLong -> MTLMatrixLayout) $ sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "instanceTransformationMatrixLayout") retCLong []

-- | Specifies the layout for the transformation matrices in the instance descriptor buffer and the motion transformation matrix buffer.
--
-- Metal interprets the value of this property as the layout for the buffers that both ``instanceDescriptorBuffer`` and ``motionTransformBuffer`` reference.
--
-- Defaults to @MTLMatrixLayoutColumnMajor@.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setInstanceTransformationMatrixLayout:") retVoid [argCLong (coerce value)]

-- | Controls the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtL4InstanceAccelerationStructureDescriptor  =
  fmap (coerce :: CLong -> MTLTransformType) $ sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "motionTransformType") retCLong []

-- | Controls the type of motion transforms, either as a matrix or individual components.
--
-- Defaults to @MTLTransformTypePackedFloat4x3@. Using a @MTLTransformTypeComponent@ allows you to represent the rotation by a quaternion (instead as of part of the matrix), allowing for correct motion interpolation.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformType:") retVoid [argCLong (coerce value)]

-- | Specify the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtL4InstanceAccelerationStructureDescriptor  =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "motionTransformStride") retCULong []

-- | Specify the stride for motion transform.
--
-- Defaults to @0@, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTL4InstanceAccelerationStructureDescriptor mtL4InstanceAccelerationStructureDescriptor => mtL4InstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtL4InstanceAccelerationStructureDescriptor  value =
  sendMsg mtL4InstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformStride:") retVoid [argCULong (fromIntegral value)]

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

-- | @Selector@ for @motionTransformBuffer@
motionTransformBufferSelector :: Selector
motionTransformBufferSelector = mkSelector "motionTransformBuffer"

-- | @Selector@ for @setMotionTransformBuffer:@
setMotionTransformBufferSelector :: Selector
setMotionTransformBufferSelector = mkSelector "setMotionTransformBuffer:"

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

