{-# LANGUAGE PatternSynonyms #-}
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
  , instanceDescriptorBufferSelector
  , setInstanceDescriptorBufferSelector
  , instanceDescriptorBufferOffsetSelector
  , setInstanceDescriptorBufferOffsetSelector
  , instanceDescriptorStrideSelector
  , setInstanceDescriptorStrideSelector
  , maxInstanceCountSelector
  , setMaxInstanceCountSelector
  , instanceCountBufferSelector
  , setInstanceCountBufferSelector
  , instanceCountBufferOffsetSelector
  , setInstanceCountBufferOffsetSelector
  , instanceDescriptorTypeSelector
  , setInstanceDescriptorTypeSelector
  , motionTransformBufferSelector
  , setMotionTransformBufferSelector
  , motionTransformBufferOffsetSelector
  , setMotionTransformBufferOffsetSelector
  , maxMotionTransformCountSelector
  , setMaxMotionTransformCountSelector
  , motionTransformCountBufferSelector
  , setMotionTransformCountBufferSelector
  , motionTransformCountBufferOffsetSelector
  , setMotionTransformCountBufferOffsetSelector
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
descriptor :: IO (Id MTLIndirectInstanceAccelerationStructureDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLIndirectInstanceAccelerationStructureDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Buffer containing instance descriptors of the type specified by the instanceDescriptorType property
--
-- ObjC selector: @- instanceDescriptorBuffer@
instanceDescriptorBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
instanceDescriptorBuffer mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorBuffer") (retPtr retVoid) []

-- | Buffer containing instance descriptors of the type specified by the instanceDescriptorType property
--
-- ObjC selector: @- setInstanceDescriptorBuffer:@
setInstanceDescriptorBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setInstanceDescriptorBuffer mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- instanceDescriptorBufferOffset@
instanceDescriptorBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorBufferOffset") retCULong []

-- | Offset into the instance descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setInstanceDescriptorBufferOffset:@
setInstanceDescriptorBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorBufferOffset:") retVoid [argCULong value]

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- instanceDescriptorStride@
instanceDescriptorStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceDescriptorStride mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorStride") retCULong []

-- | Stride, in bytes, between instance descriptors in the instance descriptor buffer. Must be at least the size of the instance descriptor type and must be a multiple of 4 bytes. Defaults to the size of the instance descriptor type.
--
-- ObjC selector: @- setInstanceDescriptorStride:@
setInstanceDescriptorStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceDescriptorStride mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorStride:") retVoid [argCULong value]

-- | Maximum number of instance descriptors
--
-- ObjC selector: @- maxInstanceCount@
maxInstanceCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxInstanceCount mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "maxInstanceCount") retCULong []

-- | Maximum number of instance descriptors
--
-- ObjC selector: @- setMaxInstanceCount:@
setMaxInstanceCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxInstanceCount mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMaxInstanceCount:") retVoid [argCULong value]

-- | Buffer containing the instance count as a uint32_t value. Value at build time must be less than or equal to maxInstanceCount.
--
-- ObjC selector: @- instanceCountBuffer@
instanceCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
instanceCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceCountBuffer") (retPtr retVoid) []

-- | Buffer containing the instance count as a uint32_t value. Value at build time must be less than or equal to maxInstanceCount.
--
-- ObjC selector: @- setInstanceCountBuffer:@
setInstanceCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setInstanceCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceCountBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset into the instance count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- instanceCountBufferOffset@
instanceCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
instanceCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceCountBufferOffset") retCULong []

-- | Offset into the instance count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setInstanceCountBufferOffset:@
setInstanceCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setInstanceCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceCountBufferOffset:") retVoid [argCULong value]

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeIndirect. Must be MTLAccelerationStructureInstanceDescriptorTypeIndirect or MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion.
--
-- ObjC selector: @- instanceDescriptorType@
instanceDescriptorType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO MTLAccelerationStructureInstanceDescriptorType
instanceDescriptorType mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (coerce :: CULong -> MTLAccelerationStructureInstanceDescriptorType) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceDescriptorType") retCULong []

-- | Type of instance descriptor in the instance descriptor buffer. Defaults to MTLAccelerationStructureInstanceDescriptorTypeIndirect. Must be MTLAccelerationStructureInstanceDescriptorTypeIndirect or MTLAccelerationStructureInstanceDescriptorTypeIndirectMotion.
--
-- ObjC selector: @- setInstanceDescriptorType:@
setInstanceDescriptorType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> MTLAccelerationStructureInstanceDescriptorType -> IO ()
setInstanceDescriptorType mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceDescriptorType:") retVoid [argCULong (coerce value)]

-- | Buffer containing transformation information for motion
--
-- ObjC selector: @- motionTransformBuffer@
motionTransformBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
motionTransformBuffer mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformBuffer") (retPtr retVoid) []

-- | Buffer containing transformation information for motion
--
-- ObjC selector: @- setMotionTransformBuffer:@
setMotionTransformBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setMotionTransformBuffer mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- motionTransformBufferOffset@
motionTransformBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformBufferOffset") retCULong []

-- | Offset into the instance motion descriptor buffer. Must be a multiple of 64 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setMotionTransformBufferOffset:@
setMotionTransformBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformBufferOffset:") retVoid [argCULong value]

-- | Maximum number of motion transforms
--
-- ObjC selector: @- maxMotionTransformCount@
maxMotionTransformCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
maxMotionTransformCount mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "maxMotionTransformCount") retCULong []

-- | Maximum number of motion transforms
--
-- ObjC selector: @- setMaxMotionTransformCount:@
setMaxMotionTransformCount :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMaxMotionTransformCount mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMaxMotionTransformCount:") retVoid [argCULong value]

-- | Buffer containing the motion transform count as a uint32_t value. Value at build time must be less than or equal to maxMotionTransformCount.
--
-- ObjC selector: @- motionTransformCountBuffer@
motionTransformCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO RawId
motionTransformCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformCountBuffer") (retPtr retVoid) []

-- | Buffer containing the motion transform count as a uint32_t value. Value at build time must be less than or equal to maxMotionTransformCount.
--
-- ObjC selector: @- setMotionTransformCountBuffer:@
setMotionTransformCountBuffer :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> RawId -> IO ()
setMotionTransformCountBuffer mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformCountBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset into the motion transform count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- motionTransformCountBufferOffset@
motionTransformCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformCountBufferOffset") retCULong []

-- | Offset into the motion transform count buffer. Must be a multiple of 4 bytes and must be aligned to the platform's buffer offset alignment.
--
-- ObjC selector: @- setMotionTransformCountBufferOffset:@
setMotionTransformCountBufferOffset :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformCountBufferOffset mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformCountBufferOffset:") retVoid [argCULong value]

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- instanceTransformationMatrixLayout@
instanceTransformationMatrixLayout :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO MTLMatrixLayout
instanceTransformationMatrixLayout mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (coerce :: CLong -> MTLMatrixLayout) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "instanceTransformationMatrixLayout") retCLong []

-- | Matrix layout of the transformation matrices in the instance descriptors in the instance descriptor buffer and the transformation matrices in the transformation matrix buffer. Defaults to MTLMatrixLayoutColumnMajor.
--
-- ObjC selector: @- setInstanceTransformationMatrixLayout:@
setInstanceTransformationMatrixLayout :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> MTLMatrixLayout -> IO ()
setInstanceTransformationMatrixLayout mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setInstanceTransformationMatrixLayout:") retVoid [argCLong (coerce value)]

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- motionTransformType@
motionTransformType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO MTLTransformType
motionTransformType mtlIndirectInstanceAccelerationStructureDescriptor  =
    fmap (coerce :: CLong -> MTLTransformType) $ sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformType") retCLong []

-- | Type of motion transforms. Defaults to MTLTransformTypePackedFloat4x3.
--
-- ObjC selector: @- setMotionTransformType:@
setMotionTransformType :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> MTLTransformType -> IO ()
setMotionTransformType mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformType:") retVoid [argCLong (coerce value)]

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- motionTransformStride@
motionTransformStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> IO CULong
motionTransformStride mtlIndirectInstanceAccelerationStructureDescriptor  =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "motionTransformStride") retCULong []

-- | Motion transform stride. Defaults to 0, indicating that transforms are tightly packed according to the motion transform type.
--
-- ObjC selector: @- setMotionTransformStride:@
setMotionTransformStride :: IsMTLIndirectInstanceAccelerationStructureDescriptor mtlIndirectInstanceAccelerationStructureDescriptor => mtlIndirectInstanceAccelerationStructureDescriptor -> CULong -> IO ()
setMotionTransformStride mtlIndirectInstanceAccelerationStructureDescriptor  value =
    sendMsg mtlIndirectInstanceAccelerationStructureDescriptor (mkSelector "setMotionTransformStride:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @instanceDescriptorBuffer@
instanceDescriptorBufferSelector :: Selector
instanceDescriptorBufferSelector = mkSelector "instanceDescriptorBuffer"

-- | @Selector@ for @setInstanceDescriptorBuffer:@
setInstanceDescriptorBufferSelector :: Selector
setInstanceDescriptorBufferSelector = mkSelector "setInstanceDescriptorBuffer:"

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

-- | @Selector@ for @instanceCountBufferOffset@
instanceCountBufferOffsetSelector :: Selector
instanceCountBufferOffsetSelector = mkSelector "instanceCountBufferOffset"

-- | @Selector@ for @setInstanceCountBufferOffset:@
setInstanceCountBufferOffsetSelector :: Selector
setInstanceCountBufferOffsetSelector = mkSelector "setInstanceCountBufferOffset:"

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

-- | @Selector@ for @motionTransformBufferOffset@
motionTransformBufferOffsetSelector :: Selector
motionTransformBufferOffsetSelector = mkSelector "motionTransformBufferOffset"

-- | @Selector@ for @setMotionTransformBufferOffset:@
setMotionTransformBufferOffsetSelector :: Selector
setMotionTransformBufferOffsetSelector = mkSelector "setMotionTransformBufferOffset:"

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

-- | @Selector@ for @motionTransformCountBufferOffset@
motionTransformCountBufferOffsetSelector :: Selector
motionTransformCountBufferOffsetSelector = mkSelector "motionTransformCountBufferOffset"

-- | @Selector@ for @setMotionTransformCountBufferOffset:@
setMotionTransformCountBufferOffsetSelector :: Selector
setMotionTransformCountBufferOffsetSelector = mkSelector "setMotionTransformCountBufferOffset:"

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

