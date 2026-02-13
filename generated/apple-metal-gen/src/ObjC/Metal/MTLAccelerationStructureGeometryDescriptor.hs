{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for all geometry descriptors. Do not use this class directly. Use one of the derived classes instead.
--
-- Generated bindings for @MTLAccelerationStructureGeometryDescriptor@.
module ObjC.Metal.MTLAccelerationStructureGeometryDescriptor
  ( MTLAccelerationStructureGeometryDescriptor
  , IsMTLAccelerationStructureGeometryDescriptor(..)
  , intersectionFunctionTableOffset
  , setIntersectionFunctionTableOffset
  , opaque
  , setOpaque
  , allowDuplicateIntersectionFunctionInvocation
  , setAllowDuplicateIntersectionFunctionInvocation
  , label
  , setLabel
  , primitiveDataBuffer
  , setPrimitiveDataBuffer
  , primitiveDataBufferOffset
  , setPrimitiveDataBufferOffset
  , primitiveDataStride
  , setPrimitiveDataStride
  , primitiveDataElementSize
  , setPrimitiveDataElementSize
  , allowDuplicateIntersectionFunctionInvocationSelector
  , intersectionFunctionTableOffsetSelector
  , labelSelector
  , opaqueSelector
  , primitiveDataBufferOffsetSelector
  , primitiveDataBufferSelector
  , primitiveDataElementSizeSelector
  , primitiveDataStrideSelector
  , setAllowDuplicateIntersectionFunctionInvocationSelector
  , setIntersectionFunctionTableOffsetSelector
  , setLabelSelector
  , setOpaqueSelector
  , setPrimitiveDataBufferOffsetSelector
  , setPrimitiveDataBufferSelector
  , setPrimitiveDataElementSizeSelector
  , setPrimitiveDataStrideSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- intersectionFunctionTableOffset@
intersectionFunctionTableOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
intersectionFunctionTableOffset mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor intersectionFunctionTableOffsetSelector

-- | @- setIntersectionFunctionTableOffset:@
setIntersectionFunctionTableOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setIntersectionFunctionTableOffset mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setIntersectionFunctionTableOffsetSelector value

-- | Whether the geometry is opaque
--
-- ObjC selector: @- opaque@
opaque :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO Bool
opaque mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor opaqueSelector

-- | Whether the geometry is opaque
--
-- ObjC selector: @- setOpaque:@
setOpaque :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> Bool -> IO ()
setOpaque mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setOpaqueSelector value

-- | Whether intersection functions may be invoked more than once per ray/primitive intersection. Defaults to YES.
--
-- ObjC selector: @- allowDuplicateIntersectionFunctionInvocation@
allowDuplicateIntersectionFunctionInvocation :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO Bool
allowDuplicateIntersectionFunctionInvocation mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor allowDuplicateIntersectionFunctionInvocationSelector

-- | Whether intersection functions may be invoked more than once per ray/primitive intersection. Defaults to YES.
--
-- ObjC selector: @- setAllowDuplicateIntersectionFunctionInvocation:@
setAllowDuplicateIntersectionFunctionInvocation :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> Bool -> IO ()
setAllowDuplicateIntersectionFunctionInvocation mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setAllowDuplicateIntersectionFunctionInvocationSelector value

-- | Label
--
-- ObjC selector: @- label@
label :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO (Id NSString)
label mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor labelSelector

-- | Label
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor, IsNSString value) => mtlAccelerationStructureGeometryDescriptor -> value -> IO ()
setLabel mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setLabelSelector (toNSString value)

-- | Data buffer containing per-primitive data. May be nil.
--
-- ObjC selector: @- primitiveDataBuffer@
primitiveDataBuffer :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO RawId
primitiveDataBuffer mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor primitiveDataBufferSelector

-- | Data buffer containing per-primitive data. May be nil.
--
-- ObjC selector: @- setPrimitiveDataBuffer:@
setPrimitiveDataBuffer :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> RawId -> IO ()
setPrimitiveDataBuffer mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setPrimitiveDataBufferSelector value

-- | Primitive data buffer offset in bytes. Must be aligned to the platform's buffer offset alignment. Defaults to 0 bytes.
--
-- ObjC selector: @- primitiveDataBufferOffset@
primitiveDataBufferOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataBufferOffset mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor primitiveDataBufferOffsetSelector

-- | Primitive data buffer offset in bytes. Must be aligned to the platform's buffer offset alignment. Defaults to 0 bytes.
--
-- ObjC selector: @- setPrimitiveDataBufferOffset:@
setPrimitiveDataBufferOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataBufferOffset mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setPrimitiveDataBufferOffsetSelector value

-- | Stride, in bytes, between per-primitive data in the primitive data buffer. Must be at least primitiveDataElementSize and must be a multiple of 4 bytes. Defaults to 0 bytes. Assumed to be equal to primitiveDataElementSize if zero.
--
-- ObjC selector: @- primitiveDataStride@
primitiveDataStride :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataStride mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor primitiveDataStrideSelector

-- | Stride, in bytes, between per-primitive data in the primitive data buffer. Must be at least primitiveDataElementSize and must be a multiple of 4 bytes. Defaults to 0 bytes. Assumed to be equal to primitiveDataElementSize if zero.
--
-- ObjC selector: @- setPrimitiveDataStride:@
setPrimitiveDataStride :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataStride mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setPrimitiveDataStrideSelector value

-- | Size, in bytes, of the data for each primitive in the primitive data buffer. Must be at most primitiveDataStride and must be a multiple of 4 bytes. Defaults to 0 bytes.
--
-- ObjC selector: @- primitiveDataElementSize@
primitiveDataElementSize :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataElementSize mtlAccelerationStructureGeometryDescriptor =
  sendMessage mtlAccelerationStructureGeometryDescriptor primitiveDataElementSizeSelector

-- | Size, in bytes, of the data for each primitive in the primitive data buffer. Must be at most primitiveDataStride and must be a multiple of 4 bytes. Defaults to 0 bytes.
--
-- ObjC selector: @- setPrimitiveDataElementSize:@
setPrimitiveDataElementSize :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataElementSize mtlAccelerationStructureGeometryDescriptor value =
  sendMessage mtlAccelerationStructureGeometryDescriptor setPrimitiveDataElementSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intersectionFunctionTableOffset@
intersectionFunctionTableOffsetSelector :: Selector '[] CULong
intersectionFunctionTableOffsetSelector = mkSelector "intersectionFunctionTableOffset"

-- | @Selector@ for @setIntersectionFunctionTableOffset:@
setIntersectionFunctionTableOffsetSelector :: Selector '[CULong] ()
setIntersectionFunctionTableOffsetSelector = mkSelector "setIntersectionFunctionTableOffset:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector '[Bool] ()
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @allowDuplicateIntersectionFunctionInvocation@
allowDuplicateIntersectionFunctionInvocationSelector :: Selector '[] Bool
allowDuplicateIntersectionFunctionInvocationSelector = mkSelector "allowDuplicateIntersectionFunctionInvocation"

-- | @Selector@ for @setAllowDuplicateIntersectionFunctionInvocation:@
setAllowDuplicateIntersectionFunctionInvocationSelector :: Selector '[Bool] ()
setAllowDuplicateIntersectionFunctionInvocationSelector = mkSelector "setAllowDuplicateIntersectionFunctionInvocation:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @primitiveDataBuffer@
primitiveDataBufferSelector :: Selector '[] RawId
primitiveDataBufferSelector = mkSelector "primitiveDataBuffer"

-- | @Selector@ for @setPrimitiveDataBuffer:@
setPrimitiveDataBufferSelector :: Selector '[RawId] ()
setPrimitiveDataBufferSelector = mkSelector "setPrimitiveDataBuffer:"

-- | @Selector@ for @primitiveDataBufferOffset@
primitiveDataBufferOffsetSelector :: Selector '[] CULong
primitiveDataBufferOffsetSelector = mkSelector "primitiveDataBufferOffset"

-- | @Selector@ for @setPrimitiveDataBufferOffset:@
setPrimitiveDataBufferOffsetSelector :: Selector '[CULong] ()
setPrimitiveDataBufferOffsetSelector = mkSelector "setPrimitiveDataBufferOffset:"

-- | @Selector@ for @primitiveDataStride@
primitiveDataStrideSelector :: Selector '[] CULong
primitiveDataStrideSelector = mkSelector "primitiveDataStride"

-- | @Selector@ for @setPrimitiveDataStride:@
setPrimitiveDataStrideSelector :: Selector '[CULong] ()
setPrimitiveDataStrideSelector = mkSelector "setPrimitiveDataStride:"

-- | @Selector@ for @primitiveDataElementSize@
primitiveDataElementSizeSelector :: Selector '[] CULong
primitiveDataElementSizeSelector = mkSelector "primitiveDataElementSize"

-- | @Selector@ for @setPrimitiveDataElementSize:@
setPrimitiveDataElementSizeSelector :: Selector '[CULong] ()
setPrimitiveDataElementSizeSelector = mkSelector "setPrimitiveDataElementSize:"

