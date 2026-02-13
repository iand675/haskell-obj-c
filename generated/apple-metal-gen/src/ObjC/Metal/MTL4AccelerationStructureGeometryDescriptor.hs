{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for all Metal 4 acceleration structure geometry descriptors.
--
-- Don't use this class directly. Use one of the derived classes instead.
--
-- Generated bindings for @MTL4AccelerationStructureGeometryDescriptor@.
module ObjC.Metal.MTL4AccelerationStructureGeometryDescriptor
  ( MTL4AccelerationStructureGeometryDescriptor
  , IsMTL4AccelerationStructureGeometryDescriptor(..)
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
  , primitiveDataStride
  , setPrimitiveDataStride
  , primitiveDataElementSize
  , setPrimitiveDataElementSize
  , allowDuplicateIntersectionFunctionInvocationSelector
  , intersectionFunctionTableOffsetSelector
  , labelSelector
  , opaqueSelector
  , primitiveDataBufferSelector
  , primitiveDataElementSizeSelector
  , primitiveDataStrideSelector
  , setAllowDuplicateIntersectionFunctionInvocationSelector
  , setIntersectionFunctionTableOffsetSelector
  , setLabelSelector
  , setOpaqueSelector
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
import ObjC.Metal.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Sets the offset that this geometry contributes to determining the intersection function to invoke when a ray intersects it.
--
-- When you perform a ray tracing operation in the Metal Shading Language, and provide the ray intersector object with an instance of ``MTLIntersectionFunctionTable``, Metal adds this offset to the instance offset from structs such as:
--
-- - ``MTLAccelerationStructureInstanceDescriptor`` - ``MTLAccelerationStructureUserIDInstanceDescriptor`` - ``MTLAccelerationStructureMotionInstanceDescriptor`` - ``MTLIndirectAccelerationStructureInstanceDescriptor`` - ``MTLIndirectAccelerationStructureMotionInstanceDescriptor``
--
-- The sum of these offsets provides an index into the intersection function table that the ray tracing system uses to retrieve and invoke the function at this index, allowing you to customize the intersection evaluation process.
--
-- ObjC selector: @- intersectionFunctionTableOffset@
intersectionFunctionTableOffset :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO CULong
intersectionFunctionTableOffset mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor intersectionFunctionTableOffsetSelector

-- | Sets the offset that this geometry contributes to determining the intersection function to invoke when a ray intersects it.
--
-- When you perform a ray tracing operation in the Metal Shading Language, and provide the ray intersector object with an instance of ``MTLIntersectionFunctionTable``, Metal adds this offset to the instance offset from structs such as:
--
-- - ``MTLAccelerationStructureInstanceDescriptor`` - ``MTLAccelerationStructureUserIDInstanceDescriptor`` - ``MTLAccelerationStructureMotionInstanceDescriptor`` - ``MTLIndirectAccelerationStructureInstanceDescriptor`` - ``MTLIndirectAccelerationStructureMotionInstanceDescriptor``
--
-- The sum of these offsets provides an index into the intersection function table that the ray tracing system uses to retrieve and invoke the function at this index, allowing you to customize the intersection evaluation process.
--
-- ObjC selector: @- setIntersectionFunctionTableOffset:@
setIntersectionFunctionTableOffset :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> CULong -> IO ()
setIntersectionFunctionTableOffset mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setIntersectionFunctionTableOffsetSelector value

-- | Provides a hint to Metal that this geometry is opaque, potentially accelerating the ray/primitive intersection process.
--
-- ObjC selector: @- opaque@
opaque :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO Bool
opaque mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor opaqueSelector

-- | Provides a hint to Metal that this geometry is opaque, potentially accelerating the ray/primitive intersection process.
--
-- ObjC selector: @- setOpaque:@
setOpaque :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> Bool -> IO ()
setOpaque mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setOpaqueSelector value

-- | A boolean value that indicates whether the ray-tracing system in Metal allows the invocation of intersection functions more than once per ray-primitive intersection.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- allowDuplicateIntersectionFunctionInvocation@
allowDuplicateIntersectionFunctionInvocation :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO Bool
allowDuplicateIntersectionFunctionInvocation mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor allowDuplicateIntersectionFunctionInvocationSelector

-- | A boolean value that indicates whether the ray-tracing system in Metal allows the invocation of intersection functions more than once per ray-primitive intersection.
--
-- The property's default value is <doc://com.apple.documentation/documentation/swift/true>.
--
-- ObjC selector: @- setAllowDuplicateIntersectionFunctionInvocation:@
setAllowDuplicateIntersectionFunctionInvocation :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> Bool -> IO ()
setAllowDuplicateIntersectionFunctionInvocation mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setAllowDuplicateIntersectionFunctionInvocationSelector value

-- | Assigns an optional label you can assign to this geometry for debugging purposes.
--
-- ObjC selector: @- label@
label :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO (Id NSString)
label mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor labelSelector

-- | Assigns an optional label you can assign to this geometry for debugging purposes.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor, IsNSString value) => mtL4AccelerationStructureGeometryDescriptor -> value -> IO ()
setLabel mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setLabelSelector (toNSString value)

-- | Assigns optional buffer containing data to associate with each primitive in this geometry.
--
-- You can use zero as the buffer address in this buffer range.
--
-- ObjC selector: @- primitiveDataBuffer@
primitiveDataBuffer :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO MTL4BufferRange
primitiveDataBuffer mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor primitiveDataBufferSelector

-- | Assigns optional buffer containing data to associate with each primitive in this geometry.
--
-- You can use zero as the buffer address in this buffer range.
--
-- ObjC selector: @- setPrimitiveDataBuffer:@
setPrimitiveDataBuffer :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> MTL4BufferRange -> IO ()
setPrimitiveDataBuffer mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setPrimitiveDataBufferSelector value

-- | Defines the stride, in bytes, between each primitive's data in the primitive data buffer ``primitiveDataBuffer`` references.
--
-- You are responsible for ensuring the stride is at least ``primitiveDataElementSize`` in size and a multiple of 4 bytes.
--
-- This property defaults to @0@ bytes,  which indicates the stride is equal to ``primitiveDataElementSize``.
--
-- ObjC selector: @- primitiveDataStride@
primitiveDataStride :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataStride mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor primitiveDataStrideSelector

-- | Defines the stride, in bytes, between each primitive's data in the primitive data buffer ``primitiveDataBuffer`` references.
--
-- You are responsible for ensuring the stride is at least ``primitiveDataElementSize`` in size and a multiple of 4 bytes.
--
-- This property defaults to @0@ bytes,  which indicates the stride is equal to ``primitiveDataElementSize``.
--
-- ObjC selector: @- setPrimitiveDataStride:@
setPrimitiveDataStride :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataStride mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setPrimitiveDataStrideSelector value

-- | Sets the size, in bytes, of the data for each primitive in the primitive data buffer ``primitiveDataBuffer`` references.
--
-- This size needs to be at most ``primitiveDataStride`` in size and a multiple of 4 bytes.
--
-- This property defaults to 0 bytes.
--
-- ObjC selector: @- primitiveDataElementSize@
primitiveDataElementSize :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataElementSize mtL4AccelerationStructureGeometryDescriptor =
  sendMessage mtL4AccelerationStructureGeometryDescriptor primitiveDataElementSizeSelector

-- | Sets the size, in bytes, of the data for each primitive in the primitive data buffer ``primitiveDataBuffer`` references.
--
-- This size needs to be at most ``primitiveDataStride`` in size and a multiple of 4 bytes.
--
-- This property defaults to 0 bytes.
--
-- ObjC selector: @- setPrimitiveDataElementSize:@
setPrimitiveDataElementSize :: IsMTL4AccelerationStructureGeometryDescriptor mtL4AccelerationStructureGeometryDescriptor => mtL4AccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataElementSize mtL4AccelerationStructureGeometryDescriptor value =
  sendMessage mtL4AccelerationStructureGeometryDescriptor setPrimitiveDataElementSizeSelector value

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
primitiveDataBufferSelector :: Selector '[] MTL4BufferRange
primitiveDataBufferSelector = mkSelector "primitiveDataBuffer"

-- | @Selector@ for @setPrimitiveDataBuffer:@
setPrimitiveDataBufferSelector :: Selector '[MTL4BufferRange] ()
setPrimitiveDataBufferSelector = mkSelector "setPrimitiveDataBuffer:"

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

