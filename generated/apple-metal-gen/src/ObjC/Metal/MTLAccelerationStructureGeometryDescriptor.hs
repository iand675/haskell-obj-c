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
  , intersectionFunctionTableOffsetSelector
  , setIntersectionFunctionTableOffsetSelector
  , opaqueSelector
  , setOpaqueSelector
  , allowDuplicateIntersectionFunctionInvocationSelector
  , setAllowDuplicateIntersectionFunctionInvocationSelector
  , labelSelector
  , setLabelSelector
  , primitiveDataBufferSelector
  , setPrimitiveDataBufferSelector
  , primitiveDataBufferOffsetSelector
  , setPrimitiveDataBufferOffsetSelector
  , primitiveDataStrideSelector
  , setPrimitiveDataStrideSelector
  , primitiveDataElementSizeSelector
  , setPrimitiveDataElementSizeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- intersectionFunctionTableOffset@
intersectionFunctionTableOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
intersectionFunctionTableOffset mtlAccelerationStructureGeometryDescriptor  =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "intersectionFunctionTableOffset") retCULong []

-- | @- setIntersectionFunctionTableOffset:@
setIntersectionFunctionTableOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setIntersectionFunctionTableOffset mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setIntersectionFunctionTableOffset:") retVoid [argCULong value]

-- | Whether the geometry is opaque
--
-- ObjC selector: @- opaque@
opaque :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO Bool
opaque mtlAccelerationStructureGeometryDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "opaque") retCULong []

-- | Whether the geometry is opaque
--
-- ObjC selector: @- setOpaque:@
setOpaque :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> Bool -> IO ()
setOpaque mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setOpaque:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether intersection functions may be invoked more than once per ray/primitive intersection. Defaults to YES.
--
-- ObjC selector: @- allowDuplicateIntersectionFunctionInvocation@
allowDuplicateIntersectionFunctionInvocation :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO Bool
allowDuplicateIntersectionFunctionInvocation mtlAccelerationStructureGeometryDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "allowDuplicateIntersectionFunctionInvocation") retCULong []

-- | Whether intersection functions may be invoked more than once per ray/primitive intersection. Defaults to YES.
--
-- ObjC selector: @- setAllowDuplicateIntersectionFunctionInvocation:@
setAllowDuplicateIntersectionFunctionInvocation :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> Bool -> IO ()
setAllowDuplicateIntersectionFunctionInvocation mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setAllowDuplicateIntersectionFunctionInvocation:") retVoid [argCULong (if value then 1 else 0)]

-- | Label
--
-- ObjC selector: @- label@
label :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO (Id NSString)
label mtlAccelerationStructureGeometryDescriptor  =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Label
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor, IsNSString value) => mtlAccelerationStructureGeometryDescriptor -> value -> IO ()
setLabel mtlAccelerationStructureGeometryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Data buffer containing per-primitive data. May be nil.
--
-- ObjC selector: @- primitiveDataBuffer@
primitiveDataBuffer :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO RawId
primitiveDataBuffer mtlAccelerationStructureGeometryDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "primitiveDataBuffer") (retPtr retVoid) []

-- | Data buffer containing per-primitive data. May be nil.
--
-- ObjC selector: @- setPrimitiveDataBuffer:@
setPrimitiveDataBuffer :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> RawId -> IO ()
setPrimitiveDataBuffer mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setPrimitiveDataBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Primitive data buffer offset in bytes. Must be aligned to the platform's buffer offset alignment. Defaults to 0 bytes.
--
-- ObjC selector: @- primitiveDataBufferOffset@
primitiveDataBufferOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataBufferOffset mtlAccelerationStructureGeometryDescriptor  =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "primitiveDataBufferOffset") retCULong []

-- | Primitive data buffer offset in bytes. Must be aligned to the platform's buffer offset alignment. Defaults to 0 bytes.
--
-- ObjC selector: @- setPrimitiveDataBufferOffset:@
setPrimitiveDataBufferOffset :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataBufferOffset mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setPrimitiveDataBufferOffset:") retVoid [argCULong value]

-- | Stride, in bytes, between per-primitive data in the primitive data buffer. Must be at least primitiveDataElementSize and must be a multiple of 4 bytes. Defaults to 0 bytes. Assumed to be equal to primitiveDataElementSize if zero.
--
-- ObjC selector: @- primitiveDataStride@
primitiveDataStride :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataStride mtlAccelerationStructureGeometryDescriptor  =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "primitiveDataStride") retCULong []

-- | Stride, in bytes, between per-primitive data in the primitive data buffer. Must be at least primitiveDataElementSize and must be a multiple of 4 bytes. Defaults to 0 bytes. Assumed to be equal to primitiveDataElementSize if zero.
--
-- ObjC selector: @- setPrimitiveDataStride:@
setPrimitiveDataStride :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataStride mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setPrimitiveDataStride:") retVoid [argCULong value]

-- | Size, in bytes, of the data for each primitive in the primitive data buffer. Must be at most primitiveDataStride and must be a multiple of 4 bytes. Defaults to 0 bytes.
--
-- ObjC selector: @- primitiveDataElementSize@
primitiveDataElementSize :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> IO CULong
primitiveDataElementSize mtlAccelerationStructureGeometryDescriptor  =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "primitiveDataElementSize") retCULong []

-- | Size, in bytes, of the data for each primitive in the primitive data buffer. Must be at most primitiveDataStride and must be a multiple of 4 bytes. Defaults to 0 bytes.
--
-- ObjC selector: @- setPrimitiveDataElementSize:@
setPrimitiveDataElementSize :: IsMTLAccelerationStructureGeometryDescriptor mtlAccelerationStructureGeometryDescriptor => mtlAccelerationStructureGeometryDescriptor -> CULong -> IO ()
setPrimitiveDataElementSize mtlAccelerationStructureGeometryDescriptor  value =
    sendMsg mtlAccelerationStructureGeometryDescriptor (mkSelector "setPrimitiveDataElementSize:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intersectionFunctionTableOffset@
intersectionFunctionTableOffsetSelector :: Selector
intersectionFunctionTableOffsetSelector = mkSelector "intersectionFunctionTableOffset"

-- | @Selector@ for @setIntersectionFunctionTableOffset:@
setIntersectionFunctionTableOffsetSelector :: Selector
setIntersectionFunctionTableOffsetSelector = mkSelector "setIntersectionFunctionTableOffset:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @setOpaque:@
setOpaqueSelector :: Selector
setOpaqueSelector = mkSelector "setOpaque:"

-- | @Selector@ for @allowDuplicateIntersectionFunctionInvocation@
allowDuplicateIntersectionFunctionInvocationSelector :: Selector
allowDuplicateIntersectionFunctionInvocationSelector = mkSelector "allowDuplicateIntersectionFunctionInvocation"

-- | @Selector@ for @setAllowDuplicateIntersectionFunctionInvocation:@
setAllowDuplicateIntersectionFunctionInvocationSelector :: Selector
setAllowDuplicateIntersectionFunctionInvocationSelector = mkSelector "setAllowDuplicateIntersectionFunctionInvocation:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @primitiveDataBuffer@
primitiveDataBufferSelector :: Selector
primitiveDataBufferSelector = mkSelector "primitiveDataBuffer"

-- | @Selector@ for @setPrimitiveDataBuffer:@
setPrimitiveDataBufferSelector :: Selector
setPrimitiveDataBufferSelector = mkSelector "setPrimitiveDataBuffer:"

-- | @Selector@ for @primitiveDataBufferOffset@
primitiveDataBufferOffsetSelector :: Selector
primitiveDataBufferOffsetSelector = mkSelector "primitiveDataBufferOffset"

-- | @Selector@ for @setPrimitiveDataBufferOffset:@
setPrimitiveDataBufferOffsetSelector :: Selector
setPrimitiveDataBufferOffsetSelector = mkSelector "setPrimitiveDataBufferOffset:"

-- | @Selector@ for @primitiveDataStride@
primitiveDataStrideSelector :: Selector
primitiveDataStrideSelector = mkSelector "primitiveDataStride"

-- | @Selector@ for @setPrimitiveDataStride:@
setPrimitiveDataStrideSelector :: Selector
setPrimitiveDataStrideSelector = mkSelector "setPrimitiveDataStride:"

-- | @Selector@ for @primitiveDataElementSize@
primitiveDataElementSizeSelector :: Selector
primitiveDataElementSizeSelector = mkSelector "primitiveDataElementSize"

-- | @Selector@ for @setPrimitiveDataElementSize:@
setPrimitiveDataElementSizeSelector :: Selector
setPrimitiveDataElementSizeSelector = mkSelector "setPrimitiveDataElementSize:"

