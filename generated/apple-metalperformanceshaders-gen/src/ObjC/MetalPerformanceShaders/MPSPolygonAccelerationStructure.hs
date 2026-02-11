{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An acceleration structure built over polygonal shapes
--
-- See MPSAccelerationStructure for more information
--
-- Generated bindings for @MPSPolygonAccelerationStructure@.
module ObjC.MetalPerformanceShaders.MPSPolygonAccelerationStructure
  ( MPSPolygonAccelerationStructure
  , IsMPSPolygonAccelerationStructure(..)
  , polygonType
  , setPolygonType
  , vertexStride
  , setVertexStride
  , indexType
  , setIndexType
  , vertexBuffer
  , setVertexBuffer
  , vertexBufferOffset
  , setVertexBufferOffset
  , indexBuffer
  , setIndexBuffer
  , indexBufferOffset
  , setIndexBufferOffset
  , maskBuffer
  , setMaskBuffer
  , maskBufferOffset
  , setMaskBufferOffset
  , polygonCount
  , setPolygonCount
  , polygonBuffers
  , setPolygonBuffers
  , polygonTypeSelector
  , setPolygonTypeSelector
  , vertexStrideSelector
  , setVertexStrideSelector
  , indexTypeSelector
  , setIndexTypeSelector
  , vertexBufferSelector
  , setVertexBufferSelector
  , vertexBufferOffsetSelector
  , setVertexBufferOffsetSelector
  , indexBufferSelector
  , setIndexBufferSelector
  , indexBufferOffsetSelector
  , setIndexBufferOffsetSelector
  , maskBufferSelector
  , setMaskBufferSelector
  , maskBufferOffsetSelector
  , setMaskBufferOffsetSelector
  , polygonCountSelector
  , setPolygonCountSelector
  , polygonBuffersSelector
  , setPolygonBuffersSelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8
  , MPSPolygonType(MPSPolygonType)
  , pattern MPSPolygonTypeTriangle
  , pattern MPSPolygonTypeQuadrilateral

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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The type of polygon. Defaults to MPSPolygonTypeTriangle. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- polygonType@
polygonType :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO MPSPolygonType
polygonType mpsPolygonAccelerationStructure  =
    fmap (coerce :: CULong -> MPSPolygonType) $ sendMsg mpsPolygonAccelerationStructure (mkSelector "polygonType") retCULong []

-- | The type of polygon. Defaults to MPSPolygonTypeTriangle. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setPolygonType:@
setPolygonType :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> MPSPolygonType -> IO ()
setPolygonType mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setPolygonType:") retVoid [argCULong (coerce value)]

-- | Offset, in bytes, between consecutive vertices in the vertex buffer. Defaults to 0 bytes, indicating that the vertices are packed according to the natural alignment of the vector_float3 type: 16 bytes.
--
-- This can be used to skip past any additional per-vertex data which may be stored alongside the position such as the vertex normal and texture coordinates. Must be a multiple of 4 bytes, and must be at least 12 bytes. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- vertexStride@
vertexStride :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO CULong
vertexStride mpsPolygonAccelerationStructure  =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "vertexStride") retCULong []

-- | Offset, in bytes, between consecutive vertices in the vertex buffer. Defaults to 0 bytes, indicating that the vertices are packed according to the natural alignment of the vector_float3 type: 16 bytes.
--
-- This can be used to skip past any additional per-vertex data which may be stored alongside the position such as the vertex normal and texture coordinates. Must be a multiple of 4 bytes, and must be at least 12 bytes. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setVertexStride:@
setVertexStride :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> CULong -> IO ()
setVertexStride mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setVertexStride:") retVoid [argCULong value]

-- | Index type. Defaults to MPSDataTypeUInt32. Only MPSDataTypeUInt16 and MPSDataTypeUInt32 are supported.
--
-- ObjC selector: @- indexType@
indexType :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO MPSDataType
indexType mpsPolygonAccelerationStructure  =
    fmap (coerce :: CUInt -> MPSDataType) $ sendMsg mpsPolygonAccelerationStructure (mkSelector "indexType") retCUInt []

-- | Index type. Defaults to MPSDataTypeUInt32. Only MPSDataTypeUInt16 and MPSDataTypeUInt32 are supported.
--
-- ObjC selector: @- setIndexType:@
setIndexType :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> MPSDataType -> IO ()
setIndexType mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setIndexType:") retVoid [argCUInt (coerce value)]

-- | Vertex buffer containing vertex data encoded as three 32 bit floats per vertex. Note that by default each vertex is aligned to the alignment of the vector_float3 type: 16 bytes. This can be changed using the vertexStride property. A vertex buffer must be provided before the acceleration structure is built.
--
-- When using triangle polygons, degenerate (zero or negative area) triangles are ignored during acceleration structure construction. This can be used to pad triangle indices if needed.
--
-- Quadrilateral polygons are internally treated as two triangles. If the quadrilateral has vertices v0, v1, v2, and v3, the two triangles will have vertices v0, v1, v2 and v0, v2, v3. A quadrilateral may be used to represent a triangle by repeating the last vertex. If the first triangle is degenerate (zero or negative area), the entire quadrilateral will be ignored. This can be used to pad quadrilateral indices if needed. All four vertices of a quadrilateral must be coplanar and the quadrilateral must be convex.
--
-- This is an alias for polygonBuffers[0].vertexBuffer. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- vertexBuffer@
vertexBuffer :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO RawId
vertexBuffer mpsPolygonAccelerationStructure  =
    fmap (RawId . castPtr) $ sendMsg mpsPolygonAccelerationStructure (mkSelector "vertexBuffer") (retPtr retVoid) []

-- | Vertex buffer containing vertex data encoded as three 32 bit floats per vertex. Note that by default each vertex is aligned to the alignment of the vector_float3 type: 16 bytes. This can be changed using the vertexStride property. A vertex buffer must be provided before the acceleration structure is built.
--
-- When using triangle polygons, degenerate (zero or negative area) triangles are ignored during acceleration structure construction. This can be used to pad triangle indices if needed.
--
-- Quadrilateral polygons are internally treated as two triangles. If the quadrilateral has vertices v0, v1, v2, and v3, the two triangles will have vertices v0, v1, v2 and v0, v2, v3. A quadrilateral may be used to represent a triangle by repeating the last vertex. If the first triangle is degenerate (zero or negative area), the entire quadrilateral will be ignored. This can be used to pad quadrilateral indices if needed. All four vertices of a quadrilateral must be coplanar and the quadrilateral must be convex.
--
-- This is an alias for polygonBuffers[0].vertexBuffer. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setVertexBuffer:@
setVertexBuffer :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> RawId -> IO ()
setVertexBuffer mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setVertexBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the vertex buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- This is an alias for polygonBuffers[0].vertexBufferOffset. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- vertexBufferOffset@
vertexBufferOffset :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO CULong
vertexBufferOffset mpsPolygonAccelerationStructure  =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "vertexBufferOffset") retCULong []

-- | Offset, in bytes, into the vertex buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- This is an alias for polygonBuffers[0].vertexBufferOffset. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setVertexBufferOffset:@
setVertexBufferOffset :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> CULong -> IO ()
setVertexBufferOffset mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setVertexBufferOffset:") retVoid [argCULong value]

-- | Index buffer containing index data. Each index references a vertex in the vertex buffer. May be nil.
--
-- This is an alias for polygonBuffers[0].indexBuffer. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO RawId
indexBuffer mpsPolygonAccelerationStructure  =
    fmap (RawId . castPtr) $ sendMsg mpsPolygonAccelerationStructure (mkSelector "indexBuffer") (retPtr retVoid) []

-- | Index buffer containing index data. Each index references a vertex in the vertex buffer. May be nil.
--
-- This is an alias for polygonBuffers[0].indexBuffer. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> RawId -> IO ()
setIndexBuffer mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setIndexBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the index buffer. Defaults to 0 bytes. Must be aligned to a multiple of the index type. Changes to this property require rebuilding the acceleration structure.
--
-- This is an alias for polygonBuffers[0].indexBufferOffset. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO CULong
indexBufferOffset mpsPolygonAccelerationStructure  =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "indexBufferOffset") retCULong []

-- | Offset, in bytes, into the index buffer. Defaults to 0 bytes. Must be aligned to a multiple of the index type. Changes to this property require rebuilding the acceleration structure.
--
-- This is an alias for polygonBuffers[0].indexBufferOffset. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> CULong -> IO ()
setIndexBufferOffset mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setIndexBufferOffset:") retVoid [argCULong value]

-- | Mask buffer containing one uint32_t mask per polygon. May be nil. Otherwise, the mask type must be specified on the MPSRayIntersector with which it is used.
--
-- This is an alias for polygonBuffers[0].maskBuffer. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- maskBuffer@
maskBuffer :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO RawId
maskBuffer mpsPolygonAccelerationStructure  =
    fmap (RawId . castPtr) $ sendMsg mpsPolygonAccelerationStructure (mkSelector "maskBuffer") (retPtr retVoid) []

-- | Mask buffer containing one uint32_t mask per polygon. May be nil. Otherwise, the mask type must be specified on the MPSRayIntersector with which it is used.
--
-- This is an alias for polygonBuffers[0].maskBuffer. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setMaskBuffer:@
setMaskBuffer :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> RawId -> IO ()
setMaskBuffer mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setMaskBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- This is an alias for polygonBuffers[0].maskBufferOffset. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- maskBufferOffset@
maskBufferOffset :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO CULong
maskBufferOffset mpsPolygonAccelerationStructure  =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "maskBufferOffset") retCULong []

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- This is an alias for polygonBuffers[0].maskBufferOffset. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setMaskBufferOffset:@
setMaskBufferOffset :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> CULong -> IO ()
setMaskBufferOffset mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setMaskBufferOffset:") retVoid [argCULong value]

-- | Number of polygons. Changes to this property require rebuilding the acceleration structure.
--
-- This is an alias for polygonBuffers[0].polygonCount. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- polygonCount@
polygonCount :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO CULong
polygonCount mpsPolygonAccelerationStructure  =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "polygonCount") retCULong []

-- | Number of polygons. Changes to this property require rebuilding the acceleration structure.
--
-- This is an alias for polygonBuffers[0].polygonCount. There must be exactly one polygon buffer to use this property, or the polygonBuffers property must be nil, in which case an MPSPolygonBuffer will be created automatically.
--
-- ObjC selector: @- setPolygonCount:@
setPolygonCount :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> CULong -> IO ()
setPolygonCount mpsPolygonAccelerationStructure  value =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "setPolygonCount:") retVoid [argCULong value]

-- | Array of polygon buffers. Each buffer contains a vertex buffer and optional index and mask buffer for an array of polygons. Changing the length of this array requires rebuilding the acceleration structure.
--
-- Using more than one MPSPolygonBuffer will reduce performance. It is better to concatenate these buffers into a single vertex buffer, index buffer, and mask buffer and use a single MPSPolygonBuffer if possible. This also applies when using an MPSInstanceAccelerationStructure: each instance or subclass of MPSPolygonAccelerationStructure in an instance hierarchy should use the same vertex buffer, index buffer, and mask buffer, although each acceleration structure may use different offsets into these buffers. This allows for the vertex, index, and mask buffers to be bound directly instead of indirectly through an argument buffer.
--
-- There must be at least one MPSPolygonBuffer. On argument buffer tier 1 devices, there must be be exactly one MPSPolygonBuffer. Use the argumentBuffersSupport property of the MTLDevice to check for support.
--
-- ObjC selector: @- polygonBuffers@
polygonBuffers :: IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure => mpsPolygonAccelerationStructure -> IO (Id NSArray)
polygonBuffers mpsPolygonAccelerationStructure  =
    sendMsg mpsPolygonAccelerationStructure (mkSelector "polygonBuffers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of polygon buffers. Each buffer contains a vertex buffer and optional index and mask buffer for an array of polygons. Changing the length of this array requires rebuilding the acceleration structure.
--
-- Using more than one MPSPolygonBuffer will reduce performance. It is better to concatenate these buffers into a single vertex buffer, index buffer, and mask buffer and use a single MPSPolygonBuffer if possible. This also applies when using an MPSInstanceAccelerationStructure: each instance or subclass of MPSPolygonAccelerationStructure in an instance hierarchy should use the same vertex buffer, index buffer, and mask buffer, although each acceleration structure may use different offsets into these buffers. This allows for the vertex, index, and mask buffers to be bound directly instead of indirectly through an argument buffer.
--
-- There must be at least one MPSPolygonBuffer. On argument buffer tier 1 devices, there must be be exactly one MPSPolygonBuffer. Use the argumentBuffersSupport property of the MTLDevice to check for support.
--
-- ObjC selector: @- setPolygonBuffers:@
setPolygonBuffers :: (IsMPSPolygonAccelerationStructure mpsPolygonAccelerationStructure, IsNSArray value) => mpsPolygonAccelerationStructure -> value -> IO ()
setPolygonBuffers mpsPolygonAccelerationStructure  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mpsPolygonAccelerationStructure (mkSelector "setPolygonBuffers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @polygonType@
polygonTypeSelector :: Selector
polygonTypeSelector = mkSelector "polygonType"

-- | @Selector@ for @setPolygonType:@
setPolygonTypeSelector :: Selector
setPolygonTypeSelector = mkSelector "setPolygonType:"

-- | @Selector@ for @vertexStride@
vertexStrideSelector :: Selector
vertexStrideSelector = mkSelector "vertexStride"

-- | @Selector@ for @setVertexStride:@
setVertexStrideSelector :: Selector
setVertexStrideSelector = mkSelector "setVertexStride:"

-- | @Selector@ for @indexType@
indexTypeSelector :: Selector
indexTypeSelector = mkSelector "indexType"

-- | @Selector@ for @setIndexType:@
setIndexTypeSelector :: Selector
setIndexTypeSelector = mkSelector "setIndexType:"

-- | @Selector@ for @vertexBuffer@
vertexBufferSelector :: Selector
vertexBufferSelector = mkSelector "vertexBuffer"

-- | @Selector@ for @setVertexBuffer:@
setVertexBufferSelector :: Selector
setVertexBufferSelector = mkSelector "setVertexBuffer:"

-- | @Selector@ for @vertexBufferOffset@
vertexBufferOffsetSelector :: Selector
vertexBufferOffsetSelector = mkSelector "vertexBufferOffset"

-- | @Selector@ for @setVertexBufferOffset:@
setVertexBufferOffsetSelector :: Selector
setVertexBufferOffsetSelector = mkSelector "setVertexBufferOffset:"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @setIndexBuffer:@
setIndexBufferSelector :: Selector
setIndexBufferSelector = mkSelector "setIndexBuffer:"

-- | @Selector@ for @indexBufferOffset@
indexBufferOffsetSelector :: Selector
indexBufferOffsetSelector = mkSelector "indexBufferOffset"

-- | @Selector@ for @setIndexBufferOffset:@
setIndexBufferOffsetSelector :: Selector
setIndexBufferOffsetSelector = mkSelector "setIndexBufferOffset:"

-- | @Selector@ for @maskBuffer@
maskBufferSelector :: Selector
maskBufferSelector = mkSelector "maskBuffer"

-- | @Selector@ for @setMaskBuffer:@
setMaskBufferSelector :: Selector
setMaskBufferSelector = mkSelector "setMaskBuffer:"

-- | @Selector@ for @maskBufferOffset@
maskBufferOffsetSelector :: Selector
maskBufferOffsetSelector = mkSelector "maskBufferOffset"

-- | @Selector@ for @setMaskBufferOffset:@
setMaskBufferOffsetSelector :: Selector
setMaskBufferOffsetSelector = mkSelector "setMaskBufferOffset:"

-- | @Selector@ for @polygonCount@
polygonCountSelector :: Selector
polygonCountSelector = mkSelector "polygonCount"

-- | @Selector@ for @setPolygonCount:@
setPolygonCountSelector :: Selector
setPolygonCountSelector = mkSelector "setPolygonCount:"

-- | @Selector@ for @polygonBuffers@
polygonBuffersSelector :: Selector
polygonBuffersSelector = mkSelector "polygonBuffers"

-- | @Selector@ for @setPolygonBuffers:@
setPolygonBuffersSelector :: Selector
setPolygonBuffersSelector = mkSelector "setPolygonBuffers:"

