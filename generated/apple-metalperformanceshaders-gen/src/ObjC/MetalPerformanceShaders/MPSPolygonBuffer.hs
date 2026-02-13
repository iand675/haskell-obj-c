{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A vertex buffer and optional index and mask buffer for a set of polygons
--
-- Generated bindings for @MPSPolygonBuffer@.
module ObjC.MetalPerformanceShaders.MPSPolygonBuffer
  ( MPSPolygonBuffer
  , IsMPSPolygonBuffer(..)
  , init_
  , initWithCoder
  , polygonBuffer
  , copyWithZone
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
  , copyWithZoneSelector
  , indexBufferOffsetSelector
  , indexBufferSelector
  , initSelector
  , initWithCoderSelector
  , maskBufferOffsetSelector
  , maskBufferSelector
  , polygonBufferSelector
  , polygonCountSelector
  , setIndexBufferOffsetSelector
  , setIndexBufferSelector
  , setMaskBufferOffsetSelector
  , setMaskBufferSelector
  , setPolygonCountSelector
  , setVertexBufferOffsetSelector
  , setVertexBufferSelector
  , vertexBufferOffsetSelector
  , vertexBufferSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the polygon buffer
--
-- ObjC selector: @- init@
init_ :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO (Id MPSPolygonBuffer)
init_ mpsPolygonBuffer =
  sendOwnedMessage mpsPolygonBuffer initSelector

-- | Initialize the polygon buffer with an NSCoder. Buffer properties such as the vertex buffer, instance buffer, etc. are set to nil. Encode and decode these buffers along with the polygon buffer instead.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsMPSPolygonBuffer mpsPolygonBuffer, IsNSCoder aDecoder) => mpsPolygonBuffer -> aDecoder -> IO (Id MPSPolygonBuffer)
initWithCoder mpsPolygonBuffer aDecoder =
  sendOwnedMessage mpsPolygonBuffer initWithCoderSelector (toNSCoder aDecoder)

-- | @+ polygonBuffer@
polygonBuffer :: IO (Id MPSPolygonBuffer)
polygonBuffer  =
  do
    cls' <- getRequiredClass "MPSPolygonBuffer"
    sendClassMessage cls' polygonBufferSelector

-- | Create a a copy of this polygon buffer
--
-- Buffer properties of the polygon buffer such as the vertex buffer, instance, buffer, etc. are set to nil. Copy these buffers and assign them to the new polygon buffer or reassign the existing buffers to the new polygon buffer.
--
-- @zone@ — This parameter is ignored. Memory zones are no longer used by Objective-C.
--
-- ObjC selector: @- copyWithZone:@
copyWithZone :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> Ptr () -> IO (Id MPSPolygonBuffer)
copyWithZone mpsPolygonBuffer zone =
  sendOwnedMessage mpsPolygonBuffer copyWithZoneSelector zone

-- | Vertex buffer containing vertex data encoded as three 32 bit floats per vertex. Note that by default each vertex is aligned to the alignment of the vector_float3 type: 16 bytes. This can be changed using the vertexStride property. A vertex buffer must be provided before the acceleration structure is built.
--
-- When using triangle polygons, degenerate (zero or negative area) triangles are ignored during acceleration structure construction. This can be used to pad triangle indices if needed.
--
-- Quadrilateral polygons are internally treated as two triangles. If the quadrilateral has vertices v0, v1, v2, and v3, the two triangles will have vertices v0, v1, v2 and v0, v2, v3. A quadrilateral may be used to represent a triangle by repeating the last vertex. If the first triangle is degenerate (zero or negative area), the entire quadrilateral will be ignored. This can be used to pad quadrilateral indices if needed. All four vertices of a quadrilateral must be coplanar and the quadrilateral must be convex.
--
-- ObjC selector: @- vertexBuffer@
vertexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO RawId
vertexBuffer mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer vertexBufferSelector

-- | Vertex buffer containing vertex data encoded as three 32 bit floats per vertex. Note that by default each vertex is aligned to the alignment of the vector_float3 type: 16 bytes. This can be changed using the vertexStride property. A vertex buffer must be provided before the acceleration structure is built.
--
-- When using triangle polygons, degenerate (zero or negative area) triangles are ignored during acceleration structure construction. This can be used to pad triangle indices if needed.
--
-- Quadrilateral polygons are internally treated as two triangles. If the quadrilateral has vertices v0, v1, v2, and v3, the two triangles will have vertices v0, v1, v2 and v0, v2, v3. A quadrilateral may be used to represent a triangle by repeating the last vertex. If the first triangle is degenerate (zero or negative area), the entire quadrilateral will be ignored. This can be used to pad quadrilateral indices if needed. All four vertices of a quadrilateral must be coplanar and the quadrilateral must be convex.
--
-- ObjC selector: @- setVertexBuffer:@
setVertexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> RawId -> IO ()
setVertexBuffer mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setVertexBufferSelector value

-- | Offset, in bytes, into the vertex buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- vertexBufferOffset@
vertexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
vertexBufferOffset mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer vertexBufferOffsetSelector

-- | Offset, in bytes, into the vertex buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setVertexBufferOffset:@
setVertexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setVertexBufferOffset mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setVertexBufferOffsetSelector value

-- | Index buffer containing index data. Each index references a vertex in the vertex buffer. May be nil.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO RawId
indexBuffer mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer indexBufferSelector

-- | Index buffer containing index data. Each index references a vertex in the vertex buffer. May be nil.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> RawId -> IO ()
setIndexBuffer mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setIndexBufferSelector value

-- | Offset, in bytes, into the index buffer. Defaults to 0 bytes. Must be aligned to a multiple of the index type. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
indexBufferOffset mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer indexBufferOffsetSelector

-- | Offset, in bytes, into the index buffer. Defaults to 0 bytes. Must be aligned to a multiple of the index type. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setIndexBufferOffset mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setIndexBufferOffsetSelector value

-- | Mask buffer containing one uint32_t mask per polygon. May be nil. Otherwise, the mask type must be specified on the MPSRayIntersector with which it is used.
--
-- ObjC selector: @- maskBuffer@
maskBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO RawId
maskBuffer mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer maskBufferSelector

-- | Mask buffer containing one uint32_t mask per polygon. May be nil. Otherwise, the mask type must be specified on the MPSRayIntersector with which it is used.
--
-- ObjC selector: @- setMaskBuffer:@
setMaskBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> RawId -> IO ()
setMaskBuffer mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setMaskBufferSelector value

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- maskBufferOffset@
maskBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
maskBufferOffset mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer maskBufferOffsetSelector

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setMaskBufferOffset:@
setMaskBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setMaskBufferOffset mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setMaskBufferOffsetSelector value

-- | Number of polygons. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- polygonCount@
polygonCount :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
polygonCount mpsPolygonBuffer =
  sendMessage mpsPolygonBuffer polygonCountSelector

-- | Number of polygons. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setPolygonCount:@
setPolygonCount :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setPolygonCount mpsPolygonBuffer value =
  sendMessage mpsPolygonBuffer setPolygonCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSPolygonBuffer)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MPSPolygonBuffer)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @polygonBuffer@
polygonBufferSelector :: Selector '[] (Id MPSPolygonBuffer)
polygonBufferSelector = mkSelector "polygonBuffer"

-- | @Selector@ for @copyWithZone:@
copyWithZoneSelector :: Selector '[Ptr ()] (Id MPSPolygonBuffer)
copyWithZoneSelector = mkSelector "copyWithZone:"

-- | @Selector@ for @vertexBuffer@
vertexBufferSelector :: Selector '[] RawId
vertexBufferSelector = mkSelector "vertexBuffer"

-- | @Selector@ for @setVertexBuffer:@
setVertexBufferSelector :: Selector '[RawId] ()
setVertexBufferSelector = mkSelector "setVertexBuffer:"

-- | @Selector@ for @vertexBufferOffset@
vertexBufferOffsetSelector :: Selector '[] CULong
vertexBufferOffsetSelector = mkSelector "vertexBufferOffset"

-- | @Selector@ for @setVertexBufferOffset:@
setVertexBufferOffsetSelector :: Selector '[CULong] ()
setVertexBufferOffsetSelector = mkSelector "setVertexBufferOffset:"

-- | @Selector@ for @indexBuffer@
indexBufferSelector :: Selector '[] RawId
indexBufferSelector = mkSelector "indexBuffer"

-- | @Selector@ for @setIndexBuffer:@
setIndexBufferSelector :: Selector '[RawId] ()
setIndexBufferSelector = mkSelector "setIndexBuffer:"

-- | @Selector@ for @indexBufferOffset@
indexBufferOffsetSelector :: Selector '[] CULong
indexBufferOffsetSelector = mkSelector "indexBufferOffset"

-- | @Selector@ for @setIndexBufferOffset:@
setIndexBufferOffsetSelector :: Selector '[CULong] ()
setIndexBufferOffsetSelector = mkSelector "setIndexBufferOffset:"

-- | @Selector@ for @maskBuffer@
maskBufferSelector :: Selector '[] RawId
maskBufferSelector = mkSelector "maskBuffer"

-- | @Selector@ for @setMaskBuffer:@
setMaskBufferSelector :: Selector '[RawId] ()
setMaskBufferSelector = mkSelector "setMaskBuffer:"

-- | @Selector@ for @maskBufferOffset@
maskBufferOffsetSelector :: Selector '[] CULong
maskBufferOffsetSelector = mkSelector "maskBufferOffset"

-- | @Selector@ for @setMaskBufferOffset:@
setMaskBufferOffsetSelector :: Selector '[CULong] ()
setMaskBufferOffsetSelector = mkSelector "setMaskBufferOffset:"

-- | @Selector@ for @polygonCount@
polygonCountSelector :: Selector '[] CULong
polygonCountSelector = mkSelector "polygonCount"

-- | @Selector@ for @setPolygonCount:@
setPolygonCountSelector :: Selector '[CULong] ()
setPolygonCountSelector = mkSelector "setPolygonCount:"

