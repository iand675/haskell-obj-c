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
  , initSelector
  , initWithCoderSelector
  , polygonBufferSelector
  , copyWithZoneSelector
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
import ObjC.Foundation.Internal.Classes

-- | Initialize the polygon buffer
--
-- ObjC selector: @- init@
init_ :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO (Id MPSPolygonBuffer)
init_ mpsPolygonBuffer  =
    sendMsg mpsPolygonBuffer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize the polygon buffer with an NSCoder. Buffer properties such as the vertex buffer, instance buffer, etc. are set to nil. Encode and decode these buffers along with the polygon buffer instead.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsMPSPolygonBuffer mpsPolygonBuffer, IsNSCoder aDecoder) => mpsPolygonBuffer -> aDecoder -> IO (Id MPSPolygonBuffer)
initWithCoder mpsPolygonBuffer  aDecoder =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpsPolygonBuffer (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ polygonBuffer@
polygonBuffer :: IO (Id MPSPolygonBuffer)
polygonBuffer  =
  do
    cls' <- getRequiredClass "MPSPolygonBuffer"
    sendClassMsg cls' (mkSelector "polygonBuffer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a a copy of this polygon buffer
--
-- Buffer properties of the polygon buffer such as the vertex buffer, instance, buffer, etc. are set to nil. Copy these buffers and assign them to the new polygon buffer or reassign the existing buffers to the new polygon buffer.
--
-- @zone@ — This parameter is ignored. Memory zones are no longer used by Objective-C.
--
-- ObjC selector: @- copyWithZone:@
copyWithZone :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> Ptr () -> IO (Id MPSPolygonBuffer)
copyWithZone mpsPolygonBuffer  zone =
    sendMsg mpsPolygonBuffer (mkSelector "copyWithZone:") (retPtr retVoid) [argPtr zone] >>= ownedObject . castPtr

-- | Vertex buffer containing vertex data encoded as three 32 bit floats per vertex. Note that by default each vertex is aligned to the alignment of the vector_float3 type: 16 bytes. This can be changed using the vertexStride property. A vertex buffer must be provided before the acceleration structure is built.
--
-- When using triangle polygons, degenerate (zero or negative area) triangles are ignored during acceleration structure construction. This can be used to pad triangle indices if needed.
--
-- Quadrilateral polygons are internally treated as two triangles. If the quadrilateral has vertices v0, v1, v2, and v3, the two triangles will have vertices v0, v1, v2 and v0, v2, v3. A quadrilateral may be used to represent a triangle by repeating the last vertex. If the first triangle is degenerate (zero or negative area), the entire quadrilateral will be ignored. This can be used to pad quadrilateral indices if needed. All four vertices of a quadrilateral must be coplanar and the quadrilateral must be convex.
--
-- ObjC selector: @- vertexBuffer@
vertexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO RawId
vertexBuffer mpsPolygonBuffer  =
    fmap (RawId . castPtr) $ sendMsg mpsPolygonBuffer (mkSelector "vertexBuffer") (retPtr retVoid) []

-- | Vertex buffer containing vertex data encoded as three 32 bit floats per vertex. Note that by default each vertex is aligned to the alignment of the vector_float3 type: 16 bytes. This can be changed using the vertexStride property. A vertex buffer must be provided before the acceleration structure is built.
--
-- When using triangle polygons, degenerate (zero or negative area) triangles are ignored during acceleration structure construction. This can be used to pad triangle indices if needed.
--
-- Quadrilateral polygons are internally treated as two triangles. If the quadrilateral has vertices v0, v1, v2, and v3, the two triangles will have vertices v0, v1, v2 and v0, v2, v3. A quadrilateral may be used to represent a triangle by repeating the last vertex. If the first triangle is degenerate (zero or negative area), the entire quadrilateral will be ignored. This can be used to pad quadrilateral indices if needed. All four vertices of a quadrilateral must be coplanar and the quadrilateral must be convex.
--
-- ObjC selector: @- setVertexBuffer:@
setVertexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> RawId -> IO ()
setVertexBuffer mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setVertexBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the vertex buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- vertexBufferOffset@
vertexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
vertexBufferOffset mpsPolygonBuffer  =
    sendMsg mpsPolygonBuffer (mkSelector "vertexBufferOffset") retCULong []

-- | Offset, in bytes, into the vertex buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setVertexBufferOffset:@
setVertexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setVertexBufferOffset mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setVertexBufferOffset:") retVoid [argCULong value]

-- | Index buffer containing index data. Each index references a vertex in the vertex buffer. May be nil.
--
-- ObjC selector: @- indexBuffer@
indexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO RawId
indexBuffer mpsPolygonBuffer  =
    fmap (RawId . castPtr) $ sendMsg mpsPolygonBuffer (mkSelector "indexBuffer") (retPtr retVoid) []

-- | Index buffer containing index data. Each index references a vertex in the vertex buffer. May be nil.
--
-- ObjC selector: @- setIndexBuffer:@
setIndexBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> RawId -> IO ()
setIndexBuffer mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setIndexBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the index buffer. Defaults to 0 bytes. Must be aligned to a multiple of the index type. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- indexBufferOffset@
indexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
indexBufferOffset mpsPolygonBuffer  =
    sendMsg mpsPolygonBuffer (mkSelector "indexBufferOffset") retCULong []

-- | Offset, in bytes, into the index buffer. Defaults to 0 bytes. Must be aligned to a multiple of the index type. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setIndexBufferOffset:@
setIndexBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setIndexBufferOffset mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setIndexBufferOffset:") retVoid [argCULong value]

-- | Mask buffer containing one uint32_t mask per polygon. May be nil. Otherwise, the mask type must be specified on the MPSRayIntersector with which it is used.
--
-- ObjC selector: @- maskBuffer@
maskBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO RawId
maskBuffer mpsPolygonBuffer  =
    fmap (RawId . castPtr) $ sendMsg mpsPolygonBuffer (mkSelector "maskBuffer") (retPtr retVoid) []

-- | Mask buffer containing one uint32_t mask per polygon. May be nil. Otherwise, the mask type must be specified on the MPSRayIntersector with which it is used.
--
-- ObjC selector: @- setMaskBuffer:@
setMaskBuffer :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> RawId -> IO ()
setMaskBuffer mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setMaskBuffer:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- maskBufferOffset@
maskBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
maskBufferOffset mpsPolygonBuffer  =
    sendMsg mpsPolygonBuffer (mkSelector "maskBufferOffset") retCULong []

-- | Offset, in bytes, into the mask buffer. Defaults to 0 bytes. Must be aligned to 4 bytes.
--
-- ObjC selector: @- setMaskBufferOffset:@
setMaskBufferOffset :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setMaskBufferOffset mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setMaskBufferOffset:") retVoid [argCULong value]

-- | Number of polygons. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- polygonCount@
polygonCount :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> IO CULong
polygonCount mpsPolygonBuffer  =
    sendMsg mpsPolygonBuffer (mkSelector "polygonCount") retCULong []

-- | Number of polygons. Changes to this property require rebuilding the acceleration structure.
--
-- ObjC selector: @- setPolygonCount:@
setPolygonCount :: IsMPSPolygonBuffer mpsPolygonBuffer => mpsPolygonBuffer -> CULong -> IO ()
setPolygonCount mpsPolygonBuffer  value =
    sendMsg mpsPolygonBuffer (mkSelector "setPolygonCount:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @polygonBuffer@
polygonBufferSelector :: Selector
polygonBufferSelector = mkSelector "polygonBuffer"

-- | @Selector@ for @copyWithZone:@
copyWithZoneSelector :: Selector
copyWithZoneSelector = mkSelector "copyWithZone:"

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

