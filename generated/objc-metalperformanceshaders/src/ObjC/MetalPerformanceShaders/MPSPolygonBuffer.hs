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
  , vertexBufferOffset
  , setVertexBufferOffset
  , indexBufferOffset
  , setIndexBufferOffset
  , maskBufferOffset
  , setMaskBufferOffset
  , polygonCount
  , setPolygonCount
  , initSelector
  , initWithCoderSelector
  , polygonBufferSelector
  , copyWithZoneSelector
  , vertexBufferOffsetSelector
  , setVertexBufferOffsetSelector
  , indexBufferOffsetSelector
  , setIndexBufferOffsetSelector
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
  sendMsg mpsPolygonBuffer (mkSelector "setVertexBufferOffset:") retVoid [argCULong (fromIntegral value)]

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
  sendMsg mpsPolygonBuffer (mkSelector "setIndexBufferOffset:") retVoid [argCULong (fromIntegral value)]

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
  sendMsg mpsPolygonBuffer (mkSelector "setMaskBufferOffset:") retVoid [argCULong (fromIntegral value)]

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
  sendMsg mpsPolygonBuffer (mkSelector "setPolygonCount:") retVoid [argCULong (fromIntegral value)]

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

-- | @Selector@ for @vertexBufferOffset@
vertexBufferOffsetSelector :: Selector
vertexBufferOffsetSelector = mkSelector "vertexBufferOffset"

-- | @Selector@ for @setVertexBufferOffset:@
setVertexBufferOffsetSelector :: Selector
setVertexBufferOffsetSelector = mkSelector "setVertexBufferOffset:"

-- | @Selector@ for @indexBufferOffset@
indexBufferOffsetSelector :: Selector
indexBufferOffsetSelector = mkSelector "indexBufferOffset"

-- | @Selector@ for @setIndexBufferOffset:@
setIndexBufferOffsetSelector :: Selector
setIndexBufferOffsetSelector = mkSelector "setIndexBufferOffset:"

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

