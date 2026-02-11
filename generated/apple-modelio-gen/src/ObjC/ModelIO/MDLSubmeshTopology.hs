{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLSubmeshTopology@.
module ObjC.ModelIO.MDLSubmeshTopology
  ( MDLSubmeshTopology
  , IsMDLSubmeshTopology(..)
  , initWithSubmesh
  , faceTopology
  , setFaceTopology
  , faceCount
  , setFaceCount
  , vertexCreaseIndices
  , setVertexCreaseIndices
  , vertexCreases
  , setVertexCreases
  , vertexCreaseCount
  , setVertexCreaseCount
  , edgeCreaseIndices
  , setEdgeCreaseIndices
  , edgeCreases
  , setEdgeCreases
  , edgeCreaseCount
  , setEdgeCreaseCount
  , holes
  , setHoles
  , holeCount
  , setHoleCount
  , initWithSubmeshSelector
  , faceTopologySelector
  , setFaceTopologySelector
  , faceCountSelector
  , setFaceCountSelector
  , vertexCreaseIndicesSelector
  , setVertexCreaseIndicesSelector
  , vertexCreasesSelector
  , setVertexCreasesSelector
  , vertexCreaseCountSelector
  , setVertexCreaseCountSelector
  , edgeCreaseIndicesSelector
  , setEdgeCreaseIndicesSelector
  , edgeCreasesSelector
  , setEdgeCreasesSelector
  , edgeCreaseCountSelector
  , setEdgeCreaseCountSelector
  , holesSelector
  , setHolesSelector
  , holeCountSelector
  , setHoleCountSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithSubmesh:
--
-- create a topology object corresponding to the topology in the submesh
--
-- ObjC selector: @- initWithSubmesh:@
initWithSubmesh :: (IsMDLSubmeshTopology mdlSubmeshTopology, IsMDLSubmesh submesh) => mdlSubmeshTopology -> submesh -> IO (Id MDLSubmeshTopology)
initWithSubmesh mdlSubmeshTopology  submesh =
  withObjCPtr submesh $ \raw_submesh ->
      sendMsg mdlSubmeshTopology (mkSelector "initWithSubmesh:") (retPtr retVoid) [argPtr (castPtr raw_submesh :: Ptr ())] >>= ownedObject . castPtr

-- | faceTopologyBuffer
--
-- A buffer of 8 bit unsigned integer values, where each entry corresponds to the number of vertices making up a face.
--
-- A submesh containing two triangles, a four sided polygon, and a line, would contain the data 3 3 4 2. If geometryType is of a fixed type, such as triangles, the buffer is optional, and will be created on demand if read.
--
-- Indices to the vertex buffer will be stored in the index buffer correspondingly. In the example above, the indices would be stored in order, three indices for the first triangle, followed by three for the second, followed by four for the polygon, and finally two indices for the line.
--
-- ObjC selector: @- faceTopology@
faceTopology :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
faceTopology mdlSubmeshTopology  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmeshTopology (mkSelector "faceTopology") (retPtr retVoid) []

-- | faceTopologyBuffer
--
-- A buffer of 8 bit unsigned integer values, where each entry corresponds to the number of vertices making up a face.
--
-- A submesh containing two triangles, a four sided polygon, and a line, would contain the data 3 3 4 2. If geometryType is of a fixed type, such as triangles, the buffer is optional, and will be created on demand if read.
--
-- Indices to the vertex buffer will be stored in the index buffer correspondingly. In the example above, the indices would be stored in order, three indices for the first triangle, followed by three for the second, followed by four for the polygon, and finally two indices for the line.
--
-- ObjC selector: @- setFaceTopology:@
setFaceTopology :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setFaceTopology mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setFaceTopology:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | faceCount
--
-- The number of faces encoded in faceTopologyBuffer
--
-- ObjC selector: @- faceCount@
faceCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
faceCount mdlSubmeshTopology  =
    sendMsg mdlSubmeshTopology (mkSelector "faceCount") retCULong []

-- | faceCount
--
-- The number of faces encoded in faceTopologyBuffer
--
-- ObjC selector: @- setFaceCount:@
setFaceCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setFaceCount mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setFaceCount:") retVoid [argCULong value]

-- | A crease value at a vertex to be applied during subdivision. Vertex creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are vertex indices. The corresponding values in the corner sharpness attribute indicate the corner sharpness of those vertices. The index buffer is sparse. If a mesh has three sharp vertices, then the index buffer will have three entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- vertexCreaseIndices@
vertexCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
vertexCreaseIndices mdlSubmeshTopology  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmeshTopology (mkSelector "vertexCreaseIndices") (retPtr retVoid) []

-- | A crease value at a vertex to be applied during subdivision. Vertex creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are vertex indices. The corresponding values in the corner sharpness attribute indicate the corner sharpness of those vertices. The index buffer is sparse. If a mesh has three sharp vertices, then the index buffer will have three entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- setVertexCreaseIndices:@
setVertexCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setVertexCreaseIndices mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setVertexCreaseIndices:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- vertexCreases@
vertexCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
vertexCreases mdlSubmeshTopology  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmeshTopology (mkSelector "vertexCreases") (retPtr retVoid) []

-- | @- setVertexCreases:@
setVertexCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setVertexCreases mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setVertexCreases:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | vertexCreaseCount
--
-- The number of vertex creases encoded in vertexCreases
--
-- ObjC selector: @- vertexCreaseCount@
vertexCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
vertexCreaseCount mdlSubmeshTopology  =
    sendMsg mdlSubmeshTopology (mkSelector "vertexCreaseCount") retCULong []

-- | vertexCreaseCount
--
-- The number of vertex creases encoded in vertexCreases
--
-- ObjC selector: @- setVertexCreaseCount:@
setVertexCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setVertexCreaseCount mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setVertexCreaseCount:") retVoid [argCULong value]

-- | A crease value at an edge to be applied during subdivision. Edge creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are edge index pairs. Accordingly, there will be two index entries for each edge sharpness entry, and the sharpness entry corresponds to the edge itself. The corresponding values in the edge sharpness attribute indicate the edge sharpness of those edges.  The index buffer is sparse. If a mesh has three sharp edges, then the index buffer will have six entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- edgeCreaseIndices@
edgeCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
edgeCreaseIndices mdlSubmeshTopology  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmeshTopology (mkSelector "edgeCreaseIndices") (retPtr retVoid) []

-- | A crease value at an edge to be applied during subdivision. Edge creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are edge index pairs. Accordingly, there will be two index entries for each edge sharpness entry, and the sharpness entry corresponds to the edge itself. The corresponding values in the edge sharpness attribute indicate the edge sharpness of those edges.  The index buffer is sparse. If a mesh has three sharp edges, then the index buffer will have six entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- setEdgeCreaseIndices:@
setEdgeCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setEdgeCreaseIndices mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setEdgeCreaseIndices:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- edgeCreases@
edgeCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
edgeCreases mdlSubmeshTopology  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmeshTopology (mkSelector "edgeCreases") (retPtr retVoid) []

-- | @- setEdgeCreases:@
setEdgeCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setEdgeCreases mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setEdgeCreases:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | edgeCreaseCount
--
-- The number of edge creases encoded in edgeCreases
--
-- ObjC selector: @- edgeCreaseCount@
edgeCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
edgeCreaseCount mdlSubmeshTopology  =
    sendMsg mdlSubmeshTopology (mkSelector "edgeCreaseCount") retCULong []

-- | edgeCreaseCount
--
-- The number of edge creases encoded in edgeCreases
--
-- ObjC selector: @- setEdgeCreaseCount:@
setEdgeCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setEdgeCreaseCount mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setEdgeCreaseCount:") retVoid [argCULong value]

-- | The hole attribute is a vertex attribute of single integer values where each integer is an index of a face that is to be used as a hole. If there are two holes in a mesh, then the vertex buffer will have two entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- holes@
holes :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
holes mdlSubmeshTopology  =
    fmap (RawId . castPtr) $ sendMsg mdlSubmeshTopology (mkSelector "holes") (retPtr retVoid) []

-- | The hole attribute is a vertex attribute of single integer values where each integer is an index of a face that is to be used as a hole. If there are two holes in a mesh, then the vertex buffer will have two entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- setHoles:@
setHoles :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setHoles mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setHoles:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | holeCount
--
-- The number of holes encoded in holes
--
-- ObjC selector: @- holeCount@
holeCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
holeCount mdlSubmeshTopology  =
    sendMsg mdlSubmeshTopology (mkSelector "holeCount") retCULong []

-- | holeCount
--
-- The number of holes encoded in holes
--
-- ObjC selector: @- setHoleCount:@
setHoleCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setHoleCount mdlSubmeshTopology  value =
    sendMsg mdlSubmeshTopology (mkSelector "setHoleCount:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSubmesh:@
initWithSubmeshSelector :: Selector
initWithSubmeshSelector = mkSelector "initWithSubmesh:"

-- | @Selector@ for @faceTopology@
faceTopologySelector :: Selector
faceTopologySelector = mkSelector "faceTopology"

-- | @Selector@ for @setFaceTopology:@
setFaceTopologySelector :: Selector
setFaceTopologySelector = mkSelector "setFaceTopology:"

-- | @Selector@ for @faceCount@
faceCountSelector :: Selector
faceCountSelector = mkSelector "faceCount"

-- | @Selector@ for @setFaceCount:@
setFaceCountSelector :: Selector
setFaceCountSelector = mkSelector "setFaceCount:"

-- | @Selector@ for @vertexCreaseIndices@
vertexCreaseIndicesSelector :: Selector
vertexCreaseIndicesSelector = mkSelector "vertexCreaseIndices"

-- | @Selector@ for @setVertexCreaseIndices:@
setVertexCreaseIndicesSelector :: Selector
setVertexCreaseIndicesSelector = mkSelector "setVertexCreaseIndices:"

-- | @Selector@ for @vertexCreases@
vertexCreasesSelector :: Selector
vertexCreasesSelector = mkSelector "vertexCreases"

-- | @Selector@ for @setVertexCreases:@
setVertexCreasesSelector :: Selector
setVertexCreasesSelector = mkSelector "setVertexCreases:"

-- | @Selector@ for @vertexCreaseCount@
vertexCreaseCountSelector :: Selector
vertexCreaseCountSelector = mkSelector "vertexCreaseCount"

-- | @Selector@ for @setVertexCreaseCount:@
setVertexCreaseCountSelector :: Selector
setVertexCreaseCountSelector = mkSelector "setVertexCreaseCount:"

-- | @Selector@ for @edgeCreaseIndices@
edgeCreaseIndicesSelector :: Selector
edgeCreaseIndicesSelector = mkSelector "edgeCreaseIndices"

-- | @Selector@ for @setEdgeCreaseIndices:@
setEdgeCreaseIndicesSelector :: Selector
setEdgeCreaseIndicesSelector = mkSelector "setEdgeCreaseIndices:"

-- | @Selector@ for @edgeCreases@
edgeCreasesSelector :: Selector
edgeCreasesSelector = mkSelector "edgeCreases"

-- | @Selector@ for @setEdgeCreases:@
setEdgeCreasesSelector :: Selector
setEdgeCreasesSelector = mkSelector "setEdgeCreases:"

-- | @Selector@ for @edgeCreaseCount@
edgeCreaseCountSelector :: Selector
edgeCreaseCountSelector = mkSelector "edgeCreaseCount"

-- | @Selector@ for @setEdgeCreaseCount:@
setEdgeCreaseCountSelector :: Selector
setEdgeCreaseCountSelector = mkSelector "setEdgeCreaseCount:"

-- | @Selector@ for @holes@
holesSelector :: Selector
holesSelector = mkSelector "holes"

-- | @Selector@ for @setHoles:@
setHolesSelector :: Selector
setHolesSelector = mkSelector "setHoles:"

-- | @Selector@ for @holeCount@
holeCountSelector :: Selector
holeCountSelector = mkSelector "holeCount"

-- | @Selector@ for @setHoleCount:@
setHoleCountSelector :: Selector
setHoleCountSelector = mkSelector "setHoleCount:"

