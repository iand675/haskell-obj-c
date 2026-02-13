{-# LANGUAGE DataKinds #-}
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
  , edgeCreaseCountSelector
  , edgeCreaseIndicesSelector
  , edgeCreasesSelector
  , faceCountSelector
  , faceTopologySelector
  , holeCountSelector
  , holesSelector
  , initWithSubmeshSelector
  , setEdgeCreaseCountSelector
  , setEdgeCreaseIndicesSelector
  , setEdgeCreasesSelector
  , setFaceCountSelector
  , setFaceTopologySelector
  , setHoleCountSelector
  , setHolesSelector
  , setVertexCreaseCountSelector
  , setVertexCreaseIndicesSelector
  , setVertexCreasesSelector
  , vertexCreaseCountSelector
  , vertexCreaseIndicesSelector
  , vertexCreasesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithSubmesh mdlSubmeshTopology submesh =
  sendOwnedMessage mdlSubmeshTopology initWithSubmeshSelector (toMDLSubmesh submesh)

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
faceTopology mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology faceTopologySelector

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
setFaceTopology mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setFaceTopologySelector value

-- | faceCount
--
-- The number of faces encoded in faceTopologyBuffer
--
-- ObjC selector: @- faceCount@
faceCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
faceCount mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology faceCountSelector

-- | faceCount
--
-- The number of faces encoded in faceTopologyBuffer
--
-- ObjC selector: @- setFaceCount:@
setFaceCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setFaceCount mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setFaceCountSelector value

-- | A crease value at a vertex to be applied during subdivision. Vertex creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are vertex indices. The corresponding values in the corner sharpness attribute indicate the corner sharpness of those vertices. The index buffer is sparse. If a mesh has three sharp vertices, then the index buffer will have three entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- vertexCreaseIndices@
vertexCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
vertexCreaseIndices mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology vertexCreaseIndicesSelector

-- | A crease value at a vertex to be applied during subdivision. Vertex creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are vertex indices. The corresponding values in the corner sharpness attribute indicate the corner sharpness of those vertices. The index buffer is sparse. If a mesh has three sharp vertices, then the index buffer will have three entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- setVertexCreaseIndices:@
setVertexCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setVertexCreaseIndices mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setVertexCreaseIndicesSelector value

-- | @- vertexCreases@
vertexCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
vertexCreases mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology vertexCreasesSelector

-- | @- setVertexCreases:@
setVertexCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setVertexCreases mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setVertexCreasesSelector value

-- | vertexCreaseCount
--
-- The number of vertex creases encoded in vertexCreases
--
-- ObjC selector: @- vertexCreaseCount@
vertexCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
vertexCreaseCount mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology vertexCreaseCountSelector

-- | vertexCreaseCount
--
-- The number of vertex creases encoded in vertexCreases
--
-- ObjC selector: @- setVertexCreaseCount:@
setVertexCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setVertexCreaseCount mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setVertexCreaseCountSelector value

-- | A crease value at an edge to be applied during subdivision. Edge creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are edge index pairs. Accordingly, there will be two index entries for each edge sharpness entry, and the sharpness entry corresponds to the edge itself. The corresponding values in the edge sharpness attribute indicate the edge sharpness of those edges.  The index buffer is sparse. If a mesh has three sharp edges, then the index buffer will have six entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- edgeCreaseIndices@
edgeCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
edgeCreaseIndices mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology edgeCreaseIndicesSelector

-- | A crease value at an edge to be applied during subdivision. Edge creases A zero value is smooth, a one value is peaked. It is intended to be used with an index buffer, where the index buffer entries are edge index pairs. Accordingly, there will be two index entries for each edge sharpness entry, and the sharpness entry corresponds to the edge itself. The corresponding values in the edge sharpness attribute indicate the edge sharpness of those edges.  The index buffer is sparse. If a mesh has three sharp edges, then the index buffer will have six entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- setEdgeCreaseIndices:@
setEdgeCreaseIndices :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setEdgeCreaseIndices mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setEdgeCreaseIndicesSelector value

-- | @- edgeCreases@
edgeCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
edgeCreases mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology edgeCreasesSelector

-- | @- setEdgeCreases:@
setEdgeCreases :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setEdgeCreases mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setEdgeCreasesSelector value

-- | edgeCreaseCount
--
-- The number of edge creases encoded in edgeCreases
--
-- ObjC selector: @- edgeCreaseCount@
edgeCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
edgeCreaseCount mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology edgeCreaseCountSelector

-- | edgeCreaseCount
--
-- The number of edge creases encoded in edgeCreases
--
-- ObjC selector: @- setEdgeCreaseCount:@
setEdgeCreaseCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setEdgeCreaseCount mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setEdgeCreaseCountSelector value

-- | The hole attribute is a vertex attribute of single integer values where each integer is an index of a face that is to be used as a hole. If there are two holes in a mesh, then the vertex buffer will have two entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- holes@
holes :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO RawId
holes mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology holesSelector

-- | The hole attribute is a vertex attribute of single integer values where each integer is an index of a face that is to be used as a hole. If there are two holes in a mesh, then the vertex buffer will have two entries. Since the number of entries in this vertex buffer is likely to be different than the number of entries in any other vertex buffer, it shouldn't be interleaved with other data.
--
-- ObjC selector: @- setHoles:@
setHoles :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> RawId -> IO ()
setHoles mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setHolesSelector value

-- | holeCount
--
-- The number of holes encoded in holes
--
-- ObjC selector: @- holeCount@
holeCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> IO CULong
holeCount mdlSubmeshTopology =
  sendMessage mdlSubmeshTopology holeCountSelector

-- | holeCount
--
-- The number of holes encoded in holes
--
-- ObjC selector: @- setHoleCount:@
setHoleCount :: IsMDLSubmeshTopology mdlSubmeshTopology => mdlSubmeshTopology -> CULong -> IO ()
setHoleCount mdlSubmeshTopology value =
  sendMessage mdlSubmeshTopology setHoleCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSubmesh:@
initWithSubmeshSelector :: Selector '[Id MDLSubmesh] (Id MDLSubmeshTopology)
initWithSubmeshSelector = mkSelector "initWithSubmesh:"

-- | @Selector@ for @faceTopology@
faceTopologySelector :: Selector '[] RawId
faceTopologySelector = mkSelector "faceTopology"

-- | @Selector@ for @setFaceTopology:@
setFaceTopologySelector :: Selector '[RawId] ()
setFaceTopologySelector = mkSelector "setFaceTopology:"

-- | @Selector@ for @faceCount@
faceCountSelector :: Selector '[] CULong
faceCountSelector = mkSelector "faceCount"

-- | @Selector@ for @setFaceCount:@
setFaceCountSelector :: Selector '[CULong] ()
setFaceCountSelector = mkSelector "setFaceCount:"

-- | @Selector@ for @vertexCreaseIndices@
vertexCreaseIndicesSelector :: Selector '[] RawId
vertexCreaseIndicesSelector = mkSelector "vertexCreaseIndices"

-- | @Selector@ for @setVertexCreaseIndices:@
setVertexCreaseIndicesSelector :: Selector '[RawId] ()
setVertexCreaseIndicesSelector = mkSelector "setVertexCreaseIndices:"

-- | @Selector@ for @vertexCreases@
vertexCreasesSelector :: Selector '[] RawId
vertexCreasesSelector = mkSelector "vertexCreases"

-- | @Selector@ for @setVertexCreases:@
setVertexCreasesSelector :: Selector '[RawId] ()
setVertexCreasesSelector = mkSelector "setVertexCreases:"

-- | @Selector@ for @vertexCreaseCount@
vertexCreaseCountSelector :: Selector '[] CULong
vertexCreaseCountSelector = mkSelector "vertexCreaseCount"

-- | @Selector@ for @setVertexCreaseCount:@
setVertexCreaseCountSelector :: Selector '[CULong] ()
setVertexCreaseCountSelector = mkSelector "setVertexCreaseCount:"

-- | @Selector@ for @edgeCreaseIndices@
edgeCreaseIndicesSelector :: Selector '[] RawId
edgeCreaseIndicesSelector = mkSelector "edgeCreaseIndices"

-- | @Selector@ for @setEdgeCreaseIndices:@
setEdgeCreaseIndicesSelector :: Selector '[RawId] ()
setEdgeCreaseIndicesSelector = mkSelector "setEdgeCreaseIndices:"

-- | @Selector@ for @edgeCreases@
edgeCreasesSelector :: Selector '[] RawId
edgeCreasesSelector = mkSelector "edgeCreases"

-- | @Selector@ for @setEdgeCreases:@
setEdgeCreasesSelector :: Selector '[RawId] ()
setEdgeCreasesSelector = mkSelector "setEdgeCreases:"

-- | @Selector@ for @edgeCreaseCount@
edgeCreaseCountSelector :: Selector '[] CULong
edgeCreaseCountSelector = mkSelector "edgeCreaseCount"

-- | @Selector@ for @setEdgeCreaseCount:@
setEdgeCreaseCountSelector :: Selector '[CULong] ()
setEdgeCreaseCountSelector = mkSelector "setEdgeCreaseCount:"

-- | @Selector@ for @holes@
holesSelector :: Selector '[] RawId
holesSelector = mkSelector "holes"

-- | @Selector@ for @setHoles:@
setHolesSelector :: Selector '[RawId] ()
setHolesSelector = mkSelector "setHoles:"

-- | @Selector@ for @holeCount@
holeCountSelector :: Selector '[] CULong
holeCountSelector = mkSelector "holeCount"

-- | @Selector@ for @setHoleCount:@
setHoleCountSelector :: Selector '[CULong] ()
setHoleCountSelector = mkSelector "setHoleCount:"

