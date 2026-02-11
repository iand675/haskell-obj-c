{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLSubmeshTopology@.
module ObjC.ModelIO.MDLSubmeshTopology
  ( MDLSubmeshTopology
  , IsMDLSubmeshTopology(..)
  , initWithSubmesh
  , faceCount
  , setFaceCount
  , vertexCreaseCount
  , setVertexCreaseCount
  , edgeCreaseCount
  , setEdgeCreaseCount
  , holeCount
  , setHoleCount
  , initWithSubmeshSelector
  , faceCountSelector
  , setFaceCountSelector
  , vertexCreaseCountSelector
  , setVertexCreaseCountSelector
  , edgeCreaseCountSelector
  , setEdgeCreaseCountSelector
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
  sendMsg mdlSubmeshTopology (mkSelector "setFaceCount:") retVoid [argCULong (fromIntegral value)]

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
  sendMsg mdlSubmeshTopology (mkSelector "setVertexCreaseCount:") retVoid [argCULong (fromIntegral value)]

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
  sendMsg mdlSubmeshTopology (mkSelector "setEdgeCreaseCount:") retVoid [argCULong (fromIntegral value)]

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
  sendMsg mdlSubmeshTopology (mkSelector "setHoleCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSubmesh:@
initWithSubmeshSelector :: Selector
initWithSubmeshSelector = mkSelector "initWithSubmesh:"

-- | @Selector@ for @faceCount@
faceCountSelector :: Selector
faceCountSelector = mkSelector "faceCount"

-- | @Selector@ for @setFaceCount:@
setFaceCountSelector :: Selector
setFaceCountSelector = mkSelector "setFaceCount:"

-- | @Selector@ for @vertexCreaseCount@
vertexCreaseCountSelector :: Selector
vertexCreaseCountSelector = mkSelector "vertexCreaseCount"

-- | @Selector@ for @setVertexCreaseCount:@
setVertexCreaseCountSelector :: Selector
setVertexCreaseCountSelector = mkSelector "setVertexCreaseCount:"

-- | @Selector@ for @edgeCreaseCount@
edgeCreaseCountSelector :: Selector
edgeCreaseCountSelector = mkSelector "edgeCreaseCount"

-- | @Selector@ for @setEdgeCreaseCount:@
setEdgeCreaseCountSelector :: Selector
setEdgeCreaseCountSelector = mkSelector "setEdgeCreaseCount:"

-- | @Selector@ for @holeCount@
holeCountSelector :: Selector
holeCountSelector = mkSelector "holeCount"

-- | @Selector@ for @setHoleCount:@
setHoleCountSelector :: Selector
setHoleCountSelector = mkSelector "setHoleCount:"

