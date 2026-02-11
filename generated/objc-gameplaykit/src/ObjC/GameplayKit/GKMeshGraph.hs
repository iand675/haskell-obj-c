{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of GKGraphNodes that are governed by a mesh formed by the space between a set of GKPolygonObstacles
--
-- Generated bindings for @GKMeshGraph@.
module ObjC.GameplayKit.GKMeshGraph
  ( GKMeshGraph
  , IsGKMeshGraph(..)
  , addObstacles
  , removeObstacles
  , connectNodeUsingObstacles
  , triangulate
  , classForGenericArgumentAtIndex
  , obstacles
  , bufferRadius
  , triangulationMode
  , setTriangulationMode
  , triangleCount
  , addObstaclesSelector
  , removeObstaclesSelector
  , connectNodeUsingObstaclesSelector
  , triangulateSelector
  , classForGenericArgumentAtIndexSelector
  , obstaclesSelector
  , bufferRadiusSelector
  , triangulationModeSelector
  , setTriangulationModeSelector
  , triangleCountSelector

  -- * Enum types
  , GKMeshGraphTriangulationMode(GKMeshGraphTriangulationMode)
  , pattern GKMeshGraphTriangulationModeVertices
  , pattern GKMeshGraphTriangulationModeCenters
  , pattern GKMeshGraphTriangulationModeEdgeMidpoints

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.GameplayKit.Internal.Structs
import ObjC.GameplayKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Adds obstacles to this mesh graph.  Only reflected after the next triangulate call.
--
-- ObjC selector: @- addObstacles:@
addObstacles :: (IsGKMeshGraph gkMeshGraph, IsNSArray obstacles) => gkMeshGraph -> obstacles -> IO ()
addObstacles gkMeshGraph  obstacles =
withObjCPtr obstacles $ \raw_obstacles ->
    sendMsg gkMeshGraph (mkSelector "addObstacles:") retVoid [argPtr (castPtr raw_obstacles :: Ptr ())]

-- | Removes obstacles from this graph.  Only reflected after the next triangulate call.
--
-- ObjC selector: @- removeObstacles:@
removeObstacles :: (IsGKMeshGraph gkMeshGraph, IsNSArray obstacles) => gkMeshGraph -> obstacles -> IO ()
removeObstacles gkMeshGraph  obstacles =
withObjCPtr obstacles $ \raw_obstacles ->
    sendMsg gkMeshGraph (mkSelector "removeObstacles:") retVoid [argPtr (castPtr raw_obstacles :: Ptr ())]

-- | Connects the node to this graph by inserting it into an existing triangle and making the appropriate connections Node must be in the space defined by the min and max coordinates of this graph.
--
-- @node@ — the node to connect
--
-- ObjC selector: @- connectNodeUsingObstacles:@
connectNodeUsingObstacles :: (IsGKMeshGraph gkMeshGraph, IsGKGraphNode2D node) => gkMeshGraph -> node -> IO ()
connectNodeUsingObstacles gkMeshGraph  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkMeshGraph (mkSelector "connectNodeUsingObstacles:") retVoid [argPtr (castPtr raw_node :: Ptr ())]

-- | Generates a new triangle mesh for the given obstacles.   This should be called after some number of calls to addObstacle The negative space between all input obstacles are triangulated to create a mesh This mesh is turned into a set of connected graph nodes based on
--
-- ObjC selector: @- triangulate@
triangulate :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> IO ()
triangulate gkMeshGraph  =
  sendMsg gkMeshGraph (mkSelector "triangulate") retVoid []

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> CULong -> IO Class
classForGenericArgumentAtIndex gkMeshGraph  index =
  fmap (Class . castPtr) $ sendMsg gkMeshGraph (mkSelector "classForGenericArgumentAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | Array of the extruded obstacles currently represented by this graph
--
-- ObjC selector: @- obstacles@
obstacles :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> IO (Id NSArray)
obstacles gkMeshGraph  =
  sendMsg gkMeshGraph (mkSelector "obstacles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The distance by which all obstacles are extruded. This is most commonly the spatial bounding radius of a potential traveler on this path
--
-- ObjC selector: @- bufferRadius@
bufferRadius :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> IO CFloat
bufferRadius gkMeshGraph  =
  sendMsg gkMeshGraph (mkSelector "bufferRadius") retCFloat []

-- | Specifies how graph nodes are generated when you triangulate this graph. You can combine triangulation modes using the | (OR) operator
--
-- See: GKMeshGraphTriangulationMode
--
-- ObjC selector: @- triangulationMode@
triangulationMode :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> IO GKMeshGraphTriangulationMode
triangulationMode gkMeshGraph  =
  fmap (coerce :: CULong -> GKMeshGraphTriangulationMode) $ sendMsg gkMeshGraph (mkSelector "triangulationMode") retCULong []

-- | Specifies how graph nodes are generated when you triangulate this graph. You can combine triangulation modes using the | (OR) operator
--
-- See: GKMeshGraphTriangulationMode
--
-- ObjC selector: @- setTriangulationMode:@
setTriangulationMode :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> GKMeshGraphTriangulationMode -> IO ()
setTriangulationMode gkMeshGraph  value =
  sendMsg gkMeshGraph (mkSelector "setTriangulationMode:") retVoid [argCULong (coerce value)]

-- | The number of triangles currently in this mesh graph
--
-- ObjC selector: @- triangleCount@
triangleCount :: IsGKMeshGraph gkMeshGraph => gkMeshGraph -> IO CULong
triangleCount gkMeshGraph  =
  sendMsg gkMeshGraph (mkSelector "triangleCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObstacles:@
addObstaclesSelector :: Selector
addObstaclesSelector = mkSelector "addObstacles:"

-- | @Selector@ for @removeObstacles:@
removeObstaclesSelector :: Selector
removeObstaclesSelector = mkSelector "removeObstacles:"

-- | @Selector@ for @connectNodeUsingObstacles:@
connectNodeUsingObstaclesSelector :: Selector
connectNodeUsingObstaclesSelector = mkSelector "connectNodeUsingObstacles:"

-- | @Selector@ for @triangulate@
triangulateSelector :: Selector
triangulateSelector = mkSelector "triangulate"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @obstacles@
obstaclesSelector :: Selector
obstaclesSelector = mkSelector "obstacles"

-- | @Selector@ for @bufferRadius@
bufferRadiusSelector :: Selector
bufferRadiusSelector = mkSelector "bufferRadius"

-- | @Selector@ for @triangulationMode@
triangulationModeSelector :: Selector
triangulationModeSelector = mkSelector "triangulationMode"

-- | @Selector@ for @setTriangulationMode:@
setTriangulationModeSelector :: Selector
setTriangulationModeSelector = mkSelector "setTriangulationMode:"

-- | @Selector@ for @triangleCount@
triangleCountSelector :: Selector
triangleCountSelector = mkSelector "triangleCount"

