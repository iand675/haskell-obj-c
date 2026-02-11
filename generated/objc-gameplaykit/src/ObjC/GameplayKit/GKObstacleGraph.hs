{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of GKGraphNodes that are governed by a set of extruded GKPolygonObstacles
--
-- Generated bindings for @GKObstacleGraph@.
module ObjC.GameplayKit.GKObstacleGraph
  ( GKObstacleGraph
  , IsGKObstacleGraph(..)
  , graphWithObstacles_bufferRadius
  , initWithObstacles_bufferRadius
  , graphWithObstacles_bufferRadius_nodeClass
  , initWithObstacles_bufferRadius_nodeClass
  , connectNodeUsingObstacles
  , connectNodeUsingObstacles_ignoringObstacles
  , connectNodeUsingObstacles_ignoringBufferRadiusOfObstacles
  , addObstacles
  , removeObstacles
  , removeAllObstacles
  , nodesForObstacle
  , lockConnectionFromNode_toNode
  , unlockConnectionFromNode_toNode
  , isConnectionLockedFromNode_toNode
  , classForGenericArgumentAtIndex
  , obstacles
  , bufferRadius
  , graphWithObstacles_bufferRadiusSelector
  , initWithObstacles_bufferRadiusSelector
  , graphWithObstacles_bufferRadius_nodeClassSelector
  , initWithObstacles_bufferRadius_nodeClassSelector
  , connectNodeUsingObstaclesSelector
  , connectNodeUsingObstacles_ignoringObstaclesSelector
  , connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector
  , addObstaclesSelector
  , removeObstaclesSelector
  , removeAllObstaclesSelector
  , nodesForObstacleSelector
  , lockConnectionFromNode_toNodeSelector
  , unlockConnectionFromNode_toNodeSelector
  , isConnectionLockedFromNode_toNodeSelector
  , classForGenericArgumentAtIndexSelector
  , obstaclesSelector
  , bufferRadiusSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates an optimal bidirectional graph based on a list of obstacles. Each vertex of each obstacle is extruded and a connection is made between each vertex that does not intersect an obstacle Guaranteed not to have any edges which intersect obstacles. Same effect as [[GKObstacleGraph alloc] init], setting bufferRadius, and then calling addObstacles.
--
-- @obstacles@ — a list of obstacles to create the graph from
--
-- @bufferRadius@ — the circular radius of a potential agent that will navigate this graph.  Obstacles are extruded by this amount to create the graph.  Must be positive.  Negative values are clipped to 0.0f
--
-- ObjC selector: @+ graphWithObstacles:bufferRadius:@
graphWithObstacles_bufferRadius :: IsNSArray obstacles => obstacles -> CFloat -> IO (Id GKObstacleGraph)
graphWithObstacles_bufferRadius obstacles bufferRadius =
  do
    cls' <- getRequiredClass "GKObstacleGraph"
    withObjCPtr obstacles $ \raw_obstacles ->
      sendClassMsg cls' (mkSelector "graphWithObstacles:bufferRadius:") (retPtr retVoid) [argPtr (castPtr raw_obstacles :: Ptr ()), argCFloat (fromIntegral bufferRadius)] >>= retainedObject . castPtr

-- | @- initWithObstacles:bufferRadius:@
initWithObstacles_bufferRadius :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> CFloat -> IO (Id GKObstacleGraph)
initWithObstacles_bufferRadius gkObstacleGraph  obstacles bufferRadius =
withObjCPtr obstacles $ \raw_obstacles ->
    sendMsg gkObstacleGraph (mkSelector "initWithObstacles:bufferRadius:") (retPtr retVoid) [argPtr (castPtr raw_obstacles :: Ptr ()), argCFloat (fromIntegral bufferRadius)] >>= ownedObject . castPtr

-- | Creates an optimal bidirectional graph based on a list of obstacles. Each vertex of each obstacle is extruded and a connection is made between each vertex that does not intersect an obstacle Guaranteed not to have any edges which intersect obstacles. Same effect as [[GKObstacleGraph alloc] init], setting bufferRadius, and then calling addObstacles.
--
-- @obstacles@ — a list of obstacles to create the graph from
--
-- @bufferRadius@ — the circular radius of a potential agent that will navigate this graph.  Obstacles are extruded by this amount to create the graph.  Must be positive.  Negative values are clipped to 0.0f
--
-- @nodeClass@ — the class of the nodes that this graph should create.  Must descend from GKGraphNode2D
--
-- ObjC selector: @+ graphWithObstacles:bufferRadius:nodeClass:@
graphWithObstacles_bufferRadius_nodeClass :: IsNSArray obstacles => obstacles -> CFloat -> Class -> IO (Id GKObstacleGraph)
graphWithObstacles_bufferRadius_nodeClass obstacles bufferRadius nodeClass =
  do
    cls' <- getRequiredClass "GKObstacleGraph"
    withObjCPtr obstacles $ \raw_obstacles ->
      sendClassMsg cls' (mkSelector "graphWithObstacles:bufferRadius:nodeClass:") (retPtr retVoid) [argPtr (castPtr raw_obstacles :: Ptr ()), argCFloat (fromIntegral bufferRadius), argPtr (unClass nodeClass)] >>= retainedObject . castPtr

-- | @- initWithObstacles:bufferRadius:nodeClass:@
initWithObstacles_bufferRadius_nodeClass :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> CFloat -> Class -> IO (Id GKObstacleGraph)
initWithObstacles_bufferRadius_nodeClass gkObstacleGraph  obstacles bufferRadius nodeClass =
withObjCPtr obstacles $ \raw_obstacles ->
    sendMsg gkObstacleGraph (mkSelector "initWithObstacles:bufferRadius:nodeClass:") (retPtr retVoid) [argPtr (castPtr raw_obstacles :: Ptr ()), argCFloat (fromIntegral bufferRadius), argPtr (unClass nodeClass)] >>= ownedObject . castPtr

-- | Connects the node to this graph by testing edge intersection with existing obstacles. Same behavior as if this node had been present during initWithObstacles.
--
-- @node@ — the node to connect
--
-- ObjC selector: @- connectNodeUsingObstacles:@
connectNodeUsingObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D node) => gkObstacleGraph -> node -> IO ()
connectNodeUsingObstacles gkObstacleGraph  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkObstacleGraph (mkSelector "connectNodeUsingObstacles:") retVoid [argPtr (castPtr raw_node :: Ptr ())]

-- | Same behavior as connectNodeUsingObstacles: except you can optionally ignore certain obstacles from being tested for intersection.
--
-- ObjC selector: @- connectNodeUsingObstacles:ignoringObstacles:@
connectNodeUsingObstacles_ignoringObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D node, IsNSArray obstaclesToIgnore) => gkObstacleGraph -> node -> obstaclesToIgnore -> IO ()
connectNodeUsingObstacles_ignoringObstacles gkObstacleGraph  node obstaclesToIgnore =
withObjCPtr node $ \raw_node ->
  withObjCPtr obstaclesToIgnore $ \raw_obstaclesToIgnore ->
      sendMsg gkObstacleGraph (mkSelector "connectNodeUsingObstacles:ignoringObstacles:") retVoid [argPtr (castPtr raw_node :: Ptr ()), argPtr (castPtr raw_obstaclesToIgnore :: Ptr ())]

-- | Same behavior as connectNodeUsingObstacles: except you can optionally ignore the bounding radius of certain obstacles from being tested for intersection
--
-- ObjC selector: @- connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:@
connectNodeUsingObstacles_ignoringBufferRadiusOfObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D node, IsNSArray obstaclesBufferRadiusToIgnore) => gkObstacleGraph -> node -> obstaclesBufferRadiusToIgnore -> IO ()
connectNodeUsingObstacles_ignoringBufferRadiusOfObstacles gkObstacleGraph  node obstaclesBufferRadiusToIgnore =
withObjCPtr node $ \raw_node ->
  withObjCPtr obstaclesBufferRadiusToIgnore $ \raw_obstaclesBufferRadiusToIgnore ->
      sendMsg gkObstacleGraph (mkSelector "connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:") retVoid [argPtr (castPtr raw_node :: Ptr ()), argPtr (castPtr raw_obstaclesBufferRadiusToIgnore :: Ptr ())]

-- | Adds obstacles to this graph. Obstacle is extruded and graph nodes are generated from its vertices and then connected to this graph Nothing is done if an obstacle is already present in this graph Any existing connections that intersect the new obstacles are destroyed unless they are protected with [GKObstacleGraph lockConnection:]
--
-- @obstacles@ — an array of obstacles to be added
--
-- See: lockConnection
--
-- ObjC selector: @- addObstacles:@
addObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> IO ()
addObstacles gkObstacleGraph  obstacles =
withObjCPtr obstacles $ \raw_obstacles ->
    sendMsg gkObstacleGraph (mkSelector "addObstacles:") retVoid [argPtr (castPtr raw_obstacles :: Ptr ())]

-- | Removes obstacles from this graph. All associated graph nodes are removed and their connections are bidirectionally removed. Connections between obstacle nodes that were previously invalidated by any of these obstacles are restored. Nothing is done if an obstacle is already present in this graph
--
-- @obstacles@ — an array of obstacles to be removed
--
-- ObjC selector: @- removeObstacles:@
removeObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> IO ()
removeObstacles gkObstacleGraph  obstacles =
withObjCPtr obstacles $ \raw_obstacles ->
    sendMsg gkObstacleGraph (mkSelector "removeObstacles:") retVoid [argPtr (castPtr raw_obstacles :: Ptr ())]

-- | Removes all obstacles from this graph.
--
-- ObjC selector: @- removeAllObstacles@
removeAllObstacles :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> IO ()
removeAllObstacles gkObstacleGraph  =
  sendMsg gkObstacleGraph (mkSelector "removeAllObstacles") retVoid []

-- | Returns an array of the graph nodes associated with a given obstacle
--
-- @obstacle@ — the obstacle who's nodes are to be retrieved
--
-- ObjC selector: @- nodesForObstacle:@
nodesForObstacle :: (IsGKObstacleGraph gkObstacleGraph, IsGKPolygonObstacle obstacle) => gkObstacleGraph -> obstacle -> IO (Id NSArray)
nodesForObstacle gkObstacleGraph  obstacle =
withObjCPtr obstacle $ \raw_obstacle ->
    sendMsg gkObstacleGraph (mkSelector "nodesForObstacle:") (retPtr retVoid) [argPtr (castPtr raw_obstacle :: Ptr ())] >>= retainedObject . castPtr

-- | Marks a connection as "locked", preventing this connection from being destroyed when you add obstacles that would intersect it
--
-- @startNode@ — startNode of the connection to lock
--
-- @endNode@ — endNode of the connection to lock
--
-- ObjC selector: @- lockConnectionFromNode:toNode:@
lockConnectionFromNode_toNode :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D startNode, IsGKGraphNode2D endNode) => gkObstacleGraph -> startNode -> endNode -> IO ()
lockConnectionFromNode_toNode gkObstacleGraph  startNode endNode =
withObjCPtr startNode $ \raw_startNode ->
  withObjCPtr endNode $ \raw_endNode ->
      sendMsg gkObstacleGraph (mkSelector "lockConnectionFromNode:toNode:") retVoid [argPtr (castPtr raw_startNode :: Ptr ()), argPtr (castPtr raw_endNode :: Ptr ())]

-- | "Unlocks" a connection, removing its protection from being destroyed when you add obstacles that would intersect it
--
-- @startNode@ — startNode of the connection to unlock
--
-- @endNode@ — endNode of the connection to unlock
--
-- See: lockConnection
--
-- ObjC selector: @- unlockConnectionFromNode:toNode:@
unlockConnectionFromNode_toNode :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D startNode, IsGKGraphNode2D endNode) => gkObstacleGraph -> startNode -> endNode -> IO ()
unlockConnectionFromNode_toNode gkObstacleGraph  startNode endNode =
withObjCPtr startNode $ \raw_startNode ->
  withObjCPtr endNode $ \raw_endNode ->
      sendMsg gkObstacleGraph (mkSelector "unlockConnectionFromNode:toNode:") retVoid [argPtr (castPtr raw_startNode :: Ptr ()), argPtr (castPtr raw_endNode :: Ptr ())]

-- | Query if a given connection is locked
--
-- @startNode@ — startNode of the connection to query
--
-- @endNode@ — endNode of the connection to query
--
-- See: lockConnection
--
-- See: unlockConnection
--
-- Returns: YES if the connection was locked with lockConnection, NO if it was never locked or was unlocked via unlockConnection
--
-- ObjC selector: @- isConnectionLockedFromNode:toNode:@
isConnectionLockedFromNode_toNode :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D startNode, IsGKGraphNode2D endNode) => gkObstacleGraph -> startNode -> endNode -> IO Bool
isConnectionLockedFromNode_toNode gkObstacleGraph  startNode endNode =
withObjCPtr startNode $ \raw_startNode ->
  withObjCPtr endNode $ \raw_endNode ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkObstacleGraph (mkSelector "isConnectionLockedFromNode:toNode:") retCULong [argPtr (castPtr raw_startNode :: Ptr ()), argPtr (castPtr raw_endNode :: Ptr ())]

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> CULong -> IO Class
classForGenericArgumentAtIndex gkObstacleGraph  index =
  fmap (Class . castPtr) $ sendMsg gkObstacleGraph (mkSelector "classForGenericArgumentAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- obstacles@
obstacles :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> IO (Id NSArray)
obstacles gkObstacleGraph  =
  sendMsg gkObstacleGraph (mkSelector "obstacles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bufferRadius@
bufferRadius :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> IO CFloat
bufferRadius gkObstacleGraph  =
  sendMsg gkObstacleGraph (mkSelector "bufferRadius") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphWithObstacles:bufferRadius:@
graphWithObstacles_bufferRadiusSelector :: Selector
graphWithObstacles_bufferRadiusSelector = mkSelector "graphWithObstacles:bufferRadius:"

-- | @Selector@ for @initWithObstacles:bufferRadius:@
initWithObstacles_bufferRadiusSelector :: Selector
initWithObstacles_bufferRadiusSelector = mkSelector "initWithObstacles:bufferRadius:"

-- | @Selector@ for @graphWithObstacles:bufferRadius:nodeClass:@
graphWithObstacles_bufferRadius_nodeClassSelector :: Selector
graphWithObstacles_bufferRadius_nodeClassSelector = mkSelector "graphWithObstacles:bufferRadius:nodeClass:"

-- | @Selector@ for @initWithObstacles:bufferRadius:nodeClass:@
initWithObstacles_bufferRadius_nodeClassSelector :: Selector
initWithObstacles_bufferRadius_nodeClassSelector = mkSelector "initWithObstacles:bufferRadius:nodeClass:"

-- | @Selector@ for @connectNodeUsingObstacles:@
connectNodeUsingObstaclesSelector :: Selector
connectNodeUsingObstaclesSelector = mkSelector "connectNodeUsingObstacles:"

-- | @Selector@ for @connectNodeUsingObstacles:ignoringObstacles:@
connectNodeUsingObstacles_ignoringObstaclesSelector :: Selector
connectNodeUsingObstacles_ignoringObstaclesSelector = mkSelector "connectNodeUsingObstacles:ignoringObstacles:"

-- | @Selector@ for @connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:@
connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector :: Selector
connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector = mkSelector "connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:"

-- | @Selector@ for @addObstacles:@
addObstaclesSelector :: Selector
addObstaclesSelector = mkSelector "addObstacles:"

-- | @Selector@ for @removeObstacles:@
removeObstaclesSelector :: Selector
removeObstaclesSelector = mkSelector "removeObstacles:"

-- | @Selector@ for @removeAllObstacles@
removeAllObstaclesSelector :: Selector
removeAllObstaclesSelector = mkSelector "removeAllObstacles"

-- | @Selector@ for @nodesForObstacle:@
nodesForObstacleSelector :: Selector
nodesForObstacleSelector = mkSelector "nodesForObstacle:"

-- | @Selector@ for @lockConnectionFromNode:toNode:@
lockConnectionFromNode_toNodeSelector :: Selector
lockConnectionFromNode_toNodeSelector = mkSelector "lockConnectionFromNode:toNode:"

-- | @Selector@ for @unlockConnectionFromNode:toNode:@
unlockConnectionFromNode_toNodeSelector :: Selector
unlockConnectionFromNode_toNodeSelector = mkSelector "unlockConnectionFromNode:toNode:"

-- | @Selector@ for @isConnectionLockedFromNode:toNode:@
isConnectionLockedFromNode_toNodeSelector :: Selector
isConnectionLockedFromNode_toNodeSelector = mkSelector "isConnectionLockedFromNode:toNode:"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @obstacles@
obstaclesSelector :: Selector
obstaclesSelector = mkSelector "obstacles"

-- | @Selector@ for @bufferRadius@
bufferRadiusSelector :: Selector
bufferRadiusSelector = mkSelector "bufferRadius"

