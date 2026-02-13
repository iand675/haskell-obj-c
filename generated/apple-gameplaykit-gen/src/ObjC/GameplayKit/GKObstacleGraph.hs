{-# LANGUAGE DataKinds #-}
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
  , addObstaclesSelector
  , bufferRadiusSelector
  , classForGenericArgumentAtIndexSelector
  , connectNodeUsingObstaclesSelector
  , connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector
  , connectNodeUsingObstacles_ignoringObstaclesSelector
  , graphWithObstacles_bufferRadiusSelector
  , graphWithObstacles_bufferRadius_nodeClassSelector
  , initWithObstacles_bufferRadiusSelector
  , initWithObstacles_bufferRadius_nodeClassSelector
  , isConnectionLockedFromNode_toNodeSelector
  , lockConnectionFromNode_toNodeSelector
  , nodesForObstacleSelector
  , obstaclesSelector
  , removeAllObstaclesSelector
  , removeObstaclesSelector
  , unlockConnectionFromNode_toNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' graphWithObstacles_bufferRadiusSelector (toNSArray obstacles) bufferRadius

-- | @- initWithObstacles:bufferRadius:@
initWithObstacles_bufferRadius :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> CFloat -> IO (Id GKObstacleGraph)
initWithObstacles_bufferRadius gkObstacleGraph obstacles bufferRadius =
  sendOwnedMessage gkObstacleGraph initWithObstacles_bufferRadiusSelector (toNSArray obstacles) bufferRadius

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
    sendClassMessage cls' graphWithObstacles_bufferRadius_nodeClassSelector (toNSArray obstacles) bufferRadius nodeClass

-- | @- initWithObstacles:bufferRadius:nodeClass:@
initWithObstacles_bufferRadius_nodeClass :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> CFloat -> Class -> IO (Id GKObstacleGraph)
initWithObstacles_bufferRadius_nodeClass gkObstacleGraph obstacles bufferRadius nodeClass =
  sendOwnedMessage gkObstacleGraph initWithObstacles_bufferRadius_nodeClassSelector (toNSArray obstacles) bufferRadius nodeClass

-- | Connects the node to this graph by testing edge intersection with existing obstacles. Same behavior as if this node had been present during initWithObstacles.
--
-- @node@ — the node to connect
--
-- ObjC selector: @- connectNodeUsingObstacles:@
connectNodeUsingObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D node) => gkObstacleGraph -> node -> IO ()
connectNodeUsingObstacles gkObstacleGraph node =
  sendMessage gkObstacleGraph connectNodeUsingObstaclesSelector (toGKGraphNode2D node)

-- | Same behavior as connectNodeUsingObstacles: except you can optionally ignore certain obstacles from being tested for intersection.
--
-- ObjC selector: @- connectNodeUsingObstacles:ignoringObstacles:@
connectNodeUsingObstacles_ignoringObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D node, IsNSArray obstaclesToIgnore) => gkObstacleGraph -> node -> obstaclesToIgnore -> IO ()
connectNodeUsingObstacles_ignoringObstacles gkObstacleGraph node obstaclesToIgnore =
  sendMessage gkObstacleGraph connectNodeUsingObstacles_ignoringObstaclesSelector (toGKGraphNode2D node) (toNSArray obstaclesToIgnore)

-- | Same behavior as connectNodeUsingObstacles: except you can optionally ignore the bounding radius of certain obstacles from being tested for intersection
--
-- ObjC selector: @- connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:@
connectNodeUsingObstacles_ignoringBufferRadiusOfObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D node, IsNSArray obstaclesBufferRadiusToIgnore) => gkObstacleGraph -> node -> obstaclesBufferRadiusToIgnore -> IO ()
connectNodeUsingObstacles_ignoringBufferRadiusOfObstacles gkObstacleGraph node obstaclesBufferRadiusToIgnore =
  sendMessage gkObstacleGraph connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector (toGKGraphNode2D node) (toNSArray obstaclesBufferRadiusToIgnore)

-- | Adds obstacles to this graph. Obstacle is extruded and graph nodes are generated from its vertices and then connected to this graph Nothing is done if an obstacle is already present in this graph Any existing connections that intersect the new obstacles are destroyed unless they are protected with [GKObstacleGraph lockConnection:]
--
-- @obstacles@ — an array of obstacles to be added
--
-- See: lockConnection
--
-- ObjC selector: @- addObstacles:@
addObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> IO ()
addObstacles gkObstacleGraph obstacles =
  sendMessage gkObstacleGraph addObstaclesSelector (toNSArray obstacles)

-- | Removes obstacles from this graph. All associated graph nodes are removed and their connections are bidirectionally removed. Connections between obstacle nodes that were previously invalidated by any of these obstacles are restored. Nothing is done if an obstacle is already present in this graph
--
-- @obstacles@ — an array of obstacles to be removed
--
-- ObjC selector: @- removeObstacles:@
removeObstacles :: (IsGKObstacleGraph gkObstacleGraph, IsNSArray obstacles) => gkObstacleGraph -> obstacles -> IO ()
removeObstacles gkObstacleGraph obstacles =
  sendMessage gkObstacleGraph removeObstaclesSelector (toNSArray obstacles)

-- | Removes all obstacles from this graph.
--
-- ObjC selector: @- removeAllObstacles@
removeAllObstacles :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> IO ()
removeAllObstacles gkObstacleGraph =
  sendMessage gkObstacleGraph removeAllObstaclesSelector

-- | Returns an array of the graph nodes associated with a given obstacle
--
-- @obstacle@ — the obstacle who's nodes are to be retrieved
--
-- ObjC selector: @- nodesForObstacle:@
nodesForObstacle :: (IsGKObstacleGraph gkObstacleGraph, IsGKPolygonObstacle obstacle) => gkObstacleGraph -> obstacle -> IO (Id NSArray)
nodesForObstacle gkObstacleGraph obstacle =
  sendMessage gkObstacleGraph nodesForObstacleSelector (toGKPolygonObstacle obstacle)

-- | Marks a connection as "locked", preventing this connection from being destroyed when you add obstacles that would intersect it
--
-- @startNode@ — startNode of the connection to lock
--
-- @endNode@ — endNode of the connection to lock
--
-- ObjC selector: @- lockConnectionFromNode:toNode:@
lockConnectionFromNode_toNode :: (IsGKObstacleGraph gkObstacleGraph, IsGKGraphNode2D startNode, IsGKGraphNode2D endNode) => gkObstacleGraph -> startNode -> endNode -> IO ()
lockConnectionFromNode_toNode gkObstacleGraph startNode endNode =
  sendMessage gkObstacleGraph lockConnectionFromNode_toNodeSelector (toGKGraphNode2D startNode) (toGKGraphNode2D endNode)

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
unlockConnectionFromNode_toNode gkObstacleGraph startNode endNode =
  sendMessage gkObstacleGraph unlockConnectionFromNode_toNodeSelector (toGKGraphNode2D startNode) (toGKGraphNode2D endNode)

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
isConnectionLockedFromNode_toNode gkObstacleGraph startNode endNode =
  sendMessage gkObstacleGraph isConnectionLockedFromNode_toNodeSelector (toGKGraphNode2D startNode) (toGKGraphNode2D endNode)

-- | Returns the class of the specified generic index
--
-- ObjC selector: @- classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndex :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> CULong -> IO Class
classForGenericArgumentAtIndex gkObstacleGraph index =
  sendMessage gkObstacleGraph classForGenericArgumentAtIndexSelector index

-- | @- obstacles@
obstacles :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> IO (Id NSArray)
obstacles gkObstacleGraph =
  sendMessage gkObstacleGraph obstaclesSelector

-- | @- bufferRadius@
bufferRadius :: IsGKObstacleGraph gkObstacleGraph => gkObstacleGraph -> IO CFloat
bufferRadius gkObstacleGraph =
  sendMessage gkObstacleGraph bufferRadiusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphWithObstacles:bufferRadius:@
graphWithObstacles_bufferRadiusSelector :: Selector '[Id NSArray, CFloat] (Id GKObstacleGraph)
graphWithObstacles_bufferRadiusSelector = mkSelector "graphWithObstacles:bufferRadius:"

-- | @Selector@ for @initWithObstacles:bufferRadius:@
initWithObstacles_bufferRadiusSelector :: Selector '[Id NSArray, CFloat] (Id GKObstacleGraph)
initWithObstacles_bufferRadiusSelector = mkSelector "initWithObstacles:bufferRadius:"

-- | @Selector@ for @graphWithObstacles:bufferRadius:nodeClass:@
graphWithObstacles_bufferRadius_nodeClassSelector :: Selector '[Id NSArray, CFloat, Class] (Id GKObstacleGraph)
graphWithObstacles_bufferRadius_nodeClassSelector = mkSelector "graphWithObstacles:bufferRadius:nodeClass:"

-- | @Selector@ for @initWithObstacles:bufferRadius:nodeClass:@
initWithObstacles_bufferRadius_nodeClassSelector :: Selector '[Id NSArray, CFloat, Class] (Id GKObstacleGraph)
initWithObstacles_bufferRadius_nodeClassSelector = mkSelector "initWithObstacles:bufferRadius:nodeClass:"

-- | @Selector@ for @connectNodeUsingObstacles:@
connectNodeUsingObstaclesSelector :: Selector '[Id GKGraphNode2D] ()
connectNodeUsingObstaclesSelector = mkSelector "connectNodeUsingObstacles:"

-- | @Selector@ for @connectNodeUsingObstacles:ignoringObstacles:@
connectNodeUsingObstacles_ignoringObstaclesSelector :: Selector '[Id GKGraphNode2D, Id NSArray] ()
connectNodeUsingObstacles_ignoringObstaclesSelector = mkSelector "connectNodeUsingObstacles:ignoringObstacles:"

-- | @Selector@ for @connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:@
connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector :: Selector '[Id GKGraphNode2D, Id NSArray] ()
connectNodeUsingObstacles_ignoringBufferRadiusOfObstaclesSelector = mkSelector "connectNodeUsingObstacles:ignoringBufferRadiusOfObstacles:"

-- | @Selector@ for @addObstacles:@
addObstaclesSelector :: Selector '[Id NSArray] ()
addObstaclesSelector = mkSelector "addObstacles:"

-- | @Selector@ for @removeObstacles:@
removeObstaclesSelector :: Selector '[Id NSArray] ()
removeObstaclesSelector = mkSelector "removeObstacles:"

-- | @Selector@ for @removeAllObstacles@
removeAllObstaclesSelector :: Selector '[] ()
removeAllObstaclesSelector = mkSelector "removeAllObstacles"

-- | @Selector@ for @nodesForObstacle:@
nodesForObstacleSelector :: Selector '[Id GKPolygonObstacle] (Id NSArray)
nodesForObstacleSelector = mkSelector "nodesForObstacle:"

-- | @Selector@ for @lockConnectionFromNode:toNode:@
lockConnectionFromNode_toNodeSelector :: Selector '[Id GKGraphNode2D, Id GKGraphNode2D] ()
lockConnectionFromNode_toNodeSelector = mkSelector "lockConnectionFromNode:toNode:"

-- | @Selector@ for @unlockConnectionFromNode:toNode:@
unlockConnectionFromNode_toNodeSelector :: Selector '[Id GKGraphNode2D, Id GKGraphNode2D] ()
unlockConnectionFromNode_toNodeSelector = mkSelector "unlockConnectionFromNode:toNode:"

-- | @Selector@ for @isConnectionLockedFromNode:toNode:@
isConnectionLockedFromNode_toNodeSelector :: Selector '[Id GKGraphNode2D, Id GKGraphNode2D] Bool
isConnectionLockedFromNode_toNodeSelector = mkSelector "isConnectionLockedFromNode:toNode:"

-- | @Selector@ for @classForGenericArgumentAtIndex:@
classForGenericArgumentAtIndexSelector :: Selector '[CULong] Class
classForGenericArgumentAtIndexSelector = mkSelector "classForGenericArgumentAtIndex:"

-- | @Selector@ for @obstacles@
obstaclesSelector :: Selector '[] (Id NSArray)
obstaclesSelector = mkSelector "obstacles"

-- | @Selector@ for @bufferRadius@
bufferRadiusSelector :: Selector '[] CFloat
bufferRadiusSelector = mkSelector "bufferRadius"

