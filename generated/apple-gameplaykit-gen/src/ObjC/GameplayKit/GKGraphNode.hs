{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A node in a directed graph. Edges are directed and can have variable costs.
--
-- Generated bindings for @GKGraphNode@.
module ObjC.GameplayKit.GKGraphNode
  ( GKGraphNode
  , IsGKGraphNode(..)
  , addConnectionsToNodes_bidirectional
  , removeConnectionsToNodes_bidirectional
  , estimatedCostToNode
  , costToNode
  , findPathToNode
  , findPathFromNode
  , connectedNodes
  , addConnectionsToNodes_bidirectionalSelector
  , connectedNodesSelector
  , costToNodeSelector
  , estimatedCostToNodeSelector
  , findPathFromNodeSelector
  , findPathToNodeSelector
  , removeConnectionsToNodes_bidirectionalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Add a connection to a group of other nodes indicating those nodes can be reached from this node. A new connection is not created if it already exists.
--
-- @nodes@ — The array of nodes that are end points for their respective connections
--
-- @bidirectional@ — should a connection also be added connecting the destination node back to this node?
--
-- ObjC selector: @- addConnectionsToNodes:bidirectional:@
addConnectionsToNodes_bidirectional :: (IsGKGraphNode gkGraphNode, IsNSArray nodes) => gkGraphNode -> nodes -> Bool -> IO ()
addConnectionsToNodes_bidirectional gkGraphNode nodes bidirectional =
  sendMessage gkGraphNode addConnectionsToNodes_bidirectionalSelector (toNSArray nodes) bidirectional

-- | Removes connections to a group of other nodes indicating those nodes can no longer be reached from this node. Nothing happens if a particular connection does not exist.
--
-- @nodes@ — The array of nodes that are end points of the edges to be removed
--
-- @bidirectional@ — should the connection also be added the destination node back to this node also be removed if it exists?
--
-- ObjC selector: @- removeConnectionsToNodes:bidirectional:@
removeConnectionsToNodes_bidirectional :: (IsGKGraphNode gkGraphNode, IsNSArray nodes) => gkGraphNode -> nodes -> Bool -> IO ()
removeConnectionsToNodes_bidirectional gkGraphNode nodes bidirectional =
  sendMessage gkGraphNode removeConnectionsToNodes_bidirectionalSelector (toNSArray nodes) bidirectional

-- | Returns the estimated heuristic cost to reach the indicated node from this node
--
-- @node@ — The end point of the edge who's cost is to be estimated
--
-- ObjC selector: @- estimatedCostToNode:@
estimatedCostToNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode node) => gkGraphNode -> node -> IO CFloat
estimatedCostToNode gkGraphNode node =
  sendMessage gkGraphNode estimatedCostToNodeSelector (toGKGraphNode node)

-- | Returns the actual cost to reach the indicated node from this node
--
-- ObjC selector: @- costToNode:@
costToNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode node) => gkGraphNode -> node -> IO CFloat
costToNode gkGraphNode node =
  sendMessage gkGraphNode costToNodeSelector (toGKGraphNode node)

-- | Attempts to find the optimal path between this node and the indicated goal node. If such a path exists, it is returned in start to end order. If it doesn't exist, the array returned will be empty.
--
-- @goalNode@ — the goal node of the pathfinding attempt
--
-- ObjC selector: @- findPathToNode:@
findPathToNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode goalNode) => gkGraphNode -> goalNode -> IO (Id NSArray)
findPathToNode gkGraphNode goalNode =
  sendMessage gkGraphNode findPathToNodeSelector (toGKGraphNode goalNode)

-- | As with findPathToNode: except this node is the goal node and a startNode is specified
--
-- @startNode@ — the start node of the pathfinding attempt
--
-- ObjC selector: @- findPathFromNode:@
findPathFromNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode startNode) => gkGraphNode -> startNode -> IO (Id NSArray)
findPathFromNode gkGraphNode startNode =
  sendMessage gkGraphNode findPathFromNodeSelector (toGKGraphNode startNode)

-- | List of other graph nodes that this node has an edge leading to.
--
-- ObjC selector: @- connectedNodes@
connectedNodes :: IsGKGraphNode gkGraphNode => gkGraphNode -> IO (Id NSArray)
connectedNodes gkGraphNode =
  sendMessage gkGraphNode connectedNodesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addConnectionsToNodes:bidirectional:@
addConnectionsToNodes_bidirectionalSelector :: Selector '[Id NSArray, Bool] ()
addConnectionsToNodes_bidirectionalSelector = mkSelector "addConnectionsToNodes:bidirectional:"

-- | @Selector@ for @removeConnectionsToNodes:bidirectional:@
removeConnectionsToNodes_bidirectionalSelector :: Selector '[Id NSArray, Bool] ()
removeConnectionsToNodes_bidirectionalSelector = mkSelector "removeConnectionsToNodes:bidirectional:"

-- | @Selector@ for @estimatedCostToNode:@
estimatedCostToNodeSelector :: Selector '[Id GKGraphNode] CFloat
estimatedCostToNodeSelector = mkSelector "estimatedCostToNode:"

-- | @Selector@ for @costToNode:@
costToNodeSelector :: Selector '[Id GKGraphNode] CFloat
costToNodeSelector = mkSelector "costToNode:"

-- | @Selector@ for @findPathToNode:@
findPathToNodeSelector :: Selector '[Id GKGraphNode] (Id NSArray)
findPathToNodeSelector = mkSelector "findPathToNode:"

-- | @Selector@ for @findPathFromNode:@
findPathFromNodeSelector :: Selector '[Id GKGraphNode] (Id NSArray)
findPathFromNodeSelector = mkSelector "findPathFromNode:"

-- | @Selector@ for @connectedNodes@
connectedNodesSelector :: Selector '[] (Id NSArray)
connectedNodesSelector = mkSelector "connectedNodes"

