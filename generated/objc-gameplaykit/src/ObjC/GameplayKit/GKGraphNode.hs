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
  , removeConnectionsToNodes_bidirectionalSelector
  , estimatedCostToNodeSelector
  , costToNodeSelector
  , findPathToNodeSelector
  , findPathFromNodeSelector
  , connectedNodesSelector


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

-- | Add a connection to a group of other nodes indicating those nodes can be reached from this node. A new connection is not created if it already exists.
--
-- @nodes@ — The array of nodes that are end points for their respective connections
--
-- @bidirectional@ — should a connection also be added connecting the destination node back to this node?
--
-- ObjC selector: @- addConnectionsToNodes:bidirectional:@
addConnectionsToNodes_bidirectional :: (IsGKGraphNode gkGraphNode, IsNSArray nodes) => gkGraphNode -> nodes -> Bool -> IO ()
addConnectionsToNodes_bidirectional gkGraphNode  nodes bidirectional =
withObjCPtr nodes $ \raw_nodes ->
    sendMsg gkGraphNode (mkSelector "addConnectionsToNodes:bidirectional:") retVoid [argPtr (castPtr raw_nodes :: Ptr ()), argCULong (if bidirectional then 1 else 0)]

-- | Removes connections to a group of other nodes indicating those nodes can no longer be reached from this node. Nothing happens if a particular connection does not exist.
--
-- @nodes@ — The array of nodes that are end points of the edges to be removed
--
-- @bidirectional@ — should the connection also be added the destination node back to this node also be removed if it exists?
--
-- ObjC selector: @- removeConnectionsToNodes:bidirectional:@
removeConnectionsToNodes_bidirectional :: (IsGKGraphNode gkGraphNode, IsNSArray nodes) => gkGraphNode -> nodes -> Bool -> IO ()
removeConnectionsToNodes_bidirectional gkGraphNode  nodes bidirectional =
withObjCPtr nodes $ \raw_nodes ->
    sendMsg gkGraphNode (mkSelector "removeConnectionsToNodes:bidirectional:") retVoid [argPtr (castPtr raw_nodes :: Ptr ()), argCULong (if bidirectional then 1 else 0)]

-- | Returns the estimated heuristic cost to reach the indicated node from this node
--
-- @node@ — The end point of the edge who's cost is to be estimated
--
-- ObjC selector: @- estimatedCostToNode:@
estimatedCostToNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode node) => gkGraphNode -> node -> IO CFloat
estimatedCostToNode gkGraphNode  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkGraphNode (mkSelector "estimatedCostToNode:") retCFloat [argPtr (castPtr raw_node :: Ptr ())]

-- | Returns the actual cost to reach the indicated node from this node
--
-- ObjC selector: @- costToNode:@
costToNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode node) => gkGraphNode -> node -> IO CFloat
costToNode gkGraphNode  node =
withObjCPtr node $ \raw_node ->
    sendMsg gkGraphNode (mkSelector "costToNode:") retCFloat [argPtr (castPtr raw_node :: Ptr ())]

-- | Attempts to find the optimal path between this node and the indicated goal node. If such a path exists, it is returned in start to end order. If it doesn't exist, the array returned will be empty.
--
-- @goalNode@ — the goal node of the pathfinding attempt
--
-- ObjC selector: @- findPathToNode:@
findPathToNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode goalNode) => gkGraphNode -> goalNode -> IO (Id NSArray)
findPathToNode gkGraphNode  goalNode =
withObjCPtr goalNode $ \raw_goalNode ->
    sendMsg gkGraphNode (mkSelector "findPathToNode:") (retPtr retVoid) [argPtr (castPtr raw_goalNode :: Ptr ())] >>= retainedObject . castPtr

-- | As with findPathToNode: except this node is the goal node and a startNode is specified
--
-- @startNode@ — the start node of the pathfinding attempt
--
-- ObjC selector: @- findPathFromNode:@
findPathFromNode :: (IsGKGraphNode gkGraphNode, IsGKGraphNode startNode) => gkGraphNode -> startNode -> IO (Id NSArray)
findPathFromNode gkGraphNode  startNode =
withObjCPtr startNode $ \raw_startNode ->
    sendMsg gkGraphNode (mkSelector "findPathFromNode:") (retPtr retVoid) [argPtr (castPtr raw_startNode :: Ptr ())] >>= retainedObject . castPtr

-- | List of other graph nodes that this node has an edge leading to.
--
-- ObjC selector: @- connectedNodes@
connectedNodes :: IsGKGraphNode gkGraphNode => gkGraphNode -> IO (Id NSArray)
connectedNodes gkGraphNode  =
  sendMsg gkGraphNode (mkSelector "connectedNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addConnectionsToNodes:bidirectional:@
addConnectionsToNodes_bidirectionalSelector :: Selector
addConnectionsToNodes_bidirectionalSelector = mkSelector "addConnectionsToNodes:bidirectional:"

-- | @Selector@ for @removeConnectionsToNodes:bidirectional:@
removeConnectionsToNodes_bidirectionalSelector :: Selector
removeConnectionsToNodes_bidirectionalSelector = mkSelector "removeConnectionsToNodes:bidirectional:"

-- | @Selector@ for @estimatedCostToNode:@
estimatedCostToNodeSelector :: Selector
estimatedCostToNodeSelector = mkSelector "estimatedCostToNode:"

-- | @Selector@ for @costToNode:@
costToNodeSelector :: Selector
costToNodeSelector = mkSelector "costToNode:"

-- | @Selector@ for @findPathToNode:@
findPathToNodeSelector :: Selector
findPathToNodeSelector = mkSelector "findPathToNode:"

-- | @Selector@ for @findPathFromNode:@
findPathFromNodeSelector :: Selector
findPathFromNodeSelector = mkSelector "findPathFromNode:"

-- | @Selector@ for @connectedNodes@
connectedNodesSelector :: Selector
connectedNodesSelector = mkSelector "connectedNodes"

