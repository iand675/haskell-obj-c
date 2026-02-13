{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Representation of a directed graph of GKGraphNodes
--
-- Generated bindings for @GKGraph@.
module ObjC.GameplayKit.GKGraph
  ( GKGraph
  , IsGKGraph(..)
  , graphWithNodes
  , initWithNodes
  , connectNodeToLowestCostNode_bidirectional
  , removeNodes
  , addNodes
  , findPathFromNode_toNode
  , nodes
  , addNodesSelector
  , connectNodeToLowestCostNode_bidirectionalSelector
  , findPathFromNode_toNodeSelector
  , graphWithNodesSelector
  , initWithNodesSelector
  , nodesSelector
  , removeNodesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a graph with the provided array of nodes.
--
-- @nodes@ — the nodes to create the graph with
--
-- ObjC selector: @+ graphWithNodes:@
graphWithNodes :: IsNSArray nodes => nodes -> IO (Id GKGraph)
graphWithNodes nodes =
  do
    cls' <- getRequiredClass "GKGraph"
    sendClassMessage cls' graphWithNodesSelector (toNSArray nodes)

-- | @- initWithNodes:@
initWithNodes :: (IsGKGraph gkGraph, IsNSArray nodes) => gkGraph -> nodes -> IO (Id GKGraph)
initWithNodes gkGraph nodes =
  sendOwnedMessage gkGraph initWithNodesSelector (toNSArray nodes)

-- | Connects the node to this graph via the lowest cost node to reach in this graph
--
-- @node@ — the node to connect
--
-- @bidirectional@ — should the connection be bidirectional? Otherwise it is one way connected into the graph
--
-- ObjC selector: @- connectNodeToLowestCostNode:bidirectional:@
connectNodeToLowestCostNode_bidirectional :: (IsGKGraph gkGraph, IsGKGraphNode node) => gkGraph -> node -> Bool -> IO ()
connectNodeToLowestCostNode_bidirectional gkGraph node bidirectional =
  sendMessage gkGraph connectNodeToLowestCostNode_bidirectionalSelector (toGKGraphNode node) bidirectional

-- | Removes nodes from this graph.   All connections starting and/or ending with this node are removed.
--
-- @nodes@ — an array of nodes to be removed
--
-- ObjC selector: @- removeNodes:@
removeNodes :: (IsGKGraph gkGraph, IsNSArray nodes) => gkGraph -> nodes -> IO ()
removeNodes gkGraph nodes =
  sendMessage gkGraph removeNodesSelector (toNSArray nodes)

-- | Adds nodes to this graph.  No new connections are added. If the node already exists in this graph this does nothing.
--
-- @nodes@ — and array of nodes to be added
--
-- ObjC selector: @- addNodes:@
addNodes :: (IsGKGraph gkGraph, IsNSArray nodes) => gkGraph -> nodes -> IO ()
addNodes gkGraph nodes =
  sendMessage gkGraph addNodesSelector (toNSArray nodes)

-- | Attempts to find the optimal path between the two nodes indicated. If such a path exists, it is returned in start to end order. If it doesn't exist, the array returned will be empty. Asserts if neither of these nodes are in this graph.  Use [GKGraphNode findPathFromNode:] instead.
--
-- @startNode@ — node to start pathing from
--
-- @endNode@ — goal node of the pathfinding attempt
--
-- ObjC selector: @- findPathFromNode:toNode:@
findPathFromNode_toNode :: (IsGKGraph gkGraph, IsGKGraphNode startNode, IsGKGraphNode endNode) => gkGraph -> startNode -> endNode -> IO (Id NSArray)
findPathFromNode_toNode gkGraph startNode endNode =
  sendMessage gkGraph findPathFromNode_toNodeSelector (toGKGraphNode startNode) (toGKGraphNode endNode)

-- | The list of nodes in this graph
--
-- ObjC selector: @- nodes@
nodes :: IsGKGraph gkGraph => gkGraph -> IO (Id NSArray)
nodes gkGraph =
  sendMessage gkGraph nodesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphWithNodes:@
graphWithNodesSelector :: Selector '[Id NSArray] (Id GKGraph)
graphWithNodesSelector = mkSelector "graphWithNodes:"

-- | @Selector@ for @initWithNodes:@
initWithNodesSelector :: Selector '[Id NSArray] (Id GKGraph)
initWithNodesSelector = mkSelector "initWithNodes:"

-- | @Selector@ for @connectNodeToLowestCostNode:bidirectional:@
connectNodeToLowestCostNode_bidirectionalSelector :: Selector '[Id GKGraphNode, Bool] ()
connectNodeToLowestCostNode_bidirectionalSelector = mkSelector "connectNodeToLowestCostNode:bidirectional:"

-- | @Selector@ for @removeNodes:@
removeNodesSelector :: Selector '[Id NSArray] ()
removeNodesSelector = mkSelector "removeNodes:"

-- | @Selector@ for @addNodes:@
addNodesSelector :: Selector '[Id NSArray] ()
addNodesSelector = mkSelector "addNodes:"

-- | @Selector@ for @findPathFromNode:toNode:@
findPathFromNode_toNodeSelector :: Selector '[Id GKGraphNode, Id GKGraphNode] (Id NSArray)
findPathFromNode_toNodeSelector = mkSelector "findPathFromNode:toNode:"

-- | @Selector@ for @nodes@
nodesSelector :: Selector '[] (Id NSArray)
nodesSelector = mkSelector "nodes"

