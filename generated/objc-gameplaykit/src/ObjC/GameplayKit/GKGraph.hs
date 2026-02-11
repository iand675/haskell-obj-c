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
  , graphWithNodesSelector
  , initWithNodesSelector
  , connectNodeToLowestCostNode_bidirectionalSelector
  , removeNodesSelector
  , addNodesSelector
  , findPathFromNode_toNodeSelector
  , nodesSelector


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

-- | Creates a graph with the provided array of nodes.
--
-- @nodes@ — the nodes to create the graph with
--
-- ObjC selector: @+ graphWithNodes:@
graphWithNodes :: IsNSArray nodes => nodes -> IO (Id GKGraph)
graphWithNodes nodes =
  do
    cls' <- getRequiredClass "GKGraph"
    withObjCPtr nodes $ \raw_nodes ->
      sendClassMsg cls' (mkSelector "graphWithNodes:") (retPtr retVoid) [argPtr (castPtr raw_nodes :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithNodes:@
initWithNodes :: (IsGKGraph gkGraph, IsNSArray nodes) => gkGraph -> nodes -> IO (Id GKGraph)
initWithNodes gkGraph  nodes =
withObjCPtr nodes $ \raw_nodes ->
    sendMsg gkGraph (mkSelector "initWithNodes:") (retPtr retVoid) [argPtr (castPtr raw_nodes :: Ptr ())] >>= ownedObject . castPtr

-- | Connects the node to this graph via the lowest cost node to reach in this graph
--
-- @node@ — the node to connect
--
-- @bidirectional@ — should the connection be bidirectional? Otherwise it is one way connected into the graph
--
-- ObjC selector: @- connectNodeToLowestCostNode:bidirectional:@
connectNodeToLowestCostNode_bidirectional :: (IsGKGraph gkGraph, IsGKGraphNode node) => gkGraph -> node -> Bool -> IO ()
connectNodeToLowestCostNode_bidirectional gkGraph  node bidirectional =
withObjCPtr node $ \raw_node ->
    sendMsg gkGraph (mkSelector "connectNodeToLowestCostNode:bidirectional:") retVoid [argPtr (castPtr raw_node :: Ptr ()), argCULong (if bidirectional then 1 else 0)]

-- | Removes nodes from this graph.   All connections starting and/or ending with this node are removed.
--
-- @nodes@ — an array of nodes to be removed
--
-- ObjC selector: @- removeNodes:@
removeNodes :: (IsGKGraph gkGraph, IsNSArray nodes) => gkGraph -> nodes -> IO ()
removeNodes gkGraph  nodes =
withObjCPtr nodes $ \raw_nodes ->
    sendMsg gkGraph (mkSelector "removeNodes:") retVoid [argPtr (castPtr raw_nodes :: Ptr ())]

-- | Adds nodes to this graph.  No new connections are added. If the node already exists in this graph this does nothing.
--
-- @nodes@ — and array of nodes to be added
--
-- ObjC selector: @- addNodes:@
addNodes :: (IsGKGraph gkGraph, IsNSArray nodes) => gkGraph -> nodes -> IO ()
addNodes gkGraph  nodes =
withObjCPtr nodes $ \raw_nodes ->
    sendMsg gkGraph (mkSelector "addNodes:") retVoid [argPtr (castPtr raw_nodes :: Ptr ())]

-- | Attempts to find the optimal path between the two nodes indicated. If such a path exists, it is returned in start to end order. If it doesn't exist, the array returned will be empty. Asserts if neither of these nodes are in this graph.  Use [GKGraphNode findPathFromNode:] instead.
--
-- @startNode@ — node to start pathing from
--
-- @endNode@ — goal node of the pathfinding attempt
--
-- ObjC selector: @- findPathFromNode:toNode:@
findPathFromNode_toNode :: (IsGKGraph gkGraph, IsGKGraphNode startNode, IsGKGraphNode endNode) => gkGraph -> startNode -> endNode -> IO (Id NSArray)
findPathFromNode_toNode gkGraph  startNode endNode =
withObjCPtr startNode $ \raw_startNode ->
  withObjCPtr endNode $ \raw_endNode ->
      sendMsg gkGraph (mkSelector "findPathFromNode:toNode:") (retPtr retVoid) [argPtr (castPtr raw_startNode :: Ptr ()), argPtr (castPtr raw_endNode :: Ptr ())] >>= retainedObject . castPtr

-- | The list of nodes in this graph
--
-- ObjC selector: @- nodes@
nodes :: IsGKGraph gkGraph => gkGraph -> IO (Id NSArray)
nodes gkGraph  =
  sendMsg gkGraph (mkSelector "nodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @graphWithNodes:@
graphWithNodesSelector :: Selector
graphWithNodesSelector = mkSelector "graphWithNodes:"

-- | @Selector@ for @initWithNodes:@
initWithNodesSelector :: Selector
initWithNodesSelector = mkSelector "initWithNodes:"

-- | @Selector@ for @connectNodeToLowestCostNode:bidirectional:@
connectNodeToLowestCostNode_bidirectionalSelector :: Selector
connectNodeToLowestCostNode_bidirectionalSelector = mkSelector "connectNodeToLowestCostNode:bidirectional:"

-- | @Selector@ for @removeNodes:@
removeNodesSelector :: Selector
removeNodesSelector = mkSelector "removeNodes:"

-- | @Selector@ for @addNodes:@
addNodesSelector :: Selector
addNodesSelector = mkSelector "addNodes:"

-- | @Selector@ for @findPathFromNode:toNode:@
findPathFromNode_toNodeSelector :: Selector
findPathFromNode_toNodeSelector = mkSelector "findPathFromNode:toNode:"

-- | @Selector@ for @nodes@
nodesSelector :: Selector
nodesSelector = mkSelector "nodes"

