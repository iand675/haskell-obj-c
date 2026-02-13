{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLFunctionStitchingGraph
--
-- A function graph that describes a directed acyclic graph.
--
-- The return value of the output node will be used as the return value for the final stitched graph.
--
-- Generated bindings for @MTLFunctionStitchingGraph@.
module ObjC.Metal.MTLFunctionStitchingGraph
  ( MTLFunctionStitchingGraph
  , IsMTLFunctionStitchingGraph(..)
  , initWithFunctionName_nodes_outputNode_attributes
  , functionName
  , setFunctionName
  , nodes
  , setNodes
  , outputNode
  , setOutputNode
  , attributes
  , setAttributes
  , attributesSelector
  , functionNameSelector
  , initWithFunctionName_nodes_outputNode_attributesSelector
  , nodesSelector
  , outputNodeSelector
  , setAttributesSelector
  , setFunctionNameSelector
  , setNodesSelector
  , setOutputNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFunctionName:nodes:outputNode:attributes:@
initWithFunctionName_nodes_outputNode_attributes :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSString functionName, IsNSArray nodes, IsMTLFunctionStitchingFunctionNode outputNode, IsNSArray attributes) => mtlFunctionStitchingGraph -> functionName -> nodes -> outputNode -> attributes -> IO (Id MTLFunctionStitchingGraph)
initWithFunctionName_nodes_outputNode_attributes mtlFunctionStitchingGraph functionName nodes outputNode attributes =
  sendOwnedMessage mtlFunctionStitchingGraph initWithFunctionName_nodes_outputNode_attributesSelector (toNSString functionName) (toNSArray nodes) (toMTLFunctionStitchingFunctionNode outputNode) (toNSArray attributes)

-- | @- functionName@
functionName :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id NSString)
functionName mtlFunctionStitchingGraph =
  sendMessage mtlFunctionStitchingGraph functionNameSelector

-- | @- setFunctionName:@
setFunctionName :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSString value) => mtlFunctionStitchingGraph -> value -> IO ()
setFunctionName mtlFunctionStitchingGraph value =
  sendMessage mtlFunctionStitchingGraph setFunctionNameSelector (toNSString value)

-- | @- nodes@
nodes :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id NSArray)
nodes mtlFunctionStitchingGraph =
  sendMessage mtlFunctionStitchingGraph nodesSelector

-- | @- setNodes:@
setNodes :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSArray value) => mtlFunctionStitchingGraph -> value -> IO ()
setNodes mtlFunctionStitchingGraph value =
  sendMessage mtlFunctionStitchingGraph setNodesSelector (toNSArray value)

-- | @- outputNode@
outputNode :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id MTLFunctionStitchingFunctionNode)
outputNode mtlFunctionStitchingGraph =
  sendMessage mtlFunctionStitchingGraph outputNodeSelector

-- | @- setOutputNode:@
setOutputNode :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsMTLFunctionStitchingFunctionNode value) => mtlFunctionStitchingGraph -> value -> IO ()
setOutputNode mtlFunctionStitchingGraph value =
  sendMessage mtlFunctionStitchingGraph setOutputNodeSelector (toMTLFunctionStitchingFunctionNode value)

-- | @- attributes@
attributes :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id NSArray)
attributes mtlFunctionStitchingGraph =
  sendMessage mtlFunctionStitchingGraph attributesSelector

-- | @- setAttributes:@
setAttributes :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSArray value) => mtlFunctionStitchingGraph -> value -> IO ()
setAttributes mtlFunctionStitchingGraph value =
  sendMessage mtlFunctionStitchingGraph setAttributesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFunctionName:nodes:outputNode:attributes:@
initWithFunctionName_nodes_outputNode_attributesSelector :: Selector '[Id NSString, Id NSArray, Id MTLFunctionStitchingFunctionNode, Id NSArray] (Id MTLFunctionStitchingGraph)
initWithFunctionName_nodes_outputNode_attributesSelector = mkSelector "initWithFunctionName:nodes:outputNode:attributes:"

-- | @Selector@ for @functionName@
functionNameSelector :: Selector '[] (Id NSString)
functionNameSelector = mkSelector "functionName"

-- | @Selector@ for @setFunctionName:@
setFunctionNameSelector :: Selector '[Id NSString] ()
setFunctionNameSelector = mkSelector "setFunctionName:"

-- | @Selector@ for @nodes@
nodesSelector :: Selector '[] (Id NSArray)
nodesSelector = mkSelector "nodes"

-- | @Selector@ for @setNodes:@
setNodesSelector :: Selector '[Id NSArray] ()
setNodesSelector = mkSelector "setNodes:"

-- | @Selector@ for @outputNode@
outputNodeSelector :: Selector '[] (Id MTLFunctionStitchingFunctionNode)
outputNodeSelector = mkSelector "outputNode"

-- | @Selector@ for @setOutputNode:@
setOutputNodeSelector :: Selector '[Id MTLFunctionStitchingFunctionNode] ()
setOutputNodeSelector = mkSelector "setOutputNode:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSArray)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector '[Id NSArray] ()
setAttributesSelector = mkSelector "setAttributes:"

