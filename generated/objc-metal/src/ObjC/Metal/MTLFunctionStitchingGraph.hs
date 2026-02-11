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
  , initWithFunctionName_nodes_outputNode_attributesSelector
  , functionNameSelector
  , setFunctionNameSelector
  , nodesSelector
  , setNodesSelector
  , outputNodeSelector
  , setOutputNodeSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFunctionName:nodes:outputNode:attributes:@
initWithFunctionName_nodes_outputNode_attributes :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSString functionName, IsNSArray nodes, IsMTLFunctionStitchingFunctionNode outputNode, IsNSArray attributes) => mtlFunctionStitchingGraph -> functionName -> nodes -> outputNode -> attributes -> IO (Id MTLFunctionStitchingGraph)
initWithFunctionName_nodes_outputNode_attributes mtlFunctionStitchingGraph  functionName nodes outputNode attributes =
withObjCPtr functionName $ \raw_functionName ->
  withObjCPtr nodes $ \raw_nodes ->
    withObjCPtr outputNode $ \raw_outputNode ->
      withObjCPtr attributes $ \raw_attributes ->
          sendMsg mtlFunctionStitchingGraph (mkSelector "initWithFunctionName:nodes:outputNode:attributes:") (retPtr retVoid) [argPtr (castPtr raw_functionName :: Ptr ()), argPtr (castPtr raw_nodes :: Ptr ()), argPtr (castPtr raw_outputNode :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= ownedObject . castPtr

-- | @- functionName@
functionName :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id NSString)
functionName mtlFunctionStitchingGraph  =
  sendMsg mtlFunctionStitchingGraph (mkSelector "functionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFunctionName:@
setFunctionName :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSString value) => mtlFunctionStitchingGraph -> value -> IO ()
setFunctionName mtlFunctionStitchingGraph  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlFunctionStitchingGraph (mkSelector "setFunctionName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodes@
nodes :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id NSArray)
nodes mtlFunctionStitchingGraph  =
  sendMsg mtlFunctionStitchingGraph (mkSelector "nodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodes:@
setNodes :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsNSArray value) => mtlFunctionStitchingGraph -> value -> IO ()
setNodes mtlFunctionStitchingGraph  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlFunctionStitchingGraph (mkSelector "setNodes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- outputNode@
outputNode :: IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph => mtlFunctionStitchingGraph -> IO (Id MTLFunctionStitchingFunctionNode)
outputNode mtlFunctionStitchingGraph  =
  sendMsg mtlFunctionStitchingGraph (mkSelector "outputNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOutputNode:@
setOutputNode :: (IsMTLFunctionStitchingGraph mtlFunctionStitchingGraph, IsMTLFunctionStitchingFunctionNode value) => mtlFunctionStitchingGraph -> value -> IO ()
setOutputNode mtlFunctionStitchingGraph  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlFunctionStitchingGraph (mkSelector "setOutputNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFunctionName:nodes:outputNode:attributes:@
initWithFunctionName_nodes_outputNode_attributesSelector :: Selector
initWithFunctionName_nodes_outputNode_attributesSelector = mkSelector "initWithFunctionName:nodes:outputNode:attributes:"

-- | @Selector@ for @functionName@
functionNameSelector :: Selector
functionNameSelector = mkSelector "functionName"

-- | @Selector@ for @setFunctionName:@
setFunctionNameSelector :: Selector
setFunctionNameSelector = mkSelector "setFunctionName:"

-- | @Selector@ for @nodes@
nodesSelector :: Selector
nodesSelector = mkSelector "nodes"

-- | @Selector@ for @setNodes:@
setNodesSelector :: Selector
setNodesSelector = mkSelector "setNodes:"

-- | @Selector@ for @outputNode@
outputNodeSelector :: Selector
outputNodeSelector = mkSelector "outputNode"

-- | @Selector@ for @setOutputNode:@
setOutputNodeSelector :: Selector
setOutputNodeSelector = mkSelector "setOutputNode:"

