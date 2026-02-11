{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | inputs and outputs will contain all of the inputs and outputs             external to the graph, which are all the inputs and outputs not             internally connected to something
--
-- Generated bindings for @MDLMaterialPropertyGraph@.
module ObjC.ModelIO.MDLMaterialPropertyGraph
  ( MDLMaterialPropertyGraph
  , IsMDLMaterialPropertyGraph(..)
  , init_
  , initWithNodes_connections
  , evaluate
  , nodes
  , connections
  , initSelector
  , initWithNodes_connectionsSelector
  , evaluateSelector
  , nodesSelector
  , connectionsSelector


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

-- | @- init@
init_ :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO (Id MDLMaterialPropertyGraph)
init_ mdlMaterialPropertyGraph  =
  sendMsg mdlMaterialPropertyGraph (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNodes:connections:@
initWithNodes_connections :: (IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph, IsNSArray nodes, IsNSArray connections) => mdlMaterialPropertyGraph -> nodes -> connections -> IO (Id MDLMaterialPropertyGraph)
initWithNodes_connections mdlMaterialPropertyGraph  nodes connections =
withObjCPtr nodes $ \raw_nodes ->
  withObjCPtr connections $ \raw_connections ->
      sendMsg mdlMaterialPropertyGraph (mkSelector "initWithNodes:connections:") (retPtr retVoid) [argPtr (castPtr raw_nodes :: Ptr ()), argPtr (castPtr raw_connections :: Ptr ())] >>= ownedObject . castPtr

-- | @- evaluate@
evaluate :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO ()
evaluate mdlMaterialPropertyGraph  =
  sendMsg mdlMaterialPropertyGraph (mkSelector "evaluate") retVoid []

-- | @- nodes@
nodes :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO (Id NSArray)
nodes mdlMaterialPropertyGraph  =
  sendMsg mdlMaterialPropertyGraph (mkSelector "nodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- connections@
connections :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO (Id NSArray)
connections mdlMaterialPropertyGraph  =
  sendMsg mdlMaterialPropertyGraph (mkSelector "connections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNodes:connections:@
initWithNodes_connectionsSelector :: Selector
initWithNodes_connectionsSelector = mkSelector "initWithNodes:connections:"

-- | @Selector@ for @evaluate@
evaluateSelector :: Selector
evaluateSelector = mkSelector "evaluate"

-- | @Selector@ for @nodes@
nodesSelector :: Selector
nodesSelector = mkSelector "nodes"

-- | @Selector@ for @connections@
connectionsSelector :: Selector
connectionsSelector = mkSelector "connections"

