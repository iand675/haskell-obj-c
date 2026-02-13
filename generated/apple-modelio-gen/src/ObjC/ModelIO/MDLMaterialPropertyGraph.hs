{-# LANGUAGE DataKinds #-}
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
  , connectionsSelector
  , evaluateSelector
  , initSelector
  , initWithNodes_connectionsSelector
  , nodesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO (Id MDLMaterialPropertyGraph)
init_ mdlMaterialPropertyGraph =
  sendOwnedMessage mdlMaterialPropertyGraph initSelector

-- | @- initWithNodes:connections:@
initWithNodes_connections :: (IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph, IsNSArray nodes, IsNSArray connections) => mdlMaterialPropertyGraph -> nodes -> connections -> IO (Id MDLMaterialPropertyGraph)
initWithNodes_connections mdlMaterialPropertyGraph nodes connections =
  sendOwnedMessage mdlMaterialPropertyGraph initWithNodes_connectionsSelector (toNSArray nodes) (toNSArray connections)

-- | @- evaluate@
evaluate :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO ()
evaluate mdlMaterialPropertyGraph =
  sendMessage mdlMaterialPropertyGraph evaluateSelector

-- | @- nodes@
nodes :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO (Id NSArray)
nodes mdlMaterialPropertyGraph =
  sendMessage mdlMaterialPropertyGraph nodesSelector

-- | @- connections@
connections :: IsMDLMaterialPropertyGraph mdlMaterialPropertyGraph => mdlMaterialPropertyGraph -> IO (Id NSArray)
connections mdlMaterialPropertyGraph =
  sendMessage mdlMaterialPropertyGraph connectionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MDLMaterialPropertyGraph)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNodes:connections:@
initWithNodes_connectionsSelector :: Selector '[Id NSArray, Id NSArray] (Id MDLMaterialPropertyGraph)
initWithNodes_connectionsSelector = mkSelector "initWithNodes:connections:"

-- | @Selector@ for @evaluate@
evaluateSelector :: Selector '[] ()
evaluateSelector = mkSelector "evaluate"

-- | @Selector@ for @nodes@
nodesSelector :: Selector '[] (Id NSArray)
nodesSelector = mkSelector "nodes"

-- | @Selector@ for @connections@
connectionsSelector :: Selector '[] (Id NSArray)
connectionsSelector = mkSelector "connections"

