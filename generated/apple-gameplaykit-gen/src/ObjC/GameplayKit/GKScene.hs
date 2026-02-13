{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A scene stores and handles loading of data related to a particular scene.
--
-- Generated bindings for @GKScene@.
module ObjC.GameplayKit.GKScene
  ( GKScene
  , IsGKScene(..)
  , sceneWithFileNamed
  , sceneWithFileNamed_rootNode
  , addEntity
  , removeEntity
  , addGraph_name
  , removeGraph
  , entities
  , rootNode
  , setRootNode
  , graphs
  , addEntitySelector
  , addGraph_nameSelector
  , entitiesSelector
  , graphsSelector
  , removeEntitySelector
  , removeGraphSelector
  , rootNodeSelector
  , sceneWithFileNamedSelector
  , sceneWithFileNamed_rootNodeSelector
  , setRootNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Loads a scene from a file contained within the bundle.
--
-- ObjC selector: @+ sceneWithFileNamed:@
sceneWithFileNamed :: IsNSString filename => filename -> IO (Id GKScene)
sceneWithFileNamed filename =
  do
    cls' <- getRequiredClass "GKScene"
    sendClassMessage cls' sceneWithFileNamedSelector (toNSString filename)

-- | Loads a scene from a file contained within the bundle and link with the specified rootNode.
--
-- ObjC selector: @+ sceneWithFileNamed:rootNode:@
sceneWithFileNamed_rootNode :: IsNSString filename => filename -> RawId -> IO (Id GKScene)
sceneWithFileNamed_rootNode filename rootNode =
  do
    cls' <- getRequiredClass "GKScene"
    sendClassMessage cls' sceneWithFileNamed_rootNodeSelector (toNSString filename) rootNode

-- | Adds an entity to the scene's list of entities.
--
-- @entity@ — the entity to add.
--
-- ObjC selector: @- addEntity:@
addEntity :: (IsGKScene gkScene, IsGKEntity entity) => gkScene -> entity -> IO ()
addEntity gkScene entity =
  sendMessage gkScene addEntitySelector (toGKEntity entity)

-- | Removes an entity from the scene's list of entities.
--
-- @entity@ — the entity to remove.
--
-- ObjC selector: @- removeEntity:@
removeEntity :: (IsGKScene gkScene, IsGKEntity entity) => gkScene -> entity -> IO ()
removeEntity gkScene entity =
  sendMessage gkScene removeEntitySelector (toGKEntity entity)

-- | Adds a graph to the scene's list of graphs.
--
-- @graph@ — the graph to add.
--
-- ObjC selector: @- addGraph:name:@
addGraph_name :: (IsGKScene gkScene, IsGKGraph graph, IsNSString name) => gkScene -> graph -> name -> IO ()
addGraph_name gkScene graph name =
  sendMessage gkScene addGraph_nameSelector (toGKGraph graph) (toNSString name)

-- | Removes a graph from the scene's list of graphs.
--
-- @name@ — the name of the corresponding graph as added via addGraph:
--
-- ObjC selector: @- removeGraph:@
removeGraph :: (IsGKScene gkScene, IsNSString name) => gkScene -> name -> IO ()
removeGraph gkScene name =
  sendMessage gkScene removeGraphSelector (toNSString name)

-- | The entities of this scene.
--
-- ObjC selector: @- entities@
entities :: IsGKScene gkScene => gkScene -> IO (Id NSArray)
entities gkScene =
  sendMessage gkScene entitiesSelector

-- | The root node for the scene.
--
-- See: GKSceneRootNodeType
--
-- ObjC selector: @- rootNode@
rootNode :: IsGKScene gkScene => gkScene -> IO RawId
rootNode gkScene =
  sendMessage gkScene rootNodeSelector

-- | The root node for the scene.
--
-- See: GKSceneRootNodeType
--
-- ObjC selector: @- setRootNode:@
setRootNode :: IsGKScene gkScene => gkScene -> RawId -> IO ()
setRootNode gkScene value =
  sendMessage gkScene setRootNodeSelector value

-- | The navigational graphs of this scene.
--
-- ObjC selector: @- graphs@
graphs :: IsGKScene gkScene => gkScene -> IO (Id NSDictionary)
graphs gkScene =
  sendMessage gkScene graphsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneWithFileNamed:@
sceneWithFileNamedSelector :: Selector '[Id NSString] (Id GKScene)
sceneWithFileNamedSelector = mkSelector "sceneWithFileNamed:"

-- | @Selector@ for @sceneWithFileNamed:rootNode:@
sceneWithFileNamed_rootNodeSelector :: Selector '[Id NSString, RawId] (Id GKScene)
sceneWithFileNamed_rootNodeSelector = mkSelector "sceneWithFileNamed:rootNode:"

-- | @Selector@ for @addEntity:@
addEntitySelector :: Selector '[Id GKEntity] ()
addEntitySelector = mkSelector "addEntity:"

-- | @Selector@ for @removeEntity:@
removeEntitySelector :: Selector '[Id GKEntity] ()
removeEntitySelector = mkSelector "removeEntity:"

-- | @Selector@ for @addGraph:name:@
addGraph_nameSelector :: Selector '[Id GKGraph, Id NSString] ()
addGraph_nameSelector = mkSelector "addGraph:name:"

-- | @Selector@ for @removeGraph:@
removeGraphSelector :: Selector '[Id NSString] ()
removeGraphSelector = mkSelector "removeGraph:"

-- | @Selector@ for @entities@
entitiesSelector :: Selector '[] (Id NSArray)
entitiesSelector = mkSelector "entities"

-- | @Selector@ for @rootNode@
rootNodeSelector :: Selector '[] RawId
rootNodeSelector = mkSelector "rootNode"

-- | @Selector@ for @setRootNode:@
setRootNodeSelector :: Selector '[RawId] ()
setRootNodeSelector = mkSelector "setRootNode:"

-- | @Selector@ for @graphs@
graphsSelector :: Selector '[] (Id NSDictionary)
graphsSelector = mkSelector "graphs"

