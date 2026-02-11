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
  , graphs
  , sceneWithFileNamedSelector
  , sceneWithFileNamed_rootNodeSelector
  , addEntitySelector
  , removeEntitySelector
  , addGraph_nameSelector
  , removeGraphSelector
  , entitiesSelector
  , graphsSelector


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

-- | Loads a scene from a file contained within the bundle.
--
-- ObjC selector: @+ sceneWithFileNamed:@
sceneWithFileNamed :: IsNSString filename => filename -> IO (Id GKScene)
sceneWithFileNamed filename =
  do
    cls' <- getRequiredClass "GKScene"
    withObjCPtr filename $ \raw_filename ->
      sendClassMsg cls' (mkSelector "sceneWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | Loads a scene from a file contained within the bundle and link with the specified rootNode.
--
-- ObjC selector: @+ sceneWithFileNamed:rootNode:@
sceneWithFileNamed_rootNode :: IsNSString filename => filename -> RawId -> IO (Id GKScene)
sceneWithFileNamed_rootNode filename rootNode =
  do
    cls' <- getRequiredClass "GKScene"
    withObjCPtr filename $ \raw_filename ->
      sendClassMsg cls' (mkSelector "sceneWithFileNamed:rootNode:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ()), argPtr (castPtr (unRawId rootNode) :: Ptr ())] >>= retainedObject . castPtr

-- | Adds an entity to the scene's list of entities.
--
-- @entity@ — the entity to add.
--
-- ObjC selector: @- addEntity:@
addEntity :: (IsGKScene gkScene, IsGKEntity entity) => gkScene -> entity -> IO ()
addEntity gkScene  entity =
withObjCPtr entity $ \raw_entity ->
    sendMsg gkScene (mkSelector "addEntity:") retVoid [argPtr (castPtr raw_entity :: Ptr ())]

-- | Removes an entity from the scene's list of entities.
--
-- @entity@ — the entity to remove.
--
-- ObjC selector: @- removeEntity:@
removeEntity :: (IsGKScene gkScene, IsGKEntity entity) => gkScene -> entity -> IO ()
removeEntity gkScene  entity =
withObjCPtr entity $ \raw_entity ->
    sendMsg gkScene (mkSelector "removeEntity:") retVoid [argPtr (castPtr raw_entity :: Ptr ())]

-- | Adds a graph to the scene's list of graphs.
--
-- @graph@ — the graph to add.
--
-- ObjC selector: @- addGraph:name:@
addGraph_name :: (IsGKScene gkScene, IsGKGraph graph, IsNSString name) => gkScene -> graph -> name -> IO ()
addGraph_name gkScene  graph name =
withObjCPtr graph $ \raw_graph ->
  withObjCPtr name $ \raw_name ->
      sendMsg gkScene (mkSelector "addGraph:name:") retVoid [argPtr (castPtr raw_graph :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | Removes a graph from the scene's list of graphs.
--
-- @name@ — the name of the corresponding graph as added via addGraph:
--
-- ObjC selector: @- removeGraph:@
removeGraph :: (IsGKScene gkScene, IsNSString name) => gkScene -> name -> IO ()
removeGraph gkScene  name =
withObjCPtr name $ \raw_name ->
    sendMsg gkScene (mkSelector "removeGraph:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | The entities of this scene.
--
-- ObjC selector: @- entities@
entities :: IsGKScene gkScene => gkScene -> IO (Id NSArray)
entities gkScene  =
  sendMsg gkScene (mkSelector "entities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The navigational graphs of this scene.
--
-- ObjC selector: @- graphs@
graphs :: IsGKScene gkScene => gkScene -> IO (Id NSDictionary)
graphs gkScene  =
  sendMsg gkScene (mkSelector "graphs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneWithFileNamed:@
sceneWithFileNamedSelector :: Selector
sceneWithFileNamedSelector = mkSelector "sceneWithFileNamed:"

-- | @Selector@ for @sceneWithFileNamed:rootNode:@
sceneWithFileNamed_rootNodeSelector :: Selector
sceneWithFileNamed_rootNodeSelector = mkSelector "sceneWithFileNamed:rootNode:"

-- | @Selector@ for @addEntity:@
addEntitySelector :: Selector
addEntitySelector = mkSelector "addEntity:"

-- | @Selector@ for @removeEntity:@
removeEntitySelector :: Selector
removeEntitySelector = mkSelector "removeEntity:"

-- | @Selector@ for @addGraph:name:@
addGraph_nameSelector :: Selector
addGraph_nameSelector = mkSelector "addGraph:name:"

-- | @Selector@ for @removeGraph:@
removeGraphSelector :: Selector
removeGraphSelector = mkSelector "removeGraph:"

-- | @Selector@ for @entities@
entitiesSelector :: Selector
entitiesSelector = mkSelector "entities"

-- | @Selector@ for @graphs@
graphsSelector :: Selector
graphsSelector = mkSelector "graphs"

