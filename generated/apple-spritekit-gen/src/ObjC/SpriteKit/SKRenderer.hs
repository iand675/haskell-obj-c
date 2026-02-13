{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A renderer for displaying a SpriteKit scene in an existing Metal workflow.
--
-- Generated bindings for @SKRenderer@.
module ObjC.SpriteKit.SKRenderer
  ( SKRenderer
  , IsSKRenderer(..)
  , rendererWithDevice
  , updateAtTime
  , scene
  , setScene
  , ignoresSiblingOrder
  , setIgnoresSiblingOrder
  , shouldCullNonVisibleNodes
  , setShouldCullNonVisibleNodes
  , showsDrawCount
  , setShowsDrawCount
  , showsNodeCount
  , setShowsNodeCount
  , showsQuadCount
  , setShowsQuadCount
  , showsPhysics
  , setShowsPhysics
  , showsFields
  , setShowsFields
  , ignoresSiblingOrderSelector
  , rendererWithDeviceSelector
  , sceneSelector
  , setIgnoresSiblingOrderSelector
  , setSceneSelector
  , setShouldCullNonVisibleNodesSelector
  , setShowsDrawCountSelector
  , setShowsFieldsSelector
  , setShowsNodeCountSelector
  , setShowsPhysicsSelector
  , setShowsQuadCountSelector
  , shouldCullNonVisibleNodesSelector
  , showsDrawCountSelector
  , showsFieldsSelector
  , showsNodeCountSelector
  , showsPhysicsSelector
  , showsQuadCountSelector
  , updateAtTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes

-- | Creates a renderer with the specified Metal device.
--
-- @device@ — A Metal device.
--
-- Returns: A new renderer object.
--
-- ObjC selector: @+ rendererWithDevice:@
rendererWithDevice :: RawId -> IO (Id SKRenderer)
rendererWithDevice device =
  do
    cls' <- getRequiredClass "SKRenderer"
    sendClassMessage cls' rendererWithDeviceSelector device

-- | Update the scene at the specified system time.
--
-- @currentTime@ — The timestamp in seconds.
--
-- ObjC selector: @- updateAtTime:@
updateAtTime :: IsSKRenderer skRenderer => skRenderer -> CDouble -> IO ()
updateAtTime skRenderer currentTime =
  sendMessage skRenderer updateAtTimeSelector currentTime

-- | The currently presented scene, otherwise nil. If in a transition, the 'incoming' scene is returned.
--
-- ObjC selector: @- scene@
scene :: IsSKRenderer skRenderer => skRenderer -> IO (Id SKScene)
scene skRenderer =
  sendMessage skRenderer sceneSelector

-- | The currently presented scene, otherwise nil. If in a transition, the 'incoming' scene is returned.
--
-- ObjC selector: @- setScene:@
setScene :: (IsSKRenderer skRenderer, IsSKScene value) => skRenderer -> value -> IO ()
setScene skRenderer value =
  sendMessage skRenderer setSceneSelector (toSKScene value)

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- ignoresSiblingOrder@
ignoresSiblingOrder :: IsSKRenderer skRenderer => skRenderer -> IO Bool
ignoresSiblingOrder skRenderer =
  sendMessage skRenderer ignoresSiblingOrderSelector

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- setIgnoresSiblingOrder:@
setIgnoresSiblingOrder :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setIgnoresSiblingOrder skRenderer value =
  sendMessage skRenderer setIgnoresSiblingOrderSelector value

-- | A boolean that indicated whether non-visible nodes should be automatically culled when rendering.
--
-- ObjC selector: @- shouldCullNonVisibleNodes@
shouldCullNonVisibleNodes :: IsSKRenderer skRenderer => skRenderer -> IO Bool
shouldCullNonVisibleNodes skRenderer =
  sendMessage skRenderer shouldCullNonVisibleNodesSelector

-- | A boolean that indicated whether non-visible nodes should be automatically culled when rendering.
--
-- ObjC selector: @- setShouldCullNonVisibleNodes:@
setShouldCullNonVisibleNodes :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShouldCullNonVisibleNodes skRenderer value =
  sendMessage skRenderer setShouldCullNonVisibleNodesSelector value

-- | Toggles display of performance stats when rendering. All default to false.
--
-- ObjC selector: @- showsDrawCount@
showsDrawCount :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsDrawCount skRenderer =
  sendMessage skRenderer showsDrawCountSelector

-- | Toggles display of performance stats when rendering. All default to false.
--
-- ObjC selector: @- setShowsDrawCount:@
setShowsDrawCount :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsDrawCount skRenderer value =
  sendMessage skRenderer setShowsDrawCountSelector value

-- | @- showsNodeCount@
showsNodeCount :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsNodeCount skRenderer =
  sendMessage skRenderer showsNodeCountSelector

-- | @- setShowsNodeCount:@
setShowsNodeCount :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsNodeCount skRenderer value =
  sendMessage skRenderer setShowsNodeCountSelector value

-- | @- showsQuadCount@
showsQuadCount :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsQuadCount skRenderer =
  sendMessage skRenderer showsQuadCountSelector

-- | @- setShowsQuadCount:@
setShowsQuadCount :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsQuadCount skRenderer value =
  sendMessage skRenderer setShowsQuadCountSelector value

-- | @- showsPhysics@
showsPhysics :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsPhysics skRenderer =
  sendMessage skRenderer showsPhysicsSelector

-- | @- setShowsPhysics:@
setShowsPhysics :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsPhysics skRenderer value =
  sendMessage skRenderer setShowsPhysicsSelector value

-- | @- showsFields@
showsFields :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsFields skRenderer =
  sendMessage skRenderer showsFieldsSelector

-- | @- setShowsFields:@
setShowsFields :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsFields skRenderer value =
  sendMessage skRenderer setShowsFieldsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rendererWithDevice:@
rendererWithDeviceSelector :: Selector '[RawId] (Id SKRenderer)
rendererWithDeviceSelector = mkSelector "rendererWithDevice:"

-- | @Selector@ for @updateAtTime:@
updateAtTimeSelector :: Selector '[CDouble] ()
updateAtTimeSelector = mkSelector "updateAtTime:"

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SKScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector '[Id SKScene] ()
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @ignoresSiblingOrder@
ignoresSiblingOrderSelector :: Selector '[] Bool
ignoresSiblingOrderSelector = mkSelector "ignoresSiblingOrder"

-- | @Selector@ for @setIgnoresSiblingOrder:@
setIgnoresSiblingOrderSelector :: Selector '[Bool] ()
setIgnoresSiblingOrderSelector = mkSelector "setIgnoresSiblingOrder:"

-- | @Selector@ for @shouldCullNonVisibleNodes@
shouldCullNonVisibleNodesSelector :: Selector '[] Bool
shouldCullNonVisibleNodesSelector = mkSelector "shouldCullNonVisibleNodes"

-- | @Selector@ for @setShouldCullNonVisibleNodes:@
setShouldCullNonVisibleNodesSelector :: Selector '[Bool] ()
setShouldCullNonVisibleNodesSelector = mkSelector "setShouldCullNonVisibleNodes:"

-- | @Selector@ for @showsDrawCount@
showsDrawCountSelector :: Selector '[] Bool
showsDrawCountSelector = mkSelector "showsDrawCount"

-- | @Selector@ for @setShowsDrawCount:@
setShowsDrawCountSelector :: Selector '[Bool] ()
setShowsDrawCountSelector = mkSelector "setShowsDrawCount:"

-- | @Selector@ for @showsNodeCount@
showsNodeCountSelector :: Selector '[] Bool
showsNodeCountSelector = mkSelector "showsNodeCount"

-- | @Selector@ for @setShowsNodeCount:@
setShowsNodeCountSelector :: Selector '[Bool] ()
setShowsNodeCountSelector = mkSelector "setShowsNodeCount:"

-- | @Selector@ for @showsQuadCount@
showsQuadCountSelector :: Selector '[] Bool
showsQuadCountSelector = mkSelector "showsQuadCount"

-- | @Selector@ for @setShowsQuadCount:@
setShowsQuadCountSelector :: Selector '[Bool] ()
setShowsQuadCountSelector = mkSelector "setShowsQuadCount:"

-- | @Selector@ for @showsPhysics@
showsPhysicsSelector :: Selector '[] Bool
showsPhysicsSelector = mkSelector "showsPhysics"

-- | @Selector@ for @setShowsPhysics:@
setShowsPhysicsSelector :: Selector '[Bool] ()
setShowsPhysicsSelector = mkSelector "setShowsPhysics:"

-- | @Selector@ for @showsFields@
showsFieldsSelector :: Selector '[] Bool
showsFieldsSelector = mkSelector "showsFields"

-- | @Selector@ for @setShowsFields:@
setShowsFieldsSelector :: Selector '[Bool] ()
setShowsFieldsSelector = mkSelector "setShowsFields:"

