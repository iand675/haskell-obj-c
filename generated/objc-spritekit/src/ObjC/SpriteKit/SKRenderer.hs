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
  , rendererWithDeviceSelector
  , updateAtTimeSelector
  , sceneSelector
  , setSceneSelector
  , ignoresSiblingOrderSelector
  , setIgnoresSiblingOrderSelector
  , shouldCullNonVisibleNodesSelector
  , setShouldCullNonVisibleNodesSelector
  , showsDrawCountSelector
  , setShowsDrawCountSelector
  , showsNodeCountSelector
  , setShowsNodeCountSelector
  , showsQuadCountSelector
  , setShowsQuadCountSelector
  , showsPhysicsSelector
  , setShowsPhysicsSelector
  , showsFieldsSelector
  , setShowsFieldsSelector


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
    sendClassMsg cls' (mkSelector "rendererWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= retainedObject . castPtr

-- | Update the scene at the specified system time.
--
-- @currentTime@ — The timestamp in seconds.
--
-- ObjC selector: @- updateAtTime:@
updateAtTime :: IsSKRenderer skRenderer => skRenderer -> CDouble -> IO ()
updateAtTime skRenderer  currentTime =
  sendMsg skRenderer (mkSelector "updateAtTime:") retVoid [argCDouble (fromIntegral currentTime)]

-- | The currently presented scene, otherwise nil. If in a transition, the 'incoming' scene is returned.
--
-- ObjC selector: @- scene@
scene :: IsSKRenderer skRenderer => skRenderer -> IO (Id SKScene)
scene skRenderer  =
  sendMsg skRenderer (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The currently presented scene, otherwise nil. If in a transition, the 'incoming' scene is returned.
--
-- ObjC selector: @- setScene:@
setScene :: (IsSKRenderer skRenderer, IsSKScene value) => skRenderer -> value -> IO ()
setScene skRenderer  value =
withObjCPtr value $ \raw_value ->
    sendMsg skRenderer (mkSelector "setScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- ignoresSiblingOrder@
ignoresSiblingOrder :: IsSKRenderer skRenderer => skRenderer -> IO Bool
ignoresSiblingOrder skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "ignoresSiblingOrder") retCULong []

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- setIgnoresSiblingOrder:@
setIgnoresSiblingOrder :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setIgnoresSiblingOrder skRenderer  value =
  sendMsg skRenderer (mkSelector "setIgnoresSiblingOrder:") retVoid [argCULong (if value then 1 else 0)]

-- | A boolean that indicated whether non-visible nodes should be automatically culled when rendering.
--
-- ObjC selector: @- shouldCullNonVisibleNodes@
shouldCullNonVisibleNodes :: IsSKRenderer skRenderer => skRenderer -> IO Bool
shouldCullNonVisibleNodes skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "shouldCullNonVisibleNodes") retCULong []

-- | A boolean that indicated whether non-visible nodes should be automatically culled when rendering.
--
-- ObjC selector: @- setShouldCullNonVisibleNodes:@
setShouldCullNonVisibleNodes :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShouldCullNonVisibleNodes skRenderer  value =
  sendMsg skRenderer (mkSelector "setShouldCullNonVisibleNodes:") retVoid [argCULong (if value then 1 else 0)]

-- | Toggles display of performance stats when rendering. All default to false.
--
-- ObjC selector: @- showsDrawCount@
showsDrawCount :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsDrawCount skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "showsDrawCount") retCULong []

-- | Toggles display of performance stats when rendering. All default to false.
--
-- ObjC selector: @- setShowsDrawCount:@
setShowsDrawCount :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsDrawCount skRenderer  value =
  sendMsg skRenderer (mkSelector "setShowsDrawCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsNodeCount@
showsNodeCount :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsNodeCount skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "showsNodeCount") retCULong []

-- | @- setShowsNodeCount:@
setShowsNodeCount :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsNodeCount skRenderer  value =
  sendMsg skRenderer (mkSelector "setShowsNodeCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsQuadCount@
showsQuadCount :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsQuadCount skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "showsQuadCount") retCULong []

-- | @- setShowsQuadCount:@
setShowsQuadCount :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsQuadCount skRenderer  value =
  sendMsg skRenderer (mkSelector "setShowsQuadCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsPhysics@
showsPhysics :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsPhysics skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "showsPhysics") retCULong []

-- | @- setShowsPhysics:@
setShowsPhysics :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsPhysics skRenderer  value =
  sendMsg skRenderer (mkSelector "setShowsPhysics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsFields@
showsFields :: IsSKRenderer skRenderer => skRenderer -> IO Bool
showsFields skRenderer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skRenderer (mkSelector "showsFields") retCULong []

-- | @- setShowsFields:@
setShowsFields :: IsSKRenderer skRenderer => skRenderer -> Bool -> IO ()
setShowsFields skRenderer  value =
  sendMsg skRenderer (mkSelector "setShowsFields:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rendererWithDevice:@
rendererWithDeviceSelector :: Selector
rendererWithDeviceSelector = mkSelector "rendererWithDevice:"

-- | @Selector@ for @updateAtTime:@
updateAtTimeSelector :: Selector
updateAtTimeSelector = mkSelector "updateAtTime:"

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @ignoresSiblingOrder@
ignoresSiblingOrderSelector :: Selector
ignoresSiblingOrderSelector = mkSelector "ignoresSiblingOrder"

-- | @Selector@ for @setIgnoresSiblingOrder:@
setIgnoresSiblingOrderSelector :: Selector
setIgnoresSiblingOrderSelector = mkSelector "setIgnoresSiblingOrder:"

-- | @Selector@ for @shouldCullNonVisibleNodes@
shouldCullNonVisibleNodesSelector :: Selector
shouldCullNonVisibleNodesSelector = mkSelector "shouldCullNonVisibleNodes"

-- | @Selector@ for @setShouldCullNonVisibleNodes:@
setShouldCullNonVisibleNodesSelector :: Selector
setShouldCullNonVisibleNodesSelector = mkSelector "setShouldCullNonVisibleNodes:"

-- | @Selector@ for @showsDrawCount@
showsDrawCountSelector :: Selector
showsDrawCountSelector = mkSelector "showsDrawCount"

-- | @Selector@ for @setShowsDrawCount:@
setShowsDrawCountSelector :: Selector
setShowsDrawCountSelector = mkSelector "setShowsDrawCount:"

-- | @Selector@ for @showsNodeCount@
showsNodeCountSelector :: Selector
showsNodeCountSelector = mkSelector "showsNodeCount"

-- | @Selector@ for @setShowsNodeCount:@
setShowsNodeCountSelector :: Selector
setShowsNodeCountSelector = mkSelector "setShowsNodeCount:"

-- | @Selector@ for @showsQuadCount@
showsQuadCountSelector :: Selector
showsQuadCountSelector = mkSelector "showsQuadCount"

-- | @Selector@ for @setShowsQuadCount:@
setShowsQuadCountSelector :: Selector
setShowsQuadCountSelector = mkSelector "setShowsQuadCount:"

-- | @Selector@ for @showsPhysics@
showsPhysicsSelector :: Selector
showsPhysicsSelector = mkSelector "showsPhysics"

-- | @Selector@ for @setShowsPhysics:@
setShowsPhysicsSelector :: Selector
setShowsPhysicsSelector = mkSelector "setShowsPhysics:"

-- | @Selector@ for @showsFields@
showsFieldsSelector :: Selector
showsFieldsSelector = mkSelector "showsFields"

-- | @Selector@ for @setShowsFields:@
setShowsFieldsSelector :: Selector
setShowsFieldsSelector = mkSelector "setShowsFields:"

