{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKView@.
module ObjC.SpriteKit.SKView
  ( SKView
  , IsSKView(..)
  , presentScene
  , presentScene_transition
  , textureFromNode
  , paused
  , setPaused
  , showsFPS
  , setShowsFPS
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
  , asynchronous
  , setAsynchronous
  , allowsTransparency
  , setAllowsTransparency
  , ignoresSiblingOrder
  , setIgnoresSiblingOrder
  , shouldCullNonVisibleNodes
  , setShouldCullNonVisibleNodes
  , preferredFramesPerSecond
  , setPreferredFramesPerSecond
  , disableDepthStencilBuffer
  , setDisableDepthStencilBuffer
  , delegate
  , setDelegate
  , frameInterval
  , setFrameInterval
  , preferredFrameRate
  , setPreferredFrameRate
  , scene
  , presentSceneSelector
  , presentScene_transitionSelector
  , textureFromNodeSelector
  , pausedSelector
  , setPausedSelector
  , showsFPSSelector
  , setShowsFPSSelector
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
  , asynchronousSelector
  , setAsynchronousSelector
  , allowsTransparencySelector
  , setAllowsTransparencySelector
  , ignoresSiblingOrderSelector
  , setIgnoresSiblingOrderSelector
  , shouldCullNonVisibleNodesSelector
  , setShouldCullNonVisibleNodesSelector
  , preferredFramesPerSecondSelector
  , setPreferredFramesPerSecondSelector
  , disableDepthStencilBufferSelector
  , setDisableDepthStencilBufferSelector
  , delegateSelector
  , setDelegateSelector
  , frameIntervalSelector
  , setFrameIntervalSelector
  , preferredFrameRateSelector
  , setPreferredFrameRateSelector
  , sceneSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Present an SKScene in the view, replacing the current scene.
--
-- @scene@ — the scene to present.
--
-- ObjC selector: @- presentScene:@
presentScene :: (IsSKView skView, IsSKScene scene) => skView -> scene -> IO ()
presentScene skView  scene =
withObjCPtr scene $ \raw_scene ->
    sendMsg skView (mkSelector "presentScene:") retVoid [argPtr (castPtr raw_scene :: Ptr ())]

-- | Present an SKScene in the view, replacing the current scene.
--
-- If there is currently a scene being presented in the view, the transition is used to swap between them.
--
-- @scene@ — the scene to present.
--
-- @transition@ — the transition to use when presenting the scene.
--
-- ObjC selector: @- presentScene:transition:@
presentScene_transition :: (IsSKView skView, IsSKScene scene, IsSKTransition transition) => skView -> scene -> transition -> IO ()
presentScene_transition skView  scene transition =
withObjCPtr scene $ \raw_scene ->
  withObjCPtr transition $ \raw_transition ->
      sendMsg skView (mkSelector "presentScene:transition:") retVoid [argPtr (castPtr raw_scene :: Ptr ()), argPtr (castPtr raw_transition :: Ptr ())]

-- | Create an SKTexture containing a snapshot of how it would have been rendered in this view. The texture is tightly cropped to the size of the node.
--
-- @node@ — the node subtree to render to the texture.
--
-- ObjC selector: @- textureFromNode:@
textureFromNode :: (IsSKView skView, IsSKNode node) => skView -> node -> IO (Id SKTexture)
textureFromNode skView  node =
withObjCPtr node $ \raw_node ->
    sendMsg skView (mkSelector "textureFromNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | Pause the entire view
--
-- ObjC selector: @- paused@
paused :: IsSKView skView => skView -> IO Bool
paused skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "paused") retCULong []

-- | Pause the entire view
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSKView skView => skView -> Bool -> IO ()
setPaused skView  value =
  sendMsg skView (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- | Toggles display of performance stats in the view. All default to false.
--
-- ObjC selector: @- showsFPS@
showsFPS :: IsSKView skView => skView -> IO Bool
showsFPS skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "showsFPS") retCULong []

-- | Toggles display of performance stats in the view. All default to false.
--
-- ObjC selector: @- setShowsFPS:@
setShowsFPS :: IsSKView skView => skView -> Bool -> IO ()
setShowsFPS skView  value =
  sendMsg skView (mkSelector "setShowsFPS:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsDrawCount@
showsDrawCount :: IsSKView skView => skView -> IO Bool
showsDrawCount skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "showsDrawCount") retCULong []

-- | @- setShowsDrawCount:@
setShowsDrawCount :: IsSKView skView => skView -> Bool -> IO ()
setShowsDrawCount skView  value =
  sendMsg skView (mkSelector "setShowsDrawCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsNodeCount@
showsNodeCount :: IsSKView skView => skView -> IO Bool
showsNodeCount skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "showsNodeCount") retCULong []

-- | @- setShowsNodeCount:@
setShowsNodeCount :: IsSKView skView => skView -> Bool -> IO ()
setShowsNodeCount skView  value =
  sendMsg skView (mkSelector "setShowsNodeCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsQuadCount@
showsQuadCount :: IsSKView skView => skView -> IO Bool
showsQuadCount skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "showsQuadCount") retCULong []

-- | @- setShowsQuadCount:@
setShowsQuadCount :: IsSKView skView => skView -> Bool -> IO ()
setShowsQuadCount skView  value =
  sendMsg skView (mkSelector "setShowsQuadCount:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsPhysics@
showsPhysics :: IsSKView skView => skView -> IO Bool
showsPhysics skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "showsPhysics") retCULong []

-- | @- setShowsPhysics:@
setShowsPhysics :: IsSKView skView => skView -> Bool -> IO ()
setShowsPhysics skView  value =
  sendMsg skView (mkSelector "setShowsPhysics:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsFields@
showsFields :: IsSKView skView => skView -> IO Bool
showsFields skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "showsFields") retCULong []

-- | @- setShowsFields:@
setShowsFields :: IsSKView skView => skView -> Bool -> IO ()
setShowsFields skView  value =
  sendMsg skView (mkSelector "setShowsFields:") retVoid [argCULong (if value then 1 else 0)]

-- | Toggles whether the view updates is rendered asynchronously or aligned with Core Animation updates. Defaults to YES.
--
-- ObjC selector: @- asynchronous@
asynchronous :: IsSKView skView => skView -> IO Bool
asynchronous skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "asynchronous") retCULong []

-- | Toggles whether the view updates is rendered asynchronously or aligned with Core Animation updates. Defaults to YES.
--
-- ObjC selector: @- setAsynchronous:@
setAsynchronous :: IsSKView skView => skView -> Bool -> IO ()
setAsynchronous skView  value =
  sendMsg skView (mkSelector "setAsynchronous:") retVoid [argCULong (if value then 1 else 0)]

-- | Toggles whether the view allows transparent rendering. This allows content under the view to show through if a non-opaque backgroundColor is set on the scene. Defaults to NO.
--
-- ObjC selector: @- allowsTransparency@
allowsTransparency :: IsSKView skView => skView -> IO Bool
allowsTransparency skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "allowsTransparency") retCULong []

-- | Toggles whether the view allows transparent rendering. This allows content under the view to show through if a non-opaque backgroundColor is set on the scene. Defaults to NO.
--
-- ObjC selector: @- setAllowsTransparency:@
setAllowsTransparency :: IsSKView skView => skView -> Bool -> IO ()
setAllowsTransparency skView  value =
  sendMsg skView (mkSelector "setAllowsTransparency:") retVoid [argCULong (if value then 1 else 0)]

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- ignoresSiblingOrder@
ignoresSiblingOrder :: IsSKView skView => skView -> IO Bool
ignoresSiblingOrder skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "ignoresSiblingOrder") retCULong []

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- setIgnoresSiblingOrder:@
setIgnoresSiblingOrder :: IsSKView skView => skView -> Bool -> IO ()
setIgnoresSiblingOrder skView  value =
  sendMsg skView (mkSelector "setIgnoresSiblingOrder:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldCullNonVisibleNodes@
shouldCullNonVisibleNodes :: IsSKView skView => skView -> IO Bool
shouldCullNonVisibleNodes skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "shouldCullNonVisibleNodes") retCULong []

-- | @- setShouldCullNonVisibleNodes:@
setShouldCullNonVisibleNodes :: IsSKView skView => skView -> Bool -> IO ()
setShouldCullNonVisibleNodes skView  value =
  sendMsg skView (mkSelector "setShouldCullNonVisibleNodes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsSKView skView => skView -> IO CLong
preferredFramesPerSecond skView  =
  sendMsg skView (mkSelector "preferredFramesPerSecond") retCLong []

-- | @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsSKView skView => skView -> CLong -> IO ()
setPreferredFramesPerSecond skView  value =
  sendMsg skView (mkSelector "setPreferredFramesPerSecond:") retVoid [argCLong (fromIntegral value)]

-- | @- disableDepthStencilBuffer@
disableDepthStencilBuffer :: IsSKView skView => skView -> IO Bool
disableDepthStencilBuffer skView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skView (mkSelector "disableDepthStencilBuffer") retCULong []

-- | @- setDisableDepthStencilBuffer:@
setDisableDepthStencilBuffer :: IsSKView skView => skView -> Bool -> IO ()
setDisableDepthStencilBuffer skView  value =
  sendMsg skView (mkSelector "setDisableDepthStencilBuffer:") retVoid [argCULong (if value then 1 else 0)]

-- | Optional view delegate, see SKViewDelegate.
--
-- ObjC selector: @- delegate@
delegate :: IsSKView skView => skView -> IO (Id NSObject)
delegate skView  =
  sendMsg skView (mkSelector "delegate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional view delegate, see SKViewDelegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: (IsSKView skView, IsNSObject value) => skView -> value -> IO ()
setDelegate skView  value =
withObjCPtr value $ \raw_value ->
    sendMsg skView (mkSelector "setDelegate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- frameInterval@
frameInterval :: IsSKView skView => skView -> IO CLong
frameInterval skView  =
  sendMsg skView (mkSelector "frameInterval") retCLong []

-- | @- setFrameInterval:@
setFrameInterval :: IsSKView skView => skView -> CLong -> IO ()
setFrameInterval skView  value =
  sendMsg skView (mkSelector "setFrameInterval:") retVoid [argCLong (fromIntegral value)]

-- | @- preferredFrameRate@
preferredFrameRate :: IsSKView skView => skView -> IO CFloat
preferredFrameRate skView  =
  sendMsg skView (mkSelector "preferredFrameRate") retCFloat []

-- | @- setPreferredFrameRate:@
setPreferredFrameRate :: IsSKView skView => skView -> CFloat -> IO ()
setPreferredFrameRate skView  value =
  sendMsg skView (mkSelector "setPreferredFrameRate:") retVoid [argCFloat (fromIntegral value)]

-- | The currently presented scene, otherwise nil. If in a transition, the 'incoming' scene is returned.
--
-- ObjC selector: @- scene@
scene :: IsSKView skView => skView -> IO (Id SKScene)
scene skView  =
  sendMsg skView (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentScene:@
presentSceneSelector :: Selector
presentSceneSelector = mkSelector "presentScene:"

-- | @Selector@ for @presentScene:transition:@
presentScene_transitionSelector :: Selector
presentScene_transitionSelector = mkSelector "presentScene:transition:"

-- | @Selector@ for @textureFromNode:@
textureFromNodeSelector :: Selector
textureFromNodeSelector = mkSelector "textureFromNode:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @showsFPS@
showsFPSSelector :: Selector
showsFPSSelector = mkSelector "showsFPS"

-- | @Selector@ for @setShowsFPS:@
setShowsFPSSelector :: Selector
setShowsFPSSelector = mkSelector "setShowsFPS:"

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

-- | @Selector@ for @asynchronous@
asynchronousSelector :: Selector
asynchronousSelector = mkSelector "asynchronous"

-- | @Selector@ for @setAsynchronous:@
setAsynchronousSelector :: Selector
setAsynchronousSelector = mkSelector "setAsynchronous:"

-- | @Selector@ for @allowsTransparency@
allowsTransparencySelector :: Selector
allowsTransparencySelector = mkSelector "allowsTransparency"

-- | @Selector@ for @setAllowsTransparency:@
setAllowsTransparencySelector :: Selector
setAllowsTransparencySelector = mkSelector "setAllowsTransparency:"

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

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @disableDepthStencilBuffer@
disableDepthStencilBufferSelector :: Selector
disableDepthStencilBufferSelector = mkSelector "disableDepthStencilBuffer"

-- | @Selector@ for @setDisableDepthStencilBuffer:@
setDisableDepthStencilBufferSelector :: Selector
setDisableDepthStencilBufferSelector = mkSelector "setDisableDepthStencilBuffer:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @frameInterval@
frameIntervalSelector :: Selector
frameIntervalSelector = mkSelector "frameInterval"

-- | @Selector@ for @setFrameInterval:@
setFrameIntervalSelector :: Selector
setFrameIntervalSelector = mkSelector "setFrameInterval:"

-- | @Selector@ for @preferredFrameRate@
preferredFrameRateSelector :: Selector
preferredFrameRateSelector = mkSelector "preferredFrameRate"

-- | @Selector@ for @setPreferredFrameRate:@
setPreferredFrameRateSelector :: Selector
setPreferredFrameRateSelector = mkSelector "setPreferredFrameRate:"

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

