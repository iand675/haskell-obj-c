{-# LANGUAGE DataKinds #-}
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
  , allowsTransparencySelector
  , asynchronousSelector
  , delegateSelector
  , disableDepthStencilBufferSelector
  , frameIntervalSelector
  , ignoresSiblingOrderSelector
  , pausedSelector
  , preferredFrameRateSelector
  , preferredFramesPerSecondSelector
  , presentSceneSelector
  , presentScene_transitionSelector
  , sceneSelector
  , setAllowsTransparencySelector
  , setAsynchronousSelector
  , setDelegateSelector
  , setDisableDepthStencilBufferSelector
  , setFrameIntervalSelector
  , setIgnoresSiblingOrderSelector
  , setPausedSelector
  , setPreferredFrameRateSelector
  , setPreferredFramesPerSecondSelector
  , setShouldCullNonVisibleNodesSelector
  , setShowsDrawCountSelector
  , setShowsFPSSelector
  , setShowsFieldsSelector
  , setShowsNodeCountSelector
  , setShowsPhysicsSelector
  , setShowsQuadCountSelector
  , shouldCullNonVisibleNodesSelector
  , showsDrawCountSelector
  , showsFPSSelector
  , showsFieldsSelector
  , showsNodeCountSelector
  , showsPhysicsSelector
  , showsQuadCountSelector
  , textureFromNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
presentScene skView scene =
  sendMessage skView presentSceneSelector (toSKScene scene)

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
presentScene_transition skView scene transition =
  sendMessage skView presentScene_transitionSelector (toSKScene scene) (toSKTransition transition)

-- | Create an SKTexture containing a snapshot of how it would have been rendered in this view. The texture is tightly cropped to the size of the node.
--
-- @node@ — the node subtree to render to the texture.
--
-- ObjC selector: @- textureFromNode:@
textureFromNode :: (IsSKView skView, IsSKNode node) => skView -> node -> IO (Id SKTexture)
textureFromNode skView node =
  sendMessage skView textureFromNodeSelector (toSKNode node)

-- | Pause the entire view
--
-- ObjC selector: @- paused@
paused :: IsSKView skView => skView -> IO Bool
paused skView =
  sendMessage skView pausedSelector

-- | Pause the entire view
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSKView skView => skView -> Bool -> IO ()
setPaused skView value =
  sendMessage skView setPausedSelector value

-- | Toggles display of performance stats in the view. All default to false.
--
-- ObjC selector: @- showsFPS@
showsFPS :: IsSKView skView => skView -> IO Bool
showsFPS skView =
  sendMessage skView showsFPSSelector

-- | Toggles display of performance stats in the view. All default to false.
--
-- ObjC selector: @- setShowsFPS:@
setShowsFPS :: IsSKView skView => skView -> Bool -> IO ()
setShowsFPS skView value =
  sendMessage skView setShowsFPSSelector value

-- | @- showsDrawCount@
showsDrawCount :: IsSKView skView => skView -> IO Bool
showsDrawCount skView =
  sendMessage skView showsDrawCountSelector

-- | @- setShowsDrawCount:@
setShowsDrawCount :: IsSKView skView => skView -> Bool -> IO ()
setShowsDrawCount skView value =
  sendMessage skView setShowsDrawCountSelector value

-- | @- showsNodeCount@
showsNodeCount :: IsSKView skView => skView -> IO Bool
showsNodeCount skView =
  sendMessage skView showsNodeCountSelector

-- | @- setShowsNodeCount:@
setShowsNodeCount :: IsSKView skView => skView -> Bool -> IO ()
setShowsNodeCount skView value =
  sendMessage skView setShowsNodeCountSelector value

-- | @- showsQuadCount@
showsQuadCount :: IsSKView skView => skView -> IO Bool
showsQuadCount skView =
  sendMessage skView showsQuadCountSelector

-- | @- setShowsQuadCount:@
setShowsQuadCount :: IsSKView skView => skView -> Bool -> IO ()
setShowsQuadCount skView value =
  sendMessage skView setShowsQuadCountSelector value

-- | @- showsPhysics@
showsPhysics :: IsSKView skView => skView -> IO Bool
showsPhysics skView =
  sendMessage skView showsPhysicsSelector

-- | @- setShowsPhysics:@
setShowsPhysics :: IsSKView skView => skView -> Bool -> IO ()
setShowsPhysics skView value =
  sendMessage skView setShowsPhysicsSelector value

-- | @- showsFields@
showsFields :: IsSKView skView => skView -> IO Bool
showsFields skView =
  sendMessage skView showsFieldsSelector

-- | @- setShowsFields:@
setShowsFields :: IsSKView skView => skView -> Bool -> IO ()
setShowsFields skView value =
  sendMessage skView setShowsFieldsSelector value

-- | Toggles whether the view updates is rendered asynchronously or aligned with Core Animation updates. Defaults to YES.
--
-- ObjC selector: @- asynchronous@
asynchronous :: IsSKView skView => skView -> IO Bool
asynchronous skView =
  sendMessage skView asynchronousSelector

-- | Toggles whether the view updates is rendered asynchronously or aligned with Core Animation updates. Defaults to YES.
--
-- ObjC selector: @- setAsynchronous:@
setAsynchronous :: IsSKView skView => skView -> Bool -> IO ()
setAsynchronous skView value =
  sendMessage skView setAsynchronousSelector value

-- | Toggles whether the view allows transparent rendering. This allows content under the view to show through if a non-opaque backgroundColor is set on the scene. Defaults to NO.
--
-- ObjC selector: @- allowsTransparency@
allowsTransparency :: IsSKView skView => skView -> IO Bool
allowsTransparency skView =
  sendMessage skView allowsTransparencySelector

-- | Toggles whether the view allows transparent rendering. This allows content under the view to show through if a non-opaque backgroundColor is set on the scene. Defaults to NO.
--
-- ObjC selector: @- setAllowsTransparency:@
setAllowsTransparency :: IsSKView skView => skView -> Bool -> IO ()
setAllowsTransparency skView value =
  sendMessage skView setAllowsTransparencySelector value

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- ignoresSiblingOrder@
ignoresSiblingOrder :: IsSKView skView => skView -> IO Bool
ignoresSiblingOrder skView =
  sendMessage skView ignoresSiblingOrderSelector

-- | Ignores sibling and traversal order to sort the rendered contents of a scene into the most efficient batching possible. This will require zPosition to be used in the scenes to properly guarantee elements are in front or behind each other.
--
-- This defaults to NO, meaning that sibling order overrides efficiency heuristics in the rendering of the scenes in the view.
--
-- Setting this to YES for a complex scene may substantially increase performance, but care must be taken as only zPosition determines render order before the efficiency heuristics are used.
--
-- ObjC selector: @- setIgnoresSiblingOrder:@
setIgnoresSiblingOrder :: IsSKView skView => skView -> Bool -> IO ()
setIgnoresSiblingOrder skView value =
  sendMessage skView setIgnoresSiblingOrderSelector value

-- | @- shouldCullNonVisibleNodes@
shouldCullNonVisibleNodes :: IsSKView skView => skView -> IO Bool
shouldCullNonVisibleNodes skView =
  sendMessage skView shouldCullNonVisibleNodesSelector

-- | @- setShouldCullNonVisibleNodes:@
setShouldCullNonVisibleNodes :: IsSKView skView => skView -> Bool -> IO ()
setShouldCullNonVisibleNodes skView value =
  sendMessage skView setShouldCullNonVisibleNodesSelector value

-- | @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsSKView skView => skView -> IO CLong
preferredFramesPerSecond skView =
  sendMessage skView preferredFramesPerSecondSelector

-- | @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsSKView skView => skView -> CLong -> IO ()
setPreferredFramesPerSecond skView value =
  sendMessage skView setPreferredFramesPerSecondSelector value

-- | @- disableDepthStencilBuffer@
disableDepthStencilBuffer :: IsSKView skView => skView -> IO Bool
disableDepthStencilBuffer skView =
  sendMessage skView disableDepthStencilBufferSelector

-- | @- setDisableDepthStencilBuffer:@
setDisableDepthStencilBuffer :: IsSKView skView => skView -> Bool -> IO ()
setDisableDepthStencilBuffer skView value =
  sendMessage skView setDisableDepthStencilBufferSelector value

-- | Optional view delegate, see SKViewDelegate.
--
-- ObjC selector: @- delegate@
delegate :: IsSKView skView => skView -> IO (Id NSObject)
delegate skView =
  sendMessage skView delegateSelector

-- | Optional view delegate, see SKViewDelegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: (IsSKView skView, IsNSObject value) => skView -> value -> IO ()
setDelegate skView value =
  sendMessage skView setDelegateSelector (toNSObject value)

-- | @- frameInterval@
frameInterval :: IsSKView skView => skView -> IO CLong
frameInterval skView =
  sendMessage skView frameIntervalSelector

-- | @- setFrameInterval:@
setFrameInterval :: IsSKView skView => skView -> CLong -> IO ()
setFrameInterval skView value =
  sendMessage skView setFrameIntervalSelector value

-- | @- preferredFrameRate@
preferredFrameRate :: IsSKView skView => skView -> IO CFloat
preferredFrameRate skView =
  sendMessage skView preferredFrameRateSelector

-- | @- setPreferredFrameRate:@
setPreferredFrameRate :: IsSKView skView => skView -> CFloat -> IO ()
setPreferredFrameRate skView value =
  sendMessage skView setPreferredFrameRateSelector value

-- | The currently presented scene, otherwise nil. If in a transition, the 'incoming' scene is returned.
--
-- ObjC selector: @- scene@
scene :: IsSKView skView => skView -> IO (Id SKScene)
scene skView =
  sendMessage skView sceneSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentScene:@
presentSceneSelector :: Selector '[Id SKScene] ()
presentSceneSelector = mkSelector "presentScene:"

-- | @Selector@ for @presentScene:transition:@
presentScene_transitionSelector :: Selector '[Id SKScene, Id SKTransition] ()
presentScene_transitionSelector = mkSelector "presentScene:transition:"

-- | @Selector@ for @textureFromNode:@
textureFromNodeSelector :: Selector '[Id SKNode] (Id SKTexture)
textureFromNodeSelector = mkSelector "textureFromNode:"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @showsFPS@
showsFPSSelector :: Selector '[] Bool
showsFPSSelector = mkSelector "showsFPS"

-- | @Selector@ for @setShowsFPS:@
setShowsFPSSelector :: Selector '[Bool] ()
setShowsFPSSelector = mkSelector "setShowsFPS:"

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

-- | @Selector@ for @asynchronous@
asynchronousSelector :: Selector '[] Bool
asynchronousSelector = mkSelector "asynchronous"

-- | @Selector@ for @setAsynchronous:@
setAsynchronousSelector :: Selector '[Bool] ()
setAsynchronousSelector = mkSelector "setAsynchronous:"

-- | @Selector@ for @allowsTransparency@
allowsTransparencySelector :: Selector '[] Bool
allowsTransparencySelector = mkSelector "allowsTransparency"

-- | @Selector@ for @setAllowsTransparency:@
setAllowsTransparencySelector :: Selector '[Bool] ()
setAllowsTransparencySelector = mkSelector "setAllowsTransparency:"

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

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector '[] CLong
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector '[CLong] ()
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @disableDepthStencilBuffer@
disableDepthStencilBufferSelector :: Selector '[] Bool
disableDepthStencilBufferSelector = mkSelector "disableDepthStencilBuffer"

-- | @Selector@ for @setDisableDepthStencilBuffer:@
setDisableDepthStencilBufferSelector :: Selector '[Bool] ()
setDisableDepthStencilBufferSelector = mkSelector "setDisableDepthStencilBuffer:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] (Id NSObject)
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[Id NSObject] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @frameInterval@
frameIntervalSelector :: Selector '[] CLong
frameIntervalSelector = mkSelector "frameInterval"

-- | @Selector@ for @setFrameInterval:@
setFrameIntervalSelector :: Selector '[CLong] ()
setFrameIntervalSelector = mkSelector "setFrameInterval:"

-- | @Selector@ for @preferredFrameRate@
preferredFrameRateSelector :: Selector '[] CFloat
preferredFrameRateSelector = mkSelector "preferredFrameRate"

-- | @Selector@ for @setPreferredFrameRate:@
setPreferredFrameRateSelector :: Selector '[CFloat] ()
setPreferredFrameRateSelector = mkSelector "setPreferredFrameRate:"

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SKScene)
sceneSelector = mkSelector "scene"

