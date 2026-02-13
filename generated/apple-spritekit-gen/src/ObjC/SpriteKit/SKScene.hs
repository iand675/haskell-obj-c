{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A scene is the root node of your content. It is used to display SpriteKit content on an SKView.
--
-- See: SKView
--
-- Generated bindings for @SKScene@.
module ObjC.SpriteKit.SKScene
  ( SKScene
  , IsSKScene(..)
  , sceneDidLoad
  , update
  , didEvaluateActions
  , didSimulatePhysics
  , didApplyConstraints
  , didFinishUpdate
  , didMoveToView
  , willMoveFromView
  , scaleMode
  , setScaleMode
  , camera
  , setCamera
  , listener
  , setListener
  , audioEngine
  , backgroundColor
  , setBackgroundColor
  , delegate
  , setDelegate
  , physicsWorld
  , view
  , audioEngineSelector
  , backgroundColorSelector
  , cameraSelector
  , delegateSelector
  , didApplyConstraintsSelector
  , didEvaluateActionsSelector
  , didFinishUpdateSelector
  , didMoveToViewSelector
  , didSimulatePhysicsSelector
  , listenerSelector
  , physicsWorldSelector
  , scaleModeSelector
  , sceneDidLoadSelector
  , setBackgroundColorSelector
  , setCameraSelector
  , setDelegateSelector
  , setListenerSelector
  , setScaleModeSelector
  , updateSelector
  , viewSelector
  , willMoveFromViewSelector

  -- * Enum types
  , SKSceneScaleMode(SKSceneScaleMode)
  , pattern SKSceneScaleModeFill
  , pattern SKSceneScaleModeAspectFill
  , pattern SKSceneScaleModeAspectFit
  , pattern SKSceneScaleModeResizeFill

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sceneDidLoad@
sceneDidLoad :: IsSKScene skScene => skScene -> IO ()
sceneDidLoad skScene =
  sendMessage skScene sceneDidLoadSelector

-- | Override this to perform per-frame game logic. Called exactly once per frame before any actions are evaluated and any physics are simulated.
--
-- @currentTime@ — the current time in the app. This must be monotonically increasing.
--
-- ObjC selector: @- update:@
update :: IsSKScene skScene => skScene -> CDouble -> IO ()
update skScene currentTime =
  sendMessage skScene updateSelector currentTime

-- | Override this to perform game logic. Called exactly once per frame after any actions have been evaluated but before any physics are simulated. Any additional actions applied is not evaluated until the next update.
--
-- ObjC selector: @- didEvaluateActions@
didEvaluateActions :: IsSKScene skScene => skScene -> IO ()
didEvaluateActions skScene =
  sendMessage skScene didEvaluateActionsSelector

-- | Override this to perform game logic. Called exactly once per frame after any actions have been evaluated and any physics have been simulated. Any additional actions applied is not evaluated until the next update. Any changes to physics bodies is not simulated until the next update.
--
-- ObjC selector: @- didSimulatePhysics@
didSimulatePhysics :: IsSKScene skScene => skScene -> IO ()
didSimulatePhysics skScene =
  sendMessage skScene didSimulatePhysicsSelector

-- | Override this to perform game logic. Called exactly once per frame after any enabled constraints have been applied. Any additional actions applied is not evaluated until the next update. Any changes to physics bodies is not simulated until the next update. Any changes to constraints will not be applied until the next update.
--
-- ObjC selector: @- didApplyConstraints@
didApplyConstraints :: IsSKScene skScene => skScene -> IO ()
didApplyConstraints skScene =
  sendMessage skScene didApplyConstraintsSelector

-- | Override this to perform game logic. Called after all update logic has been completed. Any additional actions applied are not evaluated until the next update. Any changes to physics bodies are not simulated until the next update. Any changes to constraints will not be applied until the next update.
--
-- No futher update logic will be applied to the scene after this call. Any values set on nodes here will be used when the scene is rendered for the current frame.
--
-- ObjC selector: @- didFinishUpdate@
didFinishUpdate :: IsSKScene skScene => skScene -> IO ()
didFinishUpdate skScene =
  sendMessage skScene didFinishUpdateSelector

-- | @- didMoveToView:@
didMoveToView :: (IsSKScene skScene, IsSKView view) => skScene -> view -> IO ()
didMoveToView skScene view =
  sendMessage skScene didMoveToViewSelector (toSKView view)

-- | @- willMoveFromView:@
willMoveFromView :: (IsSKScene skScene, IsSKView view) => skScene -> view -> IO ()
willMoveFromView skScene view =
  sendMessage skScene willMoveFromViewSelector (toSKView view)

-- | Used to determine how to scale the scene to match the SKView it is being displayed in.
--
-- ObjC selector: @- scaleMode@
scaleMode :: IsSKScene skScene => skScene -> IO SKSceneScaleMode
scaleMode skScene =
  sendMessage skScene scaleModeSelector

-- | Used to determine how to scale the scene to match the SKView it is being displayed in.
--
-- ObjC selector: @- setScaleMode:@
setScaleMode :: IsSKScene skScene => skScene -> SKSceneScaleMode -> IO ()
setScaleMode skScene value =
  sendMessage skScene setScaleModeSelector value

-- | The camera that is used to obtain the view scale and translation based on where the camera is in relation to the scene.
--
-- ObjC selector: @- camera@
camera :: IsSKScene skScene => skScene -> IO (Id SKCameraNode)
camera skScene =
  sendMessage skScene cameraSelector

-- | The camera that is used to obtain the view scale and translation based on where the camera is in relation to the scene.
--
-- ObjC selector: @- setCamera:@
setCamera :: (IsSKScene skScene, IsSKCameraNode value) => skScene -> value -> IO ()
setCamera skScene value =
  sendMessage skScene setCameraSelector (toSKCameraNode value)

-- | The node that is currently the listener for positional audio coming from SKAudioNodes
--
-- See: SKAudioNode
--
-- ObjC selector: @- listener@
listener :: IsSKScene skScene => skScene -> IO (Id SKNode)
listener skScene =
  sendMessage skScene listenerSelector

-- | The node that is currently the listener for positional audio coming from SKAudioNodes
--
-- See: SKAudioNode
--
-- ObjC selector: @- setListener:@
setListener :: (IsSKScene skScene, IsSKNode value) => skScene -> value -> IO ()
setListener skScene value =
  sendMessage skScene setListenerSelector (toSKNode value)

-- | @- audioEngine@
audioEngine :: IsSKScene skScene => skScene -> IO (Id AVAudioEngine)
audioEngine skScene =
  sendMessage skScene audioEngineSelector

-- | Background color, defaults to gray
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsSKScene skScene => skScene -> IO (Id NSColor)
backgroundColor skScene =
  sendMessage skScene backgroundColorSelector

-- | Background color, defaults to gray
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsSKScene skScene, IsNSColor value) => skScene -> value -> IO ()
setBackgroundColor skScene value =
  sendMessage skScene setBackgroundColorSelector (toNSColor value)

-- | @- delegate@
delegate :: IsSKScene skScene => skScene -> IO RawId
delegate skScene =
  sendMessage skScene delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSKScene skScene => skScene -> RawId -> IO ()
setDelegate skScene value =
  sendMessage skScene setDelegateSelector value

-- | Physics simulation functionality
--
-- ObjC selector: @- physicsWorld@
physicsWorld :: IsSKScene skScene => skScene -> IO (Id SKPhysicsWorld)
physicsWorld skScene =
  sendMessage skScene physicsWorldSelector

-- | The SKView this scene is currently presented in, or nil if it is not being presented.
--
-- ObjC selector: @- view@
view :: IsSKScene skScene => skScene -> IO (Id SKView)
view skScene =
  sendMessage skScene viewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneDidLoad@
sceneDidLoadSelector :: Selector '[] ()
sceneDidLoadSelector = mkSelector "sceneDidLoad"

-- | @Selector@ for @update:@
updateSelector :: Selector '[CDouble] ()
updateSelector = mkSelector "update:"

-- | @Selector@ for @didEvaluateActions@
didEvaluateActionsSelector :: Selector '[] ()
didEvaluateActionsSelector = mkSelector "didEvaluateActions"

-- | @Selector@ for @didSimulatePhysics@
didSimulatePhysicsSelector :: Selector '[] ()
didSimulatePhysicsSelector = mkSelector "didSimulatePhysics"

-- | @Selector@ for @didApplyConstraints@
didApplyConstraintsSelector :: Selector '[] ()
didApplyConstraintsSelector = mkSelector "didApplyConstraints"

-- | @Selector@ for @didFinishUpdate@
didFinishUpdateSelector :: Selector '[] ()
didFinishUpdateSelector = mkSelector "didFinishUpdate"

-- | @Selector@ for @didMoveToView:@
didMoveToViewSelector :: Selector '[Id SKView] ()
didMoveToViewSelector = mkSelector "didMoveToView:"

-- | @Selector@ for @willMoveFromView:@
willMoveFromViewSelector :: Selector '[Id SKView] ()
willMoveFromViewSelector = mkSelector "willMoveFromView:"

-- | @Selector@ for @scaleMode@
scaleModeSelector :: Selector '[] SKSceneScaleMode
scaleModeSelector = mkSelector "scaleMode"

-- | @Selector@ for @setScaleMode:@
setScaleModeSelector :: Selector '[SKSceneScaleMode] ()
setScaleModeSelector = mkSelector "setScaleMode:"

-- | @Selector@ for @camera@
cameraSelector :: Selector '[] (Id SKCameraNode)
cameraSelector = mkSelector "camera"

-- | @Selector@ for @setCamera:@
setCameraSelector :: Selector '[Id SKCameraNode] ()
setCameraSelector = mkSelector "setCamera:"

-- | @Selector@ for @listener@
listenerSelector :: Selector '[] (Id SKNode)
listenerSelector = mkSelector "listener"

-- | @Selector@ for @setListener:@
setListenerSelector :: Selector '[Id SKNode] ()
setListenerSelector = mkSelector "setListener:"

-- | @Selector@ for @audioEngine@
audioEngineSelector :: Selector '[] (Id AVAudioEngine)
audioEngineSelector = mkSelector "audioEngine"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @physicsWorld@
physicsWorldSelector :: Selector '[] (Id SKPhysicsWorld)
physicsWorldSelector = mkSelector "physicsWorld"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id SKView)
viewSelector = mkSelector "view"

