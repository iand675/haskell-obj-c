{-# LANGUAGE PatternSynonyms #-}
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
  , sceneDidLoadSelector
  , updateSelector
  , didEvaluateActionsSelector
  , didSimulatePhysicsSelector
  , didApplyConstraintsSelector
  , didFinishUpdateSelector
  , didMoveToViewSelector
  , willMoveFromViewSelector
  , scaleModeSelector
  , setScaleModeSelector
  , cameraSelector
  , setCameraSelector
  , listenerSelector
  , setListenerSelector
  , audioEngineSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , delegateSelector
  , setDelegateSelector
  , physicsWorldSelector
  , viewSelector

  -- * Enum types
  , SKSceneScaleMode(SKSceneScaleMode)
  , pattern SKSceneScaleModeFill
  , pattern SKSceneScaleModeAspectFill
  , pattern SKSceneScaleModeAspectFit
  , pattern SKSceneScaleModeResizeFill

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
import ObjC.SpriteKit.Internal.Enums
import ObjC.AVFAudio.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sceneDidLoad@
sceneDidLoad :: IsSKScene skScene => skScene -> IO ()
sceneDidLoad skScene  =
    sendMsg skScene (mkSelector "sceneDidLoad") retVoid []

-- | Override this to perform per-frame game logic. Called exactly once per frame before any actions are evaluated and any physics are simulated.
--
-- @currentTime@ — the current time in the app. This must be monotonically increasing.
--
-- ObjC selector: @- update:@
update :: IsSKScene skScene => skScene -> CDouble -> IO ()
update skScene  currentTime =
    sendMsg skScene (mkSelector "update:") retVoid [argCDouble currentTime]

-- | Override this to perform game logic. Called exactly once per frame after any actions have been evaluated but before any physics are simulated. Any additional actions applied is not evaluated until the next update.
--
-- ObjC selector: @- didEvaluateActions@
didEvaluateActions :: IsSKScene skScene => skScene -> IO ()
didEvaluateActions skScene  =
    sendMsg skScene (mkSelector "didEvaluateActions") retVoid []

-- | Override this to perform game logic. Called exactly once per frame after any actions have been evaluated and any physics have been simulated. Any additional actions applied is not evaluated until the next update. Any changes to physics bodies is not simulated until the next update.
--
-- ObjC selector: @- didSimulatePhysics@
didSimulatePhysics :: IsSKScene skScene => skScene -> IO ()
didSimulatePhysics skScene  =
    sendMsg skScene (mkSelector "didSimulatePhysics") retVoid []

-- | Override this to perform game logic. Called exactly once per frame after any enabled constraints have been applied. Any additional actions applied is not evaluated until the next update. Any changes to physics bodies is not simulated until the next update. Any changes to constraints will not be applied until the next update.
--
-- ObjC selector: @- didApplyConstraints@
didApplyConstraints :: IsSKScene skScene => skScene -> IO ()
didApplyConstraints skScene  =
    sendMsg skScene (mkSelector "didApplyConstraints") retVoid []

-- | Override this to perform game logic. Called after all update logic has been completed. Any additional actions applied are not evaluated until the next update. Any changes to physics bodies are not simulated until the next update. Any changes to constraints will not be applied until the next update.
--
-- No futher update logic will be applied to the scene after this call. Any values set on nodes here will be used when the scene is rendered for the current frame.
--
-- ObjC selector: @- didFinishUpdate@
didFinishUpdate :: IsSKScene skScene => skScene -> IO ()
didFinishUpdate skScene  =
    sendMsg skScene (mkSelector "didFinishUpdate") retVoid []

-- | @- didMoveToView:@
didMoveToView :: (IsSKScene skScene, IsSKView view) => skScene -> view -> IO ()
didMoveToView skScene  view =
  withObjCPtr view $ \raw_view ->
      sendMsg skScene (mkSelector "didMoveToView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- willMoveFromView:@
willMoveFromView :: (IsSKScene skScene, IsSKView view) => skScene -> view -> IO ()
willMoveFromView skScene  view =
  withObjCPtr view $ \raw_view ->
      sendMsg skScene (mkSelector "willMoveFromView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | Used to determine how to scale the scene to match the SKView it is being displayed in.
--
-- ObjC selector: @- scaleMode@
scaleMode :: IsSKScene skScene => skScene -> IO SKSceneScaleMode
scaleMode skScene  =
    fmap (coerce :: CLong -> SKSceneScaleMode) $ sendMsg skScene (mkSelector "scaleMode") retCLong []

-- | Used to determine how to scale the scene to match the SKView it is being displayed in.
--
-- ObjC selector: @- setScaleMode:@
setScaleMode :: IsSKScene skScene => skScene -> SKSceneScaleMode -> IO ()
setScaleMode skScene  value =
    sendMsg skScene (mkSelector "setScaleMode:") retVoid [argCLong (coerce value)]

-- | The camera that is used to obtain the view scale and translation based on where the camera is in relation to the scene.
--
-- ObjC selector: @- camera@
camera :: IsSKScene skScene => skScene -> IO (Id SKCameraNode)
camera skScene  =
    sendMsg skScene (mkSelector "camera") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The camera that is used to obtain the view scale and translation based on where the camera is in relation to the scene.
--
-- ObjC selector: @- setCamera:@
setCamera :: (IsSKScene skScene, IsSKCameraNode value) => skScene -> value -> IO ()
setCamera skScene  value =
  withObjCPtr value $ \raw_value ->
      sendMsg skScene (mkSelector "setCamera:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The node that is currently the listener for positional audio coming from SKAudioNodes
--
-- See: SKAudioNode
--
-- ObjC selector: @- listener@
listener :: IsSKScene skScene => skScene -> IO (Id SKNode)
listener skScene  =
    sendMsg skScene (mkSelector "listener") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The node that is currently the listener for positional audio coming from SKAudioNodes
--
-- See: SKAudioNode
--
-- ObjC selector: @- setListener:@
setListener :: (IsSKScene skScene, IsSKNode value) => skScene -> value -> IO ()
setListener skScene  value =
  withObjCPtr value $ \raw_value ->
      sendMsg skScene (mkSelector "setListener:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- audioEngine@
audioEngine :: IsSKScene skScene => skScene -> IO (Id AVAudioEngine)
audioEngine skScene  =
    sendMsg skScene (mkSelector "audioEngine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Background color, defaults to gray
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsSKScene skScene => skScene -> IO (Id NSColor)
backgroundColor skScene  =
    sendMsg skScene (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Background color, defaults to gray
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsSKScene skScene, IsNSColor value) => skScene -> value -> IO ()
setBackgroundColor skScene  value =
  withObjCPtr value $ \raw_value ->
      sendMsg skScene (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delegate@
delegate :: IsSKScene skScene => skScene -> IO RawId
delegate skScene  =
    fmap (RawId . castPtr) $ sendMsg skScene (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsSKScene skScene => skScene -> RawId -> IO ()
setDelegate skScene  value =
    sendMsg skScene (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Physics simulation functionality
--
-- ObjC selector: @- physicsWorld@
physicsWorld :: IsSKScene skScene => skScene -> IO (Id SKPhysicsWorld)
physicsWorld skScene  =
    sendMsg skScene (mkSelector "physicsWorld") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The SKView this scene is currently presented in, or nil if it is not being presented.
--
-- ObjC selector: @- view@
view :: IsSKScene skScene => skScene -> IO (Id SKView)
view skScene  =
    sendMsg skScene (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneDidLoad@
sceneDidLoadSelector :: Selector
sceneDidLoadSelector = mkSelector "sceneDidLoad"

-- | @Selector@ for @update:@
updateSelector :: Selector
updateSelector = mkSelector "update:"

-- | @Selector@ for @didEvaluateActions@
didEvaluateActionsSelector :: Selector
didEvaluateActionsSelector = mkSelector "didEvaluateActions"

-- | @Selector@ for @didSimulatePhysics@
didSimulatePhysicsSelector :: Selector
didSimulatePhysicsSelector = mkSelector "didSimulatePhysics"

-- | @Selector@ for @didApplyConstraints@
didApplyConstraintsSelector :: Selector
didApplyConstraintsSelector = mkSelector "didApplyConstraints"

-- | @Selector@ for @didFinishUpdate@
didFinishUpdateSelector :: Selector
didFinishUpdateSelector = mkSelector "didFinishUpdate"

-- | @Selector@ for @didMoveToView:@
didMoveToViewSelector :: Selector
didMoveToViewSelector = mkSelector "didMoveToView:"

-- | @Selector@ for @willMoveFromView:@
willMoveFromViewSelector :: Selector
willMoveFromViewSelector = mkSelector "willMoveFromView:"

-- | @Selector@ for @scaleMode@
scaleModeSelector :: Selector
scaleModeSelector = mkSelector "scaleMode"

-- | @Selector@ for @setScaleMode:@
setScaleModeSelector :: Selector
setScaleModeSelector = mkSelector "setScaleMode:"

-- | @Selector@ for @camera@
cameraSelector :: Selector
cameraSelector = mkSelector "camera"

-- | @Selector@ for @setCamera:@
setCameraSelector :: Selector
setCameraSelector = mkSelector "setCamera:"

-- | @Selector@ for @listener@
listenerSelector :: Selector
listenerSelector = mkSelector "listener"

-- | @Selector@ for @setListener:@
setListenerSelector :: Selector
setListenerSelector = mkSelector "setListener:"

-- | @Selector@ for @audioEngine@
audioEngineSelector :: Selector
audioEngineSelector = mkSelector "audioEngine"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @physicsWorld@
physicsWorldSelector :: Selector
physicsWorldSelector = mkSelector "physicsWorld"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

