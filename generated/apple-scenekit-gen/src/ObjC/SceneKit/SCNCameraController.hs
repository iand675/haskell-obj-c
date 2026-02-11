{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNCameraController@.
module ObjC.SceneKit.SCNCameraController
  ( SCNCameraController
  , IsSCNCameraController(..)
  , translateInCameraSpaceByX_Y_Z
  , frameNodes
  , rotateByX_Y
  , rollAroundTarget
  , dollyToTarget
  , clearRoll
  , stopInertia
  , delegate
  , setDelegate
  , pointOfView
  , setPointOfView
  , interactionMode
  , setInteractionMode
  , target
  , setTarget
  , automaticTarget
  , setAutomaticTarget
  , worldUp
  , setWorldUp
  , inertiaEnabled
  , setInertiaEnabled
  , inertiaFriction
  , setInertiaFriction
  , inertiaRunning
  , minimumVerticalAngle
  , setMinimumVerticalAngle
  , maximumVerticalAngle
  , setMaximumVerticalAngle
  , minimumHorizontalAngle
  , setMinimumHorizontalAngle
  , maximumHorizontalAngle
  , setMaximumHorizontalAngle
  , translateInCameraSpaceByX_Y_ZSelector
  , frameNodesSelector
  , rotateByX_YSelector
  , rollAroundTargetSelector
  , dollyToTargetSelector
  , clearRollSelector
  , stopInertiaSelector
  , delegateSelector
  , setDelegateSelector
  , pointOfViewSelector
  , setPointOfViewSelector
  , interactionModeSelector
  , setInteractionModeSelector
  , targetSelector
  , setTargetSelector
  , automaticTargetSelector
  , setAutomaticTargetSelector
  , worldUpSelector
  , setWorldUpSelector
  , inertiaEnabledSelector
  , setInertiaEnabledSelector
  , inertiaFrictionSelector
  , setInertiaFrictionSelector
  , inertiaRunningSelector
  , minimumVerticalAngleSelector
  , setMinimumVerticalAngleSelector
  , maximumVerticalAngleSelector
  , setMaximumVerticalAngleSelector
  , minimumHorizontalAngleSelector
  , setMinimumHorizontalAngleSelector
  , maximumHorizontalAngleSelector
  , setMaximumHorizontalAngleSelector

  -- * Enum types
  , SCNInteractionMode(SCNInteractionMode)
  , pattern SCNInteractionModeFly
  , pattern SCNInteractionModeOrbitTurntable
  , pattern SCNInteractionModeOrbitAngleMapping
  , pattern SCNInteractionModeOrbitCenteredArcball
  , pattern SCNInteractionModeOrbitArcball
  , pattern SCNInteractionModePan
  , pattern SCNInteractionModeTruck

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- translateInCameraSpaceByX:Y:Z:@
translateInCameraSpaceByX_Y_Z :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> CFloat -> CFloat -> IO ()
translateInCameraSpaceByX_Y_Z scnCameraController  deltaX deltaY deltaZ =
    sendMsg scnCameraController (mkSelector "translateInCameraSpaceByX:Y:Z:") retVoid [argCFloat deltaX, argCFloat deltaY, argCFloat deltaZ]

-- | @- frameNodes:@
frameNodes :: (IsSCNCameraController scnCameraController, IsNSArray nodes) => scnCameraController -> nodes -> IO ()
frameNodes scnCameraController  nodes =
  withObjCPtr nodes $ \raw_nodes ->
      sendMsg scnCameraController (mkSelector "frameNodes:") retVoid [argPtr (castPtr raw_nodes :: Ptr ())]

-- | @- rotateByX:Y:@
rotateByX_Y :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> CFloat -> IO ()
rotateByX_Y scnCameraController  deltaX deltaY =
    sendMsg scnCameraController (mkSelector "rotateByX:Y:") retVoid [argCFloat deltaX, argCFloat deltaY]

-- | @- rollAroundTarget:@
rollAroundTarget :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
rollAroundTarget scnCameraController  delta =
    sendMsg scnCameraController (mkSelector "rollAroundTarget:") retVoid [argCFloat delta]

-- | @- dollyToTarget:@
dollyToTarget :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
dollyToTarget scnCameraController  delta =
    sendMsg scnCameraController (mkSelector "dollyToTarget:") retVoid [argCFloat delta]

-- | @- clearRoll@
clearRoll :: IsSCNCameraController scnCameraController => scnCameraController -> IO ()
clearRoll scnCameraController  =
    sendMsg scnCameraController (mkSelector "clearRoll") retVoid []

-- | @- stopInertia@
stopInertia :: IsSCNCameraController scnCameraController => scnCameraController -> IO ()
stopInertia scnCameraController  =
    sendMsg scnCameraController (mkSelector "stopInertia") retVoid []

-- | @- delegate@
delegate :: IsSCNCameraController scnCameraController => scnCameraController -> IO RawId
delegate scnCameraController  =
    fmap (RawId . castPtr) $ sendMsg scnCameraController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsSCNCameraController scnCameraController => scnCameraController -> RawId -> IO ()
setDelegate scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- pointOfView@
pointOfView :: IsSCNCameraController scnCameraController => scnCameraController -> IO (Id SCNNode)
pointOfView scnCameraController  =
    sendMsg scnCameraController (mkSelector "pointOfView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfView:@
setPointOfView :: (IsSCNCameraController scnCameraController, IsSCNNode value) => scnCameraController -> value -> IO ()
setPointOfView scnCameraController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnCameraController (mkSelector "setPointOfView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- interactionMode@
interactionMode :: IsSCNCameraController scnCameraController => scnCameraController -> IO SCNInteractionMode
interactionMode scnCameraController  =
    fmap (coerce :: CLong -> SCNInteractionMode) $ sendMsg scnCameraController (mkSelector "interactionMode") retCLong []

-- | @- setInteractionMode:@
setInteractionMode :: IsSCNCameraController scnCameraController => scnCameraController -> SCNInteractionMode -> IO ()
setInteractionMode scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setInteractionMode:") retVoid [argCLong (coerce value)]

-- | @- target@
target :: IsSCNCameraController scnCameraController => scnCameraController -> IO SCNVector3
target scnCameraController  =
    sendMsgStret scnCameraController (mkSelector "target") retSCNVector3 []

-- | @- setTarget:@
setTarget :: IsSCNCameraController scnCameraController => scnCameraController -> SCNVector3 -> IO ()
setTarget scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setTarget:") retVoid [argSCNVector3 value]

-- | @- automaticTarget@
automaticTarget :: IsSCNCameraController scnCameraController => scnCameraController -> IO Bool
automaticTarget scnCameraController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCameraController (mkSelector "automaticTarget") retCULong []

-- | @- setAutomaticTarget:@
setAutomaticTarget :: IsSCNCameraController scnCameraController => scnCameraController -> Bool -> IO ()
setAutomaticTarget scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setAutomaticTarget:") retVoid [argCULong (if value then 1 else 0)]

-- | @- worldUp@
worldUp :: IsSCNCameraController scnCameraController => scnCameraController -> IO SCNVector3
worldUp scnCameraController  =
    sendMsgStret scnCameraController (mkSelector "worldUp") retSCNVector3 []

-- | @- setWorldUp:@
setWorldUp :: IsSCNCameraController scnCameraController => scnCameraController -> SCNVector3 -> IO ()
setWorldUp scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setWorldUp:") retVoid [argSCNVector3 value]

-- | @- inertiaEnabled@
inertiaEnabled :: IsSCNCameraController scnCameraController => scnCameraController -> IO Bool
inertiaEnabled scnCameraController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCameraController (mkSelector "inertiaEnabled") retCULong []

-- | @- setInertiaEnabled:@
setInertiaEnabled :: IsSCNCameraController scnCameraController => scnCameraController -> Bool -> IO ()
setInertiaEnabled scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setInertiaEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- inertiaFriction@
inertiaFriction :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
inertiaFriction scnCameraController  =
    sendMsg scnCameraController (mkSelector "inertiaFriction") retCFloat []

-- | @- setInertiaFriction:@
setInertiaFriction :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setInertiaFriction scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setInertiaFriction:") retVoid [argCFloat value]

-- | @- inertiaRunning@
inertiaRunning :: IsSCNCameraController scnCameraController => scnCameraController -> IO Bool
inertiaRunning scnCameraController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnCameraController (mkSelector "inertiaRunning") retCULong []

-- | @- minimumVerticalAngle@
minimumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
minimumVerticalAngle scnCameraController  =
    sendMsg scnCameraController (mkSelector "minimumVerticalAngle") retCFloat []

-- | @- setMinimumVerticalAngle:@
setMinimumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMinimumVerticalAngle scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setMinimumVerticalAngle:") retVoid [argCFloat value]

-- | @- maximumVerticalAngle@
maximumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
maximumVerticalAngle scnCameraController  =
    sendMsg scnCameraController (mkSelector "maximumVerticalAngle") retCFloat []

-- | @- setMaximumVerticalAngle:@
setMaximumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMaximumVerticalAngle scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setMaximumVerticalAngle:") retVoid [argCFloat value]

-- | @- minimumHorizontalAngle@
minimumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
minimumHorizontalAngle scnCameraController  =
    sendMsg scnCameraController (mkSelector "minimumHorizontalAngle") retCFloat []

-- | @- setMinimumHorizontalAngle:@
setMinimumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMinimumHorizontalAngle scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setMinimumHorizontalAngle:") retVoid [argCFloat value]

-- | @- maximumHorizontalAngle@
maximumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
maximumHorizontalAngle scnCameraController  =
    sendMsg scnCameraController (mkSelector "maximumHorizontalAngle") retCFloat []

-- | @- setMaximumHorizontalAngle:@
setMaximumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMaximumHorizontalAngle scnCameraController  value =
    sendMsg scnCameraController (mkSelector "setMaximumHorizontalAngle:") retVoid [argCFloat value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @translateInCameraSpaceByX:Y:Z:@
translateInCameraSpaceByX_Y_ZSelector :: Selector
translateInCameraSpaceByX_Y_ZSelector = mkSelector "translateInCameraSpaceByX:Y:Z:"

-- | @Selector@ for @frameNodes:@
frameNodesSelector :: Selector
frameNodesSelector = mkSelector "frameNodes:"

-- | @Selector@ for @rotateByX:Y:@
rotateByX_YSelector :: Selector
rotateByX_YSelector = mkSelector "rotateByX:Y:"

-- | @Selector@ for @rollAroundTarget:@
rollAroundTargetSelector :: Selector
rollAroundTargetSelector = mkSelector "rollAroundTarget:"

-- | @Selector@ for @dollyToTarget:@
dollyToTargetSelector :: Selector
dollyToTargetSelector = mkSelector "dollyToTarget:"

-- | @Selector@ for @clearRoll@
clearRollSelector :: Selector
clearRollSelector = mkSelector "clearRoll"

-- | @Selector@ for @stopInertia@
stopInertiaSelector :: Selector
stopInertiaSelector = mkSelector "stopInertia"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @pointOfView@
pointOfViewSelector :: Selector
pointOfViewSelector = mkSelector "pointOfView"

-- | @Selector@ for @setPointOfView:@
setPointOfViewSelector :: Selector
setPointOfViewSelector = mkSelector "setPointOfView:"

-- | @Selector@ for @interactionMode@
interactionModeSelector :: Selector
interactionModeSelector = mkSelector "interactionMode"

-- | @Selector@ for @setInteractionMode:@
setInteractionModeSelector :: Selector
setInteractionModeSelector = mkSelector "setInteractionMode:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @automaticTarget@
automaticTargetSelector :: Selector
automaticTargetSelector = mkSelector "automaticTarget"

-- | @Selector@ for @setAutomaticTarget:@
setAutomaticTargetSelector :: Selector
setAutomaticTargetSelector = mkSelector "setAutomaticTarget:"

-- | @Selector@ for @worldUp@
worldUpSelector :: Selector
worldUpSelector = mkSelector "worldUp"

-- | @Selector@ for @setWorldUp:@
setWorldUpSelector :: Selector
setWorldUpSelector = mkSelector "setWorldUp:"

-- | @Selector@ for @inertiaEnabled@
inertiaEnabledSelector :: Selector
inertiaEnabledSelector = mkSelector "inertiaEnabled"

-- | @Selector@ for @setInertiaEnabled:@
setInertiaEnabledSelector :: Selector
setInertiaEnabledSelector = mkSelector "setInertiaEnabled:"

-- | @Selector@ for @inertiaFriction@
inertiaFrictionSelector :: Selector
inertiaFrictionSelector = mkSelector "inertiaFriction"

-- | @Selector@ for @setInertiaFriction:@
setInertiaFrictionSelector :: Selector
setInertiaFrictionSelector = mkSelector "setInertiaFriction:"

-- | @Selector@ for @inertiaRunning@
inertiaRunningSelector :: Selector
inertiaRunningSelector = mkSelector "inertiaRunning"

-- | @Selector@ for @minimumVerticalAngle@
minimumVerticalAngleSelector :: Selector
minimumVerticalAngleSelector = mkSelector "minimumVerticalAngle"

-- | @Selector@ for @setMinimumVerticalAngle:@
setMinimumVerticalAngleSelector :: Selector
setMinimumVerticalAngleSelector = mkSelector "setMinimumVerticalAngle:"

-- | @Selector@ for @maximumVerticalAngle@
maximumVerticalAngleSelector :: Selector
maximumVerticalAngleSelector = mkSelector "maximumVerticalAngle"

-- | @Selector@ for @setMaximumVerticalAngle:@
setMaximumVerticalAngleSelector :: Selector
setMaximumVerticalAngleSelector = mkSelector "setMaximumVerticalAngle:"

-- | @Selector@ for @minimumHorizontalAngle@
minimumHorizontalAngleSelector :: Selector
minimumHorizontalAngleSelector = mkSelector "minimumHorizontalAngle"

-- | @Selector@ for @setMinimumHorizontalAngle:@
setMinimumHorizontalAngleSelector :: Selector
setMinimumHorizontalAngleSelector = mkSelector "setMinimumHorizontalAngle:"

-- | @Selector@ for @maximumHorizontalAngle@
maximumHorizontalAngleSelector :: Selector
maximumHorizontalAngleSelector = mkSelector "maximumHorizontalAngle"

-- | @Selector@ for @setMaximumHorizontalAngle:@
setMaximumHorizontalAngleSelector :: Selector
setMaximumHorizontalAngleSelector = mkSelector "setMaximumHorizontalAngle:"

