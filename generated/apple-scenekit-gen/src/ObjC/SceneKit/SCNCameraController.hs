{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , automaticTargetSelector
  , clearRollSelector
  , delegateSelector
  , dollyToTargetSelector
  , frameNodesSelector
  , inertiaEnabledSelector
  , inertiaFrictionSelector
  , inertiaRunningSelector
  , interactionModeSelector
  , maximumHorizontalAngleSelector
  , maximumVerticalAngleSelector
  , minimumHorizontalAngleSelector
  , minimumVerticalAngleSelector
  , pointOfViewSelector
  , rollAroundTargetSelector
  , rotateByX_YSelector
  , setAutomaticTargetSelector
  , setDelegateSelector
  , setInertiaEnabledSelector
  , setInertiaFrictionSelector
  , setInteractionModeSelector
  , setMaximumHorizontalAngleSelector
  , setMaximumVerticalAngleSelector
  , setMinimumHorizontalAngleSelector
  , setMinimumVerticalAngleSelector
  , setPointOfViewSelector
  , setTargetSelector
  , setWorldUpSelector
  , stopInertiaSelector
  , targetSelector
  , translateInCameraSpaceByX_Y_ZSelector
  , worldUpSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- translateInCameraSpaceByX:Y:Z:@
translateInCameraSpaceByX_Y_Z :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> CFloat -> CFloat -> IO ()
translateInCameraSpaceByX_Y_Z scnCameraController deltaX deltaY deltaZ =
  sendMessage scnCameraController translateInCameraSpaceByX_Y_ZSelector deltaX deltaY deltaZ

-- | @- frameNodes:@
frameNodes :: (IsSCNCameraController scnCameraController, IsNSArray nodes) => scnCameraController -> nodes -> IO ()
frameNodes scnCameraController nodes =
  sendMessage scnCameraController frameNodesSelector (toNSArray nodes)

-- | @- rotateByX:Y:@
rotateByX_Y :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> CFloat -> IO ()
rotateByX_Y scnCameraController deltaX deltaY =
  sendMessage scnCameraController rotateByX_YSelector deltaX deltaY

-- | @- rollAroundTarget:@
rollAroundTarget :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
rollAroundTarget scnCameraController delta =
  sendMessage scnCameraController rollAroundTargetSelector delta

-- | @- dollyToTarget:@
dollyToTarget :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
dollyToTarget scnCameraController delta =
  sendMessage scnCameraController dollyToTargetSelector delta

-- | @- clearRoll@
clearRoll :: IsSCNCameraController scnCameraController => scnCameraController -> IO ()
clearRoll scnCameraController =
  sendMessage scnCameraController clearRollSelector

-- | @- stopInertia@
stopInertia :: IsSCNCameraController scnCameraController => scnCameraController -> IO ()
stopInertia scnCameraController =
  sendMessage scnCameraController stopInertiaSelector

-- | @- delegate@
delegate :: IsSCNCameraController scnCameraController => scnCameraController -> IO RawId
delegate scnCameraController =
  sendMessage scnCameraController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSCNCameraController scnCameraController => scnCameraController -> RawId -> IO ()
setDelegate scnCameraController value =
  sendMessage scnCameraController setDelegateSelector value

-- | @- pointOfView@
pointOfView :: IsSCNCameraController scnCameraController => scnCameraController -> IO (Id SCNNode)
pointOfView scnCameraController =
  sendMessage scnCameraController pointOfViewSelector

-- | @- setPointOfView:@
setPointOfView :: (IsSCNCameraController scnCameraController, IsSCNNode value) => scnCameraController -> value -> IO ()
setPointOfView scnCameraController value =
  sendMessage scnCameraController setPointOfViewSelector (toSCNNode value)

-- | @- interactionMode@
interactionMode :: IsSCNCameraController scnCameraController => scnCameraController -> IO SCNInteractionMode
interactionMode scnCameraController =
  sendMessage scnCameraController interactionModeSelector

-- | @- setInteractionMode:@
setInteractionMode :: IsSCNCameraController scnCameraController => scnCameraController -> SCNInteractionMode -> IO ()
setInteractionMode scnCameraController value =
  sendMessage scnCameraController setInteractionModeSelector value

-- | @- target@
target :: IsSCNCameraController scnCameraController => scnCameraController -> IO SCNVector3
target scnCameraController =
  sendMessage scnCameraController targetSelector

-- | @- setTarget:@
setTarget :: IsSCNCameraController scnCameraController => scnCameraController -> SCNVector3 -> IO ()
setTarget scnCameraController value =
  sendMessage scnCameraController setTargetSelector value

-- | @- automaticTarget@
automaticTarget :: IsSCNCameraController scnCameraController => scnCameraController -> IO Bool
automaticTarget scnCameraController =
  sendMessage scnCameraController automaticTargetSelector

-- | @- setAutomaticTarget:@
setAutomaticTarget :: IsSCNCameraController scnCameraController => scnCameraController -> Bool -> IO ()
setAutomaticTarget scnCameraController value =
  sendMessage scnCameraController setAutomaticTargetSelector value

-- | @- worldUp@
worldUp :: IsSCNCameraController scnCameraController => scnCameraController -> IO SCNVector3
worldUp scnCameraController =
  sendMessage scnCameraController worldUpSelector

-- | @- setWorldUp:@
setWorldUp :: IsSCNCameraController scnCameraController => scnCameraController -> SCNVector3 -> IO ()
setWorldUp scnCameraController value =
  sendMessage scnCameraController setWorldUpSelector value

-- | @- inertiaEnabled@
inertiaEnabled :: IsSCNCameraController scnCameraController => scnCameraController -> IO Bool
inertiaEnabled scnCameraController =
  sendMessage scnCameraController inertiaEnabledSelector

-- | @- setInertiaEnabled:@
setInertiaEnabled :: IsSCNCameraController scnCameraController => scnCameraController -> Bool -> IO ()
setInertiaEnabled scnCameraController value =
  sendMessage scnCameraController setInertiaEnabledSelector value

-- | @- inertiaFriction@
inertiaFriction :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
inertiaFriction scnCameraController =
  sendMessage scnCameraController inertiaFrictionSelector

-- | @- setInertiaFriction:@
setInertiaFriction :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setInertiaFriction scnCameraController value =
  sendMessage scnCameraController setInertiaFrictionSelector value

-- | @- inertiaRunning@
inertiaRunning :: IsSCNCameraController scnCameraController => scnCameraController -> IO Bool
inertiaRunning scnCameraController =
  sendMessage scnCameraController inertiaRunningSelector

-- | @- minimumVerticalAngle@
minimumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
minimumVerticalAngle scnCameraController =
  sendMessage scnCameraController minimumVerticalAngleSelector

-- | @- setMinimumVerticalAngle:@
setMinimumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMinimumVerticalAngle scnCameraController value =
  sendMessage scnCameraController setMinimumVerticalAngleSelector value

-- | @- maximumVerticalAngle@
maximumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
maximumVerticalAngle scnCameraController =
  sendMessage scnCameraController maximumVerticalAngleSelector

-- | @- setMaximumVerticalAngle:@
setMaximumVerticalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMaximumVerticalAngle scnCameraController value =
  sendMessage scnCameraController setMaximumVerticalAngleSelector value

-- | @- minimumHorizontalAngle@
minimumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
minimumHorizontalAngle scnCameraController =
  sendMessage scnCameraController minimumHorizontalAngleSelector

-- | @- setMinimumHorizontalAngle:@
setMinimumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMinimumHorizontalAngle scnCameraController value =
  sendMessage scnCameraController setMinimumHorizontalAngleSelector value

-- | @- maximumHorizontalAngle@
maximumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> IO CFloat
maximumHorizontalAngle scnCameraController =
  sendMessage scnCameraController maximumHorizontalAngleSelector

-- | @- setMaximumHorizontalAngle:@
setMaximumHorizontalAngle :: IsSCNCameraController scnCameraController => scnCameraController -> CFloat -> IO ()
setMaximumHorizontalAngle scnCameraController value =
  sendMessage scnCameraController setMaximumHorizontalAngleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @translateInCameraSpaceByX:Y:Z:@
translateInCameraSpaceByX_Y_ZSelector :: Selector '[CFloat, CFloat, CFloat] ()
translateInCameraSpaceByX_Y_ZSelector = mkSelector "translateInCameraSpaceByX:Y:Z:"

-- | @Selector@ for @frameNodes:@
frameNodesSelector :: Selector '[Id NSArray] ()
frameNodesSelector = mkSelector "frameNodes:"

-- | @Selector@ for @rotateByX:Y:@
rotateByX_YSelector :: Selector '[CFloat, CFloat] ()
rotateByX_YSelector = mkSelector "rotateByX:Y:"

-- | @Selector@ for @rollAroundTarget:@
rollAroundTargetSelector :: Selector '[CFloat] ()
rollAroundTargetSelector = mkSelector "rollAroundTarget:"

-- | @Selector@ for @dollyToTarget:@
dollyToTargetSelector :: Selector '[CFloat] ()
dollyToTargetSelector = mkSelector "dollyToTarget:"

-- | @Selector@ for @clearRoll@
clearRollSelector :: Selector '[] ()
clearRollSelector = mkSelector "clearRoll"

-- | @Selector@ for @stopInertia@
stopInertiaSelector :: Selector '[] ()
stopInertiaSelector = mkSelector "stopInertia"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @pointOfView@
pointOfViewSelector :: Selector '[] (Id SCNNode)
pointOfViewSelector = mkSelector "pointOfView"

-- | @Selector@ for @setPointOfView:@
setPointOfViewSelector :: Selector '[Id SCNNode] ()
setPointOfViewSelector = mkSelector "setPointOfView:"

-- | @Selector@ for @interactionMode@
interactionModeSelector :: Selector '[] SCNInteractionMode
interactionModeSelector = mkSelector "interactionMode"

-- | @Selector@ for @setInteractionMode:@
setInteractionModeSelector :: Selector '[SCNInteractionMode] ()
setInteractionModeSelector = mkSelector "setInteractionMode:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] SCNVector3
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[SCNVector3] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @automaticTarget@
automaticTargetSelector :: Selector '[] Bool
automaticTargetSelector = mkSelector "automaticTarget"

-- | @Selector@ for @setAutomaticTarget:@
setAutomaticTargetSelector :: Selector '[Bool] ()
setAutomaticTargetSelector = mkSelector "setAutomaticTarget:"

-- | @Selector@ for @worldUp@
worldUpSelector :: Selector '[] SCNVector3
worldUpSelector = mkSelector "worldUp"

-- | @Selector@ for @setWorldUp:@
setWorldUpSelector :: Selector '[SCNVector3] ()
setWorldUpSelector = mkSelector "setWorldUp:"

-- | @Selector@ for @inertiaEnabled@
inertiaEnabledSelector :: Selector '[] Bool
inertiaEnabledSelector = mkSelector "inertiaEnabled"

-- | @Selector@ for @setInertiaEnabled:@
setInertiaEnabledSelector :: Selector '[Bool] ()
setInertiaEnabledSelector = mkSelector "setInertiaEnabled:"

-- | @Selector@ for @inertiaFriction@
inertiaFrictionSelector :: Selector '[] CFloat
inertiaFrictionSelector = mkSelector "inertiaFriction"

-- | @Selector@ for @setInertiaFriction:@
setInertiaFrictionSelector :: Selector '[CFloat] ()
setInertiaFrictionSelector = mkSelector "setInertiaFriction:"

-- | @Selector@ for @inertiaRunning@
inertiaRunningSelector :: Selector '[] Bool
inertiaRunningSelector = mkSelector "inertiaRunning"

-- | @Selector@ for @minimumVerticalAngle@
minimumVerticalAngleSelector :: Selector '[] CFloat
minimumVerticalAngleSelector = mkSelector "minimumVerticalAngle"

-- | @Selector@ for @setMinimumVerticalAngle:@
setMinimumVerticalAngleSelector :: Selector '[CFloat] ()
setMinimumVerticalAngleSelector = mkSelector "setMinimumVerticalAngle:"

-- | @Selector@ for @maximumVerticalAngle@
maximumVerticalAngleSelector :: Selector '[] CFloat
maximumVerticalAngleSelector = mkSelector "maximumVerticalAngle"

-- | @Selector@ for @setMaximumVerticalAngle:@
setMaximumVerticalAngleSelector :: Selector '[CFloat] ()
setMaximumVerticalAngleSelector = mkSelector "setMaximumVerticalAngle:"

-- | @Selector@ for @minimumHorizontalAngle@
minimumHorizontalAngleSelector :: Selector '[] CFloat
minimumHorizontalAngleSelector = mkSelector "minimumHorizontalAngle"

-- | @Selector@ for @setMinimumHorizontalAngle:@
setMinimumHorizontalAngleSelector :: Selector '[CFloat] ()
setMinimumHorizontalAngleSelector = mkSelector "setMinimumHorizontalAngle:"

-- | @Selector@ for @maximumHorizontalAngle@
maximumHorizontalAngleSelector :: Selector '[] CFloat
maximumHorizontalAngleSelector = mkSelector "maximumHorizontalAngle"

-- | @Selector@ for @setMaximumHorizontalAngle:@
setMaximumHorizontalAngleSelector :: Selector '[CFloat] ()
setMaximumHorizontalAngleSelector = mkSelector "setMaximumHorizontalAngle:"

