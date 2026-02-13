{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsSliderJoint
--
-- SCNPhysicsSliderJoint provides a linear sliding joint between two bodies.
--
-- Generated bindings for @SCNPhysicsSliderJoint@.
module ObjC.SceneKit.SCNPhysicsSliderJoint
  ( SCNPhysicsSliderJoint
  , IsSCNPhysicsSliderJoint(..)
  , jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB
  , jointWithBody_axis_anchor
  , bodyA
  , axisA
  , setAxisA
  , anchorA
  , setAnchorA
  , bodyB
  , axisB
  , setAxisB
  , anchorB
  , setAnchorB
  , minimumLinearLimit
  , setMinimumLinearLimit
  , maximumLinearLimit
  , setMaximumLinearLimit
  , minimumAngularLimit
  , setMinimumAngularLimit
  , maximumAngularLimit
  , setMaximumAngularLimit
  , motorTargetLinearVelocity
  , setMotorTargetLinearVelocity
  , motorMaximumForce
  , setMotorMaximumForce
  , motorTargetAngularVelocity
  , setMotorTargetAngularVelocity
  , motorMaximumTorque
  , setMotorMaximumTorque
  , anchorASelector
  , anchorBSelector
  , axisASelector
  , axisBSelector
  , bodyASelector
  , bodyBSelector
  , jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector
  , jointWithBody_axis_anchorSelector
  , maximumAngularLimitSelector
  , maximumLinearLimitSelector
  , minimumAngularLimitSelector
  , minimumLinearLimitSelector
  , motorMaximumForceSelector
  , motorMaximumTorqueSelector
  , motorTargetAngularVelocitySelector
  , motorTargetLinearVelocitySelector
  , setAnchorASelector
  , setAnchorBSelector
  , setAxisASelector
  , setAxisBSelector
  , setMaximumAngularLimitSelector
  , setMaximumLinearLimitSelector
  , setMinimumAngularLimitSelector
  , setMinimumLinearLimitSelector
  , setMotorMaximumForceSelector
  , setMotorMaximumTorqueSelector
  , setMotorTargetAngularVelocitySelector
  , setMotorTargetLinearVelocitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:@
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNVector3 -> SCNVector3 -> bodyB -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsSliderJoint)
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB bodyA axisA anchorA bodyB axisB anchorB =
  do
    cls' <- getRequiredClass "SCNPhysicsSliderJoint"
    sendClassMessage cls' jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector (toSCNPhysicsBody bodyA) axisA anchorA (toSCNPhysicsBody bodyB) axisB anchorB

-- | @+ jointWithBody:axis:anchor:@
jointWithBody_axis_anchor :: IsSCNPhysicsBody body => body -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsSliderJoint)
jointWithBody_axis_anchor body axis anchor =
  do
    cls' <- getRequiredClass "SCNPhysicsSliderJoint"
    sendClassMessage cls' jointWithBody_axis_anchorSelector (toSCNPhysicsBody body) axis anchor

-- | @- bodyA@
bodyA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint bodyASelector

-- | @- axisA@
axisA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
axisA scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint axisASelector

-- | @- setAxisA:@
setAxisA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAxisA scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setAxisASelector value

-- | @- anchorA@
anchorA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
anchorA scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint anchorASelector

-- | @- setAnchorA:@
setAnchorA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAnchorA scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setAnchorASelector value

-- | @- bodyB@
bodyB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint bodyBSelector

-- | @- axisB@
axisB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
axisB scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint axisBSelector

-- | @- setAxisB:@
setAxisB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAxisB scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setAxisBSelector value

-- | @- anchorB@
anchorB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
anchorB scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint anchorBSelector

-- | @- setAnchorB:@
setAnchorB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAnchorB scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setAnchorBSelector value

-- | @- minimumLinearLimit@
minimumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
minimumLinearLimit scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint minimumLinearLimitSelector

-- | @- setMinimumLinearLimit:@
setMinimumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMinimumLinearLimit scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMinimumLinearLimitSelector value

-- | @- maximumLinearLimit@
maximumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
maximumLinearLimit scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint maximumLinearLimitSelector

-- | @- setMaximumLinearLimit:@
setMaximumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMaximumLinearLimit scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMaximumLinearLimitSelector value

-- | @- minimumAngularLimit@
minimumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
minimumAngularLimit scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint minimumAngularLimitSelector

-- | @- setMinimumAngularLimit:@
setMinimumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMinimumAngularLimit scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMinimumAngularLimitSelector value

-- | @- maximumAngularLimit@
maximumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
maximumAngularLimit scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint maximumAngularLimitSelector

-- | @- setMaximumAngularLimit:@
setMaximumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMaximumAngularLimit scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMaximumAngularLimitSelector value

-- | @- motorTargetLinearVelocity@
motorTargetLinearVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorTargetLinearVelocity scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint motorTargetLinearVelocitySelector

-- | @- setMotorTargetLinearVelocity:@
setMotorTargetLinearVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorTargetLinearVelocity scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMotorTargetLinearVelocitySelector value

-- | @- motorMaximumForce@
motorMaximumForce :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorMaximumForce scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint motorMaximumForceSelector

-- | @- setMotorMaximumForce:@
setMotorMaximumForce :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorMaximumForce scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMotorMaximumForceSelector value

-- | @- motorTargetAngularVelocity@
motorTargetAngularVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorTargetAngularVelocity scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint motorTargetAngularVelocitySelector

-- | @- setMotorTargetAngularVelocity:@
setMotorTargetAngularVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorTargetAngularVelocity scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMotorTargetAngularVelocitySelector value

-- | @- motorMaximumTorque@
motorMaximumTorque :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorMaximumTorque scnPhysicsSliderJoint =
  sendMessage scnPhysicsSliderJoint motorMaximumTorqueSelector

-- | @- setMotorMaximumTorque:@
setMotorMaximumTorque :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorMaximumTorque scnPhysicsSliderJoint value =
  sendMessage scnPhysicsSliderJoint setMotorMaximumTorqueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:@
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector :: Selector '[Id SCNPhysicsBody, SCNVector3, SCNVector3, Id SCNPhysicsBody, SCNVector3, SCNVector3] (Id SCNPhysicsSliderJoint)
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector = mkSelector "jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:"

-- | @Selector@ for @jointWithBody:axis:anchor:@
jointWithBody_axis_anchorSelector :: Selector '[Id SCNPhysicsBody, SCNVector3, SCNVector3] (Id SCNPhysicsSliderJoint)
jointWithBody_axis_anchorSelector = mkSelector "jointWithBody:axis:anchor:"

-- | @Selector@ for @bodyA@
bodyASelector :: Selector '[] (Id SCNPhysicsBody)
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @axisA@
axisASelector :: Selector '[] SCNVector3
axisASelector = mkSelector "axisA"

-- | @Selector@ for @setAxisA:@
setAxisASelector :: Selector '[SCNVector3] ()
setAxisASelector = mkSelector "setAxisA:"

-- | @Selector@ for @anchorA@
anchorASelector :: Selector '[] SCNVector3
anchorASelector = mkSelector "anchorA"

-- | @Selector@ for @setAnchorA:@
setAnchorASelector :: Selector '[SCNVector3] ()
setAnchorASelector = mkSelector "setAnchorA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector '[] (Id SCNPhysicsBody)
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @axisB@
axisBSelector :: Selector '[] SCNVector3
axisBSelector = mkSelector "axisB"

-- | @Selector@ for @setAxisB:@
setAxisBSelector :: Selector '[SCNVector3] ()
setAxisBSelector = mkSelector "setAxisB:"

-- | @Selector@ for @anchorB@
anchorBSelector :: Selector '[] SCNVector3
anchorBSelector = mkSelector "anchorB"

-- | @Selector@ for @setAnchorB:@
setAnchorBSelector :: Selector '[SCNVector3] ()
setAnchorBSelector = mkSelector "setAnchorB:"

-- | @Selector@ for @minimumLinearLimit@
minimumLinearLimitSelector :: Selector '[] CDouble
minimumLinearLimitSelector = mkSelector "minimumLinearLimit"

-- | @Selector@ for @setMinimumLinearLimit:@
setMinimumLinearLimitSelector :: Selector '[CDouble] ()
setMinimumLinearLimitSelector = mkSelector "setMinimumLinearLimit:"

-- | @Selector@ for @maximumLinearLimit@
maximumLinearLimitSelector :: Selector '[] CDouble
maximumLinearLimitSelector = mkSelector "maximumLinearLimit"

-- | @Selector@ for @setMaximumLinearLimit:@
setMaximumLinearLimitSelector :: Selector '[CDouble] ()
setMaximumLinearLimitSelector = mkSelector "setMaximumLinearLimit:"

-- | @Selector@ for @minimumAngularLimit@
minimumAngularLimitSelector :: Selector '[] CDouble
minimumAngularLimitSelector = mkSelector "minimumAngularLimit"

-- | @Selector@ for @setMinimumAngularLimit:@
setMinimumAngularLimitSelector :: Selector '[CDouble] ()
setMinimumAngularLimitSelector = mkSelector "setMinimumAngularLimit:"

-- | @Selector@ for @maximumAngularLimit@
maximumAngularLimitSelector :: Selector '[] CDouble
maximumAngularLimitSelector = mkSelector "maximumAngularLimit"

-- | @Selector@ for @setMaximumAngularLimit:@
setMaximumAngularLimitSelector :: Selector '[CDouble] ()
setMaximumAngularLimitSelector = mkSelector "setMaximumAngularLimit:"

-- | @Selector@ for @motorTargetLinearVelocity@
motorTargetLinearVelocitySelector :: Selector '[] CDouble
motorTargetLinearVelocitySelector = mkSelector "motorTargetLinearVelocity"

-- | @Selector@ for @setMotorTargetLinearVelocity:@
setMotorTargetLinearVelocitySelector :: Selector '[CDouble] ()
setMotorTargetLinearVelocitySelector = mkSelector "setMotorTargetLinearVelocity:"

-- | @Selector@ for @motorMaximumForce@
motorMaximumForceSelector :: Selector '[] CDouble
motorMaximumForceSelector = mkSelector "motorMaximumForce"

-- | @Selector@ for @setMotorMaximumForce:@
setMotorMaximumForceSelector :: Selector '[CDouble] ()
setMotorMaximumForceSelector = mkSelector "setMotorMaximumForce:"

-- | @Selector@ for @motorTargetAngularVelocity@
motorTargetAngularVelocitySelector :: Selector '[] CDouble
motorTargetAngularVelocitySelector = mkSelector "motorTargetAngularVelocity"

-- | @Selector@ for @setMotorTargetAngularVelocity:@
setMotorTargetAngularVelocitySelector :: Selector '[CDouble] ()
setMotorTargetAngularVelocitySelector = mkSelector "setMotorTargetAngularVelocity:"

-- | @Selector@ for @motorMaximumTorque@
motorMaximumTorqueSelector :: Selector '[] CDouble
motorMaximumTorqueSelector = mkSelector "motorMaximumTorque"

-- | @Selector@ for @setMotorMaximumTorque:@
setMotorMaximumTorqueSelector :: Selector '[CDouble] ()
setMotorMaximumTorqueSelector = mkSelector "setMotorMaximumTorque:"

