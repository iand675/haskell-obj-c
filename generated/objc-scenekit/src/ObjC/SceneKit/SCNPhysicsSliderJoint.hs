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
  , jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector
  , jointWithBody_axis_anchorSelector
  , bodyASelector
  , axisASelector
  , setAxisASelector
  , anchorASelector
  , setAnchorASelector
  , bodyBSelector
  , axisBSelector
  , setAxisBSelector
  , anchorBSelector
  , setAnchorBSelector
  , minimumLinearLimitSelector
  , setMinimumLinearLimitSelector
  , maximumLinearLimitSelector
  , setMaximumLinearLimitSelector
  , minimumAngularLimitSelector
  , setMinimumAngularLimitSelector
  , maximumAngularLimitSelector
  , setMaximumAngularLimitSelector
  , motorTargetLinearVelocitySelector
  , setMotorTargetLinearVelocitySelector
  , motorMaximumForceSelector
  , setMotorMaximumForceSelector
  , motorTargetAngularVelocitySelector
  , setMotorTargetAngularVelocitySelector
  , motorMaximumTorqueSelector
  , setMotorMaximumTorqueSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:@
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNVector3 -> SCNVector3 -> bodyB -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsSliderJoint)
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB bodyA axisA anchorA bodyB axisB anchorB =
  do
    cls' <- getRequiredClass "SCNPhysicsSliderJoint"
    withObjCPtr bodyA $ \raw_bodyA ->
      withObjCPtr bodyB $ \raw_bodyB ->
        sendClassMsg cls' (mkSelector "jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:") (retPtr retVoid) [argPtr (castPtr raw_bodyA :: Ptr ()), argSCNVector3 axisA, argSCNVector3 anchorA, argPtr (castPtr raw_bodyB :: Ptr ()), argSCNVector3 axisB, argSCNVector3 anchorB] >>= retainedObject . castPtr

-- | @+ jointWithBody:axis:anchor:@
jointWithBody_axis_anchor :: IsSCNPhysicsBody body => body -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsSliderJoint)
jointWithBody_axis_anchor body axis anchor =
  do
    cls' <- getRequiredClass "SCNPhysicsSliderJoint"
    withObjCPtr body $ \raw_body ->
      sendClassMsg cls' (mkSelector "jointWithBody:axis:anchor:") (retPtr retVoid) [argPtr (castPtr raw_body :: Ptr ()), argSCNVector3 axis, argSCNVector3 anchor] >>= retainedObject . castPtr

-- | @- bodyA@
bodyA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "bodyA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- axisA@
axisA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
axisA scnPhysicsSliderJoint  =
  sendMsgStret scnPhysicsSliderJoint (mkSelector "axisA") retSCNVector3 []

-- | @- setAxisA:@
setAxisA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAxisA scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setAxisA:") retVoid [argSCNVector3 value]

-- | @- anchorA@
anchorA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
anchorA scnPhysicsSliderJoint  =
  sendMsgStret scnPhysicsSliderJoint (mkSelector "anchorA") retSCNVector3 []

-- | @- setAnchorA:@
setAnchorA :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAnchorA scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setAnchorA:") retVoid [argSCNVector3 value]

-- | @- bodyB@
bodyB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "bodyB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- axisB@
axisB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
axisB scnPhysicsSliderJoint  =
  sendMsgStret scnPhysicsSliderJoint (mkSelector "axisB") retSCNVector3 []

-- | @- setAxisB:@
setAxisB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAxisB scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setAxisB:") retVoid [argSCNVector3 value]

-- | @- anchorB@
anchorB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO SCNVector3
anchorB scnPhysicsSliderJoint  =
  sendMsgStret scnPhysicsSliderJoint (mkSelector "anchorB") retSCNVector3 []

-- | @- setAnchorB:@
setAnchorB :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> SCNVector3 -> IO ()
setAnchorB scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setAnchorB:") retVoid [argSCNVector3 value]

-- | @- minimumLinearLimit@
minimumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
minimumLinearLimit scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "minimumLinearLimit") retCDouble []

-- | @- setMinimumLinearLimit:@
setMinimumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMinimumLinearLimit scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMinimumLinearLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumLinearLimit@
maximumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
maximumLinearLimit scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "maximumLinearLimit") retCDouble []

-- | @- setMaximumLinearLimit:@
setMaximumLinearLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMaximumLinearLimit scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMaximumLinearLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- minimumAngularLimit@
minimumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
minimumAngularLimit scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "minimumAngularLimit") retCDouble []

-- | @- setMinimumAngularLimit:@
setMinimumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMinimumAngularLimit scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMinimumAngularLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumAngularLimit@
maximumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
maximumAngularLimit scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "maximumAngularLimit") retCDouble []

-- | @- setMaximumAngularLimit:@
setMaximumAngularLimit :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMaximumAngularLimit scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMaximumAngularLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- motorTargetLinearVelocity@
motorTargetLinearVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorTargetLinearVelocity scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "motorTargetLinearVelocity") retCDouble []

-- | @- setMotorTargetLinearVelocity:@
setMotorTargetLinearVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorTargetLinearVelocity scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMotorTargetLinearVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | @- motorMaximumForce@
motorMaximumForce :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorMaximumForce scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "motorMaximumForce") retCDouble []

-- | @- setMotorMaximumForce:@
setMotorMaximumForce :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorMaximumForce scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMotorMaximumForce:") retVoid [argCDouble (fromIntegral value)]

-- | @- motorTargetAngularVelocity@
motorTargetAngularVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorTargetAngularVelocity scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "motorTargetAngularVelocity") retCDouble []

-- | @- setMotorTargetAngularVelocity:@
setMotorTargetAngularVelocity :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorTargetAngularVelocity scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMotorTargetAngularVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | @- motorMaximumTorque@
motorMaximumTorque :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> IO CDouble
motorMaximumTorque scnPhysicsSliderJoint  =
  sendMsg scnPhysicsSliderJoint (mkSelector "motorMaximumTorque") retCDouble []

-- | @- setMotorMaximumTorque:@
setMotorMaximumTorque :: IsSCNPhysicsSliderJoint scnPhysicsSliderJoint => scnPhysicsSliderJoint -> CDouble -> IO ()
setMotorMaximumTorque scnPhysicsSliderJoint  value =
  sendMsg scnPhysicsSliderJoint (mkSelector "setMotorMaximumTorque:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:@
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector :: Selector
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector = mkSelector "jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:"

-- | @Selector@ for @jointWithBody:axis:anchor:@
jointWithBody_axis_anchorSelector :: Selector
jointWithBody_axis_anchorSelector = mkSelector "jointWithBody:axis:anchor:"

-- | @Selector@ for @bodyA@
bodyASelector :: Selector
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @axisA@
axisASelector :: Selector
axisASelector = mkSelector "axisA"

-- | @Selector@ for @setAxisA:@
setAxisASelector :: Selector
setAxisASelector = mkSelector "setAxisA:"

-- | @Selector@ for @anchorA@
anchorASelector :: Selector
anchorASelector = mkSelector "anchorA"

-- | @Selector@ for @setAnchorA:@
setAnchorASelector :: Selector
setAnchorASelector = mkSelector "setAnchorA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @axisB@
axisBSelector :: Selector
axisBSelector = mkSelector "axisB"

-- | @Selector@ for @setAxisB:@
setAxisBSelector :: Selector
setAxisBSelector = mkSelector "setAxisB:"

-- | @Selector@ for @anchorB@
anchorBSelector :: Selector
anchorBSelector = mkSelector "anchorB"

-- | @Selector@ for @setAnchorB:@
setAnchorBSelector :: Selector
setAnchorBSelector = mkSelector "setAnchorB:"

-- | @Selector@ for @minimumLinearLimit@
minimumLinearLimitSelector :: Selector
minimumLinearLimitSelector = mkSelector "minimumLinearLimit"

-- | @Selector@ for @setMinimumLinearLimit:@
setMinimumLinearLimitSelector :: Selector
setMinimumLinearLimitSelector = mkSelector "setMinimumLinearLimit:"

-- | @Selector@ for @maximumLinearLimit@
maximumLinearLimitSelector :: Selector
maximumLinearLimitSelector = mkSelector "maximumLinearLimit"

-- | @Selector@ for @setMaximumLinearLimit:@
setMaximumLinearLimitSelector :: Selector
setMaximumLinearLimitSelector = mkSelector "setMaximumLinearLimit:"

-- | @Selector@ for @minimumAngularLimit@
minimumAngularLimitSelector :: Selector
minimumAngularLimitSelector = mkSelector "minimumAngularLimit"

-- | @Selector@ for @setMinimumAngularLimit:@
setMinimumAngularLimitSelector :: Selector
setMinimumAngularLimitSelector = mkSelector "setMinimumAngularLimit:"

-- | @Selector@ for @maximumAngularLimit@
maximumAngularLimitSelector :: Selector
maximumAngularLimitSelector = mkSelector "maximumAngularLimit"

-- | @Selector@ for @setMaximumAngularLimit:@
setMaximumAngularLimitSelector :: Selector
setMaximumAngularLimitSelector = mkSelector "setMaximumAngularLimit:"

-- | @Selector@ for @motorTargetLinearVelocity@
motorTargetLinearVelocitySelector :: Selector
motorTargetLinearVelocitySelector = mkSelector "motorTargetLinearVelocity"

-- | @Selector@ for @setMotorTargetLinearVelocity:@
setMotorTargetLinearVelocitySelector :: Selector
setMotorTargetLinearVelocitySelector = mkSelector "setMotorTargetLinearVelocity:"

-- | @Selector@ for @motorMaximumForce@
motorMaximumForceSelector :: Selector
motorMaximumForceSelector = mkSelector "motorMaximumForce"

-- | @Selector@ for @setMotorMaximumForce:@
setMotorMaximumForceSelector :: Selector
setMotorMaximumForceSelector = mkSelector "setMotorMaximumForce:"

-- | @Selector@ for @motorTargetAngularVelocity@
motorTargetAngularVelocitySelector :: Selector
motorTargetAngularVelocitySelector = mkSelector "motorTargetAngularVelocity"

-- | @Selector@ for @setMotorTargetAngularVelocity:@
setMotorTargetAngularVelocitySelector :: Selector
setMotorTargetAngularVelocitySelector = mkSelector "setMotorTargetAngularVelocity:"

-- | @Selector@ for @motorMaximumTorque@
motorMaximumTorqueSelector :: Selector
motorMaximumTorqueSelector = mkSelector "motorMaximumTorque"

-- | @Selector@ for @setMotorMaximumTorque:@
setMotorMaximumTorqueSelector :: Selector
setMotorMaximumTorqueSelector = mkSelector "setMotorMaximumTorque:"

