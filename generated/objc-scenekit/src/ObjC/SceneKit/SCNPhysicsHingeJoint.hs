{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsHingeJoint
--
-- SCNPhysicsHingeJoint makes two bodies to move like they are connected by a hinge. It is for example suitable for doors, chains...
--
-- Generated bindings for @SCNPhysicsHingeJoint@.
module ObjC.SceneKit.SCNPhysicsHingeJoint
  ( SCNPhysicsHingeJoint
  , IsSCNPhysicsHingeJoint(..)
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
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNVector3 -> SCNVector3 -> bodyB -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsHingeJoint)
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB bodyA axisA anchorA bodyB axisB anchorB =
  do
    cls' <- getRequiredClass "SCNPhysicsHingeJoint"
    withObjCPtr bodyA $ \raw_bodyA ->
      withObjCPtr bodyB $ \raw_bodyB ->
        sendClassMsg cls' (mkSelector "jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:") (retPtr retVoid) [argPtr (castPtr raw_bodyA :: Ptr ()), argSCNVector3 axisA, argSCNVector3 anchorA, argPtr (castPtr raw_bodyB :: Ptr ()), argSCNVector3 axisB, argSCNVector3 anchorB] >>= retainedObject . castPtr

-- | @+ jointWithBody:axis:anchor:@
jointWithBody_axis_anchor :: IsSCNPhysicsBody body => body -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsHingeJoint)
jointWithBody_axis_anchor body axis anchor =
  do
    cls' <- getRequiredClass "SCNPhysicsHingeJoint"
    withObjCPtr body $ \raw_body ->
      sendClassMsg cls' (mkSelector "jointWithBody:axis:anchor:") (retPtr retVoid) [argPtr (castPtr raw_body :: Ptr ()), argSCNVector3 axis, argSCNVector3 anchor] >>= retainedObject . castPtr

-- | @- bodyA@
bodyA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsHingeJoint  =
  sendMsg scnPhysicsHingeJoint (mkSelector "bodyA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- axisA@
axisA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
axisA scnPhysicsHingeJoint  =
  sendMsgStret scnPhysicsHingeJoint (mkSelector "axisA") retSCNVector3 []

-- | @- setAxisA:@
setAxisA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAxisA scnPhysicsHingeJoint  value =
  sendMsg scnPhysicsHingeJoint (mkSelector "setAxisA:") retVoid [argSCNVector3 value]

-- | @- anchorA@
anchorA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
anchorA scnPhysicsHingeJoint  =
  sendMsgStret scnPhysicsHingeJoint (mkSelector "anchorA") retSCNVector3 []

-- | @- setAnchorA:@
setAnchorA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAnchorA scnPhysicsHingeJoint  value =
  sendMsg scnPhysicsHingeJoint (mkSelector "setAnchorA:") retVoid [argSCNVector3 value]

-- | @- bodyB@
bodyB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsHingeJoint  =
  sendMsg scnPhysicsHingeJoint (mkSelector "bodyB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- axisB@
axisB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
axisB scnPhysicsHingeJoint  =
  sendMsgStret scnPhysicsHingeJoint (mkSelector "axisB") retSCNVector3 []

-- | @- setAxisB:@
setAxisB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAxisB scnPhysicsHingeJoint  value =
  sendMsg scnPhysicsHingeJoint (mkSelector "setAxisB:") retVoid [argSCNVector3 value]

-- | @- anchorB@
anchorB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
anchorB scnPhysicsHingeJoint  =
  sendMsgStret scnPhysicsHingeJoint (mkSelector "anchorB") retSCNVector3 []

-- | @- setAnchorB:@
setAnchorB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAnchorB scnPhysicsHingeJoint  value =
  sendMsg scnPhysicsHingeJoint (mkSelector "setAnchorB:") retVoid [argSCNVector3 value]

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

