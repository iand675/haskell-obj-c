{-# LANGUAGE DataKinds #-}
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
  , anchorASelector
  , anchorBSelector
  , axisASelector
  , axisBSelector
  , bodyASelector
  , bodyBSelector
  , jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector
  , jointWithBody_axis_anchorSelector
  , setAnchorASelector
  , setAnchorBSelector
  , setAxisASelector
  , setAxisBSelector


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
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNVector3 -> SCNVector3 -> bodyB -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsHingeJoint)
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorB bodyA axisA anchorA bodyB axisB anchorB =
  do
    cls' <- getRequiredClass "SCNPhysicsHingeJoint"
    sendClassMessage cls' jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector (toSCNPhysicsBody bodyA) axisA anchorA (toSCNPhysicsBody bodyB) axisB anchorB

-- | @+ jointWithBody:axis:anchor:@
jointWithBody_axis_anchor :: IsSCNPhysicsBody body => body -> SCNVector3 -> SCNVector3 -> IO (Id SCNPhysicsHingeJoint)
jointWithBody_axis_anchor body axis anchor =
  do
    cls' <- getRequiredClass "SCNPhysicsHingeJoint"
    sendClassMessage cls' jointWithBody_axis_anchorSelector (toSCNPhysicsBody body) axis anchor

-- | @- bodyA@
bodyA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsHingeJoint =
  sendMessage scnPhysicsHingeJoint bodyASelector

-- | @- axisA@
axisA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
axisA scnPhysicsHingeJoint =
  sendMessage scnPhysicsHingeJoint axisASelector

-- | @- setAxisA:@
setAxisA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAxisA scnPhysicsHingeJoint value =
  sendMessage scnPhysicsHingeJoint setAxisASelector value

-- | @- anchorA@
anchorA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
anchorA scnPhysicsHingeJoint =
  sendMessage scnPhysicsHingeJoint anchorASelector

-- | @- setAnchorA:@
setAnchorA :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAnchorA scnPhysicsHingeJoint value =
  sendMessage scnPhysicsHingeJoint setAnchorASelector value

-- | @- bodyB@
bodyB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsHingeJoint =
  sendMessage scnPhysicsHingeJoint bodyBSelector

-- | @- axisB@
axisB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
axisB scnPhysicsHingeJoint =
  sendMessage scnPhysicsHingeJoint axisBSelector

-- | @- setAxisB:@
setAxisB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAxisB scnPhysicsHingeJoint value =
  sendMessage scnPhysicsHingeJoint setAxisBSelector value

-- | @- anchorB@
anchorB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> IO SCNVector3
anchorB scnPhysicsHingeJoint =
  sendMessage scnPhysicsHingeJoint anchorBSelector

-- | @- setAnchorB:@
setAnchorB :: IsSCNPhysicsHingeJoint scnPhysicsHingeJoint => scnPhysicsHingeJoint -> SCNVector3 -> IO ()
setAnchorB scnPhysicsHingeJoint value =
  sendMessage scnPhysicsHingeJoint setAnchorBSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:@
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector :: Selector '[Id SCNPhysicsBody, SCNVector3, SCNVector3, Id SCNPhysicsBody, SCNVector3, SCNVector3] (Id SCNPhysicsHingeJoint)
jointWithBodyA_axisA_anchorA_bodyB_axisB_anchorBSelector = mkSelector "jointWithBodyA:axisA:anchorA:bodyB:axisB:anchorB:"

-- | @Selector@ for @jointWithBody:axis:anchor:@
jointWithBody_axis_anchorSelector :: Selector '[Id SCNPhysicsBody, SCNVector3, SCNVector3] (Id SCNPhysicsHingeJoint)
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

