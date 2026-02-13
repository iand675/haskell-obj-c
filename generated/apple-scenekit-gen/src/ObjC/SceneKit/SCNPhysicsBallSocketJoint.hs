{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsBallSocketJoint
--
-- SCNPhysicsBallSocketJoint makes two bodies to move like they are connected by a ball-and-socket joint (i.e it allows rotations around all axes).
--
-- Generated bindings for @SCNPhysicsBallSocketJoint@.
module ObjC.SceneKit.SCNPhysicsBallSocketJoint
  ( SCNPhysicsBallSocketJoint
  , IsSCNPhysicsBallSocketJoint(..)
  , jointWithBodyA_anchorA_bodyB_anchorB
  , jointWithBody_anchor
  , bodyA
  , anchorA
  , setAnchorA
  , bodyB
  , anchorB
  , setAnchorB
  , anchorASelector
  , anchorBSelector
  , bodyASelector
  , bodyBSelector
  , jointWithBodyA_anchorA_bodyB_anchorBSelector
  , jointWithBody_anchorSelector
  , setAnchorASelector
  , setAnchorBSelector


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

-- | @+ jointWithBodyA:anchorA:bodyB:anchorB:@
jointWithBodyA_anchorA_bodyB_anchorB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNVector3 -> bodyB -> SCNVector3 -> IO (Id SCNPhysicsBallSocketJoint)
jointWithBodyA_anchorA_bodyB_anchorB bodyA anchorA bodyB anchorB =
  do
    cls' <- getRequiredClass "SCNPhysicsBallSocketJoint"
    sendClassMessage cls' jointWithBodyA_anchorA_bodyB_anchorBSelector (toSCNPhysicsBody bodyA) anchorA (toSCNPhysicsBody bodyB) anchorB

-- | @+ jointWithBody:anchor:@
jointWithBody_anchor :: IsSCNPhysicsBody body => body -> SCNVector3 -> IO (Id SCNPhysicsBallSocketJoint)
jointWithBody_anchor body anchor =
  do
    cls' <- getRequiredClass "SCNPhysicsBallSocketJoint"
    sendClassMessage cls' jointWithBody_anchorSelector (toSCNPhysicsBody body) anchor

-- | @- bodyA@
bodyA :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsBallSocketJoint =
  sendMessage scnPhysicsBallSocketJoint bodyASelector

-- | @- anchorA@
anchorA :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO SCNVector3
anchorA scnPhysicsBallSocketJoint =
  sendMessage scnPhysicsBallSocketJoint anchorASelector

-- | @- setAnchorA:@
setAnchorA :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> SCNVector3 -> IO ()
setAnchorA scnPhysicsBallSocketJoint value =
  sendMessage scnPhysicsBallSocketJoint setAnchorASelector value

-- | @- bodyB@
bodyB :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsBallSocketJoint =
  sendMessage scnPhysicsBallSocketJoint bodyBSelector

-- | @- anchorB@
anchorB :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO SCNVector3
anchorB scnPhysicsBallSocketJoint =
  sendMessage scnPhysicsBallSocketJoint anchorBSelector

-- | @- setAnchorB:@
setAnchorB :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> SCNVector3 -> IO ()
setAnchorB scnPhysicsBallSocketJoint value =
  sendMessage scnPhysicsBallSocketJoint setAnchorBSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:anchorA:bodyB:anchorB:@
jointWithBodyA_anchorA_bodyB_anchorBSelector :: Selector '[Id SCNPhysicsBody, SCNVector3, Id SCNPhysicsBody, SCNVector3] (Id SCNPhysicsBallSocketJoint)
jointWithBodyA_anchorA_bodyB_anchorBSelector = mkSelector "jointWithBodyA:anchorA:bodyB:anchorB:"

-- | @Selector@ for @jointWithBody:anchor:@
jointWithBody_anchorSelector :: Selector '[Id SCNPhysicsBody, SCNVector3] (Id SCNPhysicsBallSocketJoint)
jointWithBody_anchorSelector = mkSelector "jointWithBody:anchor:"

-- | @Selector@ for @bodyA@
bodyASelector :: Selector '[] (Id SCNPhysicsBody)
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @anchorA@
anchorASelector :: Selector '[] SCNVector3
anchorASelector = mkSelector "anchorA"

-- | @Selector@ for @setAnchorA:@
setAnchorASelector :: Selector '[SCNVector3] ()
setAnchorASelector = mkSelector "setAnchorA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector '[] (Id SCNPhysicsBody)
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @anchorB@
anchorBSelector :: Selector '[] SCNVector3
anchorBSelector = mkSelector "anchorB"

-- | @Selector@ for @setAnchorB:@
setAnchorBSelector :: Selector '[SCNVector3] ()
setAnchorBSelector = mkSelector "setAnchorB:"

