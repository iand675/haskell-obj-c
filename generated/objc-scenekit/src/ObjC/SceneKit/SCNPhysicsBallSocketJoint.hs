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
  , jointWithBodyA_anchorA_bodyB_anchorBSelector
  , jointWithBody_anchorSelector
  , bodyASelector
  , anchorASelector
  , setAnchorASelector
  , bodyBSelector
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

-- | @+ jointWithBodyA:anchorA:bodyB:anchorB:@
jointWithBodyA_anchorA_bodyB_anchorB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNVector3 -> bodyB -> SCNVector3 -> IO (Id SCNPhysicsBallSocketJoint)
jointWithBodyA_anchorA_bodyB_anchorB bodyA anchorA bodyB anchorB =
  do
    cls' <- getRequiredClass "SCNPhysicsBallSocketJoint"
    withObjCPtr bodyA $ \raw_bodyA ->
      withObjCPtr bodyB $ \raw_bodyB ->
        sendClassMsg cls' (mkSelector "jointWithBodyA:anchorA:bodyB:anchorB:") (retPtr retVoid) [argPtr (castPtr raw_bodyA :: Ptr ()), argSCNVector3 anchorA, argPtr (castPtr raw_bodyB :: Ptr ()), argSCNVector3 anchorB] >>= retainedObject . castPtr

-- | @+ jointWithBody:anchor:@
jointWithBody_anchor :: IsSCNPhysicsBody body => body -> SCNVector3 -> IO (Id SCNPhysicsBallSocketJoint)
jointWithBody_anchor body anchor =
  do
    cls' <- getRequiredClass "SCNPhysicsBallSocketJoint"
    withObjCPtr body $ \raw_body ->
      sendClassMsg cls' (mkSelector "jointWithBody:anchor:") (retPtr retVoid) [argPtr (castPtr raw_body :: Ptr ()), argSCNVector3 anchor] >>= retainedObject . castPtr

-- | @- bodyA@
bodyA :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsBallSocketJoint  =
  sendMsg scnPhysicsBallSocketJoint (mkSelector "bodyA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- anchorA@
anchorA :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO SCNVector3
anchorA scnPhysicsBallSocketJoint  =
  sendMsgStret scnPhysicsBallSocketJoint (mkSelector "anchorA") retSCNVector3 []

-- | @- setAnchorA:@
setAnchorA :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> SCNVector3 -> IO ()
setAnchorA scnPhysicsBallSocketJoint  value =
  sendMsg scnPhysicsBallSocketJoint (mkSelector "setAnchorA:") retVoid [argSCNVector3 value]

-- | @- bodyB@
bodyB :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsBallSocketJoint  =
  sendMsg scnPhysicsBallSocketJoint (mkSelector "bodyB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- anchorB@
anchorB :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> IO SCNVector3
anchorB scnPhysicsBallSocketJoint  =
  sendMsgStret scnPhysicsBallSocketJoint (mkSelector "anchorB") retSCNVector3 []

-- | @- setAnchorB:@
setAnchorB :: IsSCNPhysicsBallSocketJoint scnPhysicsBallSocketJoint => scnPhysicsBallSocketJoint -> SCNVector3 -> IO ()
setAnchorB scnPhysicsBallSocketJoint  value =
  sendMsg scnPhysicsBallSocketJoint (mkSelector "setAnchorB:") retVoid [argSCNVector3 value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:anchorA:bodyB:anchorB:@
jointWithBodyA_anchorA_bodyB_anchorBSelector :: Selector
jointWithBodyA_anchorA_bodyB_anchorBSelector = mkSelector "jointWithBodyA:anchorA:bodyB:anchorB:"

-- | @Selector@ for @jointWithBody:anchor:@
jointWithBody_anchorSelector :: Selector
jointWithBody_anchorSelector = mkSelector "jointWithBody:anchor:"

-- | @Selector@ for @bodyA@
bodyASelector :: Selector
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @anchorA@
anchorASelector :: Selector
anchorASelector = mkSelector "anchorA"

-- | @Selector@ for @setAnchorA:@
setAnchorASelector :: Selector
setAnchorASelector = mkSelector "setAnchorA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @anchorB@
anchorBSelector :: Selector
anchorBSelector = mkSelector "anchorB"

-- | @Selector@ for @setAnchorB:@
setAnchorBSelector :: Selector
setAnchorBSelector = mkSelector "setAnchorB:"

