{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsConeTwistJoint
--
-- Generated bindings for @SCNPhysicsConeTwistJoint@.
module ObjC.SceneKit.SCNPhysicsConeTwistJoint
  ( SCNPhysicsConeTwistJoint
  , IsSCNPhysicsConeTwistJoint(..)
  , jointWithBodyA_frameA_bodyB_frameB
  , jointWithBody_frame
  , bodyA
  , frameA
  , setFrameA
  , bodyB
  , frameB
  , setFrameB
  , maximumAngularLimit1
  , setMaximumAngularLimit1
  , maximumAngularLimit2
  , setMaximumAngularLimit2
  , maximumTwistAngle
  , setMaximumTwistAngle
  , bodyASelector
  , bodyBSelector
  , frameASelector
  , frameBSelector
  , jointWithBodyA_frameA_bodyB_frameBSelector
  , jointWithBody_frameSelector
  , maximumAngularLimit1Selector
  , maximumAngularLimit2Selector
  , maximumTwistAngleSelector
  , setFrameASelector
  , setFrameBSelector
  , setMaximumAngularLimit1Selector
  , setMaximumAngularLimit2Selector
  , setMaximumTwistAngleSelector


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

-- | @+ jointWithBodyA:frameA:bodyB:frameB:@
jointWithBodyA_frameA_bodyB_frameB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNMatrix4 -> bodyB -> SCNMatrix4 -> IO (Id SCNPhysicsConeTwistJoint)
jointWithBodyA_frameA_bodyB_frameB bodyA frameA bodyB frameB =
  do
    cls' <- getRequiredClass "SCNPhysicsConeTwistJoint"
    sendClassMessage cls' jointWithBodyA_frameA_bodyB_frameBSelector (toSCNPhysicsBody bodyA) frameA (toSCNPhysicsBody bodyB) frameB

-- | @+ jointWithBody:frame:@
jointWithBody_frame :: IsSCNPhysicsBody body => body -> SCNMatrix4 -> IO (Id SCNPhysicsConeTwistJoint)
jointWithBody_frame body frame =
  do
    cls' <- getRequiredClass "SCNPhysicsConeTwistJoint"
    sendClassMessage cls' jointWithBody_frameSelector (toSCNPhysicsBody body) frame

-- | @- bodyA@
bodyA :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint bodyASelector

-- | @- frameA@
frameA :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO SCNMatrix4
frameA scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint frameASelector

-- | @- setFrameA:@
setFrameA :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> SCNMatrix4 -> IO ()
setFrameA scnPhysicsConeTwistJoint value =
  sendMessage scnPhysicsConeTwistJoint setFrameASelector value

-- | @- bodyB@
bodyB :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint bodyBSelector

-- | @- frameB@
frameB :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO SCNMatrix4
frameB scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint frameBSelector

-- | @- setFrameB:@
setFrameB :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> SCNMatrix4 -> IO ()
setFrameB scnPhysicsConeTwistJoint value =
  sendMessage scnPhysicsConeTwistJoint setFrameBSelector value

-- | @- maximumAngularLimit1@
maximumAngularLimit1 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO CDouble
maximumAngularLimit1 scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint maximumAngularLimit1Selector

-- | @- setMaximumAngularLimit1:@
setMaximumAngularLimit1 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> CDouble -> IO ()
setMaximumAngularLimit1 scnPhysicsConeTwistJoint value =
  sendMessage scnPhysicsConeTwistJoint setMaximumAngularLimit1Selector value

-- | @- maximumAngularLimit2@
maximumAngularLimit2 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO CDouble
maximumAngularLimit2 scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint maximumAngularLimit2Selector

-- | @- setMaximumAngularLimit2:@
setMaximumAngularLimit2 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> CDouble -> IO ()
setMaximumAngularLimit2 scnPhysicsConeTwistJoint value =
  sendMessage scnPhysicsConeTwistJoint setMaximumAngularLimit2Selector value

-- | @- maximumTwistAngle@
maximumTwistAngle :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO CDouble
maximumTwistAngle scnPhysicsConeTwistJoint =
  sendMessage scnPhysicsConeTwistJoint maximumTwistAngleSelector

-- | @- setMaximumTwistAngle:@
setMaximumTwistAngle :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> CDouble -> IO ()
setMaximumTwistAngle scnPhysicsConeTwistJoint value =
  sendMessage scnPhysicsConeTwistJoint setMaximumTwistAngleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:frameA:bodyB:frameB:@
jointWithBodyA_frameA_bodyB_frameBSelector :: Selector '[Id SCNPhysicsBody, SCNMatrix4, Id SCNPhysicsBody, SCNMatrix4] (Id SCNPhysicsConeTwistJoint)
jointWithBodyA_frameA_bodyB_frameBSelector = mkSelector "jointWithBodyA:frameA:bodyB:frameB:"

-- | @Selector@ for @jointWithBody:frame:@
jointWithBody_frameSelector :: Selector '[Id SCNPhysicsBody, SCNMatrix4] (Id SCNPhysicsConeTwistJoint)
jointWithBody_frameSelector = mkSelector "jointWithBody:frame:"

-- | @Selector@ for @bodyA@
bodyASelector :: Selector '[] (Id SCNPhysicsBody)
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @frameA@
frameASelector :: Selector '[] SCNMatrix4
frameASelector = mkSelector "frameA"

-- | @Selector@ for @setFrameA:@
setFrameASelector :: Selector '[SCNMatrix4] ()
setFrameASelector = mkSelector "setFrameA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector '[] (Id SCNPhysicsBody)
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @frameB@
frameBSelector :: Selector '[] SCNMatrix4
frameBSelector = mkSelector "frameB"

-- | @Selector@ for @setFrameB:@
setFrameBSelector :: Selector '[SCNMatrix4] ()
setFrameBSelector = mkSelector "setFrameB:"

-- | @Selector@ for @maximumAngularLimit1@
maximumAngularLimit1Selector :: Selector '[] CDouble
maximumAngularLimit1Selector = mkSelector "maximumAngularLimit1"

-- | @Selector@ for @setMaximumAngularLimit1:@
setMaximumAngularLimit1Selector :: Selector '[CDouble] ()
setMaximumAngularLimit1Selector = mkSelector "setMaximumAngularLimit1:"

-- | @Selector@ for @maximumAngularLimit2@
maximumAngularLimit2Selector :: Selector '[] CDouble
maximumAngularLimit2Selector = mkSelector "maximumAngularLimit2"

-- | @Selector@ for @setMaximumAngularLimit2:@
setMaximumAngularLimit2Selector :: Selector '[CDouble] ()
setMaximumAngularLimit2Selector = mkSelector "setMaximumAngularLimit2:"

-- | @Selector@ for @maximumTwistAngle@
maximumTwistAngleSelector :: Selector '[] CDouble
maximumTwistAngleSelector = mkSelector "maximumTwistAngle"

-- | @Selector@ for @setMaximumTwistAngle:@
setMaximumTwistAngleSelector :: Selector '[CDouble] ()
setMaximumTwistAngleSelector = mkSelector "setMaximumTwistAngle:"

