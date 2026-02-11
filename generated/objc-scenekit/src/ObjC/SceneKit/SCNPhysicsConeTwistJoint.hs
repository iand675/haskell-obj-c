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
  , jointWithBodyA_frameA_bodyB_frameBSelector
  , jointWithBody_frameSelector
  , bodyASelector
  , frameASelector
  , setFrameASelector
  , bodyBSelector
  , frameBSelector
  , setFrameBSelector
  , maximumAngularLimit1Selector
  , setMaximumAngularLimit1Selector
  , maximumAngularLimit2Selector
  , setMaximumAngularLimit2Selector
  , maximumTwistAngleSelector
  , setMaximumTwistAngleSelector


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

-- | @+ jointWithBodyA:frameA:bodyB:frameB:@
jointWithBodyA_frameA_bodyB_frameB :: (IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB) => bodyA -> SCNMatrix4 -> bodyB -> SCNMatrix4 -> IO (Id SCNPhysicsConeTwistJoint)
jointWithBodyA_frameA_bodyB_frameB bodyA frameA bodyB frameB =
  do
    cls' <- getRequiredClass "SCNPhysicsConeTwistJoint"
    withObjCPtr bodyA $ \raw_bodyA ->
      withObjCPtr bodyB $ \raw_bodyB ->
        sendClassMsg cls' (mkSelector "jointWithBodyA:frameA:bodyB:frameB:") (retPtr retVoid) [argPtr (castPtr raw_bodyA :: Ptr ()), argSCNMatrix4 frameA, argPtr (castPtr raw_bodyB :: Ptr ()), argSCNMatrix4 frameB] >>= retainedObject . castPtr

-- | @+ jointWithBody:frame:@
jointWithBody_frame :: IsSCNPhysicsBody body => body -> SCNMatrix4 -> IO (Id SCNPhysicsConeTwistJoint)
jointWithBody_frame body frame =
  do
    cls' <- getRequiredClass "SCNPhysicsConeTwistJoint"
    withObjCPtr body $ \raw_body ->
      sendClassMsg cls' (mkSelector "jointWithBody:frame:") (retPtr retVoid) [argPtr (castPtr raw_body :: Ptr ()), argSCNMatrix4 frame] >>= retainedObject . castPtr

-- | @- bodyA@
bodyA :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO (Id SCNPhysicsBody)
bodyA scnPhysicsConeTwistJoint  =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "bodyA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- frameA@
frameA :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO SCNMatrix4
frameA scnPhysicsConeTwistJoint  =
  sendMsgStret scnPhysicsConeTwistJoint (mkSelector "frameA") retSCNMatrix4 []

-- | @- setFrameA:@
setFrameA :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> SCNMatrix4 -> IO ()
setFrameA scnPhysicsConeTwistJoint  value =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "setFrameA:") retVoid [argSCNMatrix4 value]

-- | @- bodyB@
bodyB :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO (Id SCNPhysicsBody)
bodyB scnPhysicsConeTwistJoint  =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "bodyB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- frameB@
frameB :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO SCNMatrix4
frameB scnPhysicsConeTwistJoint  =
  sendMsgStret scnPhysicsConeTwistJoint (mkSelector "frameB") retSCNMatrix4 []

-- | @- setFrameB:@
setFrameB :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> SCNMatrix4 -> IO ()
setFrameB scnPhysicsConeTwistJoint  value =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "setFrameB:") retVoid [argSCNMatrix4 value]

-- | @- maximumAngularLimit1@
maximumAngularLimit1 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO CDouble
maximumAngularLimit1 scnPhysicsConeTwistJoint  =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "maximumAngularLimit1") retCDouble []

-- | @- setMaximumAngularLimit1:@
setMaximumAngularLimit1 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> CDouble -> IO ()
setMaximumAngularLimit1 scnPhysicsConeTwistJoint  value =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "setMaximumAngularLimit1:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumAngularLimit2@
maximumAngularLimit2 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO CDouble
maximumAngularLimit2 scnPhysicsConeTwistJoint  =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "maximumAngularLimit2") retCDouble []

-- | @- setMaximumAngularLimit2:@
setMaximumAngularLimit2 :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> CDouble -> IO ()
setMaximumAngularLimit2 scnPhysicsConeTwistJoint  value =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "setMaximumAngularLimit2:") retVoid [argCDouble (fromIntegral value)]

-- | @- maximumTwistAngle@
maximumTwistAngle :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> IO CDouble
maximumTwistAngle scnPhysicsConeTwistJoint  =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "maximumTwistAngle") retCDouble []

-- | @- setMaximumTwistAngle:@
setMaximumTwistAngle :: IsSCNPhysicsConeTwistJoint scnPhysicsConeTwistJoint => scnPhysicsConeTwistJoint -> CDouble -> IO ()
setMaximumTwistAngle scnPhysicsConeTwistJoint  value =
  sendMsg scnPhysicsConeTwistJoint (mkSelector "setMaximumTwistAngle:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @jointWithBodyA:frameA:bodyB:frameB:@
jointWithBodyA_frameA_bodyB_frameBSelector :: Selector
jointWithBodyA_frameA_bodyB_frameBSelector = mkSelector "jointWithBodyA:frameA:bodyB:frameB:"

-- | @Selector@ for @jointWithBody:frame:@
jointWithBody_frameSelector :: Selector
jointWithBody_frameSelector = mkSelector "jointWithBody:frame:"

-- | @Selector@ for @bodyA@
bodyASelector :: Selector
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @frameA@
frameASelector :: Selector
frameASelector = mkSelector "frameA"

-- | @Selector@ for @setFrameA:@
setFrameASelector :: Selector
setFrameASelector = mkSelector "setFrameA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @frameB@
frameBSelector :: Selector
frameBSelector = mkSelector "frameB"

-- | @Selector@ for @setFrameB:@
setFrameBSelector :: Selector
setFrameBSelector = mkSelector "setFrameB:"

-- | @Selector@ for @maximumAngularLimit1@
maximumAngularLimit1Selector :: Selector
maximumAngularLimit1Selector = mkSelector "maximumAngularLimit1"

-- | @Selector@ for @setMaximumAngularLimit1:@
setMaximumAngularLimit1Selector :: Selector
setMaximumAngularLimit1Selector = mkSelector "setMaximumAngularLimit1:"

-- | @Selector@ for @maximumAngularLimit2@
maximumAngularLimit2Selector :: Selector
maximumAngularLimit2Selector = mkSelector "maximumAngularLimit2"

-- | @Selector@ for @setMaximumAngularLimit2:@
setMaximumAngularLimit2Selector :: Selector
setMaximumAngularLimit2Selector = mkSelector "setMaximumAngularLimit2:"

-- | @Selector@ for @maximumTwistAngle@
maximumTwistAngleSelector :: Selector
maximumTwistAngleSelector = mkSelector "maximumTwistAngle"

-- | @Selector@ for @setMaximumTwistAngle:@
setMaximumTwistAngleSelector :: Selector
setMaximumTwistAngleSelector = mkSelector "setMaximumTwistAngle:"

