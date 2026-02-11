{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsWorld
--
-- The SCNPhysicsWorld class describes and allows to control the physics simulation of a 3d scene.
--
-- The SCNPhysicsWorld class should not be allocated directly but retrieved from the SCNScene class using the physicsWorld property.
--
-- Generated bindings for @SCNPhysicsWorld@.
module ObjC.SceneKit.SCNPhysicsWorld
  ( SCNPhysicsWorld
  , IsSCNPhysicsWorld(..)
  , addBehavior
  , removeBehavior
  , removeAllBehaviors
  , rayTestWithSegmentFromPoint_toPoint_options
  , contactTestBetweenBody_andBody_options
  , contactTestWithBody_options
  , convexSweepTestWithShape_fromTransform_toTransform_options
  , updateCollisionPairs
  , gravity
  , setGravity
  , speed
  , setSpeed
  , timeStep
  , setTimeStep
  , allBehaviors
  , addBehaviorSelector
  , removeBehaviorSelector
  , removeAllBehaviorsSelector
  , rayTestWithSegmentFromPoint_toPoint_optionsSelector
  , contactTestBetweenBody_andBody_optionsSelector
  , contactTestWithBody_optionsSelector
  , convexSweepTestWithShape_fromTransform_toTransform_optionsSelector
  , updateCollisionPairsSelector
  , gravitySelector
  , setGravitySelector
  , speedSelector
  , setSpeedSelector
  , timeStepSelector
  , setTimeStepSelector
  , allBehaviorsSelector


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

-- | @- addBehavior:@
addBehavior :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBehavior behavior) => scnPhysicsWorld -> behavior -> IO ()
addBehavior scnPhysicsWorld  behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg scnPhysicsWorld (mkSelector "addBehavior:") retVoid [argPtr (castPtr raw_behavior :: Ptr ())]

-- | @- removeBehavior:@
removeBehavior :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBehavior behavior) => scnPhysicsWorld -> behavior -> IO ()
removeBehavior scnPhysicsWorld  behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg scnPhysicsWorld (mkSelector "removeBehavior:") retVoid [argPtr (castPtr raw_behavior :: Ptr ())]

-- | @- removeAllBehaviors@
removeAllBehaviors :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO ()
removeAllBehaviors scnPhysicsWorld  =
  sendMsg scnPhysicsWorld (mkSelector "removeAllBehaviors") retVoid []

-- | @- rayTestWithSegmentFromPoint:toPoint:options:@
rayTestWithSegmentFromPoint_toPoint_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsNSDictionary options) => scnPhysicsWorld -> SCNVector3 -> SCNVector3 -> options -> IO (Id NSArray)
rayTestWithSegmentFromPoint_toPoint_options scnPhysicsWorld  origin dest options =
withObjCPtr options $ \raw_options ->
    sendMsg scnPhysicsWorld (mkSelector "rayTestWithSegmentFromPoint:toPoint:options:") (retPtr retVoid) [argSCNVector3 origin, argSCNVector3 dest, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- contactTestBetweenBody:andBody:options:@
contactTestBetweenBody_andBody_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB, IsNSDictionary options) => scnPhysicsWorld -> bodyA -> bodyB -> options -> IO (Id NSArray)
contactTestBetweenBody_andBody_options scnPhysicsWorld  bodyA bodyB options =
withObjCPtr bodyA $ \raw_bodyA ->
  withObjCPtr bodyB $ \raw_bodyB ->
    withObjCPtr options $ \raw_options ->
        sendMsg scnPhysicsWorld (mkSelector "contactTestBetweenBody:andBody:options:") (retPtr retVoid) [argPtr (castPtr raw_bodyA :: Ptr ()), argPtr (castPtr raw_bodyB :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- contactTestWithBody:options:@
contactTestWithBody_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBody body, IsNSDictionary options) => scnPhysicsWorld -> body -> options -> IO (Id NSArray)
contactTestWithBody_options scnPhysicsWorld  body options =
withObjCPtr body $ \raw_body ->
  withObjCPtr options $ \raw_options ->
      sendMsg scnPhysicsWorld (mkSelector "contactTestWithBody:options:") (retPtr retVoid) [argPtr (castPtr raw_body :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- convexSweepTestWithShape:fromTransform:toTransform:options:@
convexSweepTestWithShape_fromTransform_toTransform_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsShape shape, IsNSDictionary options) => scnPhysicsWorld -> shape -> SCNMatrix4 -> SCNMatrix4 -> options -> IO (Id NSArray)
convexSweepTestWithShape_fromTransform_toTransform_options scnPhysicsWorld  shape from to options =
withObjCPtr shape $ \raw_shape ->
  withObjCPtr options $ \raw_options ->
      sendMsg scnPhysicsWorld (mkSelector "convexSweepTestWithShape:fromTransform:toTransform:options:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argSCNMatrix4 from, argSCNMatrix4 to, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- updateCollisionPairs@
updateCollisionPairs :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO ()
updateCollisionPairs scnPhysicsWorld  =
  sendMsg scnPhysicsWorld (mkSelector "updateCollisionPairs") retVoid []

-- | @- gravity@
gravity :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO SCNVector3
gravity scnPhysicsWorld  =
  sendMsgStret scnPhysicsWorld (mkSelector "gravity") retSCNVector3 []

-- | @- setGravity:@
setGravity :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> SCNVector3 -> IO ()
setGravity scnPhysicsWorld  value =
  sendMsg scnPhysicsWorld (mkSelector "setGravity:") retVoid [argSCNVector3 value]

-- | @- speed@
speed :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO CDouble
speed scnPhysicsWorld  =
  sendMsg scnPhysicsWorld (mkSelector "speed") retCDouble []

-- | @- setSpeed:@
setSpeed :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> CDouble -> IO ()
setSpeed scnPhysicsWorld  value =
  sendMsg scnPhysicsWorld (mkSelector "setSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- timeStep@
timeStep :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO CDouble
timeStep scnPhysicsWorld  =
  sendMsg scnPhysicsWorld (mkSelector "timeStep") retCDouble []

-- | @- setTimeStep:@
setTimeStep :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> CDouble -> IO ()
setTimeStep scnPhysicsWorld  value =
  sendMsg scnPhysicsWorld (mkSelector "setTimeStep:") retVoid [argCDouble (fromIntegral value)]

-- | @- allBehaviors@
allBehaviors :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO (Id NSArray)
allBehaviors scnPhysicsWorld  =
  sendMsg scnPhysicsWorld (mkSelector "allBehaviors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addBehavior:@
addBehaviorSelector :: Selector
addBehaviorSelector = mkSelector "addBehavior:"

-- | @Selector@ for @removeBehavior:@
removeBehaviorSelector :: Selector
removeBehaviorSelector = mkSelector "removeBehavior:"

-- | @Selector@ for @removeAllBehaviors@
removeAllBehaviorsSelector :: Selector
removeAllBehaviorsSelector = mkSelector "removeAllBehaviors"

-- | @Selector@ for @rayTestWithSegmentFromPoint:toPoint:options:@
rayTestWithSegmentFromPoint_toPoint_optionsSelector :: Selector
rayTestWithSegmentFromPoint_toPoint_optionsSelector = mkSelector "rayTestWithSegmentFromPoint:toPoint:options:"

-- | @Selector@ for @contactTestBetweenBody:andBody:options:@
contactTestBetweenBody_andBody_optionsSelector :: Selector
contactTestBetweenBody_andBody_optionsSelector = mkSelector "contactTestBetweenBody:andBody:options:"

-- | @Selector@ for @contactTestWithBody:options:@
contactTestWithBody_optionsSelector :: Selector
contactTestWithBody_optionsSelector = mkSelector "contactTestWithBody:options:"

-- | @Selector@ for @convexSweepTestWithShape:fromTransform:toTransform:options:@
convexSweepTestWithShape_fromTransform_toTransform_optionsSelector :: Selector
convexSweepTestWithShape_fromTransform_toTransform_optionsSelector = mkSelector "convexSweepTestWithShape:fromTransform:toTransform:options:"

-- | @Selector@ for @updateCollisionPairs@
updateCollisionPairsSelector :: Selector
updateCollisionPairsSelector = mkSelector "updateCollisionPairs"

-- | @Selector@ for @gravity@
gravitySelector :: Selector
gravitySelector = mkSelector "gravity"

-- | @Selector@ for @setGravity:@
setGravitySelector :: Selector
setGravitySelector = mkSelector "setGravity:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @timeStep@
timeStepSelector :: Selector
timeStepSelector = mkSelector "timeStep"

-- | @Selector@ for @setTimeStep:@
setTimeStepSelector :: Selector
setTimeStepSelector = mkSelector "setTimeStep:"

-- | @Selector@ for @allBehaviors@
allBehaviorsSelector :: Selector
allBehaviorsSelector = mkSelector "allBehaviors"

