{-# LANGUAGE DataKinds #-}
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
  , contactDelegate
  , setContactDelegate
  , allBehaviors
  , addBehaviorSelector
  , allBehaviorsSelector
  , contactDelegateSelector
  , contactTestBetweenBody_andBody_optionsSelector
  , contactTestWithBody_optionsSelector
  , convexSweepTestWithShape_fromTransform_toTransform_optionsSelector
  , gravitySelector
  , rayTestWithSegmentFromPoint_toPoint_optionsSelector
  , removeAllBehaviorsSelector
  , removeBehaviorSelector
  , setContactDelegateSelector
  , setGravitySelector
  , setSpeedSelector
  , setTimeStepSelector
  , speedSelector
  , timeStepSelector
  , updateCollisionPairsSelector


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

-- | @- addBehavior:@
addBehavior :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBehavior behavior) => scnPhysicsWorld -> behavior -> IO ()
addBehavior scnPhysicsWorld behavior =
  sendMessage scnPhysicsWorld addBehaviorSelector (toSCNPhysicsBehavior behavior)

-- | @- removeBehavior:@
removeBehavior :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBehavior behavior) => scnPhysicsWorld -> behavior -> IO ()
removeBehavior scnPhysicsWorld behavior =
  sendMessage scnPhysicsWorld removeBehaviorSelector (toSCNPhysicsBehavior behavior)

-- | @- removeAllBehaviors@
removeAllBehaviors :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO ()
removeAllBehaviors scnPhysicsWorld =
  sendMessage scnPhysicsWorld removeAllBehaviorsSelector

-- | @- rayTestWithSegmentFromPoint:toPoint:options:@
rayTestWithSegmentFromPoint_toPoint_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsNSDictionary options) => scnPhysicsWorld -> SCNVector3 -> SCNVector3 -> options -> IO (Id NSArray)
rayTestWithSegmentFromPoint_toPoint_options scnPhysicsWorld origin dest options =
  sendMessage scnPhysicsWorld rayTestWithSegmentFromPoint_toPoint_optionsSelector origin dest (toNSDictionary options)

-- | @- contactTestBetweenBody:andBody:options:@
contactTestBetweenBody_andBody_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBody bodyA, IsSCNPhysicsBody bodyB, IsNSDictionary options) => scnPhysicsWorld -> bodyA -> bodyB -> options -> IO (Id NSArray)
contactTestBetweenBody_andBody_options scnPhysicsWorld bodyA bodyB options =
  sendMessage scnPhysicsWorld contactTestBetweenBody_andBody_optionsSelector (toSCNPhysicsBody bodyA) (toSCNPhysicsBody bodyB) (toNSDictionary options)

-- | @- contactTestWithBody:options:@
contactTestWithBody_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsBody body, IsNSDictionary options) => scnPhysicsWorld -> body -> options -> IO (Id NSArray)
contactTestWithBody_options scnPhysicsWorld body options =
  sendMessage scnPhysicsWorld contactTestWithBody_optionsSelector (toSCNPhysicsBody body) (toNSDictionary options)

-- | @- convexSweepTestWithShape:fromTransform:toTransform:options:@
convexSweepTestWithShape_fromTransform_toTransform_options :: (IsSCNPhysicsWorld scnPhysicsWorld, IsSCNPhysicsShape shape, IsNSDictionary options) => scnPhysicsWorld -> shape -> SCNMatrix4 -> SCNMatrix4 -> options -> IO (Id NSArray)
convexSweepTestWithShape_fromTransform_toTransform_options scnPhysicsWorld shape from to options =
  sendMessage scnPhysicsWorld convexSweepTestWithShape_fromTransform_toTransform_optionsSelector (toSCNPhysicsShape shape) from to (toNSDictionary options)

-- | @- updateCollisionPairs@
updateCollisionPairs :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO ()
updateCollisionPairs scnPhysicsWorld =
  sendMessage scnPhysicsWorld updateCollisionPairsSelector

-- | @- gravity@
gravity :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO SCNVector3
gravity scnPhysicsWorld =
  sendMessage scnPhysicsWorld gravitySelector

-- | @- setGravity:@
setGravity :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> SCNVector3 -> IO ()
setGravity scnPhysicsWorld value =
  sendMessage scnPhysicsWorld setGravitySelector value

-- | @- speed@
speed :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO CDouble
speed scnPhysicsWorld =
  sendMessage scnPhysicsWorld speedSelector

-- | @- setSpeed:@
setSpeed :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> CDouble -> IO ()
setSpeed scnPhysicsWorld value =
  sendMessage scnPhysicsWorld setSpeedSelector value

-- | @- timeStep@
timeStep :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO CDouble
timeStep scnPhysicsWorld =
  sendMessage scnPhysicsWorld timeStepSelector

-- | @- setTimeStep:@
setTimeStep :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> CDouble -> IO ()
setTimeStep scnPhysicsWorld value =
  sendMessage scnPhysicsWorld setTimeStepSelector value

-- | @- contactDelegate@
contactDelegate :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO RawId
contactDelegate scnPhysicsWorld =
  sendMessage scnPhysicsWorld contactDelegateSelector

-- | @- setContactDelegate:@
setContactDelegate :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> RawId -> IO ()
setContactDelegate scnPhysicsWorld value =
  sendMessage scnPhysicsWorld setContactDelegateSelector value

-- | @- allBehaviors@
allBehaviors :: IsSCNPhysicsWorld scnPhysicsWorld => scnPhysicsWorld -> IO (Id NSArray)
allBehaviors scnPhysicsWorld =
  sendMessage scnPhysicsWorld allBehaviorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addBehavior:@
addBehaviorSelector :: Selector '[Id SCNPhysicsBehavior] ()
addBehaviorSelector = mkSelector "addBehavior:"

-- | @Selector@ for @removeBehavior:@
removeBehaviorSelector :: Selector '[Id SCNPhysicsBehavior] ()
removeBehaviorSelector = mkSelector "removeBehavior:"

-- | @Selector@ for @removeAllBehaviors@
removeAllBehaviorsSelector :: Selector '[] ()
removeAllBehaviorsSelector = mkSelector "removeAllBehaviors"

-- | @Selector@ for @rayTestWithSegmentFromPoint:toPoint:options:@
rayTestWithSegmentFromPoint_toPoint_optionsSelector :: Selector '[SCNVector3, SCNVector3, Id NSDictionary] (Id NSArray)
rayTestWithSegmentFromPoint_toPoint_optionsSelector = mkSelector "rayTestWithSegmentFromPoint:toPoint:options:"

-- | @Selector@ for @contactTestBetweenBody:andBody:options:@
contactTestBetweenBody_andBody_optionsSelector :: Selector '[Id SCNPhysicsBody, Id SCNPhysicsBody, Id NSDictionary] (Id NSArray)
contactTestBetweenBody_andBody_optionsSelector = mkSelector "contactTestBetweenBody:andBody:options:"

-- | @Selector@ for @contactTestWithBody:options:@
contactTestWithBody_optionsSelector :: Selector '[Id SCNPhysicsBody, Id NSDictionary] (Id NSArray)
contactTestWithBody_optionsSelector = mkSelector "contactTestWithBody:options:"

-- | @Selector@ for @convexSweepTestWithShape:fromTransform:toTransform:options:@
convexSweepTestWithShape_fromTransform_toTransform_optionsSelector :: Selector '[Id SCNPhysicsShape, SCNMatrix4, SCNMatrix4, Id NSDictionary] (Id NSArray)
convexSweepTestWithShape_fromTransform_toTransform_optionsSelector = mkSelector "convexSweepTestWithShape:fromTransform:toTransform:options:"

-- | @Selector@ for @updateCollisionPairs@
updateCollisionPairsSelector :: Selector '[] ()
updateCollisionPairsSelector = mkSelector "updateCollisionPairs"

-- | @Selector@ for @gravity@
gravitySelector :: Selector '[] SCNVector3
gravitySelector = mkSelector "gravity"

-- | @Selector@ for @setGravity:@
setGravitySelector :: Selector '[SCNVector3] ()
setGravitySelector = mkSelector "setGravity:"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CDouble
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[CDouble] ()
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @timeStep@
timeStepSelector :: Selector '[] CDouble
timeStepSelector = mkSelector "timeStep"

-- | @Selector@ for @setTimeStep:@
setTimeStepSelector :: Selector '[CDouble] ()
setTimeStepSelector = mkSelector "setTimeStep:"

-- | @Selector@ for @contactDelegate@
contactDelegateSelector :: Selector '[] RawId
contactDelegateSelector = mkSelector "contactDelegate"

-- | @Selector@ for @setContactDelegate:@
setContactDelegateSelector :: Selector '[RawId] ()
setContactDelegateSelector = mkSelector "setContactDelegate:"

-- | @Selector@ for @allBehaviors@
allBehaviorsSelector :: Selector '[] (Id NSArray)
allBehaviorsSelector = mkSelector "allBehaviors"

