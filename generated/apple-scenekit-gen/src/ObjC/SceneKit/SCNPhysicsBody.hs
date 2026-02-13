{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsBody
--
-- The SCNPhysicsBody class describes the physics properties (such as mass, friction...) of a node.
--
-- Generated bindings for @SCNPhysicsBody@.
module ObjC.SceneKit.SCNPhysicsBody
  ( SCNPhysicsBody
  , IsSCNPhysicsBody(..)
  , staticBody
  , dynamicBody
  , kinematicBody
  , bodyWithType_shape
  , applyForce_impulse
  , applyForce_atPosition_impulse
  , applyTorque_impulse
  , clearAllForces
  , resetTransform
  , setResting
  , type_
  , setType
  , mass
  , setMass
  , momentOfInertia
  , setMomentOfInertia
  , usesDefaultMomentOfInertia
  , setUsesDefaultMomentOfInertia
  , charge
  , setCharge
  , friction
  , setFriction
  , restitution
  , setRestitution
  , rollingFriction
  , setRollingFriction
  , physicsShape
  , setPhysicsShape
  , isResting
  , allowsResting
  , setAllowsResting
  , velocity
  , setVelocity
  , angularVelocity
  , setAngularVelocity
  , damping
  , setDamping
  , angularDamping
  , setAngularDamping
  , velocityFactor
  , setVelocityFactor
  , angularVelocityFactor
  , setAngularVelocityFactor
  , categoryBitMask
  , setCategoryBitMask
  , collisionBitMask
  , setCollisionBitMask
  , contactTestBitMask
  , setContactTestBitMask
  , affectedByGravity
  , setAffectedByGravity
  , continuousCollisionDetectionThreshold
  , setContinuousCollisionDetectionThreshold
  , centerOfMassOffset
  , setCenterOfMassOffset
  , linearRestingThreshold
  , setLinearRestingThreshold
  , angularRestingThreshold
  , setAngularRestingThreshold
  , affectedByGravitySelector
  , allowsRestingSelector
  , angularDampingSelector
  , angularRestingThresholdSelector
  , angularVelocityFactorSelector
  , angularVelocitySelector
  , applyForce_atPosition_impulseSelector
  , applyForce_impulseSelector
  , applyTorque_impulseSelector
  , bodyWithType_shapeSelector
  , categoryBitMaskSelector
  , centerOfMassOffsetSelector
  , chargeSelector
  , clearAllForcesSelector
  , collisionBitMaskSelector
  , contactTestBitMaskSelector
  , continuousCollisionDetectionThresholdSelector
  , dampingSelector
  , dynamicBodySelector
  , frictionSelector
  , isRestingSelector
  , kinematicBodySelector
  , linearRestingThresholdSelector
  , massSelector
  , momentOfInertiaSelector
  , physicsShapeSelector
  , resetTransformSelector
  , restitutionSelector
  , rollingFrictionSelector
  , setAffectedByGravitySelector
  , setAllowsRestingSelector
  , setAngularDampingSelector
  , setAngularRestingThresholdSelector
  , setAngularVelocityFactorSelector
  , setAngularVelocitySelector
  , setCategoryBitMaskSelector
  , setCenterOfMassOffsetSelector
  , setChargeSelector
  , setCollisionBitMaskSelector
  , setContactTestBitMaskSelector
  , setContinuousCollisionDetectionThresholdSelector
  , setDampingSelector
  , setFrictionSelector
  , setLinearRestingThresholdSelector
  , setMassSelector
  , setMomentOfInertiaSelector
  , setPhysicsShapeSelector
  , setRestingSelector
  , setRestitutionSelector
  , setRollingFrictionSelector
  , setTypeSelector
  , setUsesDefaultMomentOfInertiaSelector
  , setVelocityFactorSelector
  , setVelocitySelector
  , staticBodySelector
  , typeSelector
  , usesDefaultMomentOfInertiaSelector
  , velocityFactorSelector
  , velocitySelector

  -- * Enum types
  , SCNPhysicsBodyType(SCNPhysicsBodyType)
  , pattern SCNPhysicsBodyTypeStatic
  , pattern SCNPhysicsBodyTypeDynamic
  , pattern SCNPhysicsBodyTypeKinematic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ staticBody@
staticBody :: IO (Id SCNPhysicsBody)
staticBody  =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMessage cls' staticBodySelector

-- | @+ dynamicBody@
dynamicBody :: IO (Id SCNPhysicsBody)
dynamicBody  =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMessage cls' dynamicBodySelector

-- | @+ kinematicBody@
kinematicBody :: IO (Id SCNPhysicsBody)
kinematicBody  =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMessage cls' kinematicBodySelector

-- | @+ bodyWithType:shape:@
bodyWithType_shape :: IsSCNPhysicsShape shape => SCNPhysicsBodyType -> shape -> IO (Id SCNPhysicsBody)
bodyWithType_shape type_ shape =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMessage cls' bodyWithType_shapeSelector type_ (toSCNPhysicsShape shape)

-- | @- applyForce:impulse:@
applyForce_impulse :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> Bool -> IO ()
applyForce_impulse scnPhysicsBody direction impulse =
  sendMessage scnPhysicsBody applyForce_impulseSelector direction impulse

-- | @- applyForce:atPosition:impulse:@
applyForce_atPosition_impulse :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> SCNVector3 -> Bool -> IO ()
applyForce_atPosition_impulse scnPhysicsBody direction position impulse =
  sendMessage scnPhysicsBody applyForce_atPosition_impulseSelector direction position impulse

-- | @- applyTorque:impulse:@
applyTorque_impulse :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector4 -> Bool -> IO ()
applyTorque_impulse scnPhysicsBody torque impulse =
  sendMessage scnPhysicsBody applyTorque_impulseSelector torque impulse

-- | @- clearAllForces@
clearAllForces :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO ()
clearAllForces scnPhysicsBody =
  sendMessage scnPhysicsBody clearAllForcesSelector

-- | @- resetTransform@
resetTransform :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO ()
resetTransform scnPhysicsBody =
  sendMessage scnPhysicsBody resetTransformSelector

-- | @- setResting:@
setResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setResting scnPhysicsBody resting =
  sendMessage scnPhysicsBody setRestingSelector resting

-- | @- type@
type_ :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNPhysicsBodyType
type_ scnPhysicsBody =
  sendMessage scnPhysicsBody typeSelector

-- | @- setType:@
setType :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNPhysicsBodyType -> IO ()
setType scnPhysicsBody value =
  sendMessage scnPhysicsBody setTypeSelector value

-- | @- mass@
mass :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
mass scnPhysicsBody =
  sendMessage scnPhysicsBody massSelector

-- | @- setMass:@
setMass :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setMass scnPhysicsBody value =
  sendMessage scnPhysicsBody setMassSelector value

-- | @- momentOfInertia@
momentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
momentOfInertia scnPhysicsBody =
  sendMessage scnPhysicsBody momentOfInertiaSelector

-- | @- setMomentOfInertia:@
setMomentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setMomentOfInertia scnPhysicsBody value =
  sendMessage scnPhysicsBody setMomentOfInertiaSelector value

-- | @- usesDefaultMomentOfInertia@
usesDefaultMomentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
usesDefaultMomentOfInertia scnPhysicsBody =
  sendMessage scnPhysicsBody usesDefaultMomentOfInertiaSelector

-- | @- setUsesDefaultMomentOfInertia:@
setUsesDefaultMomentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setUsesDefaultMomentOfInertia scnPhysicsBody value =
  sendMessage scnPhysicsBody setUsesDefaultMomentOfInertiaSelector value

-- | @- charge@
charge :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
charge scnPhysicsBody =
  sendMessage scnPhysicsBody chargeSelector

-- | @- setCharge:@
setCharge :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setCharge scnPhysicsBody value =
  sendMessage scnPhysicsBody setChargeSelector value

-- | @- friction@
friction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
friction scnPhysicsBody =
  sendMessage scnPhysicsBody frictionSelector

-- | @- setFriction:@
setFriction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setFriction scnPhysicsBody value =
  sendMessage scnPhysicsBody setFrictionSelector value

-- | @- restitution@
restitution :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
restitution scnPhysicsBody =
  sendMessage scnPhysicsBody restitutionSelector

-- | @- setRestitution:@
setRestitution :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setRestitution scnPhysicsBody value =
  sendMessage scnPhysicsBody setRestitutionSelector value

-- | @- rollingFriction@
rollingFriction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
rollingFriction scnPhysicsBody =
  sendMessage scnPhysicsBody rollingFrictionSelector

-- | @- setRollingFriction:@
setRollingFriction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setRollingFriction scnPhysicsBody value =
  sendMessage scnPhysicsBody setRollingFrictionSelector value

-- | @- physicsShape@
physicsShape :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO (Id SCNPhysicsShape)
physicsShape scnPhysicsBody =
  sendMessage scnPhysicsBody physicsShapeSelector

-- | @- setPhysicsShape:@
setPhysicsShape :: (IsSCNPhysicsBody scnPhysicsBody, IsSCNPhysicsShape value) => scnPhysicsBody -> value -> IO ()
setPhysicsShape scnPhysicsBody value =
  sendMessage scnPhysicsBody setPhysicsShapeSelector (toSCNPhysicsShape value)

-- | @- isResting@
isResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
isResting scnPhysicsBody =
  sendMessage scnPhysicsBody isRestingSelector

-- | @- allowsResting@
allowsResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
allowsResting scnPhysicsBody =
  sendMessage scnPhysicsBody allowsRestingSelector

-- | @- setAllowsResting:@
setAllowsResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setAllowsResting scnPhysicsBody value =
  sendMessage scnPhysicsBody setAllowsRestingSelector value

-- | @- velocity@
velocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
velocity scnPhysicsBody =
  sendMessage scnPhysicsBody velocitySelector

-- | @- setVelocity:@
setVelocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setVelocity scnPhysicsBody value =
  sendMessage scnPhysicsBody setVelocitySelector value

-- | @- angularVelocity@
angularVelocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector4
angularVelocity scnPhysicsBody =
  sendMessage scnPhysicsBody angularVelocitySelector

-- | @- setAngularVelocity:@
setAngularVelocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector4 -> IO ()
setAngularVelocity scnPhysicsBody value =
  sendMessage scnPhysicsBody setAngularVelocitySelector value

-- | @- damping@
damping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
damping scnPhysicsBody =
  sendMessage scnPhysicsBody dampingSelector

-- | @- setDamping:@
setDamping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setDamping scnPhysicsBody value =
  sendMessage scnPhysicsBody setDampingSelector value

-- | @- angularDamping@
angularDamping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
angularDamping scnPhysicsBody =
  sendMessage scnPhysicsBody angularDampingSelector

-- | @- setAngularDamping:@
setAngularDamping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setAngularDamping scnPhysicsBody value =
  sendMessage scnPhysicsBody setAngularDampingSelector value

-- | @- velocityFactor@
velocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
velocityFactor scnPhysicsBody =
  sendMessage scnPhysicsBody velocityFactorSelector

-- | @- setVelocityFactor:@
setVelocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setVelocityFactor scnPhysicsBody value =
  sendMessage scnPhysicsBody setVelocityFactorSelector value

-- | @- angularVelocityFactor@
angularVelocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
angularVelocityFactor scnPhysicsBody =
  sendMessage scnPhysicsBody angularVelocityFactorSelector

-- | @- setAngularVelocityFactor:@
setAngularVelocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setAngularVelocityFactor scnPhysicsBody value =
  sendMessage scnPhysicsBody setAngularVelocityFactorSelector value

-- | @- categoryBitMask@
categoryBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CULong
categoryBitMask scnPhysicsBody =
  sendMessage scnPhysicsBody categoryBitMaskSelector

-- | @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CULong -> IO ()
setCategoryBitMask scnPhysicsBody value =
  sendMessage scnPhysicsBody setCategoryBitMaskSelector value

-- | @- collisionBitMask@
collisionBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CULong
collisionBitMask scnPhysicsBody =
  sendMessage scnPhysicsBody collisionBitMaskSelector

-- | @- setCollisionBitMask:@
setCollisionBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CULong -> IO ()
setCollisionBitMask scnPhysicsBody value =
  sendMessage scnPhysicsBody setCollisionBitMaskSelector value

-- | @- contactTestBitMask@
contactTestBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CULong
contactTestBitMask scnPhysicsBody =
  sendMessage scnPhysicsBody contactTestBitMaskSelector

-- | @- setContactTestBitMask:@
setContactTestBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CULong -> IO ()
setContactTestBitMask scnPhysicsBody value =
  sendMessage scnPhysicsBody setContactTestBitMaskSelector value

-- | @- affectedByGravity@
affectedByGravity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
affectedByGravity scnPhysicsBody =
  sendMessage scnPhysicsBody affectedByGravitySelector

-- | @- setAffectedByGravity:@
setAffectedByGravity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setAffectedByGravity scnPhysicsBody value =
  sendMessage scnPhysicsBody setAffectedByGravitySelector value

-- | @- continuousCollisionDetectionThreshold@
continuousCollisionDetectionThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
continuousCollisionDetectionThreshold scnPhysicsBody =
  sendMessage scnPhysicsBody continuousCollisionDetectionThresholdSelector

-- | @- setContinuousCollisionDetectionThreshold:@
setContinuousCollisionDetectionThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setContinuousCollisionDetectionThreshold scnPhysicsBody value =
  sendMessage scnPhysicsBody setContinuousCollisionDetectionThresholdSelector value

-- | @- centerOfMassOffset@
centerOfMassOffset :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
centerOfMassOffset scnPhysicsBody =
  sendMessage scnPhysicsBody centerOfMassOffsetSelector

-- | @- setCenterOfMassOffset:@
setCenterOfMassOffset :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setCenterOfMassOffset scnPhysicsBody value =
  sendMessage scnPhysicsBody setCenterOfMassOffsetSelector value

-- | @- linearRestingThreshold@
linearRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
linearRestingThreshold scnPhysicsBody =
  sendMessage scnPhysicsBody linearRestingThresholdSelector

-- | @- setLinearRestingThreshold:@
setLinearRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setLinearRestingThreshold scnPhysicsBody value =
  sendMessage scnPhysicsBody setLinearRestingThresholdSelector value

-- | @- angularRestingThreshold@
angularRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
angularRestingThreshold scnPhysicsBody =
  sendMessage scnPhysicsBody angularRestingThresholdSelector

-- | @- setAngularRestingThreshold:@
setAngularRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setAngularRestingThreshold scnPhysicsBody value =
  sendMessage scnPhysicsBody setAngularRestingThresholdSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @staticBody@
staticBodySelector :: Selector '[] (Id SCNPhysicsBody)
staticBodySelector = mkSelector "staticBody"

-- | @Selector@ for @dynamicBody@
dynamicBodySelector :: Selector '[] (Id SCNPhysicsBody)
dynamicBodySelector = mkSelector "dynamicBody"

-- | @Selector@ for @kinematicBody@
kinematicBodySelector :: Selector '[] (Id SCNPhysicsBody)
kinematicBodySelector = mkSelector "kinematicBody"

-- | @Selector@ for @bodyWithType:shape:@
bodyWithType_shapeSelector :: Selector '[SCNPhysicsBodyType, Id SCNPhysicsShape] (Id SCNPhysicsBody)
bodyWithType_shapeSelector = mkSelector "bodyWithType:shape:"

-- | @Selector@ for @applyForce:impulse:@
applyForce_impulseSelector :: Selector '[SCNVector3, Bool] ()
applyForce_impulseSelector = mkSelector "applyForce:impulse:"

-- | @Selector@ for @applyForce:atPosition:impulse:@
applyForce_atPosition_impulseSelector :: Selector '[SCNVector3, SCNVector3, Bool] ()
applyForce_atPosition_impulseSelector = mkSelector "applyForce:atPosition:impulse:"

-- | @Selector@ for @applyTorque:impulse:@
applyTorque_impulseSelector :: Selector '[SCNVector4, Bool] ()
applyTorque_impulseSelector = mkSelector "applyTorque:impulse:"

-- | @Selector@ for @clearAllForces@
clearAllForcesSelector :: Selector '[] ()
clearAllForcesSelector = mkSelector "clearAllForces"

-- | @Selector@ for @resetTransform@
resetTransformSelector :: Selector '[] ()
resetTransformSelector = mkSelector "resetTransform"

-- | @Selector@ for @setResting:@
setRestingSelector :: Selector '[Bool] ()
setRestingSelector = mkSelector "setResting:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] SCNPhysicsBodyType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[SCNPhysicsBodyType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @mass@
massSelector :: Selector '[] CDouble
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector '[CDouble] ()
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @momentOfInertia@
momentOfInertiaSelector :: Selector '[] SCNVector3
momentOfInertiaSelector = mkSelector "momentOfInertia"

-- | @Selector@ for @setMomentOfInertia:@
setMomentOfInertiaSelector :: Selector '[SCNVector3] ()
setMomentOfInertiaSelector = mkSelector "setMomentOfInertia:"

-- | @Selector@ for @usesDefaultMomentOfInertia@
usesDefaultMomentOfInertiaSelector :: Selector '[] Bool
usesDefaultMomentOfInertiaSelector = mkSelector "usesDefaultMomentOfInertia"

-- | @Selector@ for @setUsesDefaultMomentOfInertia:@
setUsesDefaultMomentOfInertiaSelector :: Selector '[Bool] ()
setUsesDefaultMomentOfInertiaSelector = mkSelector "setUsesDefaultMomentOfInertia:"

-- | @Selector@ for @charge@
chargeSelector :: Selector '[] CDouble
chargeSelector = mkSelector "charge"

-- | @Selector@ for @setCharge:@
setChargeSelector :: Selector '[CDouble] ()
setChargeSelector = mkSelector "setCharge:"

-- | @Selector@ for @friction@
frictionSelector :: Selector '[] CDouble
frictionSelector = mkSelector "friction"

-- | @Selector@ for @setFriction:@
setFrictionSelector :: Selector '[CDouble] ()
setFrictionSelector = mkSelector "setFriction:"

-- | @Selector@ for @restitution@
restitutionSelector :: Selector '[] CDouble
restitutionSelector = mkSelector "restitution"

-- | @Selector@ for @setRestitution:@
setRestitutionSelector :: Selector '[CDouble] ()
setRestitutionSelector = mkSelector "setRestitution:"

-- | @Selector@ for @rollingFriction@
rollingFrictionSelector :: Selector '[] CDouble
rollingFrictionSelector = mkSelector "rollingFriction"

-- | @Selector@ for @setRollingFriction:@
setRollingFrictionSelector :: Selector '[CDouble] ()
setRollingFrictionSelector = mkSelector "setRollingFriction:"

-- | @Selector@ for @physicsShape@
physicsShapeSelector :: Selector '[] (Id SCNPhysicsShape)
physicsShapeSelector = mkSelector "physicsShape"

-- | @Selector@ for @setPhysicsShape:@
setPhysicsShapeSelector :: Selector '[Id SCNPhysicsShape] ()
setPhysicsShapeSelector = mkSelector "setPhysicsShape:"

-- | @Selector@ for @isResting@
isRestingSelector :: Selector '[] Bool
isRestingSelector = mkSelector "isResting"

-- | @Selector@ for @allowsResting@
allowsRestingSelector :: Selector '[] Bool
allowsRestingSelector = mkSelector "allowsResting"

-- | @Selector@ for @setAllowsResting:@
setAllowsRestingSelector :: Selector '[Bool] ()
setAllowsRestingSelector = mkSelector "setAllowsResting:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector '[] SCNVector3
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector '[SCNVector3] ()
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @angularVelocity@
angularVelocitySelector :: Selector '[] SCNVector4
angularVelocitySelector = mkSelector "angularVelocity"

-- | @Selector@ for @setAngularVelocity:@
setAngularVelocitySelector :: Selector '[SCNVector4] ()
setAngularVelocitySelector = mkSelector "setAngularVelocity:"

-- | @Selector@ for @damping@
dampingSelector :: Selector '[] CDouble
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector '[CDouble] ()
setDampingSelector = mkSelector "setDamping:"

-- | @Selector@ for @angularDamping@
angularDampingSelector :: Selector '[] CDouble
angularDampingSelector = mkSelector "angularDamping"

-- | @Selector@ for @setAngularDamping:@
setAngularDampingSelector :: Selector '[CDouble] ()
setAngularDampingSelector = mkSelector "setAngularDamping:"

-- | @Selector@ for @velocityFactor@
velocityFactorSelector :: Selector '[] SCNVector3
velocityFactorSelector = mkSelector "velocityFactor"

-- | @Selector@ for @setVelocityFactor:@
setVelocityFactorSelector :: Selector '[SCNVector3] ()
setVelocityFactorSelector = mkSelector "setVelocityFactor:"

-- | @Selector@ for @angularVelocityFactor@
angularVelocityFactorSelector :: Selector '[] SCNVector3
angularVelocityFactorSelector = mkSelector "angularVelocityFactor"

-- | @Selector@ for @setAngularVelocityFactor:@
setAngularVelocityFactorSelector :: Selector '[SCNVector3] ()
setAngularVelocityFactorSelector = mkSelector "setAngularVelocityFactor:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CULong
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CULong] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @collisionBitMask@
collisionBitMaskSelector :: Selector '[] CULong
collisionBitMaskSelector = mkSelector "collisionBitMask"

-- | @Selector@ for @setCollisionBitMask:@
setCollisionBitMaskSelector :: Selector '[CULong] ()
setCollisionBitMaskSelector = mkSelector "setCollisionBitMask:"

-- | @Selector@ for @contactTestBitMask@
contactTestBitMaskSelector :: Selector '[] CULong
contactTestBitMaskSelector = mkSelector "contactTestBitMask"

-- | @Selector@ for @setContactTestBitMask:@
setContactTestBitMaskSelector :: Selector '[CULong] ()
setContactTestBitMaskSelector = mkSelector "setContactTestBitMask:"

-- | @Selector@ for @affectedByGravity@
affectedByGravitySelector :: Selector '[] Bool
affectedByGravitySelector = mkSelector "affectedByGravity"

-- | @Selector@ for @setAffectedByGravity:@
setAffectedByGravitySelector :: Selector '[Bool] ()
setAffectedByGravitySelector = mkSelector "setAffectedByGravity:"

-- | @Selector@ for @continuousCollisionDetectionThreshold@
continuousCollisionDetectionThresholdSelector :: Selector '[] CDouble
continuousCollisionDetectionThresholdSelector = mkSelector "continuousCollisionDetectionThreshold"

-- | @Selector@ for @setContinuousCollisionDetectionThreshold:@
setContinuousCollisionDetectionThresholdSelector :: Selector '[CDouble] ()
setContinuousCollisionDetectionThresholdSelector = mkSelector "setContinuousCollisionDetectionThreshold:"

-- | @Selector@ for @centerOfMassOffset@
centerOfMassOffsetSelector :: Selector '[] SCNVector3
centerOfMassOffsetSelector = mkSelector "centerOfMassOffset"

-- | @Selector@ for @setCenterOfMassOffset:@
setCenterOfMassOffsetSelector :: Selector '[SCNVector3] ()
setCenterOfMassOffsetSelector = mkSelector "setCenterOfMassOffset:"

-- | @Selector@ for @linearRestingThreshold@
linearRestingThresholdSelector :: Selector '[] CDouble
linearRestingThresholdSelector = mkSelector "linearRestingThreshold"

-- | @Selector@ for @setLinearRestingThreshold:@
setLinearRestingThresholdSelector :: Selector '[CDouble] ()
setLinearRestingThresholdSelector = mkSelector "setLinearRestingThreshold:"

-- | @Selector@ for @angularRestingThreshold@
angularRestingThresholdSelector :: Selector '[] CDouble
angularRestingThresholdSelector = mkSelector "angularRestingThreshold"

-- | @Selector@ for @setAngularRestingThreshold:@
setAngularRestingThresholdSelector :: Selector '[CDouble] ()
setAngularRestingThresholdSelector = mkSelector "setAngularRestingThreshold:"

