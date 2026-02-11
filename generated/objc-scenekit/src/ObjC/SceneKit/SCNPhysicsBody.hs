{-# LANGUAGE PatternSynonyms #-}
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
  , staticBodySelector
  , dynamicBodySelector
  , kinematicBodySelector
  , bodyWithType_shapeSelector
  , applyForce_impulseSelector
  , applyForce_atPosition_impulseSelector
  , applyTorque_impulseSelector
  , clearAllForcesSelector
  , resetTransformSelector
  , setRestingSelector
  , typeSelector
  , setTypeSelector
  , massSelector
  , setMassSelector
  , momentOfInertiaSelector
  , setMomentOfInertiaSelector
  , usesDefaultMomentOfInertiaSelector
  , setUsesDefaultMomentOfInertiaSelector
  , chargeSelector
  , setChargeSelector
  , frictionSelector
  , setFrictionSelector
  , restitutionSelector
  , setRestitutionSelector
  , rollingFrictionSelector
  , setRollingFrictionSelector
  , physicsShapeSelector
  , setPhysicsShapeSelector
  , isRestingSelector
  , allowsRestingSelector
  , setAllowsRestingSelector
  , velocitySelector
  , setVelocitySelector
  , angularVelocitySelector
  , setAngularVelocitySelector
  , dampingSelector
  , setDampingSelector
  , angularDampingSelector
  , setAngularDampingSelector
  , velocityFactorSelector
  , setVelocityFactorSelector
  , angularVelocityFactorSelector
  , setAngularVelocityFactorSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector
  , collisionBitMaskSelector
  , setCollisionBitMaskSelector
  , contactTestBitMaskSelector
  , setContactTestBitMaskSelector
  , affectedByGravitySelector
  , setAffectedByGravitySelector
  , continuousCollisionDetectionThresholdSelector
  , setContinuousCollisionDetectionThresholdSelector
  , centerOfMassOffsetSelector
  , setCenterOfMassOffsetSelector
  , linearRestingThresholdSelector
  , setLinearRestingThresholdSelector
  , angularRestingThresholdSelector
  , setAngularRestingThresholdSelector

  -- * Enum types
  , SCNPhysicsBodyType(SCNPhysicsBodyType)
  , pattern SCNPhysicsBodyTypeStatic
  , pattern SCNPhysicsBodyTypeDynamic
  , pattern SCNPhysicsBodyTypeKinematic

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
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ staticBody@
staticBody :: IO (Id SCNPhysicsBody)
staticBody  =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMsg cls' (mkSelector "staticBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dynamicBody@
dynamicBody :: IO (Id SCNPhysicsBody)
dynamicBody  =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMsg cls' (mkSelector "dynamicBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kinematicBody@
kinematicBody :: IO (Id SCNPhysicsBody)
kinematicBody  =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    sendClassMsg cls' (mkSelector "kinematicBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ bodyWithType:shape:@
bodyWithType_shape :: IsSCNPhysicsShape shape => SCNPhysicsBodyType -> shape -> IO (Id SCNPhysicsBody)
bodyWithType_shape type_ shape =
  do
    cls' <- getRequiredClass "SCNPhysicsBody"
    withObjCPtr shape $ \raw_shape ->
      sendClassMsg cls' (mkSelector "bodyWithType:shape:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_shape :: Ptr ())] >>= retainedObject . castPtr

-- | @- applyForce:impulse:@
applyForce_impulse :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> Bool -> IO ()
applyForce_impulse scnPhysicsBody  direction impulse =
  sendMsg scnPhysicsBody (mkSelector "applyForce:impulse:") retVoid [argSCNVector3 direction, argCULong (if impulse then 1 else 0)]

-- | @- applyForce:atPosition:impulse:@
applyForce_atPosition_impulse :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> SCNVector3 -> Bool -> IO ()
applyForce_atPosition_impulse scnPhysicsBody  direction position impulse =
  sendMsg scnPhysicsBody (mkSelector "applyForce:atPosition:impulse:") retVoid [argSCNVector3 direction, argSCNVector3 position, argCULong (if impulse then 1 else 0)]

-- | @- applyTorque:impulse:@
applyTorque_impulse :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector4 -> Bool -> IO ()
applyTorque_impulse scnPhysicsBody  torque impulse =
  sendMsg scnPhysicsBody (mkSelector "applyTorque:impulse:") retVoid [argSCNVector4 torque, argCULong (if impulse then 1 else 0)]

-- | @- clearAllForces@
clearAllForces :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO ()
clearAllForces scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "clearAllForces") retVoid []

-- | @- resetTransform@
resetTransform :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO ()
resetTransform scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "resetTransform") retVoid []

-- | @- setResting:@
setResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setResting scnPhysicsBody  resting =
  sendMsg scnPhysicsBody (mkSelector "setResting:") retVoid [argCULong (if resting then 1 else 0)]

-- | @- type@
type_ :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNPhysicsBodyType
type_ scnPhysicsBody  =
  fmap (coerce :: CLong -> SCNPhysicsBodyType) $ sendMsg scnPhysicsBody (mkSelector "type") retCLong []

-- | @- setType:@
setType :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNPhysicsBodyType -> IO ()
setType scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setType:") retVoid [argCLong (coerce value)]

-- | @- mass@
mass :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
mass scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "mass") retCDouble []

-- | @- setMass:@
setMass :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setMass scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setMass:") retVoid [argCDouble (fromIntegral value)]

-- | @- momentOfInertia@
momentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
momentOfInertia scnPhysicsBody  =
  sendMsgStret scnPhysicsBody (mkSelector "momentOfInertia") retSCNVector3 []

-- | @- setMomentOfInertia:@
setMomentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setMomentOfInertia scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setMomentOfInertia:") retVoid [argSCNVector3 value]

-- | @- usesDefaultMomentOfInertia@
usesDefaultMomentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
usesDefaultMomentOfInertia scnPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsBody (mkSelector "usesDefaultMomentOfInertia") retCULong []

-- | @- setUsesDefaultMomentOfInertia:@
setUsesDefaultMomentOfInertia :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setUsesDefaultMomentOfInertia scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setUsesDefaultMomentOfInertia:") retVoid [argCULong (if value then 1 else 0)]

-- | @- charge@
charge :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
charge scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "charge") retCDouble []

-- | @- setCharge:@
setCharge :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setCharge scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setCharge:") retVoid [argCDouble (fromIntegral value)]

-- | @- friction@
friction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
friction scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "friction") retCDouble []

-- | @- setFriction:@
setFriction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setFriction scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setFriction:") retVoid [argCDouble (fromIntegral value)]

-- | @- restitution@
restitution :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
restitution scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "restitution") retCDouble []

-- | @- setRestitution:@
setRestitution :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setRestitution scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setRestitution:") retVoid [argCDouble (fromIntegral value)]

-- | @- rollingFriction@
rollingFriction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
rollingFriction scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "rollingFriction") retCDouble []

-- | @- setRollingFriction:@
setRollingFriction :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setRollingFriction scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setRollingFriction:") retVoid [argCDouble (fromIntegral value)]

-- | @- physicsShape@
physicsShape :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO (Id SCNPhysicsShape)
physicsShape scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "physicsShape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhysicsShape:@
setPhysicsShape :: (IsSCNPhysicsBody scnPhysicsBody, IsSCNPhysicsShape value) => scnPhysicsBody -> value -> IO ()
setPhysicsShape scnPhysicsBody  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnPhysicsBody (mkSelector "setPhysicsShape:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isResting@
isResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
isResting scnPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsBody (mkSelector "isResting") retCULong []

-- | @- allowsResting@
allowsResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
allowsResting scnPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsBody (mkSelector "allowsResting") retCULong []

-- | @- setAllowsResting:@
setAllowsResting :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setAllowsResting scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setAllowsResting:") retVoid [argCULong (if value then 1 else 0)]

-- | @- velocity@
velocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
velocity scnPhysicsBody  =
  sendMsgStret scnPhysicsBody (mkSelector "velocity") retSCNVector3 []

-- | @- setVelocity:@
setVelocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setVelocity scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setVelocity:") retVoid [argSCNVector3 value]

-- | @- angularVelocity@
angularVelocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector4
angularVelocity scnPhysicsBody  =
  sendMsgStret scnPhysicsBody (mkSelector "angularVelocity") retSCNVector4 []

-- | @- setAngularVelocity:@
setAngularVelocity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector4 -> IO ()
setAngularVelocity scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setAngularVelocity:") retVoid [argSCNVector4 value]

-- | @- damping@
damping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
damping scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "damping") retCDouble []

-- | @- setDamping:@
setDamping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setDamping scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setDamping:") retVoid [argCDouble (fromIntegral value)]

-- | @- angularDamping@
angularDamping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
angularDamping scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "angularDamping") retCDouble []

-- | @- setAngularDamping:@
setAngularDamping :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setAngularDamping scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setAngularDamping:") retVoid [argCDouble (fromIntegral value)]

-- | @- velocityFactor@
velocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
velocityFactor scnPhysicsBody  =
  sendMsgStret scnPhysicsBody (mkSelector "velocityFactor") retSCNVector3 []

-- | @- setVelocityFactor:@
setVelocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setVelocityFactor scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setVelocityFactor:") retVoid [argSCNVector3 value]

-- | @- angularVelocityFactor@
angularVelocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
angularVelocityFactor scnPhysicsBody  =
  sendMsgStret scnPhysicsBody (mkSelector "angularVelocityFactor") retSCNVector3 []

-- | @- setAngularVelocityFactor:@
setAngularVelocityFactor :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setAngularVelocityFactor scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setAngularVelocityFactor:") retVoid [argSCNVector3 value]

-- | @- categoryBitMask@
categoryBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CULong
categoryBitMask scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "categoryBitMask") retCULong []

-- | @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CULong -> IO ()
setCategoryBitMask scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

-- | @- collisionBitMask@
collisionBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CULong
collisionBitMask scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "collisionBitMask") retCULong []

-- | @- setCollisionBitMask:@
setCollisionBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CULong -> IO ()
setCollisionBitMask scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setCollisionBitMask:") retVoid [argCULong (fromIntegral value)]

-- | @- contactTestBitMask@
contactTestBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CULong
contactTestBitMask scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "contactTestBitMask") retCULong []

-- | @- setContactTestBitMask:@
setContactTestBitMask :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CULong -> IO ()
setContactTestBitMask scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setContactTestBitMask:") retVoid [argCULong (fromIntegral value)]

-- | @- affectedByGravity@
affectedByGravity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO Bool
affectedByGravity scnPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsBody (mkSelector "affectedByGravity") retCULong []

-- | @- setAffectedByGravity:@
setAffectedByGravity :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> Bool -> IO ()
setAffectedByGravity scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setAffectedByGravity:") retVoid [argCULong (if value then 1 else 0)]

-- | @- continuousCollisionDetectionThreshold@
continuousCollisionDetectionThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
continuousCollisionDetectionThreshold scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "continuousCollisionDetectionThreshold") retCDouble []

-- | @- setContinuousCollisionDetectionThreshold:@
setContinuousCollisionDetectionThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setContinuousCollisionDetectionThreshold scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setContinuousCollisionDetectionThreshold:") retVoid [argCDouble (fromIntegral value)]

-- | @- centerOfMassOffset@
centerOfMassOffset :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO SCNVector3
centerOfMassOffset scnPhysicsBody  =
  sendMsgStret scnPhysicsBody (mkSelector "centerOfMassOffset") retSCNVector3 []

-- | @- setCenterOfMassOffset:@
setCenterOfMassOffset :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> SCNVector3 -> IO ()
setCenterOfMassOffset scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setCenterOfMassOffset:") retVoid [argSCNVector3 value]

-- | @- linearRestingThreshold@
linearRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
linearRestingThreshold scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "linearRestingThreshold") retCDouble []

-- | @- setLinearRestingThreshold:@
setLinearRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setLinearRestingThreshold scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setLinearRestingThreshold:") retVoid [argCDouble (fromIntegral value)]

-- | @- angularRestingThreshold@
angularRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> IO CDouble
angularRestingThreshold scnPhysicsBody  =
  sendMsg scnPhysicsBody (mkSelector "angularRestingThreshold") retCDouble []

-- | @- setAngularRestingThreshold:@
setAngularRestingThreshold :: IsSCNPhysicsBody scnPhysicsBody => scnPhysicsBody -> CDouble -> IO ()
setAngularRestingThreshold scnPhysicsBody  value =
  sendMsg scnPhysicsBody (mkSelector "setAngularRestingThreshold:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @staticBody@
staticBodySelector :: Selector
staticBodySelector = mkSelector "staticBody"

-- | @Selector@ for @dynamicBody@
dynamicBodySelector :: Selector
dynamicBodySelector = mkSelector "dynamicBody"

-- | @Selector@ for @kinematicBody@
kinematicBodySelector :: Selector
kinematicBodySelector = mkSelector "kinematicBody"

-- | @Selector@ for @bodyWithType:shape:@
bodyWithType_shapeSelector :: Selector
bodyWithType_shapeSelector = mkSelector "bodyWithType:shape:"

-- | @Selector@ for @applyForce:impulse:@
applyForce_impulseSelector :: Selector
applyForce_impulseSelector = mkSelector "applyForce:impulse:"

-- | @Selector@ for @applyForce:atPosition:impulse:@
applyForce_atPosition_impulseSelector :: Selector
applyForce_atPosition_impulseSelector = mkSelector "applyForce:atPosition:impulse:"

-- | @Selector@ for @applyTorque:impulse:@
applyTorque_impulseSelector :: Selector
applyTorque_impulseSelector = mkSelector "applyTorque:impulse:"

-- | @Selector@ for @clearAllForces@
clearAllForcesSelector :: Selector
clearAllForcesSelector = mkSelector "clearAllForces"

-- | @Selector@ for @resetTransform@
resetTransformSelector :: Selector
resetTransformSelector = mkSelector "resetTransform"

-- | @Selector@ for @setResting:@
setRestingSelector :: Selector
setRestingSelector = mkSelector "setResting:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @mass@
massSelector :: Selector
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @momentOfInertia@
momentOfInertiaSelector :: Selector
momentOfInertiaSelector = mkSelector "momentOfInertia"

-- | @Selector@ for @setMomentOfInertia:@
setMomentOfInertiaSelector :: Selector
setMomentOfInertiaSelector = mkSelector "setMomentOfInertia:"

-- | @Selector@ for @usesDefaultMomentOfInertia@
usesDefaultMomentOfInertiaSelector :: Selector
usesDefaultMomentOfInertiaSelector = mkSelector "usesDefaultMomentOfInertia"

-- | @Selector@ for @setUsesDefaultMomentOfInertia:@
setUsesDefaultMomentOfInertiaSelector :: Selector
setUsesDefaultMomentOfInertiaSelector = mkSelector "setUsesDefaultMomentOfInertia:"

-- | @Selector@ for @charge@
chargeSelector :: Selector
chargeSelector = mkSelector "charge"

-- | @Selector@ for @setCharge:@
setChargeSelector :: Selector
setChargeSelector = mkSelector "setCharge:"

-- | @Selector@ for @friction@
frictionSelector :: Selector
frictionSelector = mkSelector "friction"

-- | @Selector@ for @setFriction:@
setFrictionSelector :: Selector
setFrictionSelector = mkSelector "setFriction:"

-- | @Selector@ for @restitution@
restitutionSelector :: Selector
restitutionSelector = mkSelector "restitution"

-- | @Selector@ for @setRestitution:@
setRestitutionSelector :: Selector
setRestitutionSelector = mkSelector "setRestitution:"

-- | @Selector@ for @rollingFriction@
rollingFrictionSelector :: Selector
rollingFrictionSelector = mkSelector "rollingFriction"

-- | @Selector@ for @setRollingFriction:@
setRollingFrictionSelector :: Selector
setRollingFrictionSelector = mkSelector "setRollingFriction:"

-- | @Selector@ for @physicsShape@
physicsShapeSelector :: Selector
physicsShapeSelector = mkSelector "physicsShape"

-- | @Selector@ for @setPhysicsShape:@
setPhysicsShapeSelector :: Selector
setPhysicsShapeSelector = mkSelector "setPhysicsShape:"

-- | @Selector@ for @isResting@
isRestingSelector :: Selector
isRestingSelector = mkSelector "isResting"

-- | @Selector@ for @allowsResting@
allowsRestingSelector :: Selector
allowsRestingSelector = mkSelector "allowsResting"

-- | @Selector@ for @setAllowsResting:@
setAllowsRestingSelector :: Selector
setAllowsRestingSelector = mkSelector "setAllowsResting:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @angularVelocity@
angularVelocitySelector :: Selector
angularVelocitySelector = mkSelector "angularVelocity"

-- | @Selector@ for @setAngularVelocity:@
setAngularVelocitySelector :: Selector
setAngularVelocitySelector = mkSelector "setAngularVelocity:"

-- | @Selector@ for @damping@
dampingSelector :: Selector
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector
setDampingSelector = mkSelector "setDamping:"

-- | @Selector@ for @angularDamping@
angularDampingSelector :: Selector
angularDampingSelector = mkSelector "angularDamping"

-- | @Selector@ for @setAngularDamping:@
setAngularDampingSelector :: Selector
setAngularDampingSelector = mkSelector "setAngularDamping:"

-- | @Selector@ for @velocityFactor@
velocityFactorSelector :: Selector
velocityFactorSelector = mkSelector "velocityFactor"

-- | @Selector@ for @setVelocityFactor:@
setVelocityFactorSelector :: Selector
setVelocityFactorSelector = mkSelector "setVelocityFactor:"

-- | @Selector@ for @angularVelocityFactor@
angularVelocityFactorSelector :: Selector
angularVelocityFactorSelector = mkSelector "angularVelocityFactor"

-- | @Selector@ for @setAngularVelocityFactor:@
setAngularVelocityFactorSelector :: Selector
setAngularVelocityFactorSelector = mkSelector "setAngularVelocityFactor:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @collisionBitMask@
collisionBitMaskSelector :: Selector
collisionBitMaskSelector = mkSelector "collisionBitMask"

-- | @Selector@ for @setCollisionBitMask:@
setCollisionBitMaskSelector :: Selector
setCollisionBitMaskSelector = mkSelector "setCollisionBitMask:"

-- | @Selector@ for @contactTestBitMask@
contactTestBitMaskSelector :: Selector
contactTestBitMaskSelector = mkSelector "contactTestBitMask"

-- | @Selector@ for @setContactTestBitMask:@
setContactTestBitMaskSelector :: Selector
setContactTestBitMaskSelector = mkSelector "setContactTestBitMask:"

-- | @Selector@ for @affectedByGravity@
affectedByGravitySelector :: Selector
affectedByGravitySelector = mkSelector "affectedByGravity"

-- | @Selector@ for @setAffectedByGravity:@
setAffectedByGravitySelector :: Selector
setAffectedByGravitySelector = mkSelector "setAffectedByGravity:"

-- | @Selector@ for @continuousCollisionDetectionThreshold@
continuousCollisionDetectionThresholdSelector :: Selector
continuousCollisionDetectionThresholdSelector = mkSelector "continuousCollisionDetectionThreshold"

-- | @Selector@ for @setContinuousCollisionDetectionThreshold:@
setContinuousCollisionDetectionThresholdSelector :: Selector
setContinuousCollisionDetectionThresholdSelector = mkSelector "setContinuousCollisionDetectionThreshold:"

-- | @Selector@ for @centerOfMassOffset@
centerOfMassOffsetSelector :: Selector
centerOfMassOffsetSelector = mkSelector "centerOfMassOffset"

-- | @Selector@ for @setCenterOfMassOffset:@
setCenterOfMassOffsetSelector :: Selector
setCenterOfMassOffsetSelector = mkSelector "setCenterOfMassOffset:"

-- | @Selector@ for @linearRestingThreshold@
linearRestingThresholdSelector :: Selector
linearRestingThresholdSelector = mkSelector "linearRestingThreshold"

-- | @Selector@ for @setLinearRestingThreshold:@
setLinearRestingThresholdSelector :: Selector
setLinearRestingThresholdSelector = mkSelector "setLinearRestingThreshold:"

-- | @Selector@ for @angularRestingThreshold@
angularRestingThresholdSelector :: Selector
angularRestingThresholdSelector = mkSelector "angularRestingThreshold"

-- | @Selector@ for @setAngularRestingThreshold:@
setAngularRestingThresholdSelector :: Selector
setAngularRestingThresholdSelector = mkSelector "setAngularRestingThreshold:"

