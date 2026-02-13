{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A SpriteKit physics body. These are the physical representations of your nodes. These specify the area and mass and any collision masking needed.
--
-- All bodies have zero, one or more shapes that define its area. A body with no shapes is ethereal and does not collide with other bodies.
--
-- Generated bindings for @SKPhysicsBody@.
module ObjC.SpriteKit.SKPhysicsBody
  ( SKPhysicsBody
  , IsSKPhysicsBody(..)
  , bodyWithCircleOfRadius
  , bodyWithPolygonFromPath
  , bodyWithEdgeChainFromPath
  , bodyWithEdgeLoopFromPath
  , bodyWithBodies
  , applyTorque
  , applyAngularImpulse
  , allContactedBodies
  , dynamic
  , setDynamic
  , usesPreciseCollisionDetection
  , setUsesPreciseCollisionDetection
  , allowsRotation
  , setAllowsRotation
  , pinned
  , setPinned
  , resting
  , setResting
  , friction
  , setFriction
  , charge
  , setCharge
  , restitution
  , setRestitution
  , linearDamping
  , setLinearDamping
  , angularDamping
  , setAngularDamping
  , density
  , setDensity
  , mass
  , setMass
  , area
  , affectedByGravity
  , setAffectedByGravity
  , fieldBitMask
  , setFieldBitMask
  , categoryBitMask
  , setCategoryBitMask
  , collisionBitMask
  , setCollisionBitMask
  , contactTestBitMask
  , setContactTestBitMask
  , joints
  , node
  , angularVelocity
  , setAngularVelocity
  , affectedByGravitySelector
  , allContactedBodiesSelector
  , allowsRotationSelector
  , angularDampingSelector
  , angularVelocitySelector
  , applyAngularImpulseSelector
  , applyTorqueSelector
  , areaSelector
  , bodyWithBodiesSelector
  , bodyWithCircleOfRadiusSelector
  , bodyWithEdgeChainFromPathSelector
  , bodyWithEdgeLoopFromPathSelector
  , bodyWithPolygonFromPathSelector
  , categoryBitMaskSelector
  , chargeSelector
  , collisionBitMaskSelector
  , contactTestBitMaskSelector
  , densitySelector
  , dynamicSelector
  , fieldBitMaskSelector
  , frictionSelector
  , jointsSelector
  , linearDampingSelector
  , massSelector
  , nodeSelector
  , pinnedSelector
  , restingSelector
  , restitutionSelector
  , setAffectedByGravitySelector
  , setAllowsRotationSelector
  , setAngularDampingSelector
  , setAngularVelocitySelector
  , setCategoryBitMaskSelector
  , setChargeSelector
  , setCollisionBitMaskSelector
  , setContactTestBitMaskSelector
  , setDensitySelector
  , setDynamicSelector
  , setFieldBitMaskSelector
  , setFrictionSelector
  , setLinearDampingSelector
  , setMassSelector
  , setPinnedSelector
  , setRestingSelector
  , setRestitutionSelector
  , setUsesPreciseCollisionDetectionSelector
  , usesPreciseCollisionDetectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a circle of radius r centered at the node's origin.
--
-- @r@ — the radius in points
--
-- ObjC selector: @+ bodyWithCircleOfRadius:@
bodyWithCircleOfRadius :: CDouble -> IO (Id SKPhysicsBody)
bodyWithCircleOfRadius r =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMessage cls' bodyWithCircleOfRadiusSelector r

-- | The path must represent a convex or concave polygon with counter clockwise winding and no self intersection. Positions are relative to the node's origin.
--
-- @path@ — the path to use
--
-- ObjC selector: @+ bodyWithPolygonFromPath:@
bodyWithPolygonFromPath :: RawId -> IO (Id SKPhysicsBody)
bodyWithPolygonFromPath path =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMessage cls' bodyWithPolygonFromPathSelector path

-- | Creates an edge chain from a path. The path must have no self intersection. Edges have no volume and are intended to be used to create static environments. Edges can collide with bodies of volume, but not with each other.
--
-- @path@ — the path to use
--
-- ObjC selector: @+ bodyWithEdgeChainFromPath:@
bodyWithEdgeChainFromPath :: RawId -> IO (Id SKPhysicsBody)
bodyWithEdgeChainFromPath path =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMessage cls' bodyWithEdgeChainFromPathSelector path

-- | Creates an edge loop from a path. A loop is automatically created by joining the last point to the first. The path must have no self intersection. Edges have no volume and are intended to be used to create static environments. Edges can collide with body's of volume, but not with each other.
--
-- @path@ — the path to use
--
-- ObjC selector: @+ bodyWithEdgeLoopFromPath:@
bodyWithEdgeLoopFromPath :: RawId -> IO (Id SKPhysicsBody)
bodyWithEdgeLoopFromPath path =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMessage cls' bodyWithEdgeLoopFromPathSelector path

-- | Creates an compound body that is the union of the bodies used to create it.
--
-- ObjC selector: @+ bodyWithBodies:@
bodyWithBodies :: IsNSArray bodies => bodies -> IO (Id SKPhysicsBody)
bodyWithBodies bodies =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMessage cls' bodyWithBodiesSelector (toNSArray bodies)

-- | @- applyTorque:@
applyTorque :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
applyTorque skPhysicsBody torque =
  sendMessage skPhysicsBody applyTorqueSelector torque

-- | @- applyAngularImpulse:@
applyAngularImpulse :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
applyAngularImpulse skPhysicsBody impulse =
  sendMessage skPhysicsBody applyAngularImpulseSelector impulse

-- | @- allContactedBodies@
allContactedBodies :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO (Id NSArray)
allContactedBodies skPhysicsBody =
  sendMessage skPhysicsBody allContactedBodiesSelector

-- | @- dynamic@
dynamic :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
dynamic skPhysicsBody =
  sendMessage skPhysicsBody dynamicSelector

-- | @- setDynamic:@
setDynamic :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setDynamic skPhysicsBody value =
  sendMessage skPhysicsBody setDynamicSelector value

-- | @- usesPreciseCollisionDetection@
usesPreciseCollisionDetection :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
usesPreciseCollisionDetection skPhysicsBody =
  sendMessage skPhysicsBody usesPreciseCollisionDetectionSelector

-- | @- setUsesPreciseCollisionDetection:@
setUsesPreciseCollisionDetection :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setUsesPreciseCollisionDetection skPhysicsBody value =
  sendMessage skPhysicsBody setUsesPreciseCollisionDetectionSelector value

-- | @- allowsRotation@
allowsRotation :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
allowsRotation skPhysicsBody =
  sendMessage skPhysicsBody allowsRotationSelector

-- | @- setAllowsRotation:@
setAllowsRotation :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setAllowsRotation skPhysicsBody value =
  sendMessage skPhysicsBody setAllowsRotationSelector value

-- | @- pinned@
pinned :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
pinned skPhysicsBody =
  sendMessage skPhysicsBody pinnedSelector

-- | @- setPinned:@
setPinned :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setPinned skPhysicsBody value =
  sendMessage skPhysicsBody setPinnedSelector value

-- | If the physics simulation has determined that this body is at rest it may set the resting property to YES. Resting bodies do not participate in the simulation until some collision with a non-resting  object, or an impulse is applied, that unrests it. If all bodies in the world are resting then the simulation as a whole is "at rest".
--
-- ObjC selector: @- resting@
resting :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
resting skPhysicsBody =
  sendMessage skPhysicsBody restingSelector

-- | If the physics simulation has determined that this body is at rest it may set the resting property to YES. Resting bodies do not participate in the simulation until some collision with a non-resting  object, or an impulse is applied, that unrests it. If all bodies in the world are resting then the simulation as a whole is "at rest".
--
-- ObjC selector: @- setResting:@
setResting :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setResting skPhysicsBody value =
  sendMessage skPhysicsBody setRestingSelector value

-- | Determines the 'roughness' for the surface of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- friction@
friction :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
friction skPhysicsBody =
  sendMessage skPhysicsBody frictionSelector

-- | Determines the 'roughness' for the surface of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- setFriction:@
setFriction :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setFriction skPhysicsBody value =
  sendMessage skPhysicsBody setFrictionSelector value

-- | Specifies the charge on the body. Charge determines the degree to which a body is affected by electric and magnetic fields. Note that this is a unitless quantity, it is up to the developer to set charge and field strength appropriately. Defaults to 0.0
--
-- ObjC selector: @- charge@
charge :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
charge skPhysicsBody =
  sendMessage skPhysicsBody chargeSelector

-- | Specifies the charge on the body. Charge determines the degree to which a body is affected by electric and magnetic fields. Note that this is a unitless quantity, it is up to the developer to set charge and field strength appropriately. Defaults to 0.0
--
-- ObjC selector: @- setCharge:@
setCharge :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setCharge skPhysicsBody value =
  sendMessage skPhysicsBody setChargeSelector value

-- | Determines the 'bounciness' of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- restitution@
restitution :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
restitution skPhysicsBody =
  sendMessage skPhysicsBody restitutionSelector

-- | Determines the 'bounciness' of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- setRestitution:@
setRestitution :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setRestitution skPhysicsBody value =
  sendMessage skPhysicsBody setRestitutionSelector value

-- | Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Used in conjunction with per frame impulses, an object can be made to move at a constant speed. For example, if an object 64 points in size and default density and a linearDamping of 25 will slide across the screen in a few seconds if an impulse of magnitude 10 is applied every update.
--
-- ObjC selector: @- linearDamping@
linearDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
linearDamping skPhysicsBody =
  sendMessage skPhysicsBody linearDampingSelector

-- | Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Used in conjunction with per frame impulses, an object can be made to move at a constant speed. For example, if an object 64 points in size and default density and a linearDamping of 25 will slide across the screen in a few seconds if an impulse of magnitude 10 is applied every update.
--
-- ObjC selector: @- setLinearDamping:@
setLinearDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setLinearDamping skPhysicsBody value =
  sendMessage skPhysicsBody setLinearDampingSelector value

-- | Optionally reduce the body's angular velocity each frame to simulate rotational friction. (0.0 - 1.0). Defaults to 0.1
--
-- ObjC selector: @- angularDamping@
angularDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
angularDamping skPhysicsBody =
  sendMessage skPhysicsBody angularDampingSelector

-- | Optionally reduce the body's angular velocity each frame to simulate rotational friction. (0.0 - 1.0). Defaults to 0.1
--
-- ObjC selector: @- setAngularDamping:@
setAngularDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setAngularDamping skPhysicsBody value =
  sendMessage skPhysicsBody setAngularDampingSelector value

-- | The density of the body.
--
-- The unit is arbitrary, as long as the relative densities are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- density@
density :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
density skPhysicsBody =
  sendMessage skPhysicsBody densitySelector

-- | The density of the body.
--
-- The unit is arbitrary, as long as the relative densities are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- setDensity:@
setDensity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setDensity skPhysicsBody value =
  sendMessage skPhysicsBody setDensitySelector value

-- | The mass of the body.
--
-- The unit is arbitrary, as long as the relative masses are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- mass@
mass :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
mass skPhysicsBody =
  sendMessage skPhysicsBody massSelector

-- | The mass of the body.
--
-- The unit is arbitrary, as long as the relative masses are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- setMass:@
setMass :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setMass skPhysicsBody value =
  sendMessage skPhysicsBody setMassSelector value

-- | The area of the body.
--
-- The unit is arbitrary, as long as the relative areas are consistent throughout the application.
--
-- ObjC selector: @- area@
area :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
area skPhysicsBody =
  sendMessage skPhysicsBody areaSelector

-- | Bodies are affected by field forces such as gravity if this property is set and the field's category mask is set appropriately. The default value is YES.
--
-- If this is set a force is applied to the object based on the mass. Set the field force vector in the scene to modify the strength of the force.
--
-- ObjC selector: @- affectedByGravity@
affectedByGravity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
affectedByGravity skPhysicsBody =
  sendMessage skPhysicsBody affectedByGravitySelector

-- | Bodies are affected by field forces such as gravity if this property is set and the field's category mask is set appropriately. The default value is YES.
--
-- If this is set a force is applied to the object based on the mass. Set the field force vector in the scene to modify the strength of the force.
--
-- ObjC selector: @- setAffectedByGravity:@
setAffectedByGravity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setAffectedByGravity skPhysicsBody value =
  sendMessage skPhysicsBody setAffectedByGravitySelector value

-- | Defines what logical 'categories' of fields this body responds to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- fieldBitMask@
fieldBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
fieldBitMask skPhysicsBody =
  sendMessage skPhysicsBody fieldBitMaskSelector

-- | Defines what logical 'categories' of fields this body responds to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- setFieldBitMask:@
setFieldBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setFieldBitMask skPhysicsBody value =
  sendMessage skPhysicsBody setFieldBitMaskSelector value

-- | Defines what logical 'categories' this body belongs to. Defaults to all bits set (all categories).
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
categoryBitMask skPhysicsBody =
  sendMessage skPhysicsBody categoryBitMaskSelector

-- | Defines what logical 'categories' this body belongs to. Defaults to all bits set (all categories).
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setCategoryBitMask skPhysicsBody value =
  sendMessage skPhysicsBody setCategoryBitMaskSelector value

-- | Defines what logical 'categories' of bodies this body responds to collisions with. Defaults to all bits set (all categories).
--
-- ObjC selector: @- collisionBitMask@
collisionBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
collisionBitMask skPhysicsBody =
  sendMessage skPhysicsBody collisionBitMaskSelector

-- | Defines what logical 'categories' of bodies this body responds to collisions with. Defaults to all bits set (all categories).
--
-- ObjC selector: @- setCollisionBitMask:@
setCollisionBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setCollisionBitMask skPhysicsBody value =
  sendMessage skPhysicsBody setCollisionBitMaskSelector value

-- | Defines what logical 'categories' of bodies this body generates intersection notifications with. Defaults to all bits cleared (no categories).
--
-- ObjC selector: @- contactTestBitMask@
contactTestBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
contactTestBitMask skPhysicsBody =
  sendMessage skPhysicsBody contactTestBitMaskSelector

-- | Defines what logical 'categories' of bodies this body generates intersection notifications with. Defaults to all bits cleared (no categories).
--
-- ObjC selector: @- setContactTestBitMask:@
setContactTestBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setContactTestBitMask skPhysicsBody value =
  sendMessage skPhysicsBody setContactTestBitMaskSelector value

-- | @- joints@
joints :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO (Id NSArray)
joints skPhysicsBody =
  sendMessage skPhysicsBody jointsSelector

-- | The representedObject this physicsBody is currently bound to, or nil if it is not.
--
-- ObjC selector: @- node@
node :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO (Id SKNode)
node skPhysicsBody =
  sendMessage skPhysicsBody nodeSelector

-- | @- angularVelocity@
angularVelocity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
angularVelocity skPhysicsBody =
  sendMessage skPhysicsBody angularVelocitySelector

-- | @- setAngularVelocity:@
setAngularVelocity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setAngularVelocity skPhysicsBody value =
  sendMessage skPhysicsBody setAngularVelocitySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyWithCircleOfRadius:@
bodyWithCircleOfRadiusSelector :: Selector '[CDouble] (Id SKPhysicsBody)
bodyWithCircleOfRadiusSelector = mkSelector "bodyWithCircleOfRadius:"

-- | @Selector@ for @bodyWithPolygonFromPath:@
bodyWithPolygonFromPathSelector :: Selector '[RawId] (Id SKPhysicsBody)
bodyWithPolygonFromPathSelector = mkSelector "bodyWithPolygonFromPath:"

-- | @Selector@ for @bodyWithEdgeChainFromPath:@
bodyWithEdgeChainFromPathSelector :: Selector '[RawId] (Id SKPhysicsBody)
bodyWithEdgeChainFromPathSelector = mkSelector "bodyWithEdgeChainFromPath:"

-- | @Selector@ for @bodyWithEdgeLoopFromPath:@
bodyWithEdgeLoopFromPathSelector :: Selector '[RawId] (Id SKPhysicsBody)
bodyWithEdgeLoopFromPathSelector = mkSelector "bodyWithEdgeLoopFromPath:"

-- | @Selector@ for @bodyWithBodies:@
bodyWithBodiesSelector :: Selector '[Id NSArray] (Id SKPhysicsBody)
bodyWithBodiesSelector = mkSelector "bodyWithBodies:"

-- | @Selector@ for @applyTorque:@
applyTorqueSelector :: Selector '[CDouble] ()
applyTorqueSelector = mkSelector "applyTorque:"

-- | @Selector@ for @applyAngularImpulse:@
applyAngularImpulseSelector :: Selector '[CDouble] ()
applyAngularImpulseSelector = mkSelector "applyAngularImpulse:"

-- | @Selector@ for @allContactedBodies@
allContactedBodiesSelector :: Selector '[] (Id NSArray)
allContactedBodiesSelector = mkSelector "allContactedBodies"

-- | @Selector@ for @dynamic@
dynamicSelector :: Selector '[] Bool
dynamicSelector = mkSelector "dynamic"

-- | @Selector@ for @setDynamic:@
setDynamicSelector :: Selector '[Bool] ()
setDynamicSelector = mkSelector "setDynamic:"

-- | @Selector@ for @usesPreciseCollisionDetection@
usesPreciseCollisionDetectionSelector :: Selector '[] Bool
usesPreciseCollisionDetectionSelector = mkSelector "usesPreciseCollisionDetection"

-- | @Selector@ for @setUsesPreciseCollisionDetection:@
setUsesPreciseCollisionDetectionSelector :: Selector '[Bool] ()
setUsesPreciseCollisionDetectionSelector = mkSelector "setUsesPreciseCollisionDetection:"

-- | @Selector@ for @allowsRotation@
allowsRotationSelector :: Selector '[] Bool
allowsRotationSelector = mkSelector "allowsRotation"

-- | @Selector@ for @setAllowsRotation:@
setAllowsRotationSelector :: Selector '[Bool] ()
setAllowsRotationSelector = mkSelector "setAllowsRotation:"

-- | @Selector@ for @pinned@
pinnedSelector :: Selector '[] Bool
pinnedSelector = mkSelector "pinned"

-- | @Selector@ for @setPinned:@
setPinnedSelector :: Selector '[Bool] ()
setPinnedSelector = mkSelector "setPinned:"

-- | @Selector@ for @resting@
restingSelector :: Selector '[] Bool
restingSelector = mkSelector "resting"

-- | @Selector@ for @setResting:@
setRestingSelector :: Selector '[Bool] ()
setRestingSelector = mkSelector "setResting:"

-- | @Selector@ for @friction@
frictionSelector :: Selector '[] CDouble
frictionSelector = mkSelector "friction"

-- | @Selector@ for @setFriction:@
setFrictionSelector :: Selector '[CDouble] ()
setFrictionSelector = mkSelector "setFriction:"

-- | @Selector@ for @charge@
chargeSelector :: Selector '[] CDouble
chargeSelector = mkSelector "charge"

-- | @Selector@ for @setCharge:@
setChargeSelector :: Selector '[CDouble] ()
setChargeSelector = mkSelector "setCharge:"

-- | @Selector@ for @restitution@
restitutionSelector :: Selector '[] CDouble
restitutionSelector = mkSelector "restitution"

-- | @Selector@ for @setRestitution:@
setRestitutionSelector :: Selector '[CDouble] ()
setRestitutionSelector = mkSelector "setRestitution:"

-- | @Selector@ for @linearDamping@
linearDampingSelector :: Selector '[] CDouble
linearDampingSelector = mkSelector "linearDamping"

-- | @Selector@ for @setLinearDamping:@
setLinearDampingSelector :: Selector '[CDouble] ()
setLinearDampingSelector = mkSelector "setLinearDamping:"

-- | @Selector@ for @angularDamping@
angularDampingSelector :: Selector '[] CDouble
angularDampingSelector = mkSelector "angularDamping"

-- | @Selector@ for @setAngularDamping:@
setAngularDampingSelector :: Selector '[CDouble] ()
setAngularDampingSelector = mkSelector "setAngularDamping:"

-- | @Selector@ for @density@
densitySelector :: Selector '[] CDouble
densitySelector = mkSelector "density"

-- | @Selector@ for @setDensity:@
setDensitySelector :: Selector '[CDouble] ()
setDensitySelector = mkSelector "setDensity:"

-- | @Selector@ for @mass@
massSelector :: Selector '[] CDouble
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector '[CDouble] ()
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @area@
areaSelector :: Selector '[] CDouble
areaSelector = mkSelector "area"

-- | @Selector@ for @affectedByGravity@
affectedByGravitySelector :: Selector '[] Bool
affectedByGravitySelector = mkSelector "affectedByGravity"

-- | @Selector@ for @setAffectedByGravity:@
setAffectedByGravitySelector :: Selector '[Bool] ()
setAffectedByGravitySelector = mkSelector "setAffectedByGravity:"

-- | @Selector@ for @fieldBitMask@
fieldBitMaskSelector :: Selector '[] CUInt
fieldBitMaskSelector = mkSelector "fieldBitMask"

-- | @Selector@ for @setFieldBitMask:@
setFieldBitMaskSelector :: Selector '[CUInt] ()
setFieldBitMaskSelector = mkSelector "setFieldBitMask:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CUInt
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CUInt] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @collisionBitMask@
collisionBitMaskSelector :: Selector '[] CUInt
collisionBitMaskSelector = mkSelector "collisionBitMask"

-- | @Selector@ for @setCollisionBitMask:@
setCollisionBitMaskSelector :: Selector '[CUInt] ()
setCollisionBitMaskSelector = mkSelector "setCollisionBitMask:"

-- | @Selector@ for @contactTestBitMask@
contactTestBitMaskSelector :: Selector '[] CUInt
contactTestBitMaskSelector = mkSelector "contactTestBitMask"

-- | @Selector@ for @setContactTestBitMask:@
setContactTestBitMaskSelector :: Selector '[CUInt] ()
setContactTestBitMaskSelector = mkSelector "setContactTestBitMask:"

-- | @Selector@ for @joints@
jointsSelector :: Selector '[] (Id NSArray)
jointsSelector = mkSelector "joints"

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id SKNode)
nodeSelector = mkSelector "node"

-- | @Selector@ for @angularVelocity@
angularVelocitySelector :: Selector '[] CDouble
angularVelocitySelector = mkSelector "angularVelocity"

-- | @Selector@ for @setAngularVelocity:@
setAngularVelocitySelector :: Selector '[CDouble] ()
setAngularVelocitySelector = mkSelector "setAngularVelocity:"

