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
  , bodyWithCircleOfRadiusSelector
  , bodyWithPolygonFromPathSelector
  , bodyWithEdgeChainFromPathSelector
  , bodyWithEdgeLoopFromPathSelector
  , bodyWithBodiesSelector
  , applyTorqueSelector
  , applyAngularImpulseSelector
  , allContactedBodiesSelector
  , dynamicSelector
  , setDynamicSelector
  , usesPreciseCollisionDetectionSelector
  , setUsesPreciseCollisionDetectionSelector
  , allowsRotationSelector
  , setAllowsRotationSelector
  , pinnedSelector
  , setPinnedSelector
  , restingSelector
  , setRestingSelector
  , frictionSelector
  , setFrictionSelector
  , chargeSelector
  , setChargeSelector
  , restitutionSelector
  , setRestitutionSelector
  , linearDampingSelector
  , setLinearDampingSelector
  , angularDampingSelector
  , setAngularDampingSelector
  , densitySelector
  , setDensitySelector
  , massSelector
  , setMassSelector
  , areaSelector
  , affectedByGravitySelector
  , setAffectedByGravitySelector
  , fieldBitMaskSelector
  , setFieldBitMaskSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector
  , collisionBitMaskSelector
  , setCollisionBitMaskSelector
  , contactTestBitMaskSelector
  , setContactTestBitMaskSelector
  , jointsSelector
  , nodeSelector
  , angularVelocitySelector
  , setAngularVelocitySelector


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
    sendClassMsg cls' (mkSelector "bodyWithCircleOfRadius:") (retPtr retVoid) [argCDouble (fromIntegral r)] >>= retainedObject . castPtr

-- | The path must represent a convex or concave polygon with counter clockwise winding and no self intersection. Positions are relative to the node's origin.
--
-- @path@ — the path to use
--
-- ObjC selector: @+ bodyWithPolygonFromPath:@
bodyWithPolygonFromPath :: RawId -> IO (Id SKPhysicsBody)
bodyWithPolygonFromPath path =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMsg cls' (mkSelector "bodyWithPolygonFromPath:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an edge chain from a path. The path must have no self intersection. Edges have no volume and are intended to be used to create static environments. Edges can collide with bodies of volume, but not with each other.
--
-- @path@ — the path to use
--
-- ObjC selector: @+ bodyWithEdgeChainFromPath:@
bodyWithEdgeChainFromPath :: RawId -> IO (Id SKPhysicsBody)
bodyWithEdgeChainFromPath path =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMsg cls' (mkSelector "bodyWithEdgeChainFromPath:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an edge loop from a path. A loop is automatically created by joining the last point to the first. The path must have no self intersection. Edges have no volume and are intended to be used to create static environments. Edges can collide with body's of volume, but not with each other.
--
-- @path@ — the path to use
--
-- ObjC selector: @+ bodyWithEdgeLoopFromPath:@
bodyWithEdgeLoopFromPath :: RawId -> IO (Id SKPhysicsBody)
bodyWithEdgeLoopFromPath path =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    sendClassMsg cls' (mkSelector "bodyWithEdgeLoopFromPath:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an compound body that is the union of the bodies used to create it.
--
-- ObjC selector: @+ bodyWithBodies:@
bodyWithBodies :: IsNSArray bodies => bodies -> IO (Id SKPhysicsBody)
bodyWithBodies bodies =
  do
    cls' <- getRequiredClass "SKPhysicsBody"
    withObjCPtr bodies $ \raw_bodies ->
      sendClassMsg cls' (mkSelector "bodyWithBodies:") (retPtr retVoid) [argPtr (castPtr raw_bodies :: Ptr ())] >>= retainedObject . castPtr

-- | @- applyTorque:@
applyTorque :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
applyTorque skPhysicsBody  torque =
  sendMsg skPhysicsBody (mkSelector "applyTorque:") retVoid [argCDouble (fromIntegral torque)]

-- | @- applyAngularImpulse:@
applyAngularImpulse :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
applyAngularImpulse skPhysicsBody  impulse =
  sendMsg skPhysicsBody (mkSelector "applyAngularImpulse:") retVoid [argCDouble (fromIntegral impulse)]

-- | @- allContactedBodies@
allContactedBodies :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO (Id NSArray)
allContactedBodies skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "allContactedBodies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dynamic@
dynamic :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
dynamic skPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsBody (mkSelector "dynamic") retCULong []

-- | @- setDynamic:@
setDynamic :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setDynamic skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setDynamic:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesPreciseCollisionDetection@
usesPreciseCollisionDetection :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
usesPreciseCollisionDetection skPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsBody (mkSelector "usesPreciseCollisionDetection") retCULong []

-- | @- setUsesPreciseCollisionDetection:@
setUsesPreciseCollisionDetection :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setUsesPreciseCollisionDetection skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setUsesPreciseCollisionDetection:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsRotation@
allowsRotation :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
allowsRotation skPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsBody (mkSelector "allowsRotation") retCULong []

-- | @- setAllowsRotation:@
setAllowsRotation :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setAllowsRotation skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setAllowsRotation:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pinned@
pinned :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
pinned skPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsBody (mkSelector "pinned") retCULong []

-- | @- setPinned:@
setPinned :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setPinned skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setPinned:") retVoid [argCULong (if value then 1 else 0)]

-- | If the physics simulation has determined that this body is at rest it may set the resting property to YES. Resting bodies do not participate in the simulation until some collision with a non-resting  object, or an impulse is applied, that unrests it. If all bodies in the world are resting then the simulation as a whole is "at rest".
--
-- ObjC selector: @- resting@
resting :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
resting skPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsBody (mkSelector "resting") retCULong []

-- | If the physics simulation has determined that this body is at rest it may set the resting property to YES. Resting bodies do not participate in the simulation until some collision with a non-resting  object, or an impulse is applied, that unrests it. If all bodies in the world are resting then the simulation as a whole is "at rest".
--
-- ObjC selector: @- setResting:@
setResting :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setResting skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setResting:") retVoid [argCULong (if value then 1 else 0)]

-- | Determines the 'roughness' for the surface of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- friction@
friction :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
friction skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "friction") retCDouble []

-- | Determines the 'roughness' for the surface of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- setFriction:@
setFriction :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setFriction skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setFriction:") retVoid [argCDouble (fromIntegral value)]

-- | Specifies the charge on the body. Charge determines the degree to which a body is affected by electric and magnetic fields. Note that this is a unitless quantity, it is up to the developer to set charge and field strength appropriately. Defaults to 0.0
--
-- ObjC selector: @- charge@
charge :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
charge skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "charge") retCDouble []

-- | Specifies the charge on the body. Charge determines the degree to which a body is affected by electric and magnetic fields. Note that this is a unitless quantity, it is up to the developer to set charge and field strength appropriately. Defaults to 0.0
--
-- ObjC selector: @- setCharge:@
setCharge :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setCharge skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setCharge:") retVoid [argCDouble (fromIntegral value)]

-- | Determines the 'bounciness' of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- restitution@
restitution :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
restitution skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "restitution") retCDouble []

-- | Determines the 'bounciness' of the physics body (0.0 - 1.0). Defaults to 0.2
--
-- ObjC selector: @- setRestitution:@
setRestitution :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setRestitution skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setRestitution:") retVoid [argCDouble (fromIntegral value)]

-- | Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Used in conjunction with per frame impulses, an object can be made to move at a constant speed. For example, if an object 64 points in size and default density and a linearDamping of 25 will slide across the screen in a few seconds if an impulse of magnitude 10 is applied every update.
--
-- ObjC selector: @- linearDamping@
linearDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
linearDamping skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "linearDamping") retCDouble []

-- | Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Used in conjunction with per frame impulses, an object can be made to move at a constant speed. For example, if an object 64 points in size and default density and a linearDamping of 25 will slide across the screen in a few seconds if an impulse of magnitude 10 is applied every update.
--
-- ObjC selector: @- setLinearDamping:@
setLinearDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setLinearDamping skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setLinearDamping:") retVoid [argCDouble (fromIntegral value)]

-- | Optionally reduce the body's angular velocity each frame to simulate rotational friction. (0.0 - 1.0). Defaults to 0.1
--
-- ObjC selector: @- angularDamping@
angularDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
angularDamping skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "angularDamping") retCDouble []

-- | Optionally reduce the body's angular velocity each frame to simulate rotational friction. (0.0 - 1.0). Defaults to 0.1
--
-- ObjC selector: @- setAngularDamping:@
setAngularDamping :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setAngularDamping skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setAngularDamping:") retVoid [argCDouble (fromIntegral value)]

-- | The density of the body.
--
-- The unit is arbitrary, as long as the relative densities are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- density@
density :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
density skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "density") retCDouble []

-- | The density of the body.
--
-- The unit is arbitrary, as long as the relative densities are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- setDensity:@
setDensity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setDensity skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setDensity:") retVoid [argCDouble (fromIntegral value)]

-- | The mass of the body.
--
-- The unit is arbitrary, as long as the relative masses are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- mass@
mass :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
mass skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "mass") retCDouble []

-- | The mass of the body.
--
-- The unit is arbitrary, as long as the relative masses are consistent throughout the application. Note that density and mass are inherently related (they are directly proportional), so changing one also changes the other. Both are provided so either can be used depending on what is more relevant to your usage.
--
-- ObjC selector: @- setMass:@
setMass :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setMass skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setMass:") retVoid [argCDouble (fromIntegral value)]

-- | The area of the body.
--
-- The unit is arbitrary, as long as the relative areas are consistent throughout the application.
--
-- ObjC selector: @- area@
area :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
area skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "area") retCDouble []

-- | Bodies are affected by field forces such as gravity if this property is set and the field's category mask is set appropriately. The default value is YES.
--
-- If this is set a force is applied to the object based on the mass. Set the field force vector in the scene to modify the strength of the force.
--
-- ObjC selector: @- affectedByGravity@
affectedByGravity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO Bool
affectedByGravity skPhysicsBody  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsBody (mkSelector "affectedByGravity") retCULong []

-- | Bodies are affected by field forces such as gravity if this property is set and the field's category mask is set appropriately. The default value is YES.
--
-- If this is set a force is applied to the object based on the mass. Set the field force vector in the scene to modify the strength of the force.
--
-- ObjC selector: @- setAffectedByGravity:@
setAffectedByGravity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> Bool -> IO ()
setAffectedByGravity skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setAffectedByGravity:") retVoid [argCULong (if value then 1 else 0)]

-- | Defines what logical 'categories' of fields this body responds to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- fieldBitMask@
fieldBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
fieldBitMask skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "fieldBitMask") retCUInt []

-- | Defines what logical 'categories' of fields this body responds to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- setFieldBitMask:@
setFieldBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setFieldBitMask skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setFieldBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | Defines what logical 'categories' this body belongs to. Defaults to all bits set (all categories).
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
categoryBitMask skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "categoryBitMask") retCUInt []

-- | Defines what logical 'categories' this body belongs to. Defaults to all bits set (all categories).
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setCategoryBitMask skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setCategoryBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | Defines what logical 'categories' of bodies this body responds to collisions with. Defaults to all bits set (all categories).
--
-- ObjC selector: @- collisionBitMask@
collisionBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
collisionBitMask skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "collisionBitMask") retCUInt []

-- | Defines what logical 'categories' of bodies this body responds to collisions with. Defaults to all bits set (all categories).
--
-- ObjC selector: @- setCollisionBitMask:@
setCollisionBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setCollisionBitMask skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setCollisionBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | Defines what logical 'categories' of bodies this body generates intersection notifications with. Defaults to all bits cleared (no categories).
--
-- ObjC selector: @- contactTestBitMask@
contactTestBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CUInt
contactTestBitMask skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "contactTestBitMask") retCUInt []

-- | Defines what logical 'categories' of bodies this body generates intersection notifications with. Defaults to all bits cleared (no categories).
--
-- ObjC selector: @- setContactTestBitMask:@
setContactTestBitMask :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CUInt -> IO ()
setContactTestBitMask skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setContactTestBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | @- joints@
joints :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO (Id NSArray)
joints skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "joints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The representedObject this physicsBody is currently bound to, or nil if it is not.
--
-- ObjC selector: @- node@
node :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO (Id SKNode)
node skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- angularVelocity@
angularVelocity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> IO CDouble
angularVelocity skPhysicsBody  =
  sendMsg skPhysicsBody (mkSelector "angularVelocity") retCDouble []

-- | @- setAngularVelocity:@
setAngularVelocity :: IsSKPhysicsBody skPhysicsBody => skPhysicsBody -> CDouble -> IO ()
setAngularVelocity skPhysicsBody  value =
  sendMsg skPhysicsBody (mkSelector "setAngularVelocity:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyWithCircleOfRadius:@
bodyWithCircleOfRadiusSelector :: Selector
bodyWithCircleOfRadiusSelector = mkSelector "bodyWithCircleOfRadius:"

-- | @Selector@ for @bodyWithPolygonFromPath:@
bodyWithPolygonFromPathSelector :: Selector
bodyWithPolygonFromPathSelector = mkSelector "bodyWithPolygonFromPath:"

-- | @Selector@ for @bodyWithEdgeChainFromPath:@
bodyWithEdgeChainFromPathSelector :: Selector
bodyWithEdgeChainFromPathSelector = mkSelector "bodyWithEdgeChainFromPath:"

-- | @Selector@ for @bodyWithEdgeLoopFromPath:@
bodyWithEdgeLoopFromPathSelector :: Selector
bodyWithEdgeLoopFromPathSelector = mkSelector "bodyWithEdgeLoopFromPath:"

-- | @Selector@ for @bodyWithBodies:@
bodyWithBodiesSelector :: Selector
bodyWithBodiesSelector = mkSelector "bodyWithBodies:"

-- | @Selector@ for @applyTorque:@
applyTorqueSelector :: Selector
applyTorqueSelector = mkSelector "applyTorque:"

-- | @Selector@ for @applyAngularImpulse:@
applyAngularImpulseSelector :: Selector
applyAngularImpulseSelector = mkSelector "applyAngularImpulse:"

-- | @Selector@ for @allContactedBodies@
allContactedBodiesSelector :: Selector
allContactedBodiesSelector = mkSelector "allContactedBodies"

-- | @Selector@ for @dynamic@
dynamicSelector :: Selector
dynamicSelector = mkSelector "dynamic"

-- | @Selector@ for @setDynamic:@
setDynamicSelector :: Selector
setDynamicSelector = mkSelector "setDynamic:"

-- | @Selector@ for @usesPreciseCollisionDetection@
usesPreciseCollisionDetectionSelector :: Selector
usesPreciseCollisionDetectionSelector = mkSelector "usesPreciseCollisionDetection"

-- | @Selector@ for @setUsesPreciseCollisionDetection:@
setUsesPreciseCollisionDetectionSelector :: Selector
setUsesPreciseCollisionDetectionSelector = mkSelector "setUsesPreciseCollisionDetection:"

-- | @Selector@ for @allowsRotation@
allowsRotationSelector :: Selector
allowsRotationSelector = mkSelector "allowsRotation"

-- | @Selector@ for @setAllowsRotation:@
setAllowsRotationSelector :: Selector
setAllowsRotationSelector = mkSelector "setAllowsRotation:"

-- | @Selector@ for @pinned@
pinnedSelector :: Selector
pinnedSelector = mkSelector "pinned"

-- | @Selector@ for @setPinned:@
setPinnedSelector :: Selector
setPinnedSelector = mkSelector "setPinned:"

-- | @Selector@ for @resting@
restingSelector :: Selector
restingSelector = mkSelector "resting"

-- | @Selector@ for @setResting:@
setRestingSelector :: Selector
setRestingSelector = mkSelector "setResting:"

-- | @Selector@ for @friction@
frictionSelector :: Selector
frictionSelector = mkSelector "friction"

-- | @Selector@ for @setFriction:@
setFrictionSelector :: Selector
setFrictionSelector = mkSelector "setFriction:"

-- | @Selector@ for @charge@
chargeSelector :: Selector
chargeSelector = mkSelector "charge"

-- | @Selector@ for @setCharge:@
setChargeSelector :: Selector
setChargeSelector = mkSelector "setCharge:"

-- | @Selector@ for @restitution@
restitutionSelector :: Selector
restitutionSelector = mkSelector "restitution"

-- | @Selector@ for @setRestitution:@
setRestitutionSelector :: Selector
setRestitutionSelector = mkSelector "setRestitution:"

-- | @Selector@ for @linearDamping@
linearDampingSelector :: Selector
linearDampingSelector = mkSelector "linearDamping"

-- | @Selector@ for @setLinearDamping:@
setLinearDampingSelector :: Selector
setLinearDampingSelector = mkSelector "setLinearDamping:"

-- | @Selector@ for @angularDamping@
angularDampingSelector :: Selector
angularDampingSelector = mkSelector "angularDamping"

-- | @Selector@ for @setAngularDamping:@
setAngularDampingSelector :: Selector
setAngularDampingSelector = mkSelector "setAngularDamping:"

-- | @Selector@ for @density@
densitySelector :: Selector
densitySelector = mkSelector "density"

-- | @Selector@ for @setDensity:@
setDensitySelector :: Selector
setDensitySelector = mkSelector "setDensity:"

-- | @Selector@ for @mass@
massSelector :: Selector
massSelector = mkSelector "mass"

-- | @Selector@ for @setMass:@
setMassSelector :: Selector
setMassSelector = mkSelector "setMass:"

-- | @Selector@ for @area@
areaSelector :: Selector
areaSelector = mkSelector "area"

-- | @Selector@ for @affectedByGravity@
affectedByGravitySelector :: Selector
affectedByGravitySelector = mkSelector "affectedByGravity"

-- | @Selector@ for @setAffectedByGravity:@
setAffectedByGravitySelector :: Selector
setAffectedByGravitySelector = mkSelector "setAffectedByGravity:"

-- | @Selector@ for @fieldBitMask@
fieldBitMaskSelector :: Selector
fieldBitMaskSelector = mkSelector "fieldBitMask"

-- | @Selector@ for @setFieldBitMask:@
setFieldBitMaskSelector :: Selector
setFieldBitMaskSelector = mkSelector "setFieldBitMask:"

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

-- | @Selector@ for @joints@
jointsSelector :: Selector
jointsSelector = mkSelector "joints"

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @angularVelocity@
angularVelocitySelector :: Selector
angularVelocitySelector = mkSelector "angularVelocity"

-- | @Selector@ for @setAngularVelocity:@
setAngularVelocitySelector :: Selector
setAngularVelocitySelector = mkSelector "setAngularVelocity:"

