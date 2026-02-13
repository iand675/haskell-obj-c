{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKFieldNode@.
module ObjC.SpriteKit.SKFieldNode
  ( SKFieldNode
  , IsSKFieldNode(..)
  , dragField
  , vortexField
  , radialGravityField
  , velocityFieldWithTexture
  , noiseFieldWithSmoothness_animationSpeed
  , turbulenceFieldWithSmoothness_animationSpeed
  , springField
  , electricField
  , magneticField
  , customFieldWithEvaluationBlock
  , region
  , setRegion
  , strength
  , setStrength
  , falloff
  , setFalloff
  , minimumRadius
  , setMinimumRadius
  , enabled
  , setEnabled
  , exclusive
  , setExclusive
  , categoryBitMask
  , setCategoryBitMask
  , smoothness
  , setSmoothness
  , animationSpeed
  , setAnimationSpeed
  , texture
  , setTexture
  , animationSpeedSelector
  , categoryBitMaskSelector
  , customFieldWithEvaluationBlockSelector
  , dragFieldSelector
  , electricFieldSelector
  , enabledSelector
  , exclusiveSelector
  , falloffSelector
  , magneticFieldSelector
  , minimumRadiusSelector
  , noiseFieldWithSmoothness_animationSpeedSelector
  , radialGravityFieldSelector
  , regionSelector
  , setAnimationSpeedSelector
  , setCategoryBitMaskSelector
  , setEnabledSelector
  , setExclusiveSelector
  , setFalloffSelector
  , setMinimumRadiusSelector
  , setRegionSelector
  , setSmoothnessSelector
  , setStrengthSelector
  , setTextureSelector
  , smoothnessSelector
  , springFieldSelector
  , strengthSelector
  , textureSelector
  , turbulenceFieldWithSmoothness_animationSpeedSelector
  , velocityFieldWithTextureSelector
  , vortexFieldSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Slows an object proportionally to the object’s velocity. Use this to simulate effects such as friction from motion through the air.
--
-- ObjC selector: @+ dragField@
dragField :: IO (Id SKFieldNode)
dragField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' dragFieldSelector

-- | Applies a force tangential to the direction from the sample point to the field's position. The force will be CCW to the direction. Make the strength negative to apply force in the CW direction. Amount is proportional to distance from center and the object's mass. Use this to create effects such as tornadoes.
--
-- ObjC selector: @+ vortexField@
vortexField :: IO (Id SKFieldNode)
vortexField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' vortexFieldSelector

-- | Applies a force in the direction of the origin of the field in local space. To repel objects, use a negative strength. The force is proportional to the distance from the field origin. Varies with the mass of the object according to F = ma The field node's rotation property can be used to orient the gravity in a particular direction.
--
-- ObjC selector: @+ radialGravityField@
radialGravityField :: IO (Id SKFieldNode)
radialGravityField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' radialGravityFieldSelector

-- | The supplied texture contains velocity values for any object entering the field’s area of effect. If no texture is supplied, the direction property is used instead. Velocity fields override the effect of any other acceleration applied to the body. Velocity fields are typically used for such effects as advection, for example, a velocity field might describe the velocity on the surface of a river. An object placed in the river would then follow the river.
--
-- @velocityTexture@ — The R and G channels of the supplied texture become x and y components of velocity. B and A, if present in the SKTexture, are ignored.
--
-- See: texture
--
-- ObjC selector: @+ velocityFieldWithTexture:@
velocityFieldWithTexture :: IsSKTexture velocityTexture => velocityTexture -> IO (Id SKFieldNode)
velocityFieldWithTexture velocityTexture =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' velocityFieldWithTextureSelector (toSKTexture velocityTexture)

-- | A time varying differentiable Perlin simplex noise field. By default a smooth noise is calculated, and the field is time varying. Use this to simulate such effects as fireflies, or snow. To freeze the noise in place, set animationSpeed to 0.0. Mass is ignored.
--
-- @smoothness@ — value of 0 means as noisy as possible, 1 means as smooth as possible
--
-- @speed@ — is the general rate in Hz that any particular texel changes to a different value
--
-- See: smoothness
--
-- See: animationSpeed
--
-- ObjC selector: @+ noiseFieldWithSmoothness:animationSpeed:@
noiseFieldWithSmoothness_animationSpeed :: CDouble -> CDouble -> IO (Id SKFieldNode)
noiseFieldWithSmoothness_animationSpeed smoothness speed =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' noiseFieldWithSmoothness_animationSpeedSelector smoothness speed

-- | Just like Noise, except the strength of the noise is proportional to the velocity of the object in the field.
--
-- @smoothness@ — value of 0 means as noisy as possible, 1 means as smooth as possible
--
-- @speed@ — is the general rate in Hz that any particular texel changes to a different value
--
-- See: smoothness
--
-- See: animationSpeed
--
-- ObjC selector: @+ turbulenceFieldWithSmoothness:animationSpeed:@
turbulenceFieldWithSmoothness_animationSpeed :: CDouble -> CDouble -> IO (Id SKFieldNode)
turbulenceFieldWithSmoothness_animationSpeed smoothness speed =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' turbulenceFieldWithSmoothness_animationSpeedSelector smoothness speed

-- | A Hooke’s law force - a force linearly proportional to distance from the center of the field. An object in this field will oscillate with a period proportional to the inverse of the mass. An example use is to keep objects confined to a particular region.
--
-- ObjC selector: @+ springField@
springField :: IO (Id SKFieldNode)
springField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' springFieldSelector

-- | A force proportional to the charge on the object. A charge property has been added to SKPhysicsBodies to accomplish this. An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the first part of the Lorentz equation, F = qE
--
-- ObjC selector: @+ electricField@
electricField :: IO (Id SKFieldNode)
electricField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' electricFieldSelector

-- | A force proportional to the charge on the object and the object’s velocity. A charge property has been added to SKPhysicsBodies to accomplish this. An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the second part of the Lorentz equation, F = qvB
--
-- ObjC selector: @+ magneticField@
magneticField :: IO (Id SKFieldNode)
magneticField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' magneticFieldSelector

-- | @+ customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlock :: Ptr () -> IO (Id SKFieldNode)
customFieldWithEvaluationBlock block =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMessage cls' customFieldWithEvaluationBlockSelector block

-- | The region property is the domain of the field's effect. No force is applied to objects outside the region.
--
-- ObjC selector: @- region@
region :: IsSKFieldNode skFieldNode => skFieldNode -> IO (Id SKRegion)
region skFieldNode =
  sendMessage skFieldNode regionSelector

-- | The region property is the domain of the field's effect. No force is applied to objects outside the region.
--
-- ObjC selector: @- setRegion:@
setRegion :: (IsSKFieldNode skFieldNode, IsSKRegion value) => skFieldNode -> value -> IO ()
setRegion skFieldNode value =
  sendMessage skFieldNode setRegionSelector (toSKRegion value)

-- | strength scaling value. default 1.0
--
-- ObjC selector: @- strength@
strength :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
strength skFieldNode =
  sendMessage skFieldNode strengthSelector

-- | strength scaling value. default 1.0
--
-- ObjC selector: @- setStrength:@
setStrength :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setStrength skFieldNode value =
  sendMessage skFieldNode setStrengthSelector value

-- | The falloff exponent used to calculate field strength at a distance.    Falloff starts at the minimum radius.
--
-- The default exponent is zero, which results in a uniform field with no falloff.
--
-- See: minimumRadius
--
-- ObjC selector: @- falloff@
falloff :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
falloff skFieldNode =
  sendMessage skFieldNode falloffSelector

-- | The falloff exponent used to calculate field strength at a distance.    Falloff starts at the minimum radius.
--
-- The default exponent is zero, which results in a uniform field with no falloff.
--
-- See: minimumRadius
--
-- ObjC selector: @- setFalloff:@
setFalloff :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setFalloff skFieldNode value =
  sendMessage skFieldNode setFalloffSelector value

-- | minimum radius of effect. Default is very small.
--
-- ObjC selector: @- minimumRadius@
minimumRadius :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
minimumRadius skFieldNode =
  sendMessage skFieldNode minimumRadiusSelector

-- | minimum radius of effect. Default is very small.
--
-- ObjC selector: @- setMinimumRadius:@
setMinimumRadius :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setMinimumRadius skFieldNode value =
  sendMessage skFieldNode setMinimumRadiusSelector value

-- | If enabled, a field has an effect.
--
-- default YES
--
-- ObjC selector: @- enabled@
enabled :: IsSKFieldNode skFieldNode => skFieldNode -> IO Bool
enabled skFieldNode =
  sendMessage skFieldNode enabledSelector

-- | If enabled, a field has an effect.
--
-- default YES
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSKFieldNode skFieldNode => skFieldNode -> Bool -> IO ()
setEnabled skFieldNode value =
  sendMessage skFieldNode setEnabledSelector value

-- | If a field is exclusive, it suppresses any other field in its region of effect.    If two or more exclusive fields overlap, it is undefined which one of them will take effect
--
-- See: region
--
-- ObjC selector: @- exclusive@
exclusive :: IsSKFieldNode skFieldNode => skFieldNode -> IO Bool
exclusive skFieldNode =
  sendMessage skFieldNode exclusiveSelector

-- | If a field is exclusive, it suppresses any other field in its region of effect.    If two or more exclusive fields overlap, it is undefined which one of them will take effect
--
-- See: region
--
-- ObjC selector: @- setExclusive:@
setExclusive :: IsSKFieldNode skFieldNode => skFieldNode -> Bool -> IO ()
setExclusive skFieldNode value =
  sendMessage skFieldNode setExclusiveSelector value

-- | Logical categories the field belongs to. Default is all categories.    These categories correspond to fieldBitMasks, and can be used to enforce that a particular field applies    to a particular category of objects.
--
-- See: SKPhysicsBody.fieldBitMask
--
-- See: SKEmitterNode.fieldBitMask
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSKFieldNode skFieldNode => skFieldNode -> IO CUInt
categoryBitMask skFieldNode =
  sendMessage skFieldNode categoryBitMaskSelector

-- | Logical categories the field belongs to. Default is all categories.    These categories correspond to fieldBitMasks, and can be used to enforce that a particular field applies    to a particular category of objects.
--
-- See: SKPhysicsBody.fieldBitMask
--
-- See: SKEmitterNode.fieldBitMask
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSKFieldNode skFieldNode => skFieldNode -> CUInt -> IO ()
setCategoryBitMask skFieldNode value =
  sendMessage skFieldNode setCategoryBitMaskSelector value

-- | fields without a smoothness component will return 0
--
-- See: noiseFieldWithSmoothness:smoothness:animationSpeed
--
-- See: turbulenceFieldWithSmoothness:smoothness:animationSpeed
--
-- ObjC selector: @- smoothness@
smoothness :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
smoothness skFieldNode =
  sendMessage skFieldNode smoothnessSelector

-- | fields without a smoothness component will return 0
--
-- See: noiseFieldWithSmoothness:smoothness:animationSpeed
--
-- See: turbulenceFieldWithSmoothness:smoothness:animationSpeed
--
-- ObjC selector: @- setSmoothness:@
setSmoothness :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setSmoothness skFieldNode value =
  sendMessage skFieldNode setSmoothnessSelector value

-- | fields that can be animated can have non zero values.
--
-- A value of 2 will animated twice as fast as a value of 1.
--
-- See: noiseFieldWithSmoothness:smoothness:animationSpeed
--
-- See: turbulenceFieldWithSmoothness:smoothness:animationSpeed
--
-- ObjC selector: @- animationSpeed@
animationSpeed :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
animationSpeed skFieldNode =
  sendMessage skFieldNode animationSpeedSelector

-- | fields that can be animated can have non zero values.
--
-- A value of 2 will animated twice as fast as a value of 1.
--
-- See: noiseFieldWithSmoothness:smoothness:animationSpeed
--
-- See: turbulenceFieldWithSmoothness:smoothness:animationSpeed
--
-- ObjC selector: @- setAnimationSpeed:@
setAnimationSpeed :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setAnimationSpeed skFieldNode value =
  sendMessage skFieldNode setAnimationSpeedSelector value

-- | fields constructed with a texture can be uppdated by assigning a new texture
--
-- See: velocityFieldWithTexture:velocityTexture
--
-- ObjC selector: @- texture@
texture :: IsSKFieldNode skFieldNode => skFieldNode -> IO (Id SKTexture)
texture skFieldNode =
  sendMessage skFieldNode textureSelector

-- | fields constructed with a texture can be uppdated by assigning a new texture
--
-- See: velocityFieldWithTexture:velocityTexture
--
-- ObjC selector: @- setTexture:@
setTexture :: (IsSKFieldNode skFieldNode, IsSKTexture value) => skFieldNode -> value -> IO ()
setTexture skFieldNode value =
  sendMessage skFieldNode setTextureSelector (toSKTexture value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dragField@
dragFieldSelector :: Selector '[] (Id SKFieldNode)
dragFieldSelector = mkSelector "dragField"

-- | @Selector@ for @vortexField@
vortexFieldSelector :: Selector '[] (Id SKFieldNode)
vortexFieldSelector = mkSelector "vortexField"

-- | @Selector@ for @radialGravityField@
radialGravityFieldSelector :: Selector '[] (Id SKFieldNode)
radialGravityFieldSelector = mkSelector "radialGravityField"

-- | @Selector@ for @velocityFieldWithTexture:@
velocityFieldWithTextureSelector :: Selector '[Id SKTexture] (Id SKFieldNode)
velocityFieldWithTextureSelector = mkSelector "velocityFieldWithTexture:"

-- | @Selector@ for @noiseFieldWithSmoothness:animationSpeed:@
noiseFieldWithSmoothness_animationSpeedSelector :: Selector '[CDouble, CDouble] (Id SKFieldNode)
noiseFieldWithSmoothness_animationSpeedSelector = mkSelector "noiseFieldWithSmoothness:animationSpeed:"

-- | @Selector@ for @turbulenceFieldWithSmoothness:animationSpeed:@
turbulenceFieldWithSmoothness_animationSpeedSelector :: Selector '[CDouble, CDouble] (Id SKFieldNode)
turbulenceFieldWithSmoothness_animationSpeedSelector = mkSelector "turbulenceFieldWithSmoothness:animationSpeed:"

-- | @Selector@ for @springField@
springFieldSelector :: Selector '[] (Id SKFieldNode)
springFieldSelector = mkSelector "springField"

-- | @Selector@ for @electricField@
electricFieldSelector :: Selector '[] (Id SKFieldNode)
electricFieldSelector = mkSelector "electricField"

-- | @Selector@ for @magneticField@
magneticFieldSelector :: Selector '[] (Id SKFieldNode)
magneticFieldSelector = mkSelector "magneticField"

-- | @Selector@ for @customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlockSelector :: Selector '[Ptr ()] (Id SKFieldNode)
customFieldWithEvaluationBlockSelector = mkSelector "customFieldWithEvaluationBlock:"

-- | @Selector@ for @region@
regionSelector :: Selector '[] (Id SKRegion)
regionSelector = mkSelector "region"

-- | @Selector@ for @setRegion:@
setRegionSelector :: Selector '[Id SKRegion] ()
setRegionSelector = mkSelector "setRegion:"

-- | @Selector@ for @strength@
strengthSelector :: Selector '[] CFloat
strengthSelector = mkSelector "strength"

-- | @Selector@ for @setStrength:@
setStrengthSelector :: Selector '[CFloat] ()
setStrengthSelector = mkSelector "setStrength:"

-- | @Selector@ for @falloff@
falloffSelector :: Selector '[] CFloat
falloffSelector = mkSelector "falloff"

-- | @Selector@ for @setFalloff:@
setFalloffSelector :: Selector '[CFloat] ()
setFalloffSelector = mkSelector "setFalloff:"

-- | @Selector@ for @minimumRadius@
minimumRadiusSelector :: Selector '[] CFloat
minimumRadiusSelector = mkSelector "minimumRadius"

-- | @Selector@ for @setMinimumRadius:@
setMinimumRadiusSelector :: Selector '[CFloat] ()
setMinimumRadiusSelector = mkSelector "setMinimumRadius:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @exclusive@
exclusiveSelector :: Selector '[] Bool
exclusiveSelector = mkSelector "exclusive"

-- | @Selector@ for @setExclusive:@
setExclusiveSelector :: Selector '[Bool] ()
setExclusiveSelector = mkSelector "setExclusive:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CUInt
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CUInt] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @smoothness@
smoothnessSelector :: Selector '[] CFloat
smoothnessSelector = mkSelector "smoothness"

-- | @Selector@ for @setSmoothness:@
setSmoothnessSelector :: Selector '[CFloat] ()
setSmoothnessSelector = mkSelector "setSmoothness:"

-- | @Selector@ for @animationSpeed@
animationSpeedSelector :: Selector '[] CFloat
animationSpeedSelector = mkSelector "animationSpeed"

-- | @Selector@ for @setAnimationSpeed:@
setAnimationSpeedSelector :: Selector '[CFloat] ()
setAnimationSpeedSelector = mkSelector "setAnimationSpeed:"

-- | @Selector@ for @texture@
textureSelector :: Selector '[] (Id SKTexture)
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector '[Id SKTexture] ()
setTextureSelector = mkSelector "setTexture:"

