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
  , dragFieldSelector
  , vortexFieldSelector
  , radialGravityFieldSelector
  , velocityFieldWithTextureSelector
  , noiseFieldWithSmoothness_animationSpeedSelector
  , turbulenceFieldWithSmoothness_animationSpeedSelector
  , springFieldSelector
  , electricFieldSelector
  , magneticFieldSelector
  , customFieldWithEvaluationBlockSelector
  , regionSelector
  , setRegionSelector
  , strengthSelector
  , setStrengthSelector
  , falloffSelector
  , setFalloffSelector
  , minimumRadiusSelector
  , setMinimumRadiusSelector
  , enabledSelector
  , setEnabledSelector
  , exclusiveSelector
  , setExclusiveSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector
  , smoothnessSelector
  , setSmoothnessSelector
  , animationSpeedSelector
  , setAnimationSpeedSelector
  , textureSelector
  , setTextureSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    sendClassMsg cls' (mkSelector "dragField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a force tangential to the direction from the sample point to the field's position. The force will be CCW to the direction. Make the strength negative to apply force in the CW direction. Amount is proportional to distance from center and the object's mass. Use this to create effects such as tornadoes.
--
-- ObjC selector: @+ vortexField@
vortexField :: IO (Id SKFieldNode)
vortexField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMsg cls' (mkSelector "vortexField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a force in the direction of the origin of the field in local space. To repel objects, use a negative strength. The force is proportional to the distance from the field origin. Varies with the mass of the object according to F = ma The field node's rotation property can be used to orient the gravity in a particular direction.
--
-- ObjC selector: @+ radialGravityField@
radialGravityField :: IO (Id SKFieldNode)
radialGravityField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMsg cls' (mkSelector "radialGravityField") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    withObjCPtr velocityTexture $ \raw_velocityTexture ->
      sendClassMsg cls' (mkSelector "velocityFieldWithTexture:") (retPtr retVoid) [argPtr (castPtr raw_velocityTexture :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "noiseFieldWithSmoothness:animationSpeed:") (retPtr retVoid) [argCDouble (fromIntegral smoothness), argCDouble (fromIntegral speed)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "turbulenceFieldWithSmoothness:animationSpeed:") (retPtr retVoid) [argCDouble (fromIntegral smoothness), argCDouble (fromIntegral speed)] >>= retainedObject . castPtr

-- | A Hooke’s law force - a force linearly proportional to distance from the center of the field. An object in this field will oscillate with a period proportional to the inverse of the mass. An example use is to keep objects confined to a particular region.
--
-- ObjC selector: @+ springField@
springField :: IO (Id SKFieldNode)
springField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMsg cls' (mkSelector "springField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A force proportional to the charge on the object. A charge property has been added to SKPhysicsBodies to accomplish this. An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the first part of the Lorentz equation, F = qE
--
-- ObjC selector: @+ electricField@
electricField :: IO (Id SKFieldNode)
electricField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMsg cls' (mkSelector "electricField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A force proportional to the charge on the object and the object’s velocity. A charge property has been added to SKPhysicsBodies to accomplish this. An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the second part of the Lorentz equation, F = qvB
--
-- ObjC selector: @+ magneticField@
magneticField :: IO (Id SKFieldNode)
magneticField  =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMsg cls' (mkSelector "magneticField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlock :: Ptr () -> IO (Id SKFieldNode)
customFieldWithEvaluationBlock block =
  do
    cls' <- getRequiredClass "SKFieldNode"
    sendClassMsg cls' (mkSelector "customFieldWithEvaluationBlock:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | The region property is the domain of the field's effect. No force is applied to objects outside the region.
--
-- ObjC selector: @- region@
region :: IsSKFieldNode skFieldNode => skFieldNode -> IO (Id SKRegion)
region skFieldNode  =
  sendMsg skFieldNode (mkSelector "region") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The region property is the domain of the field's effect. No force is applied to objects outside the region.
--
-- ObjC selector: @- setRegion:@
setRegion :: (IsSKFieldNode skFieldNode, IsSKRegion value) => skFieldNode -> value -> IO ()
setRegion skFieldNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skFieldNode (mkSelector "setRegion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | strength scaling value. default 1.0
--
-- ObjC selector: @- strength@
strength :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
strength skFieldNode  =
  sendMsg skFieldNode (mkSelector "strength") retCFloat []

-- | strength scaling value. default 1.0
--
-- ObjC selector: @- setStrength:@
setStrength :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setStrength skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setStrength:") retVoid [argCFloat (fromIntegral value)]

-- | The falloff exponent used to calculate field strength at a distance.    Falloff starts at the minimum radius.
--
-- The default exponent is zero, which results in a uniform field with no falloff.
--
-- See: minimumRadius
--
-- ObjC selector: @- falloff@
falloff :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
falloff skFieldNode  =
  sendMsg skFieldNode (mkSelector "falloff") retCFloat []

-- | The falloff exponent used to calculate field strength at a distance.    Falloff starts at the minimum radius.
--
-- The default exponent is zero, which results in a uniform field with no falloff.
--
-- See: minimumRadius
--
-- ObjC selector: @- setFalloff:@
setFalloff :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setFalloff skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setFalloff:") retVoid [argCFloat (fromIntegral value)]

-- | minimum radius of effect. Default is very small.
--
-- ObjC selector: @- minimumRadius@
minimumRadius :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
minimumRadius skFieldNode  =
  sendMsg skFieldNode (mkSelector "minimumRadius") retCFloat []

-- | minimum radius of effect. Default is very small.
--
-- ObjC selector: @- setMinimumRadius:@
setMinimumRadius :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setMinimumRadius skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setMinimumRadius:") retVoid [argCFloat (fromIntegral value)]

-- | If enabled, a field has an effect.
--
-- default YES
--
-- ObjC selector: @- enabled@
enabled :: IsSKFieldNode skFieldNode => skFieldNode -> IO Bool
enabled skFieldNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skFieldNode (mkSelector "enabled") retCULong []

-- | If enabled, a field has an effect.
--
-- default YES
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSKFieldNode skFieldNode => skFieldNode -> Bool -> IO ()
setEnabled skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | If a field is exclusive, it suppresses any other field in its region of effect.    If two or more exclusive fields overlap, it is undefined which one of them will take effect
--
-- See: region
--
-- ObjC selector: @- exclusive@
exclusive :: IsSKFieldNode skFieldNode => skFieldNode -> IO Bool
exclusive skFieldNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skFieldNode (mkSelector "exclusive") retCULong []

-- | If a field is exclusive, it suppresses any other field in its region of effect.    If two or more exclusive fields overlap, it is undefined which one of them will take effect
--
-- See: region
--
-- ObjC selector: @- setExclusive:@
setExclusive :: IsSKFieldNode skFieldNode => skFieldNode -> Bool -> IO ()
setExclusive skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setExclusive:") retVoid [argCULong (if value then 1 else 0)]

-- | Logical categories the field belongs to. Default is all categories.    These categories correspond to fieldBitMasks, and can be used to enforce that a particular field applies    to a particular category of objects.
--
-- See: SKPhysicsBody.fieldBitMask
--
-- See: SKEmitterNode.fieldBitMask
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSKFieldNode skFieldNode => skFieldNode -> IO CUInt
categoryBitMask skFieldNode  =
  sendMsg skFieldNode (mkSelector "categoryBitMask") retCUInt []

-- | Logical categories the field belongs to. Default is all categories.    These categories correspond to fieldBitMasks, and can be used to enforce that a particular field applies    to a particular category of objects.
--
-- See: SKPhysicsBody.fieldBitMask
--
-- See: SKEmitterNode.fieldBitMask
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSKFieldNode skFieldNode => skFieldNode -> CUInt -> IO ()
setCategoryBitMask skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setCategoryBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | fields without a smoothness component will return 0
--
-- See: noiseFieldWithSmoothness:smoothness:animationSpeed
--
-- See: turbulenceFieldWithSmoothness:smoothness:animationSpeed
--
-- ObjC selector: @- smoothness@
smoothness :: IsSKFieldNode skFieldNode => skFieldNode -> IO CFloat
smoothness skFieldNode  =
  sendMsg skFieldNode (mkSelector "smoothness") retCFloat []

-- | fields without a smoothness component will return 0
--
-- See: noiseFieldWithSmoothness:smoothness:animationSpeed
--
-- See: turbulenceFieldWithSmoothness:smoothness:animationSpeed
--
-- ObjC selector: @- setSmoothness:@
setSmoothness :: IsSKFieldNode skFieldNode => skFieldNode -> CFloat -> IO ()
setSmoothness skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setSmoothness:") retVoid [argCFloat (fromIntegral value)]

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
animationSpeed skFieldNode  =
  sendMsg skFieldNode (mkSelector "animationSpeed") retCFloat []

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
setAnimationSpeed skFieldNode  value =
  sendMsg skFieldNode (mkSelector "setAnimationSpeed:") retVoid [argCFloat (fromIntegral value)]

-- | fields constructed with a texture can be uppdated by assigning a new texture
--
-- See: velocityFieldWithTexture:velocityTexture
--
-- ObjC selector: @- texture@
texture :: IsSKFieldNode skFieldNode => skFieldNode -> IO (Id SKTexture)
texture skFieldNode  =
  sendMsg skFieldNode (mkSelector "texture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | fields constructed with a texture can be uppdated by assigning a new texture
--
-- See: velocityFieldWithTexture:velocityTexture
--
-- ObjC selector: @- setTexture:@
setTexture :: (IsSKFieldNode skFieldNode, IsSKTexture value) => skFieldNode -> value -> IO ()
setTexture skFieldNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skFieldNode (mkSelector "setTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dragField@
dragFieldSelector :: Selector
dragFieldSelector = mkSelector "dragField"

-- | @Selector@ for @vortexField@
vortexFieldSelector :: Selector
vortexFieldSelector = mkSelector "vortexField"

-- | @Selector@ for @radialGravityField@
radialGravityFieldSelector :: Selector
radialGravityFieldSelector = mkSelector "radialGravityField"

-- | @Selector@ for @velocityFieldWithTexture:@
velocityFieldWithTextureSelector :: Selector
velocityFieldWithTextureSelector = mkSelector "velocityFieldWithTexture:"

-- | @Selector@ for @noiseFieldWithSmoothness:animationSpeed:@
noiseFieldWithSmoothness_animationSpeedSelector :: Selector
noiseFieldWithSmoothness_animationSpeedSelector = mkSelector "noiseFieldWithSmoothness:animationSpeed:"

-- | @Selector@ for @turbulenceFieldWithSmoothness:animationSpeed:@
turbulenceFieldWithSmoothness_animationSpeedSelector :: Selector
turbulenceFieldWithSmoothness_animationSpeedSelector = mkSelector "turbulenceFieldWithSmoothness:animationSpeed:"

-- | @Selector@ for @springField@
springFieldSelector :: Selector
springFieldSelector = mkSelector "springField"

-- | @Selector@ for @electricField@
electricFieldSelector :: Selector
electricFieldSelector = mkSelector "electricField"

-- | @Selector@ for @magneticField@
magneticFieldSelector :: Selector
magneticFieldSelector = mkSelector "magneticField"

-- | @Selector@ for @customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlockSelector :: Selector
customFieldWithEvaluationBlockSelector = mkSelector "customFieldWithEvaluationBlock:"

-- | @Selector@ for @region@
regionSelector :: Selector
regionSelector = mkSelector "region"

-- | @Selector@ for @setRegion:@
setRegionSelector :: Selector
setRegionSelector = mkSelector "setRegion:"

-- | @Selector@ for @strength@
strengthSelector :: Selector
strengthSelector = mkSelector "strength"

-- | @Selector@ for @setStrength:@
setStrengthSelector :: Selector
setStrengthSelector = mkSelector "setStrength:"

-- | @Selector@ for @falloff@
falloffSelector :: Selector
falloffSelector = mkSelector "falloff"

-- | @Selector@ for @setFalloff:@
setFalloffSelector :: Selector
setFalloffSelector = mkSelector "setFalloff:"

-- | @Selector@ for @minimumRadius@
minimumRadiusSelector :: Selector
minimumRadiusSelector = mkSelector "minimumRadius"

-- | @Selector@ for @setMinimumRadius:@
setMinimumRadiusSelector :: Selector
setMinimumRadiusSelector = mkSelector "setMinimumRadius:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @exclusive@
exclusiveSelector :: Selector
exclusiveSelector = mkSelector "exclusive"

-- | @Selector@ for @setExclusive:@
setExclusiveSelector :: Selector
setExclusiveSelector = mkSelector "setExclusive:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

-- | @Selector@ for @smoothness@
smoothnessSelector :: Selector
smoothnessSelector = mkSelector "smoothness"

-- | @Selector@ for @setSmoothness:@
setSmoothnessSelector :: Selector
setSmoothnessSelector = mkSelector "setSmoothness:"

-- | @Selector@ for @animationSpeed@
animationSpeedSelector :: Selector
animationSpeedSelector = mkSelector "animationSpeed"

-- | @Selector@ for @setAnimationSpeed:@
setAnimationSpeedSelector :: Selector
setAnimationSpeedSelector = mkSelector "setAnimationSpeed:"

-- | @Selector@ for @texture@
textureSelector :: Selector
textureSelector = mkSelector "texture"

-- | @Selector@ for @setTexture:@
setTextureSelector :: Selector
setTextureSelector = mkSelector "setTexture:"

