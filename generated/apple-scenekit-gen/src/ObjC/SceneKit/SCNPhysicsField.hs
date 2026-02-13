{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNPhysicsField
--
-- SCNPhysicsField is an abstract class that describes a force field that applies in the physics world.
--
-- Generated bindings for @SCNPhysicsField@.
module ObjC.SceneKit.SCNPhysicsField
  ( SCNPhysicsField
  , IsSCNPhysicsField(..)
  , dragField
  , vortexField
  , radialGravityField
  , linearGravityField
  , noiseFieldWithSmoothness_animationSpeed
  , turbulenceFieldWithSmoothness_animationSpeed
  , springField
  , electricField
  , magneticField
  , customFieldWithEvaluationBlock
  , strength
  , setStrength
  , falloffExponent
  , setFalloffExponent
  , minimumDistance
  , setMinimumDistance
  , active
  , setActive
  , exclusive
  , setExclusive
  , halfExtent
  , setHalfExtent
  , usesEllipsoidalExtent
  , setUsesEllipsoidalExtent
  , scope
  , setScope
  , offset
  , setOffset
  , direction
  , setDirection
  , categoryBitMask
  , setCategoryBitMask
  , activeSelector
  , categoryBitMaskSelector
  , customFieldWithEvaluationBlockSelector
  , directionSelector
  , dragFieldSelector
  , electricFieldSelector
  , exclusiveSelector
  , falloffExponentSelector
  , halfExtentSelector
  , linearGravityFieldSelector
  , magneticFieldSelector
  , minimumDistanceSelector
  , noiseFieldWithSmoothness_animationSpeedSelector
  , offsetSelector
  , radialGravityFieldSelector
  , scopeSelector
  , setActiveSelector
  , setCategoryBitMaskSelector
  , setDirectionSelector
  , setExclusiveSelector
  , setFalloffExponentSelector
  , setHalfExtentSelector
  , setMinimumDistanceSelector
  , setOffsetSelector
  , setScopeSelector
  , setStrengthSelector
  , setUsesEllipsoidalExtentSelector
  , springFieldSelector
  , strengthSelector
  , turbulenceFieldWithSmoothness_animationSpeedSelector
  , usesEllipsoidalExtentSelector
  , vortexFieldSelector

  -- * Enum types
  , SCNPhysicsFieldScope(SCNPhysicsFieldScope)
  , pattern SCNPhysicsFieldScopeInsideExtent
  , pattern SCNPhysicsFieldScopeOutsideExtent

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

-- | Slows an object proportionally to the object’s velocity. Use this to simulate effects such as friction from motion through the air.
--
-- ObjC selector: @+ dragField@
dragField :: IO (Id SCNPhysicsField)
dragField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' dragFieldSelector

-- | Applies a force tangential to the direction from the sample point to the field's position. The force will be CCW to the direction. Make the strength negative to apply force in the CW direction. Amount is proportional to distance from center and the object's mass. Use this to create effects such as tornadoes.
--
-- ObjC selector: @+ vortexField@
vortexField :: IO (Id SCNPhysicsField)
vortexField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' vortexFieldSelector

-- | Applies a force in the direction of the origin of the field in local space. To repel objects, use a negative strength. The force is proportional to the distance from the field origin. Varies with the mass of the object according to F = ma The field node's rotation property can be used to orient the gravity in a particular direction.
--
-- ObjC selector: @+ radialGravityField@
radialGravityField :: IO (Id SCNPhysicsField)
radialGravityField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' radialGravityFieldSelector

-- | Applies a force in the direction of the "direction" vector in the local space. To repel objects, use a negative strength. The force is the same everywhere in the field. Varies with the mass of the object according to F = ma The field node's rotation property can be used to orient the gravity in a particular direction.
--
-- ObjC selector: @+ linearGravityField@
linearGravityField :: IO (Id SCNPhysicsField)
linearGravityField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' linearGravityFieldSelector

-- | A time varying differentiable Perlin simplex noise field. A smoothness of 0 means as noisy as possible. Use this to simulate such effects as fireflies, or snow. To freeze the noise in place, set animationSpeed to 0.0. Mass is ignored. You can change the "smoothness" and "animationSpeed" using KVC.
--
-- ObjC selector: @+ noiseFieldWithSmoothness:animationSpeed:@
noiseFieldWithSmoothness_animationSpeed :: CDouble -> CDouble -> IO (Id SCNPhysicsField)
noiseFieldWithSmoothness_animationSpeed smoothness speed =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' noiseFieldWithSmoothness_animationSpeedSelector smoothness speed

-- | Just like Noise, except the strength of the noise is proportional to the velocity of the object in the field.
--
-- ObjC selector: @+ turbulenceFieldWithSmoothness:animationSpeed:@
turbulenceFieldWithSmoothness_animationSpeed :: CDouble -> CDouble -> IO (Id SCNPhysicsField)
turbulenceFieldWithSmoothness_animationSpeed smoothness speed =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' turbulenceFieldWithSmoothness_animationSpeedSelector smoothness speed

-- | A Hooke’s law force - a force linearly proportional to distance from the center of the field. An object in this field will oscillate with a period proportional to the inverse of the mass. An example use is to keep objects confined to a particular region.
--
-- ObjC selector: @+ springField@
springField :: IO (Id SCNPhysicsField)
springField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' springFieldSelector

-- | A force proportional to the charge on the object. An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the first part of the Lorentz equation, F = qE
--
-- ObjC selector: @+ electricField@
electricField :: IO (Id SCNPhysicsField)
electricField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' electricFieldSelector

-- | A force proportional to the charge on the object and the object’s velocity.  An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the second part of the Lorentz equation, F = qvB
--
-- ObjC selector: @+ magneticField@
magneticField :: IO (Id SCNPhysicsField)
magneticField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' magneticFieldSelector

-- | A field force with a custom force evaluator.
--
-- ObjC selector: @+ customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlock :: Ptr () -> IO (Id SCNPhysicsField)
customFieldWithEvaluationBlock block =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMessage cls' customFieldWithEvaluationBlockSelector block

-- | @- strength@
strength :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CDouble
strength scnPhysicsField =
  sendMessage scnPhysicsField strengthSelector

-- | @- setStrength:@
setStrength :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CDouble -> IO ()
setStrength scnPhysicsField value =
  sendMessage scnPhysicsField setStrengthSelector value

-- | @- falloffExponent@
falloffExponent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CDouble
falloffExponent scnPhysicsField =
  sendMessage scnPhysicsField falloffExponentSelector

-- | @- setFalloffExponent:@
setFalloffExponent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CDouble -> IO ()
setFalloffExponent scnPhysicsField value =
  sendMessage scnPhysicsField setFalloffExponentSelector value

-- | @- minimumDistance@
minimumDistance :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CDouble
minimumDistance scnPhysicsField =
  sendMessage scnPhysicsField minimumDistanceSelector

-- | @- setMinimumDistance:@
setMinimumDistance :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CDouble -> IO ()
setMinimumDistance scnPhysicsField value =
  sendMessage scnPhysicsField setMinimumDistanceSelector value

-- | @- active@
active :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO Bool
active scnPhysicsField =
  sendMessage scnPhysicsField activeSelector

-- | @- setActive:@
setActive :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> Bool -> IO ()
setActive scnPhysicsField value =
  sendMessage scnPhysicsField setActiveSelector value

-- | @- exclusive@
exclusive :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO Bool
exclusive scnPhysicsField =
  sendMessage scnPhysicsField exclusiveSelector

-- | @- setExclusive:@
setExclusive :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> Bool -> IO ()
setExclusive scnPhysicsField value =
  sendMessage scnPhysicsField setExclusiveSelector value

-- | @- halfExtent@
halfExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNVector3
halfExtent scnPhysicsField =
  sendMessage scnPhysicsField halfExtentSelector

-- | @- setHalfExtent:@
setHalfExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNVector3 -> IO ()
setHalfExtent scnPhysicsField value =
  sendMessage scnPhysicsField setHalfExtentSelector value

-- | @- usesEllipsoidalExtent@
usesEllipsoidalExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO Bool
usesEllipsoidalExtent scnPhysicsField =
  sendMessage scnPhysicsField usesEllipsoidalExtentSelector

-- | @- setUsesEllipsoidalExtent:@
setUsesEllipsoidalExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> Bool -> IO ()
setUsesEllipsoidalExtent scnPhysicsField value =
  sendMessage scnPhysicsField setUsesEllipsoidalExtentSelector value

-- | @- scope@
scope :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNPhysicsFieldScope
scope scnPhysicsField =
  sendMessage scnPhysicsField scopeSelector

-- | @- setScope:@
setScope :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNPhysicsFieldScope -> IO ()
setScope scnPhysicsField value =
  sendMessage scnPhysicsField setScopeSelector value

-- | @- offset@
offset :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNVector3
offset scnPhysicsField =
  sendMessage scnPhysicsField offsetSelector

-- | @- setOffset:@
setOffset :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNVector3 -> IO ()
setOffset scnPhysicsField value =
  sendMessage scnPhysicsField setOffsetSelector value

-- | @- direction@
direction :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNVector3
direction scnPhysicsField =
  sendMessage scnPhysicsField directionSelector

-- | @- setDirection:@
setDirection :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNVector3 -> IO ()
setDirection scnPhysicsField value =
  sendMessage scnPhysicsField setDirectionSelector value

-- | categoryBitMask
--
-- Determines the node physicsBody's categories that will be influenced by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CULong
categoryBitMask scnPhysicsField =
  sendMessage scnPhysicsField categoryBitMaskSelector

-- | categoryBitMask
--
-- Determines the node physicsBody's categories that will be influenced by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CULong -> IO ()
setCategoryBitMask scnPhysicsField value =
  sendMessage scnPhysicsField setCategoryBitMaskSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dragField@
dragFieldSelector :: Selector '[] (Id SCNPhysicsField)
dragFieldSelector = mkSelector "dragField"

-- | @Selector@ for @vortexField@
vortexFieldSelector :: Selector '[] (Id SCNPhysicsField)
vortexFieldSelector = mkSelector "vortexField"

-- | @Selector@ for @radialGravityField@
radialGravityFieldSelector :: Selector '[] (Id SCNPhysicsField)
radialGravityFieldSelector = mkSelector "radialGravityField"

-- | @Selector@ for @linearGravityField@
linearGravityFieldSelector :: Selector '[] (Id SCNPhysicsField)
linearGravityFieldSelector = mkSelector "linearGravityField"

-- | @Selector@ for @noiseFieldWithSmoothness:animationSpeed:@
noiseFieldWithSmoothness_animationSpeedSelector :: Selector '[CDouble, CDouble] (Id SCNPhysicsField)
noiseFieldWithSmoothness_animationSpeedSelector = mkSelector "noiseFieldWithSmoothness:animationSpeed:"

-- | @Selector@ for @turbulenceFieldWithSmoothness:animationSpeed:@
turbulenceFieldWithSmoothness_animationSpeedSelector :: Selector '[CDouble, CDouble] (Id SCNPhysicsField)
turbulenceFieldWithSmoothness_animationSpeedSelector = mkSelector "turbulenceFieldWithSmoothness:animationSpeed:"

-- | @Selector@ for @springField@
springFieldSelector :: Selector '[] (Id SCNPhysicsField)
springFieldSelector = mkSelector "springField"

-- | @Selector@ for @electricField@
electricFieldSelector :: Selector '[] (Id SCNPhysicsField)
electricFieldSelector = mkSelector "electricField"

-- | @Selector@ for @magneticField@
magneticFieldSelector :: Selector '[] (Id SCNPhysicsField)
magneticFieldSelector = mkSelector "magneticField"

-- | @Selector@ for @customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlockSelector :: Selector '[Ptr ()] (Id SCNPhysicsField)
customFieldWithEvaluationBlockSelector = mkSelector "customFieldWithEvaluationBlock:"

-- | @Selector@ for @strength@
strengthSelector :: Selector '[] CDouble
strengthSelector = mkSelector "strength"

-- | @Selector@ for @setStrength:@
setStrengthSelector :: Selector '[CDouble] ()
setStrengthSelector = mkSelector "setStrength:"

-- | @Selector@ for @falloffExponent@
falloffExponentSelector :: Selector '[] CDouble
falloffExponentSelector = mkSelector "falloffExponent"

-- | @Selector@ for @setFalloffExponent:@
setFalloffExponentSelector :: Selector '[CDouble] ()
setFalloffExponentSelector = mkSelector "setFalloffExponent:"

-- | @Selector@ for @minimumDistance@
minimumDistanceSelector :: Selector '[] CDouble
minimumDistanceSelector = mkSelector "minimumDistance"

-- | @Selector@ for @setMinimumDistance:@
setMinimumDistanceSelector :: Selector '[CDouble] ()
setMinimumDistanceSelector = mkSelector "setMinimumDistance:"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Bool] ()
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @exclusive@
exclusiveSelector :: Selector '[] Bool
exclusiveSelector = mkSelector "exclusive"

-- | @Selector@ for @setExclusive:@
setExclusiveSelector :: Selector '[Bool] ()
setExclusiveSelector = mkSelector "setExclusive:"

-- | @Selector@ for @halfExtent@
halfExtentSelector :: Selector '[] SCNVector3
halfExtentSelector = mkSelector "halfExtent"

-- | @Selector@ for @setHalfExtent:@
setHalfExtentSelector :: Selector '[SCNVector3] ()
setHalfExtentSelector = mkSelector "setHalfExtent:"

-- | @Selector@ for @usesEllipsoidalExtent@
usesEllipsoidalExtentSelector :: Selector '[] Bool
usesEllipsoidalExtentSelector = mkSelector "usesEllipsoidalExtent"

-- | @Selector@ for @setUsesEllipsoidalExtent:@
setUsesEllipsoidalExtentSelector :: Selector '[Bool] ()
setUsesEllipsoidalExtentSelector = mkSelector "setUsesEllipsoidalExtent:"

-- | @Selector@ for @scope@
scopeSelector :: Selector '[] SCNPhysicsFieldScope
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector '[SCNPhysicsFieldScope] ()
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] SCNVector3
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[SCNVector3] ()
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @direction@
directionSelector :: Selector '[] SCNVector3
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector '[SCNVector3] ()
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector '[] CULong
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector '[CULong] ()
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

