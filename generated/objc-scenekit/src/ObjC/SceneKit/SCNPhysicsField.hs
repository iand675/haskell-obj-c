{-# LANGUAGE PatternSynonyms #-}
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
  , dragFieldSelector
  , vortexFieldSelector
  , radialGravityFieldSelector
  , linearGravityFieldSelector
  , noiseFieldWithSmoothness_animationSpeedSelector
  , turbulenceFieldWithSmoothness_animationSpeedSelector
  , springFieldSelector
  , electricFieldSelector
  , magneticFieldSelector
  , customFieldWithEvaluationBlockSelector
  , strengthSelector
  , setStrengthSelector
  , falloffExponentSelector
  , setFalloffExponentSelector
  , minimumDistanceSelector
  , setMinimumDistanceSelector
  , activeSelector
  , setActiveSelector
  , exclusiveSelector
  , setExclusiveSelector
  , halfExtentSelector
  , setHalfExtentSelector
  , usesEllipsoidalExtentSelector
  , setUsesEllipsoidalExtentSelector
  , scopeSelector
  , setScopeSelector
  , offsetSelector
  , setOffsetSelector
  , directionSelector
  , setDirectionSelector
  , categoryBitMaskSelector
  , setCategoryBitMaskSelector

  -- * Enum types
  , SCNPhysicsFieldScope(SCNPhysicsFieldScope)
  , pattern SCNPhysicsFieldScopeInsideExtent
  , pattern SCNPhysicsFieldScopeOutsideExtent

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

-- | Slows an object proportionally to the object’s velocity. Use this to simulate effects such as friction from motion through the air.
--
-- ObjC selector: @+ dragField@
dragField :: IO (Id SCNPhysicsField)
dragField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "dragField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a force tangential to the direction from the sample point to the field's position. The force will be CCW to the direction. Make the strength negative to apply force in the CW direction. Amount is proportional to distance from center and the object's mass. Use this to create effects such as tornadoes.
--
-- ObjC selector: @+ vortexField@
vortexField :: IO (Id SCNPhysicsField)
vortexField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "vortexField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a force in the direction of the origin of the field in local space. To repel objects, use a negative strength. The force is proportional to the distance from the field origin. Varies with the mass of the object according to F = ma The field node's rotation property can be used to orient the gravity in a particular direction.
--
-- ObjC selector: @+ radialGravityField@
radialGravityField :: IO (Id SCNPhysicsField)
radialGravityField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "radialGravityField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a force in the direction of the "direction" vector in the local space. To repel objects, use a negative strength. The force is the same everywhere in the field. Varies with the mass of the object according to F = ma The field node's rotation property can be used to orient the gravity in a particular direction.
--
-- ObjC selector: @+ linearGravityField@
linearGravityField :: IO (Id SCNPhysicsField)
linearGravityField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "linearGravityField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A time varying differentiable Perlin simplex noise field. A smoothness of 0 means as noisy as possible. Use this to simulate such effects as fireflies, or snow. To freeze the noise in place, set animationSpeed to 0.0. Mass is ignored. You can change the "smoothness" and "animationSpeed" using KVC.
--
-- ObjC selector: @+ noiseFieldWithSmoothness:animationSpeed:@
noiseFieldWithSmoothness_animationSpeed :: CDouble -> CDouble -> IO (Id SCNPhysicsField)
noiseFieldWithSmoothness_animationSpeed smoothness speed =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "noiseFieldWithSmoothness:animationSpeed:") (retPtr retVoid) [argCDouble (fromIntegral smoothness), argCDouble (fromIntegral speed)] >>= retainedObject . castPtr

-- | Just like Noise, except the strength of the noise is proportional to the velocity of the object in the field.
--
-- ObjC selector: @+ turbulenceFieldWithSmoothness:animationSpeed:@
turbulenceFieldWithSmoothness_animationSpeed :: CDouble -> CDouble -> IO (Id SCNPhysicsField)
turbulenceFieldWithSmoothness_animationSpeed smoothness speed =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "turbulenceFieldWithSmoothness:animationSpeed:") (retPtr retVoid) [argCDouble (fromIntegral smoothness), argCDouble (fromIntegral speed)] >>= retainedObject . castPtr

-- | A Hooke’s law force - a force linearly proportional to distance from the center of the field. An object in this field will oscillate with a period proportional to the inverse of the mass. An example use is to keep objects confined to a particular region.
--
-- ObjC selector: @+ springField@
springField :: IO (Id SCNPhysicsField)
springField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "springField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A force proportional to the charge on the object. An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the first part of the Lorentz equation, F = qE
--
-- ObjC selector: @+ electricField@
electricField :: IO (Id SCNPhysicsField)
electricField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "electricField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A force proportional to the charge on the object and the object’s velocity.  An example use of this field is to make objects behavior differently from one another when they enter a region, or to make an object's behavior different than its mass based behavior This field models the second part of the Lorentz equation, F = qvB
--
-- ObjC selector: @+ magneticField@
magneticField :: IO (Id SCNPhysicsField)
magneticField  =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "magneticField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A field force with a custom force evaluator.
--
-- ObjC selector: @+ customFieldWithEvaluationBlock:@
customFieldWithEvaluationBlock :: Ptr () -> IO (Id SCNPhysicsField)
customFieldWithEvaluationBlock block =
  do
    cls' <- getRequiredClass "SCNPhysicsField"
    sendClassMsg cls' (mkSelector "customFieldWithEvaluationBlock:") (retPtr retVoid) [argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- | @- strength@
strength :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CDouble
strength scnPhysicsField  =
  sendMsg scnPhysicsField (mkSelector "strength") retCDouble []

-- | @- setStrength:@
setStrength :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CDouble -> IO ()
setStrength scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setStrength:") retVoid [argCDouble (fromIntegral value)]

-- | @- falloffExponent@
falloffExponent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CDouble
falloffExponent scnPhysicsField  =
  sendMsg scnPhysicsField (mkSelector "falloffExponent") retCDouble []

-- | @- setFalloffExponent:@
setFalloffExponent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CDouble -> IO ()
setFalloffExponent scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setFalloffExponent:") retVoid [argCDouble (fromIntegral value)]

-- | @- minimumDistance@
minimumDistance :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CDouble
minimumDistance scnPhysicsField  =
  sendMsg scnPhysicsField (mkSelector "minimumDistance") retCDouble []

-- | @- setMinimumDistance:@
setMinimumDistance :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CDouble -> IO ()
setMinimumDistance scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setMinimumDistance:") retVoid [argCDouble (fromIntegral value)]

-- | @- active@
active :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO Bool
active scnPhysicsField  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsField (mkSelector "active") retCULong []

-- | @- setActive:@
setActive :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> Bool -> IO ()
setActive scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setActive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- exclusive@
exclusive :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO Bool
exclusive scnPhysicsField  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsField (mkSelector "exclusive") retCULong []

-- | @- setExclusive:@
setExclusive :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> Bool -> IO ()
setExclusive scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setExclusive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- halfExtent@
halfExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNVector3
halfExtent scnPhysicsField  =
  sendMsgStret scnPhysicsField (mkSelector "halfExtent") retSCNVector3 []

-- | @- setHalfExtent:@
setHalfExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNVector3 -> IO ()
setHalfExtent scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setHalfExtent:") retVoid [argSCNVector3 value]

-- | @- usesEllipsoidalExtent@
usesEllipsoidalExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO Bool
usesEllipsoidalExtent scnPhysicsField  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnPhysicsField (mkSelector "usesEllipsoidalExtent") retCULong []

-- | @- setUsesEllipsoidalExtent:@
setUsesEllipsoidalExtent :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> Bool -> IO ()
setUsesEllipsoidalExtent scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setUsesEllipsoidalExtent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scope@
scope :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNPhysicsFieldScope
scope scnPhysicsField  =
  fmap (coerce :: CLong -> SCNPhysicsFieldScope) $ sendMsg scnPhysicsField (mkSelector "scope") retCLong []

-- | @- setScope:@
setScope :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNPhysicsFieldScope -> IO ()
setScope scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setScope:") retVoid [argCLong (coerce value)]

-- | @- offset@
offset :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNVector3
offset scnPhysicsField  =
  sendMsgStret scnPhysicsField (mkSelector "offset") retSCNVector3 []

-- | @- setOffset:@
setOffset :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNVector3 -> IO ()
setOffset scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setOffset:") retVoid [argSCNVector3 value]

-- | @- direction@
direction :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO SCNVector3
direction scnPhysicsField  =
  sendMsgStret scnPhysicsField (mkSelector "direction") retSCNVector3 []

-- | @- setDirection:@
setDirection :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> SCNVector3 -> IO ()
setDirection scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setDirection:") retVoid [argSCNVector3 value]

-- | categoryBitMask
--
-- Determines the node physicsBody's categories that will be influenced by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- categoryBitMask@
categoryBitMask :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> IO CULong
categoryBitMask scnPhysicsField  =
  sendMsg scnPhysicsField (mkSelector "categoryBitMask") retCULong []

-- | categoryBitMask
--
-- Determines the node physicsBody's categories that will be influenced by the receiver. Defaults to all bit set.
--
-- ObjC selector: @- setCategoryBitMask:@
setCategoryBitMask :: IsSCNPhysicsField scnPhysicsField => scnPhysicsField -> CULong -> IO ()
setCategoryBitMask scnPhysicsField  value =
  sendMsg scnPhysicsField (mkSelector "setCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

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

-- | @Selector@ for @linearGravityField@
linearGravityFieldSelector :: Selector
linearGravityFieldSelector = mkSelector "linearGravityField"

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

-- | @Selector@ for @strength@
strengthSelector :: Selector
strengthSelector = mkSelector "strength"

-- | @Selector@ for @setStrength:@
setStrengthSelector :: Selector
setStrengthSelector = mkSelector "setStrength:"

-- | @Selector@ for @falloffExponent@
falloffExponentSelector :: Selector
falloffExponentSelector = mkSelector "falloffExponent"

-- | @Selector@ for @setFalloffExponent:@
setFalloffExponentSelector :: Selector
setFalloffExponentSelector = mkSelector "setFalloffExponent:"

-- | @Selector@ for @minimumDistance@
minimumDistanceSelector :: Selector
minimumDistanceSelector = mkSelector "minimumDistance"

-- | @Selector@ for @setMinimumDistance:@
setMinimumDistanceSelector :: Selector
setMinimumDistanceSelector = mkSelector "setMinimumDistance:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @exclusive@
exclusiveSelector :: Selector
exclusiveSelector = mkSelector "exclusive"

-- | @Selector@ for @setExclusive:@
setExclusiveSelector :: Selector
setExclusiveSelector = mkSelector "setExclusive:"

-- | @Selector@ for @halfExtent@
halfExtentSelector :: Selector
halfExtentSelector = mkSelector "halfExtent"

-- | @Selector@ for @setHalfExtent:@
setHalfExtentSelector :: Selector
setHalfExtentSelector = mkSelector "setHalfExtent:"

-- | @Selector@ for @usesEllipsoidalExtent@
usesEllipsoidalExtentSelector :: Selector
usesEllipsoidalExtentSelector = mkSelector "usesEllipsoidalExtent"

-- | @Selector@ for @setUsesEllipsoidalExtent:@
setUsesEllipsoidalExtentSelector :: Selector
setUsesEllipsoidalExtentSelector = mkSelector "setUsesEllipsoidalExtent:"

-- | @Selector@ for @scope@
scopeSelector :: Selector
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @setDirection:@
setDirectionSelector :: Selector
setDirectionSelector = mkSelector "setDirection:"

-- | @Selector@ for @categoryBitMask@
categoryBitMaskSelector :: Selector
categoryBitMaskSelector = mkSelector "categoryBitMask"

-- | @Selector@ for @setCategoryBitMask:@
setCategoryBitMaskSelector :: Selector
setCategoryBitMaskSelector = mkSelector "setCategoryBitMask:"

