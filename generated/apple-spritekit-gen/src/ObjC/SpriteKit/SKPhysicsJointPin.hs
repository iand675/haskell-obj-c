{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsJointPin@.
module ObjC.SpriteKit.SKPhysicsJointPin
  ( SKPhysicsJointPin
  , IsSKPhysicsJointPin(..)
  , shouldEnableLimits
  , setShouldEnableLimits
  , lowerAngleLimit
  , setLowerAngleLimit
  , upperAngleLimit
  , setUpperAngleLimit
  , frictionTorque
  , setFrictionTorque
  , rotationSpeed
  , setRotationSpeed
  , frictionTorqueSelector
  , lowerAngleLimitSelector
  , rotationSpeedSelector
  , setFrictionTorqueSelector
  , setLowerAngleLimitSelector
  , setRotationSpeedSelector
  , setShouldEnableLimitsSelector
  , setUpperAngleLimitSelector
  , shouldEnableLimitsSelector
  , upperAngleLimitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- shouldEnableLimits@
shouldEnableLimits :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO Bool
shouldEnableLimits skPhysicsJointPin =
  sendMessage skPhysicsJointPin shouldEnableLimitsSelector

-- | @- setShouldEnableLimits:@
setShouldEnableLimits :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> Bool -> IO ()
setShouldEnableLimits skPhysicsJointPin value =
  sendMessage skPhysicsJointPin setShouldEnableLimitsSelector value

-- | @- lowerAngleLimit@
lowerAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
lowerAngleLimit skPhysicsJointPin =
  sendMessage skPhysicsJointPin lowerAngleLimitSelector

-- | @- setLowerAngleLimit:@
setLowerAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setLowerAngleLimit skPhysicsJointPin value =
  sendMessage skPhysicsJointPin setLowerAngleLimitSelector value

-- | @- upperAngleLimit@
upperAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
upperAngleLimit skPhysicsJointPin =
  sendMessage skPhysicsJointPin upperAngleLimitSelector

-- | @- setUpperAngleLimit:@
setUpperAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setUpperAngleLimit skPhysicsJointPin value =
  sendMessage skPhysicsJointPin setUpperAngleLimitSelector value

-- | @- frictionTorque@
frictionTorque :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
frictionTorque skPhysicsJointPin =
  sendMessage skPhysicsJointPin frictionTorqueSelector

-- | @- setFrictionTorque:@
setFrictionTorque :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setFrictionTorque skPhysicsJointPin value =
  sendMessage skPhysicsJointPin setFrictionTorqueSelector value

-- | @- rotationSpeed@
rotationSpeed :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
rotationSpeed skPhysicsJointPin =
  sendMessage skPhysicsJointPin rotationSpeedSelector

-- | @- setRotationSpeed:@
setRotationSpeed :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setRotationSpeed skPhysicsJointPin value =
  sendMessage skPhysicsJointPin setRotationSpeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldEnableLimits@
shouldEnableLimitsSelector :: Selector '[] Bool
shouldEnableLimitsSelector = mkSelector "shouldEnableLimits"

-- | @Selector@ for @setShouldEnableLimits:@
setShouldEnableLimitsSelector :: Selector '[Bool] ()
setShouldEnableLimitsSelector = mkSelector "setShouldEnableLimits:"

-- | @Selector@ for @lowerAngleLimit@
lowerAngleLimitSelector :: Selector '[] CDouble
lowerAngleLimitSelector = mkSelector "lowerAngleLimit"

-- | @Selector@ for @setLowerAngleLimit:@
setLowerAngleLimitSelector :: Selector '[CDouble] ()
setLowerAngleLimitSelector = mkSelector "setLowerAngleLimit:"

-- | @Selector@ for @upperAngleLimit@
upperAngleLimitSelector :: Selector '[] CDouble
upperAngleLimitSelector = mkSelector "upperAngleLimit"

-- | @Selector@ for @setUpperAngleLimit:@
setUpperAngleLimitSelector :: Selector '[CDouble] ()
setUpperAngleLimitSelector = mkSelector "setUpperAngleLimit:"

-- | @Selector@ for @frictionTorque@
frictionTorqueSelector :: Selector '[] CDouble
frictionTorqueSelector = mkSelector "frictionTorque"

-- | @Selector@ for @setFrictionTorque:@
setFrictionTorqueSelector :: Selector '[CDouble] ()
setFrictionTorqueSelector = mkSelector "setFrictionTorque:"

-- | @Selector@ for @rotationSpeed@
rotationSpeedSelector :: Selector '[] CDouble
rotationSpeedSelector = mkSelector "rotationSpeed"

-- | @Selector@ for @setRotationSpeed:@
setRotationSpeedSelector :: Selector '[CDouble] ()
setRotationSpeedSelector = mkSelector "setRotationSpeed:"

