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
  , shouldEnableLimitsSelector
  , setShouldEnableLimitsSelector
  , lowerAngleLimitSelector
  , setLowerAngleLimitSelector
  , upperAngleLimitSelector
  , setUpperAngleLimitSelector
  , frictionTorqueSelector
  , setFrictionTorqueSelector
  , rotationSpeedSelector
  , setRotationSpeedSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- shouldEnableLimits@
shouldEnableLimits :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO Bool
shouldEnableLimits skPhysicsJointPin  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsJointPin (mkSelector "shouldEnableLimits") retCULong []

-- | @- setShouldEnableLimits:@
setShouldEnableLimits :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> Bool -> IO ()
setShouldEnableLimits skPhysicsJointPin  value =
  sendMsg skPhysicsJointPin (mkSelector "setShouldEnableLimits:") retVoid [argCULong (if value then 1 else 0)]

-- | @- lowerAngleLimit@
lowerAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
lowerAngleLimit skPhysicsJointPin  =
  sendMsg skPhysicsJointPin (mkSelector "lowerAngleLimit") retCDouble []

-- | @- setLowerAngleLimit:@
setLowerAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setLowerAngleLimit skPhysicsJointPin  value =
  sendMsg skPhysicsJointPin (mkSelector "setLowerAngleLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- upperAngleLimit@
upperAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
upperAngleLimit skPhysicsJointPin  =
  sendMsg skPhysicsJointPin (mkSelector "upperAngleLimit") retCDouble []

-- | @- setUpperAngleLimit:@
setUpperAngleLimit :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setUpperAngleLimit skPhysicsJointPin  value =
  sendMsg skPhysicsJointPin (mkSelector "setUpperAngleLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- frictionTorque@
frictionTorque :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
frictionTorque skPhysicsJointPin  =
  sendMsg skPhysicsJointPin (mkSelector "frictionTorque") retCDouble []

-- | @- setFrictionTorque:@
setFrictionTorque :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setFrictionTorque skPhysicsJointPin  value =
  sendMsg skPhysicsJointPin (mkSelector "setFrictionTorque:") retVoid [argCDouble (fromIntegral value)]

-- | @- rotationSpeed@
rotationSpeed :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> IO CDouble
rotationSpeed skPhysicsJointPin  =
  sendMsg skPhysicsJointPin (mkSelector "rotationSpeed") retCDouble []

-- | @- setRotationSpeed:@
setRotationSpeed :: IsSKPhysicsJointPin skPhysicsJointPin => skPhysicsJointPin -> CDouble -> IO ()
setRotationSpeed skPhysicsJointPin  value =
  sendMsg skPhysicsJointPin (mkSelector "setRotationSpeed:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldEnableLimits@
shouldEnableLimitsSelector :: Selector
shouldEnableLimitsSelector = mkSelector "shouldEnableLimits"

-- | @Selector@ for @setShouldEnableLimits:@
setShouldEnableLimitsSelector :: Selector
setShouldEnableLimitsSelector = mkSelector "setShouldEnableLimits:"

-- | @Selector@ for @lowerAngleLimit@
lowerAngleLimitSelector :: Selector
lowerAngleLimitSelector = mkSelector "lowerAngleLimit"

-- | @Selector@ for @setLowerAngleLimit:@
setLowerAngleLimitSelector :: Selector
setLowerAngleLimitSelector = mkSelector "setLowerAngleLimit:"

-- | @Selector@ for @upperAngleLimit@
upperAngleLimitSelector :: Selector
upperAngleLimitSelector = mkSelector "upperAngleLimit"

-- | @Selector@ for @setUpperAngleLimit:@
setUpperAngleLimitSelector :: Selector
setUpperAngleLimitSelector = mkSelector "setUpperAngleLimit:"

-- | @Selector@ for @frictionTorque@
frictionTorqueSelector :: Selector
frictionTorqueSelector = mkSelector "frictionTorque"

-- | @Selector@ for @setFrictionTorque:@
setFrictionTorqueSelector :: Selector
setFrictionTorqueSelector = mkSelector "setFrictionTorque:"

-- | @Selector@ for @rotationSpeed@
rotationSpeedSelector :: Selector
rotationSpeedSelector = mkSelector "rotationSpeed"

-- | @Selector@ for @setRotationSpeed:@
setRotationSpeedSelector :: Selector
setRotationSpeedSelector = mkSelector "setRotationSpeed:"

