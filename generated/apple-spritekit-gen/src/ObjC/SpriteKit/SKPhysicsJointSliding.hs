{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsJointSliding@.
module ObjC.SpriteKit.SKPhysicsJointSliding
  ( SKPhysicsJointSliding
  , IsSKPhysicsJointSliding(..)
  , shouldEnableLimits
  , setShouldEnableLimits
  , lowerDistanceLimit
  , setLowerDistanceLimit
  , upperDistanceLimit
  , setUpperDistanceLimit
  , lowerDistanceLimitSelector
  , setLowerDistanceLimitSelector
  , setShouldEnableLimitsSelector
  , setUpperDistanceLimitSelector
  , shouldEnableLimitsSelector
  , upperDistanceLimitSelector


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
shouldEnableLimits :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> IO Bool
shouldEnableLimits skPhysicsJointSliding =
  sendMessage skPhysicsJointSliding shouldEnableLimitsSelector

-- | @- setShouldEnableLimits:@
setShouldEnableLimits :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> Bool -> IO ()
setShouldEnableLimits skPhysicsJointSliding value =
  sendMessage skPhysicsJointSliding setShouldEnableLimitsSelector value

-- | @- lowerDistanceLimit@
lowerDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> IO CDouble
lowerDistanceLimit skPhysicsJointSliding =
  sendMessage skPhysicsJointSliding lowerDistanceLimitSelector

-- | @- setLowerDistanceLimit:@
setLowerDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> CDouble -> IO ()
setLowerDistanceLimit skPhysicsJointSliding value =
  sendMessage skPhysicsJointSliding setLowerDistanceLimitSelector value

-- | @- upperDistanceLimit@
upperDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> IO CDouble
upperDistanceLimit skPhysicsJointSliding =
  sendMessage skPhysicsJointSliding upperDistanceLimitSelector

-- | @- setUpperDistanceLimit:@
setUpperDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> CDouble -> IO ()
setUpperDistanceLimit skPhysicsJointSliding value =
  sendMessage skPhysicsJointSliding setUpperDistanceLimitSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldEnableLimits@
shouldEnableLimitsSelector :: Selector '[] Bool
shouldEnableLimitsSelector = mkSelector "shouldEnableLimits"

-- | @Selector@ for @setShouldEnableLimits:@
setShouldEnableLimitsSelector :: Selector '[Bool] ()
setShouldEnableLimitsSelector = mkSelector "setShouldEnableLimits:"

-- | @Selector@ for @lowerDistanceLimit@
lowerDistanceLimitSelector :: Selector '[] CDouble
lowerDistanceLimitSelector = mkSelector "lowerDistanceLimit"

-- | @Selector@ for @setLowerDistanceLimit:@
setLowerDistanceLimitSelector :: Selector '[CDouble] ()
setLowerDistanceLimitSelector = mkSelector "setLowerDistanceLimit:"

-- | @Selector@ for @upperDistanceLimit@
upperDistanceLimitSelector :: Selector '[] CDouble
upperDistanceLimitSelector = mkSelector "upperDistanceLimit"

-- | @Selector@ for @setUpperDistanceLimit:@
setUpperDistanceLimitSelector :: Selector '[CDouble] ()
setUpperDistanceLimitSelector = mkSelector "setUpperDistanceLimit:"

