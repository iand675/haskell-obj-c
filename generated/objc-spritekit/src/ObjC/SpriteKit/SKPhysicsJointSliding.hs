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
  , shouldEnableLimitsSelector
  , setShouldEnableLimitsSelector
  , lowerDistanceLimitSelector
  , setLowerDistanceLimitSelector
  , upperDistanceLimitSelector
  , setUpperDistanceLimitSelector


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
shouldEnableLimits :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> IO Bool
shouldEnableLimits skPhysicsJointSliding  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skPhysicsJointSliding (mkSelector "shouldEnableLimits") retCULong []

-- | @- setShouldEnableLimits:@
setShouldEnableLimits :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> Bool -> IO ()
setShouldEnableLimits skPhysicsJointSliding  value =
  sendMsg skPhysicsJointSliding (mkSelector "setShouldEnableLimits:") retVoid [argCULong (if value then 1 else 0)]

-- | @- lowerDistanceLimit@
lowerDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> IO CDouble
lowerDistanceLimit skPhysicsJointSliding  =
  sendMsg skPhysicsJointSliding (mkSelector "lowerDistanceLimit") retCDouble []

-- | @- setLowerDistanceLimit:@
setLowerDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> CDouble -> IO ()
setLowerDistanceLimit skPhysicsJointSliding  value =
  sendMsg skPhysicsJointSliding (mkSelector "setLowerDistanceLimit:") retVoid [argCDouble (fromIntegral value)]

-- | @- upperDistanceLimit@
upperDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> IO CDouble
upperDistanceLimit skPhysicsJointSliding  =
  sendMsg skPhysicsJointSliding (mkSelector "upperDistanceLimit") retCDouble []

-- | @- setUpperDistanceLimit:@
setUpperDistanceLimit :: IsSKPhysicsJointSliding skPhysicsJointSliding => skPhysicsJointSliding -> CDouble -> IO ()
setUpperDistanceLimit skPhysicsJointSliding  value =
  sendMsg skPhysicsJointSliding (mkSelector "setUpperDistanceLimit:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @shouldEnableLimits@
shouldEnableLimitsSelector :: Selector
shouldEnableLimitsSelector = mkSelector "shouldEnableLimits"

-- | @Selector@ for @setShouldEnableLimits:@
setShouldEnableLimitsSelector :: Selector
setShouldEnableLimitsSelector = mkSelector "setShouldEnableLimits:"

-- | @Selector@ for @lowerDistanceLimit@
lowerDistanceLimitSelector :: Selector
lowerDistanceLimitSelector = mkSelector "lowerDistanceLimit"

-- | @Selector@ for @setLowerDistanceLimit:@
setLowerDistanceLimitSelector :: Selector
setLowerDistanceLimitSelector = mkSelector "setLowerDistanceLimit:"

-- | @Selector@ for @upperDistanceLimit@
upperDistanceLimitSelector :: Selector
upperDistanceLimitSelector = mkSelector "upperDistanceLimit"

-- | @Selector@ for @setUpperDistanceLimit:@
setUpperDistanceLimitSelector :: Selector
setUpperDistanceLimitSelector = mkSelector "setUpperDistanceLimit:"

