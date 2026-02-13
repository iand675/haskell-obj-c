{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKReachConstraints@.
module ObjC.SpriteKit.SKReachConstraints
  ( SKReachConstraints
  , IsSKReachConstraints(..)
  , initWithLowerAngleLimit_upperAngleLimit
  , lowerAngleLimit
  , setLowerAngleLimit
  , upperAngleLimit
  , setUpperAngleLimit
  , initWithLowerAngleLimit_upperAngleLimitSelector
  , lowerAngleLimitSelector
  , setLowerAngleLimitSelector
  , setUpperAngleLimitSelector
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

-- | @- initWithLowerAngleLimit:upperAngleLimit:@
initWithLowerAngleLimit_upperAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> CDouble -> CDouble -> IO (Id SKReachConstraints)
initWithLowerAngleLimit_upperAngleLimit skReachConstraints lowerAngleLimit upperAngleLimit =
  sendOwnedMessage skReachConstraints initWithLowerAngleLimit_upperAngleLimitSelector lowerAngleLimit upperAngleLimit

-- | Lower angle limit in radians
--
-- ObjC selector: @- lowerAngleLimit@
lowerAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> IO CDouble
lowerAngleLimit skReachConstraints =
  sendMessage skReachConstraints lowerAngleLimitSelector

-- | Lower angle limit in radians
--
-- ObjC selector: @- setLowerAngleLimit:@
setLowerAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> CDouble -> IO ()
setLowerAngleLimit skReachConstraints value =
  sendMessage skReachConstraints setLowerAngleLimitSelector value

-- | Upper angle limit in radians
--
-- ObjC selector: @- upperAngleLimit@
upperAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> IO CDouble
upperAngleLimit skReachConstraints =
  sendMessage skReachConstraints upperAngleLimitSelector

-- | Upper angle limit in radians
--
-- ObjC selector: @- setUpperAngleLimit:@
setUpperAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> CDouble -> IO ()
setUpperAngleLimit skReachConstraints value =
  sendMessage skReachConstraints setUpperAngleLimitSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLowerAngleLimit:upperAngleLimit:@
initWithLowerAngleLimit_upperAngleLimitSelector :: Selector '[CDouble, CDouble] (Id SKReachConstraints)
initWithLowerAngleLimit_upperAngleLimitSelector = mkSelector "initWithLowerAngleLimit:upperAngleLimit:"

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

