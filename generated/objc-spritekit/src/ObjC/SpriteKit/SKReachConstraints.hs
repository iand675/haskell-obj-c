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
  , upperAngleLimitSelector
  , setUpperAngleLimitSelector


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

-- | @- initWithLowerAngleLimit:upperAngleLimit:@
initWithLowerAngleLimit_upperAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> CDouble -> CDouble -> IO (Id SKReachConstraints)
initWithLowerAngleLimit_upperAngleLimit skReachConstraints  lowerAngleLimit upperAngleLimit =
  sendMsg skReachConstraints (mkSelector "initWithLowerAngleLimit:upperAngleLimit:") (retPtr retVoid) [argCDouble (fromIntegral lowerAngleLimit), argCDouble (fromIntegral upperAngleLimit)] >>= ownedObject . castPtr

-- | Lower angle limit in radians
--
-- ObjC selector: @- lowerAngleLimit@
lowerAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> IO CDouble
lowerAngleLimit skReachConstraints  =
  sendMsg skReachConstraints (mkSelector "lowerAngleLimit") retCDouble []

-- | Lower angle limit in radians
--
-- ObjC selector: @- setLowerAngleLimit:@
setLowerAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> CDouble -> IO ()
setLowerAngleLimit skReachConstraints  value =
  sendMsg skReachConstraints (mkSelector "setLowerAngleLimit:") retVoid [argCDouble (fromIntegral value)]

-- | Upper angle limit in radians
--
-- ObjC selector: @- upperAngleLimit@
upperAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> IO CDouble
upperAngleLimit skReachConstraints  =
  sendMsg skReachConstraints (mkSelector "upperAngleLimit") retCDouble []

-- | Upper angle limit in radians
--
-- ObjC selector: @- setUpperAngleLimit:@
setUpperAngleLimit :: IsSKReachConstraints skReachConstraints => skReachConstraints -> CDouble -> IO ()
setUpperAngleLimit skReachConstraints  value =
  sendMsg skReachConstraints (mkSelector "setUpperAngleLimit:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLowerAngleLimit:upperAngleLimit:@
initWithLowerAngleLimit_upperAngleLimitSelector :: Selector
initWithLowerAngleLimit_upperAngleLimitSelector = mkSelector "initWithLowerAngleLimit:upperAngleLimit:"

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

