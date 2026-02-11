{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsJointSpring@.
module ObjC.SpriteKit.SKPhysicsJointSpring
  ( SKPhysicsJointSpring
  , IsSKPhysicsJointSpring(..)
  , damping
  , setDamping
  , frequency
  , setFrequency
  , dampingSelector
  , setDampingSelector
  , frequencySelector
  , setFrequencySelector


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

-- | @- damping@
damping :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> IO CDouble
damping skPhysicsJointSpring  =
  sendMsg skPhysicsJointSpring (mkSelector "damping") retCDouble []

-- | @- setDamping:@
setDamping :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> CDouble -> IO ()
setDamping skPhysicsJointSpring  value =
  sendMsg skPhysicsJointSpring (mkSelector "setDamping:") retVoid [argCDouble (fromIntegral value)]

-- | @- frequency@
frequency :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> IO CDouble
frequency skPhysicsJointSpring  =
  sendMsg skPhysicsJointSpring (mkSelector "frequency") retCDouble []

-- | @- setFrequency:@
setFrequency :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> CDouble -> IO ()
setFrequency skPhysicsJointSpring  value =
  sendMsg skPhysicsJointSpring (mkSelector "setFrequency:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @damping@
dampingSelector :: Selector
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector
setDampingSelector = mkSelector "setDamping:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector
setFrequencySelector = mkSelector "setFrequency:"

