{-# LANGUAGE DataKinds #-}
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
  , frequencySelector
  , setDampingSelector
  , setFrequencySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- damping@
damping :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> IO CDouble
damping skPhysicsJointSpring =
  sendMessage skPhysicsJointSpring dampingSelector

-- | @- setDamping:@
setDamping :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> CDouble -> IO ()
setDamping skPhysicsJointSpring value =
  sendMessage skPhysicsJointSpring setDampingSelector value

-- | @- frequency@
frequency :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> IO CDouble
frequency skPhysicsJointSpring =
  sendMessage skPhysicsJointSpring frequencySelector

-- | @- setFrequency:@
setFrequency :: IsSKPhysicsJointSpring skPhysicsJointSpring => skPhysicsJointSpring -> CDouble -> IO ()
setFrequency skPhysicsJointSpring value =
  sendMessage skPhysicsJointSpring setFrequencySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @damping@
dampingSelector :: Selector '[] CDouble
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector '[CDouble] ()
setDampingSelector = mkSelector "setDamping:"

-- | @Selector@ for @frequency@
frequencySelector :: Selector '[] CDouble
frequencySelector = mkSelector "frequency"

-- | @Selector@ for @setFrequency:@
setFrequencySelector :: Selector '[CDouble] ()
setFrequencySelector = mkSelector "setFrequency:"

