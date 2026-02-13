{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsContact@.
module ObjC.SpriteKit.SKPhysicsContact
  ( SKPhysicsContact
  , IsSKPhysicsContact(..)
  , bodyA
  , bodyB
  , collisionImpulse
  , bodyASelector
  , bodyBSelector
  , collisionImpulseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- bodyA@
bodyA :: IsSKPhysicsContact skPhysicsContact => skPhysicsContact -> IO (Id SKPhysicsBody)
bodyA skPhysicsContact =
  sendMessage skPhysicsContact bodyASelector

-- | @- bodyB@
bodyB :: IsSKPhysicsContact skPhysicsContact => skPhysicsContact -> IO (Id SKPhysicsBody)
bodyB skPhysicsContact =
  sendMessage skPhysicsContact bodyBSelector

-- | @- collisionImpulse@
collisionImpulse :: IsSKPhysicsContact skPhysicsContact => skPhysicsContact -> IO CDouble
collisionImpulse skPhysicsContact =
  sendMessage skPhysicsContact collisionImpulseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyA@
bodyASelector :: Selector '[] (Id SKPhysicsBody)
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector '[] (Id SKPhysicsBody)
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @collisionImpulse@
collisionImpulseSelector :: Selector '[] CDouble
collisionImpulseSelector = mkSelector "collisionImpulse"

