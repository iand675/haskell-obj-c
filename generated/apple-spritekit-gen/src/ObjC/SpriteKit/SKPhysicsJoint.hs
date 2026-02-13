{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsJoint@.
module ObjC.SpriteKit.SKPhysicsJoint
  ( SKPhysicsJoint
  , IsSKPhysicsJoint(..)
  , bodyA
  , setBodyA
  , bodyB
  , setBodyB
  , reactionTorque
  , bodyASelector
  , bodyBSelector
  , reactionTorqueSelector
  , setBodyASelector
  , setBodyBSelector


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
bodyA :: IsSKPhysicsJoint skPhysicsJoint => skPhysicsJoint -> IO (Id SKPhysicsBody)
bodyA skPhysicsJoint =
  sendMessage skPhysicsJoint bodyASelector

-- | @- setBodyA:@
setBodyA :: (IsSKPhysicsJoint skPhysicsJoint, IsSKPhysicsBody value) => skPhysicsJoint -> value -> IO ()
setBodyA skPhysicsJoint value =
  sendMessage skPhysicsJoint setBodyASelector (toSKPhysicsBody value)

-- | @- bodyB@
bodyB :: IsSKPhysicsJoint skPhysicsJoint => skPhysicsJoint -> IO (Id SKPhysicsBody)
bodyB skPhysicsJoint =
  sendMessage skPhysicsJoint bodyBSelector

-- | @- setBodyB:@
setBodyB :: (IsSKPhysicsJoint skPhysicsJoint, IsSKPhysicsBody value) => skPhysicsJoint -> value -> IO ()
setBodyB skPhysicsJoint value =
  sendMessage skPhysicsJoint setBodyBSelector (toSKPhysicsBody value)

-- | @- reactionTorque@
reactionTorque :: IsSKPhysicsJoint skPhysicsJoint => skPhysicsJoint -> IO CDouble
reactionTorque skPhysicsJoint =
  sendMessage skPhysicsJoint reactionTorqueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyA@
bodyASelector :: Selector '[] (Id SKPhysicsBody)
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @setBodyA:@
setBodyASelector :: Selector '[Id SKPhysicsBody] ()
setBodyASelector = mkSelector "setBodyA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector '[] (Id SKPhysicsBody)
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @setBodyB:@
setBodyBSelector :: Selector '[Id SKPhysicsBody] ()
setBodyBSelector = mkSelector "setBodyB:"

-- | @Selector@ for @reactionTorque@
reactionTorqueSelector :: Selector '[] CDouble
reactionTorqueSelector = mkSelector "reactionTorque"

