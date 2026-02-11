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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- bodyA@
bodyA :: IsSKPhysicsContact skPhysicsContact => skPhysicsContact -> IO (Id SKPhysicsBody)
bodyA skPhysicsContact  =
  sendMsg skPhysicsContact (mkSelector "bodyA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bodyB@
bodyB :: IsSKPhysicsContact skPhysicsContact => skPhysicsContact -> IO (Id SKPhysicsBody)
bodyB skPhysicsContact  =
  sendMsg skPhysicsContact (mkSelector "bodyB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collisionImpulse@
collisionImpulse :: IsSKPhysicsContact skPhysicsContact => skPhysicsContact -> IO CDouble
collisionImpulse skPhysicsContact  =
  sendMsg skPhysicsContact (mkSelector "collisionImpulse") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyA@
bodyASelector :: Selector
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @collisionImpulse@
collisionImpulseSelector :: Selector
collisionImpulseSelector = mkSelector "collisionImpulse"

