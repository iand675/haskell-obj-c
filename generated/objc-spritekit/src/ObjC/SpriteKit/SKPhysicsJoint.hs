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
  , setBodyASelector
  , bodyBSelector
  , setBodyBSelector
  , reactionTorqueSelector


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
bodyA :: IsSKPhysicsJoint skPhysicsJoint => skPhysicsJoint -> IO (Id SKPhysicsBody)
bodyA skPhysicsJoint  =
  sendMsg skPhysicsJoint (mkSelector "bodyA") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBodyA:@
setBodyA :: (IsSKPhysicsJoint skPhysicsJoint, IsSKPhysicsBody value) => skPhysicsJoint -> value -> IO ()
setBodyA skPhysicsJoint  value =
withObjCPtr value $ \raw_value ->
    sendMsg skPhysicsJoint (mkSelector "setBodyA:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- bodyB@
bodyB :: IsSKPhysicsJoint skPhysicsJoint => skPhysicsJoint -> IO (Id SKPhysicsBody)
bodyB skPhysicsJoint  =
  sendMsg skPhysicsJoint (mkSelector "bodyB") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBodyB:@
setBodyB :: (IsSKPhysicsJoint skPhysicsJoint, IsSKPhysicsBody value) => skPhysicsJoint -> value -> IO ()
setBodyB skPhysicsJoint  value =
withObjCPtr value $ \raw_value ->
    sendMsg skPhysicsJoint (mkSelector "setBodyB:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- reactionTorque@
reactionTorque :: IsSKPhysicsJoint skPhysicsJoint => skPhysicsJoint -> IO CDouble
reactionTorque skPhysicsJoint  =
  sendMsg skPhysicsJoint (mkSelector "reactionTorque") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyA@
bodyASelector :: Selector
bodyASelector = mkSelector "bodyA"

-- | @Selector@ for @setBodyA:@
setBodyASelector :: Selector
setBodyASelector = mkSelector "setBodyA:"

-- | @Selector@ for @bodyB@
bodyBSelector :: Selector
bodyBSelector = mkSelector "bodyB"

-- | @Selector@ for @setBodyB:@
setBodyBSelector :: Selector
setBodyBSelector = mkSelector "setBodyB:"

-- | @Selector@ for @reactionTorque@
reactionTorqueSelector :: Selector
reactionTorqueSelector = mkSelector "reactionTorque"

