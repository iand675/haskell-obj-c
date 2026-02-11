{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPhysicsWorld@.
module ObjC.SpriteKit.SKPhysicsWorld
  ( SKPhysicsWorld
  , IsSKPhysicsWorld(..)
  , addJoint
  , removeJoint
  , removeAllJoints
  , speed
  , setSpeed
  , contactDelegate
  , setContactDelegate
  , addJointSelector
  , removeJointSelector
  , removeAllJointsSelector
  , speedSelector
  , setSpeedSelector
  , contactDelegateSelector
  , setContactDelegateSelector


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

-- | @- addJoint:@
addJoint :: (IsSKPhysicsWorld skPhysicsWorld, IsSKPhysicsJoint joint) => skPhysicsWorld -> joint -> IO ()
addJoint skPhysicsWorld  joint =
  withObjCPtr joint $ \raw_joint ->
      sendMsg skPhysicsWorld (mkSelector "addJoint:") retVoid [argPtr (castPtr raw_joint :: Ptr ())]

-- | @- removeJoint:@
removeJoint :: (IsSKPhysicsWorld skPhysicsWorld, IsSKPhysicsJoint joint) => skPhysicsWorld -> joint -> IO ()
removeJoint skPhysicsWorld  joint =
  withObjCPtr joint $ \raw_joint ->
      sendMsg skPhysicsWorld (mkSelector "removeJoint:") retVoid [argPtr (castPtr raw_joint :: Ptr ())]

-- | @- removeAllJoints@
removeAllJoints :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> IO ()
removeAllJoints skPhysicsWorld  =
    sendMsg skPhysicsWorld (mkSelector "removeAllJoints") retVoid []

-- | @- speed@
speed :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> IO CDouble
speed skPhysicsWorld  =
    sendMsg skPhysicsWorld (mkSelector "speed") retCDouble []

-- | @- setSpeed:@
setSpeed :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> CDouble -> IO ()
setSpeed skPhysicsWorld  value =
    sendMsg skPhysicsWorld (mkSelector "setSpeed:") retVoid [argCDouble value]

-- | @- contactDelegate@
contactDelegate :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> IO RawId
contactDelegate skPhysicsWorld  =
    fmap (RawId . castPtr) $ sendMsg skPhysicsWorld (mkSelector "contactDelegate") (retPtr retVoid) []

-- | @- setContactDelegate:@
setContactDelegate :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> RawId -> IO ()
setContactDelegate skPhysicsWorld  value =
    sendMsg skPhysicsWorld (mkSelector "setContactDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addJoint:@
addJointSelector :: Selector
addJointSelector = mkSelector "addJoint:"

-- | @Selector@ for @removeJoint:@
removeJointSelector :: Selector
removeJointSelector = mkSelector "removeJoint:"

-- | @Selector@ for @removeAllJoints@
removeAllJointsSelector :: Selector
removeAllJointsSelector = mkSelector "removeAllJoints"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @contactDelegate@
contactDelegateSelector :: Selector
contactDelegateSelector = mkSelector "contactDelegate"

-- | @Selector@ for @setContactDelegate:@
setContactDelegateSelector :: Selector
setContactDelegateSelector = mkSelector "setContactDelegate:"

