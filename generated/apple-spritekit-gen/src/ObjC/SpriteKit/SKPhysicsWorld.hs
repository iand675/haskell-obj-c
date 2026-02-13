{-# LANGUAGE DataKinds #-}
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
  , contactDelegateSelector
  , removeAllJointsSelector
  , removeJointSelector
  , setContactDelegateSelector
  , setSpeedSelector
  , speedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addJoint:@
addJoint :: (IsSKPhysicsWorld skPhysicsWorld, IsSKPhysicsJoint joint) => skPhysicsWorld -> joint -> IO ()
addJoint skPhysicsWorld joint =
  sendMessage skPhysicsWorld addJointSelector (toSKPhysicsJoint joint)

-- | @- removeJoint:@
removeJoint :: (IsSKPhysicsWorld skPhysicsWorld, IsSKPhysicsJoint joint) => skPhysicsWorld -> joint -> IO ()
removeJoint skPhysicsWorld joint =
  sendMessage skPhysicsWorld removeJointSelector (toSKPhysicsJoint joint)

-- | @- removeAllJoints@
removeAllJoints :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> IO ()
removeAllJoints skPhysicsWorld =
  sendMessage skPhysicsWorld removeAllJointsSelector

-- | @- speed@
speed :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> IO CDouble
speed skPhysicsWorld =
  sendMessage skPhysicsWorld speedSelector

-- | @- setSpeed:@
setSpeed :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> CDouble -> IO ()
setSpeed skPhysicsWorld value =
  sendMessage skPhysicsWorld setSpeedSelector value

-- | @- contactDelegate@
contactDelegate :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> IO RawId
contactDelegate skPhysicsWorld =
  sendMessage skPhysicsWorld contactDelegateSelector

-- | @- setContactDelegate:@
setContactDelegate :: IsSKPhysicsWorld skPhysicsWorld => skPhysicsWorld -> RawId -> IO ()
setContactDelegate skPhysicsWorld value =
  sendMessage skPhysicsWorld setContactDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addJoint:@
addJointSelector :: Selector '[Id SKPhysicsJoint] ()
addJointSelector = mkSelector "addJoint:"

-- | @Selector@ for @removeJoint:@
removeJointSelector :: Selector '[Id SKPhysicsJoint] ()
removeJointSelector = mkSelector "removeJoint:"

-- | @Selector@ for @removeAllJoints@
removeAllJointsSelector :: Selector '[] ()
removeAllJointsSelector = mkSelector "removeAllJoints"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CDouble
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[CDouble] ()
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @contactDelegate@
contactDelegateSelector :: Selector '[] RawId
contactDelegateSelector = mkSelector "contactDelegate"

-- | @Selector@ for @setContactDelegate:@
setContactDelegateSelector :: Selector '[RawId] ()
setContactDelegateSelector = mkSelector "setContactDelegate:"

