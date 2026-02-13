{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNLookAtConstraint
--
-- A SCNLookAtConstraint applies on a node's orientation so that it always look at another node.
--
-- Generated bindings for @SCNLookAtConstraint@.
module ObjC.SceneKit.SCNLookAtConstraint
  ( SCNLookAtConstraint
  , IsSCNLookAtConstraint(..)
  , lookAtConstraintWithTarget
  , target
  , setTarget
  , targetOffset
  , setTargetOffset
  , localFront
  , setLocalFront
  , worldUp
  , setWorldUp
  , gimbalLockEnabled
  , setGimbalLockEnabled
  , gimbalLockEnabledSelector
  , localFrontSelector
  , lookAtConstraintWithTargetSelector
  , setGimbalLockEnabledSelector
  , setLocalFrontSelector
  , setTargetOffsetSelector
  , setTargetSelector
  , setWorldUpSelector
  , targetOffsetSelector
  , targetSelector
  , worldUpSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | lookAtConstraintWithTarget:
--
-- Creates and returns a SCNLookAtConstraint object with the specified target.
--
-- @target@ — The target node to look at.
--
-- ObjC selector: @+ lookAtConstraintWithTarget:@
lookAtConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNLookAtConstraint)
lookAtConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNLookAtConstraint"
    sendClassMessage cls' lookAtConstraintWithTargetSelector (toSCNNode target)

-- | @- target@
target :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO (Id SCNNode)
target scnLookAtConstraint =
  sendMessage scnLookAtConstraint targetSelector

-- | @- setTarget:@
setTarget :: (IsSCNLookAtConstraint scnLookAtConstraint, IsSCNNode target) => scnLookAtConstraint -> target -> IO ()
setTarget scnLookAtConstraint target =
  sendMessage scnLookAtConstraint setTargetSelector (toSCNNode target)

-- | targetOffset
--
-- Offset look at position in target space. Defaults to zero. Animatable
--
-- ObjC selector: @- targetOffset@
targetOffset :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO SCNVector3
targetOffset scnLookAtConstraint =
  sendMessage scnLookAtConstraint targetOffsetSelector

-- | targetOffset
--
-- Offset look at position in target space. Defaults to zero. Animatable
--
-- ObjC selector: @- setTargetOffset:@
setTargetOffset :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> SCNVector3 -> IO ()
setTargetOffset scnLookAtConstraint value =
  sendMessage scnLookAtConstraint setTargetOffsetSelector value

-- | targetOffset
--
-- Front direction in the constraint owner local space. Defaults to -[SCNNode localFront]. Animatable
--
-- ObjC selector: @- localFront@
localFront :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO SCNVector3
localFront scnLookAtConstraint =
  sendMessage scnLookAtConstraint localFrontSelector

-- | targetOffset
--
-- Front direction in the constraint owner local space. Defaults to -[SCNNode localFront]. Animatable
--
-- ObjC selector: @- setLocalFront:@
setLocalFront :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> SCNVector3 -> IO ()
setLocalFront scnLookAtConstraint value =
  sendMessage scnLookAtConstraint setLocalFrontSelector value

-- | worldUp
--
-- Up reference direction in world space. Defaults to -[SCNNode localUp]. Animatable
--
-- ObjC selector: @- worldUp@
worldUp :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO SCNVector3
worldUp scnLookAtConstraint =
  sendMessage scnLookAtConstraint worldUpSelector

-- | worldUp
--
-- Up reference direction in world space. Defaults to -[SCNNode localUp]. Animatable
--
-- ObjC selector: @- setWorldUp:@
setWorldUp :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> SCNVector3 -> IO ()
setWorldUp scnLookAtConstraint value =
  sendMessage scnLookAtConstraint setWorldUpSelector value

-- | gimbalLockEnabled
--
-- Specifies whether the receiver enables the gimbal lock. Defaults to NO.
--
-- Enabling the gimbal lock prevents the receiver from rotating the constrained node around to roll axis.
--
-- ObjC selector: @- gimbalLockEnabled@
gimbalLockEnabled :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO Bool
gimbalLockEnabled scnLookAtConstraint =
  sendMessage scnLookAtConstraint gimbalLockEnabledSelector

-- | gimbalLockEnabled
--
-- Specifies whether the receiver enables the gimbal lock. Defaults to NO.
--
-- Enabling the gimbal lock prevents the receiver from rotating the constrained node around to roll axis.
--
-- ObjC selector: @- setGimbalLockEnabled:@
setGimbalLockEnabled :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> Bool -> IO ()
setGimbalLockEnabled scnLookAtConstraint value =
  sendMessage scnLookAtConstraint setGimbalLockEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lookAtConstraintWithTarget:@
lookAtConstraintWithTargetSelector :: Selector '[Id SCNNode] (Id SCNLookAtConstraint)
lookAtConstraintWithTargetSelector = mkSelector "lookAtConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id SCNNode)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id SCNNode] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @targetOffset@
targetOffsetSelector :: Selector '[] SCNVector3
targetOffsetSelector = mkSelector "targetOffset"

-- | @Selector@ for @setTargetOffset:@
setTargetOffsetSelector :: Selector '[SCNVector3] ()
setTargetOffsetSelector = mkSelector "setTargetOffset:"

-- | @Selector@ for @localFront@
localFrontSelector :: Selector '[] SCNVector3
localFrontSelector = mkSelector "localFront"

-- | @Selector@ for @setLocalFront:@
setLocalFrontSelector :: Selector '[SCNVector3] ()
setLocalFrontSelector = mkSelector "setLocalFront:"

-- | @Selector@ for @worldUp@
worldUpSelector :: Selector '[] SCNVector3
worldUpSelector = mkSelector "worldUp"

-- | @Selector@ for @setWorldUp:@
setWorldUpSelector :: Selector '[SCNVector3] ()
setWorldUpSelector = mkSelector "setWorldUp:"

-- | @Selector@ for @gimbalLockEnabled@
gimbalLockEnabledSelector :: Selector '[] Bool
gimbalLockEnabledSelector = mkSelector "gimbalLockEnabled"

-- | @Selector@ for @setGimbalLockEnabled:@
setGimbalLockEnabledSelector :: Selector '[Bool] ()
setGimbalLockEnabledSelector = mkSelector "setGimbalLockEnabled:"

