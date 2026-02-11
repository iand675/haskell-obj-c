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
  , lookAtConstraintWithTargetSelector
  , targetSelector
  , setTargetSelector
  , targetOffsetSelector
  , setTargetOffsetSelector
  , localFrontSelector
  , setLocalFrontSelector
  , worldUpSelector
  , setWorldUpSelector
  , gimbalLockEnabledSelector
  , setGimbalLockEnabledSelector


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
    withObjCPtr target $ \raw_target ->
      sendClassMsg cls' (mkSelector "lookAtConstraintWithTarget:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ())] >>= retainedObject . castPtr

-- | @- target@
target :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO (Id SCNNode)
target scnLookAtConstraint  =
  sendMsg scnLookAtConstraint (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTarget:@
setTarget :: (IsSCNLookAtConstraint scnLookAtConstraint, IsSCNNode target) => scnLookAtConstraint -> target -> IO ()
setTarget scnLookAtConstraint  target =
withObjCPtr target $ \raw_target ->
    sendMsg scnLookAtConstraint (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_target :: Ptr ())]

-- | targetOffset
--
-- Offset look at position in target space. Defaults to zero. Animatable
--
-- ObjC selector: @- targetOffset@
targetOffset :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO SCNVector3
targetOffset scnLookAtConstraint  =
  sendMsgStret scnLookAtConstraint (mkSelector "targetOffset") retSCNVector3 []

-- | targetOffset
--
-- Offset look at position in target space. Defaults to zero. Animatable
--
-- ObjC selector: @- setTargetOffset:@
setTargetOffset :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> SCNVector3 -> IO ()
setTargetOffset scnLookAtConstraint  value =
  sendMsg scnLookAtConstraint (mkSelector "setTargetOffset:") retVoid [argSCNVector3 value]

-- | targetOffset
--
-- Front direction in the constraint owner local space. Defaults to -[SCNNode localFront]. Animatable
--
-- ObjC selector: @- localFront@
localFront :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO SCNVector3
localFront scnLookAtConstraint  =
  sendMsgStret scnLookAtConstraint (mkSelector "localFront") retSCNVector3 []

-- | targetOffset
--
-- Front direction in the constraint owner local space. Defaults to -[SCNNode localFront]. Animatable
--
-- ObjC selector: @- setLocalFront:@
setLocalFront :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> SCNVector3 -> IO ()
setLocalFront scnLookAtConstraint  value =
  sendMsg scnLookAtConstraint (mkSelector "setLocalFront:") retVoid [argSCNVector3 value]

-- | worldUp
--
-- Up reference direction in world space. Defaults to -[SCNNode localUp]. Animatable
--
-- ObjC selector: @- worldUp@
worldUp :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO SCNVector3
worldUp scnLookAtConstraint  =
  sendMsgStret scnLookAtConstraint (mkSelector "worldUp") retSCNVector3 []

-- | worldUp
--
-- Up reference direction in world space. Defaults to -[SCNNode localUp]. Animatable
--
-- ObjC selector: @- setWorldUp:@
setWorldUp :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> SCNVector3 -> IO ()
setWorldUp scnLookAtConstraint  value =
  sendMsg scnLookAtConstraint (mkSelector "setWorldUp:") retVoid [argSCNVector3 value]

-- | gimbalLockEnabled
--
-- Specifies whether the receiver enables the gimbal lock. Defaults to NO.
--
-- Enabling the gimbal lock prevents the receiver from rotating the constrained node around to roll axis.
--
-- ObjC selector: @- gimbalLockEnabled@
gimbalLockEnabled :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> IO Bool
gimbalLockEnabled scnLookAtConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnLookAtConstraint (mkSelector "gimbalLockEnabled") retCULong []

-- | gimbalLockEnabled
--
-- Specifies whether the receiver enables the gimbal lock. Defaults to NO.
--
-- Enabling the gimbal lock prevents the receiver from rotating the constrained node around to roll axis.
--
-- ObjC selector: @- setGimbalLockEnabled:@
setGimbalLockEnabled :: IsSCNLookAtConstraint scnLookAtConstraint => scnLookAtConstraint -> Bool -> IO ()
setGimbalLockEnabled scnLookAtConstraint  value =
  sendMsg scnLookAtConstraint (mkSelector "setGimbalLockEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lookAtConstraintWithTarget:@
lookAtConstraintWithTargetSelector :: Selector
lookAtConstraintWithTargetSelector = mkSelector "lookAtConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @targetOffset@
targetOffsetSelector :: Selector
targetOffsetSelector = mkSelector "targetOffset"

-- | @Selector@ for @setTargetOffset:@
setTargetOffsetSelector :: Selector
setTargetOffsetSelector = mkSelector "setTargetOffset:"

-- | @Selector@ for @localFront@
localFrontSelector :: Selector
localFrontSelector = mkSelector "localFront"

-- | @Selector@ for @setLocalFront:@
setLocalFrontSelector :: Selector
setLocalFrontSelector = mkSelector "setLocalFront:"

-- | @Selector@ for @worldUp@
worldUpSelector :: Selector
worldUpSelector = mkSelector "worldUp"

-- | @Selector@ for @setWorldUp:@
setWorldUpSelector :: Selector
setWorldUpSelector = mkSelector "setWorldUp:"

-- | @Selector@ for @gimbalLockEnabled@
gimbalLockEnabledSelector :: Selector
gimbalLockEnabledSelector = mkSelector "gimbalLockEnabled"

-- | @Selector@ for @setGimbalLockEnabled:@
setGimbalLockEnabledSelector :: Selector
setGimbalLockEnabledSelector = mkSelector "setGimbalLockEnabled:"

