{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNReplicatorConstraint
--
-- A SCNReplicatorConstraint replicates the position/orientation/scale of a target node
--
-- Generated bindings for @SCNReplicatorConstraint@.
module ObjC.SceneKit.SCNReplicatorConstraint
  ( SCNReplicatorConstraint
  , IsSCNReplicatorConstraint(..)
  , replicatorConstraintWithTarget
  , target
  , setTarget
  , replicatesOrientation
  , setReplicatesOrientation
  , replicatesPosition
  , setReplicatesPosition
  , replicatesScale
  , setReplicatesScale
  , orientationOffset
  , setOrientationOffset
  , positionOffset
  , setPositionOffset
  , scaleOffset
  , setScaleOffset
  , replicatorConstraintWithTargetSelector
  , targetSelector
  , setTargetSelector
  , replicatesOrientationSelector
  , setReplicatesOrientationSelector
  , replicatesPositionSelector
  , setReplicatesPositionSelector
  , replicatesScaleSelector
  , setReplicatesScaleSelector
  , orientationOffsetSelector
  , setOrientationOffsetSelector
  , positionOffsetSelector
  , setPositionOffsetSelector
  , scaleOffsetSelector
  , setScaleOffsetSelector


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

-- | replicatorWithTargetNode
--
-- Creates and returns a SCNReplicatorConstraint constraint.
--
-- ObjC selector: @+ replicatorConstraintWithTarget:@
replicatorConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNReplicatorConstraint)
replicatorConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNReplicatorConstraint"
    withObjCPtr target $ \raw_target ->
      sendClassMsg cls' (mkSelector "replicatorConstraintWithTarget:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ())] >>= retainedObject . castPtr

-- | target
--
-- Defines the target node to replicate
--
-- ObjC selector: @- target@
target :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO (Id SCNNode)
target scnReplicatorConstraint  =
  sendMsg scnReplicatorConstraint (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | target
--
-- Defines the target node to replicate
--
-- ObjC selector: @- setTarget:@
setTarget :: (IsSCNReplicatorConstraint scnReplicatorConstraint, IsSCNNode value) => scnReplicatorConstraint -> value -> IO ()
setTarget scnReplicatorConstraint  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnReplicatorConstraint (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | replicatesOrientation
--
-- Defines whether or not the constraint should replicate the target orientation. Defaults to YES.
--
-- ObjC selector: @- replicatesOrientation@
replicatesOrientation :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO Bool
replicatesOrientation scnReplicatorConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnReplicatorConstraint (mkSelector "replicatesOrientation") retCULong []

-- | replicatesOrientation
--
-- Defines whether or not the constraint should replicate the target orientation. Defaults to YES.
--
-- ObjC selector: @- setReplicatesOrientation:@
setReplicatesOrientation :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> Bool -> IO ()
setReplicatesOrientation scnReplicatorConstraint  value =
  sendMsg scnReplicatorConstraint (mkSelector "setReplicatesOrientation:") retVoid [argCULong (if value then 1 else 0)]

-- | replicatesPosition
--
-- Defines whether or not the constraint should replicate the target position. Defaults to YES.
--
-- ObjC selector: @- replicatesPosition@
replicatesPosition :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO Bool
replicatesPosition scnReplicatorConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnReplicatorConstraint (mkSelector "replicatesPosition") retCULong []

-- | replicatesPosition
--
-- Defines whether or not the constraint should replicate the target position. Defaults to YES.
--
-- ObjC selector: @- setReplicatesPosition:@
setReplicatesPosition :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> Bool -> IO ()
setReplicatesPosition scnReplicatorConstraint  value =
  sendMsg scnReplicatorConstraint (mkSelector "setReplicatesPosition:") retVoid [argCULong (if value then 1 else 0)]

-- | replicatesScale
--
-- Defines whether or not the constraint should replicate the target scale. Defaults to YES.
--
-- ObjC selector: @- replicatesScale@
replicatesScale :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO Bool
replicatesScale scnReplicatorConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnReplicatorConstraint (mkSelector "replicatesScale") retCULong []

-- | replicatesScale
--
-- Defines whether or not the constraint should replicate the target scale. Defaults to YES.
--
-- ObjC selector: @- setReplicatesScale:@
setReplicatesScale :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> Bool -> IO ()
setReplicatesScale scnReplicatorConstraint  value =
  sendMsg scnReplicatorConstraint (mkSelector "setReplicatesScale:") retVoid [argCULong (if value then 1 else 0)]

-- | orientationOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- orientationOffset@
orientationOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO SCNQuaternion
orientationOffset scnReplicatorConstraint  =
  sendMsgStret scnReplicatorConstraint (mkSelector "orientationOffset") retSCNQuaternion []

-- | orientationOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- setOrientationOffset:@
setOrientationOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> SCNQuaternion -> IO ()
setOrientationOffset scnReplicatorConstraint  value =
  sendMsg scnReplicatorConstraint (mkSelector "setOrientationOffset:") retVoid [argSCNQuaternion value]

-- | positionOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- positionOffset@
positionOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO SCNVector3
positionOffset scnReplicatorConstraint  =
  sendMsgStret scnReplicatorConstraint (mkSelector "positionOffset") retSCNVector3 []

-- | positionOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- setPositionOffset:@
setPositionOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> SCNVector3 -> IO ()
setPositionOffset scnReplicatorConstraint  value =
  sendMsg scnReplicatorConstraint (mkSelector "setPositionOffset:") retVoid [argSCNVector3 value]

-- | scaleOffset
--
-- Defines an addition scale offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- scaleOffset@
scaleOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO SCNVector3
scaleOffset scnReplicatorConstraint  =
  sendMsgStret scnReplicatorConstraint (mkSelector "scaleOffset") retSCNVector3 []

-- | scaleOffset
--
-- Defines an addition scale offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- setScaleOffset:@
setScaleOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> SCNVector3 -> IO ()
setScaleOffset scnReplicatorConstraint  value =
  sendMsg scnReplicatorConstraint (mkSelector "setScaleOffset:") retVoid [argSCNVector3 value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replicatorConstraintWithTarget:@
replicatorConstraintWithTargetSelector :: Selector
replicatorConstraintWithTargetSelector = mkSelector "replicatorConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @replicatesOrientation@
replicatesOrientationSelector :: Selector
replicatesOrientationSelector = mkSelector "replicatesOrientation"

-- | @Selector@ for @setReplicatesOrientation:@
setReplicatesOrientationSelector :: Selector
setReplicatesOrientationSelector = mkSelector "setReplicatesOrientation:"

-- | @Selector@ for @replicatesPosition@
replicatesPositionSelector :: Selector
replicatesPositionSelector = mkSelector "replicatesPosition"

-- | @Selector@ for @setReplicatesPosition:@
setReplicatesPositionSelector :: Selector
setReplicatesPositionSelector = mkSelector "setReplicatesPosition:"

-- | @Selector@ for @replicatesScale@
replicatesScaleSelector :: Selector
replicatesScaleSelector = mkSelector "replicatesScale"

-- | @Selector@ for @setReplicatesScale:@
setReplicatesScaleSelector :: Selector
setReplicatesScaleSelector = mkSelector "setReplicatesScale:"

-- | @Selector@ for @orientationOffset@
orientationOffsetSelector :: Selector
orientationOffsetSelector = mkSelector "orientationOffset"

-- | @Selector@ for @setOrientationOffset:@
setOrientationOffsetSelector :: Selector
setOrientationOffsetSelector = mkSelector "setOrientationOffset:"

-- | @Selector@ for @positionOffset@
positionOffsetSelector :: Selector
positionOffsetSelector = mkSelector "positionOffset"

-- | @Selector@ for @setPositionOffset:@
setPositionOffsetSelector :: Selector
setPositionOffsetSelector = mkSelector "setPositionOffset:"

-- | @Selector@ for @scaleOffset@
scaleOffsetSelector :: Selector
scaleOffsetSelector = mkSelector "scaleOffset"

-- | @Selector@ for @setScaleOffset:@
setScaleOffsetSelector :: Selector
setScaleOffsetSelector = mkSelector "setScaleOffset:"

