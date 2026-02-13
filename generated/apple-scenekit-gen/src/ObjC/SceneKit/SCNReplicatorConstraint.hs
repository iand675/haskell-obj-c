{-# LANGUAGE DataKinds #-}
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
  , orientationOffsetSelector
  , positionOffsetSelector
  , replicatesOrientationSelector
  , replicatesPositionSelector
  , replicatesScaleSelector
  , replicatorConstraintWithTargetSelector
  , scaleOffsetSelector
  , setOrientationOffsetSelector
  , setPositionOffsetSelector
  , setReplicatesOrientationSelector
  , setReplicatesPositionSelector
  , setReplicatesScaleSelector
  , setScaleOffsetSelector
  , setTargetSelector
  , targetSelector


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

-- | replicatorWithTargetNode
--
-- Creates and returns a SCNReplicatorConstraint constraint.
--
-- ObjC selector: @+ replicatorConstraintWithTarget:@
replicatorConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNReplicatorConstraint)
replicatorConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNReplicatorConstraint"
    sendClassMessage cls' replicatorConstraintWithTargetSelector (toSCNNode target)

-- | target
--
-- Defines the target node to replicate
--
-- ObjC selector: @- target@
target :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO (Id SCNNode)
target scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint targetSelector

-- | target
--
-- Defines the target node to replicate
--
-- ObjC selector: @- setTarget:@
setTarget :: (IsSCNReplicatorConstraint scnReplicatorConstraint, IsSCNNode value) => scnReplicatorConstraint -> value -> IO ()
setTarget scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setTargetSelector (toSCNNode value)

-- | replicatesOrientation
--
-- Defines whether or not the constraint should replicate the target orientation. Defaults to YES.
--
-- ObjC selector: @- replicatesOrientation@
replicatesOrientation :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO Bool
replicatesOrientation scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint replicatesOrientationSelector

-- | replicatesOrientation
--
-- Defines whether or not the constraint should replicate the target orientation. Defaults to YES.
--
-- ObjC selector: @- setReplicatesOrientation:@
setReplicatesOrientation :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> Bool -> IO ()
setReplicatesOrientation scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setReplicatesOrientationSelector value

-- | replicatesPosition
--
-- Defines whether or not the constraint should replicate the target position. Defaults to YES.
--
-- ObjC selector: @- replicatesPosition@
replicatesPosition :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO Bool
replicatesPosition scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint replicatesPositionSelector

-- | replicatesPosition
--
-- Defines whether or not the constraint should replicate the target position. Defaults to YES.
--
-- ObjC selector: @- setReplicatesPosition:@
setReplicatesPosition :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> Bool -> IO ()
setReplicatesPosition scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setReplicatesPositionSelector value

-- | replicatesScale
--
-- Defines whether or not the constraint should replicate the target scale. Defaults to YES.
--
-- ObjC selector: @- replicatesScale@
replicatesScale :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO Bool
replicatesScale scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint replicatesScaleSelector

-- | replicatesScale
--
-- Defines whether or not the constraint should replicate the target scale. Defaults to YES.
--
-- ObjC selector: @- setReplicatesScale:@
setReplicatesScale :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> Bool -> IO ()
setReplicatesScale scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setReplicatesScaleSelector value

-- | orientationOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- orientationOffset@
orientationOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO SCNQuaternion
orientationOffset scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint orientationOffsetSelector

-- | orientationOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- setOrientationOffset:@
setOrientationOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> SCNQuaternion -> IO ()
setOrientationOffset scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setOrientationOffsetSelector value

-- | positionOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- positionOffset@
positionOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO SCNVector3
positionOffset scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint positionOffsetSelector

-- | positionOffset
--
-- Defines an addition orientation offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- setPositionOffset:@
setPositionOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> SCNVector3 -> IO ()
setPositionOffset scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setPositionOffsetSelector value

-- | scaleOffset
--
-- Defines an addition scale offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- scaleOffset@
scaleOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> IO SCNVector3
scaleOffset scnReplicatorConstraint =
  sendMessage scnReplicatorConstraint scaleOffsetSelector

-- | scaleOffset
--
-- Defines an addition scale offset. Defaults to no offset. Animatable.
--
-- ObjC selector: @- setScaleOffset:@
setScaleOffset :: IsSCNReplicatorConstraint scnReplicatorConstraint => scnReplicatorConstraint -> SCNVector3 -> IO ()
setScaleOffset scnReplicatorConstraint value =
  sendMessage scnReplicatorConstraint setScaleOffsetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replicatorConstraintWithTarget:@
replicatorConstraintWithTargetSelector :: Selector '[Id SCNNode] (Id SCNReplicatorConstraint)
replicatorConstraintWithTargetSelector = mkSelector "replicatorConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id SCNNode)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id SCNNode] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @replicatesOrientation@
replicatesOrientationSelector :: Selector '[] Bool
replicatesOrientationSelector = mkSelector "replicatesOrientation"

-- | @Selector@ for @setReplicatesOrientation:@
setReplicatesOrientationSelector :: Selector '[Bool] ()
setReplicatesOrientationSelector = mkSelector "setReplicatesOrientation:"

-- | @Selector@ for @replicatesPosition@
replicatesPositionSelector :: Selector '[] Bool
replicatesPositionSelector = mkSelector "replicatesPosition"

-- | @Selector@ for @setReplicatesPosition:@
setReplicatesPositionSelector :: Selector '[Bool] ()
setReplicatesPositionSelector = mkSelector "setReplicatesPosition:"

-- | @Selector@ for @replicatesScale@
replicatesScaleSelector :: Selector '[] Bool
replicatesScaleSelector = mkSelector "replicatesScale"

-- | @Selector@ for @setReplicatesScale:@
setReplicatesScaleSelector :: Selector '[Bool] ()
setReplicatesScaleSelector = mkSelector "setReplicatesScale:"

-- | @Selector@ for @orientationOffset@
orientationOffsetSelector :: Selector '[] SCNQuaternion
orientationOffsetSelector = mkSelector "orientationOffset"

-- | @Selector@ for @setOrientationOffset:@
setOrientationOffsetSelector :: Selector '[SCNQuaternion] ()
setOrientationOffsetSelector = mkSelector "setOrientationOffset:"

-- | @Selector@ for @positionOffset@
positionOffsetSelector :: Selector '[] SCNVector3
positionOffsetSelector = mkSelector "positionOffset"

-- | @Selector@ for @setPositionOffset:@
setPositionOffsetSelector :: Selector '[SCNVector3] ()
setPositionOffsetSelector = mkSelector "setPositionOffset:"

-- | @Selector@ for @scaleOffset@
scaleOffsetSelector :: Selector '[] SCNVector3
scaleOffsetSelector = mkSelector "scaleOffset"

-- | @Selector@ for @setScaleOffset:@
setScaleOffsetSelector :: Selector '[SCNVector3] ()
setScaleOffsetSelector = mkSelector "setScaleOffset:"

