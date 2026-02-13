{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNAccelerationConstraint
--
-- A SCNAccelerationConstraint caps the acceleration and velocity of a node
--
-- Generated bindings for @SCNAccelerationConstraint@.
module ObjC.SceneKit.SCNAccelerationConstraint
  ( SCNAccelerationConstraint
  , IsSCNAccelerationConstraint(..)
  , accelerationConstraint
  , maximumLinearAcceleration
  , setMaximumLinearAcceleration
  , maximumLinearVelocity
  , setMaximumLinearVelocity
  , decelerationDistance
  , setDecelerationDistance
  , damping
  , setDamping
  , accelerationConstraintSelector
  , dampingSelector
  , decelerationDistanceSelector
  , maximumLinearAccelerationSelector
  , maximumLinearVelocitySelector
  , setDampingSelector
  , setDecelerationDistanceSelector
  , setMaximumLinearAccelerationSelector
  , setMaximumLinearVelocitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | accelerationConstraint
--
-- Creates and returns a SCNAccelerationConstraint object.
--
-- ObjC selector: @+ accelerationConstraint@
accelerationConstraint :: IO (Id SCNAccelerationConstraint)
accelerationConstraint  =
  do
    cls' <- getRequiredClass "SCNAccelerationConstraint"
    sendClassMessage cls' accelerationConstraintSelector

-- | maximumLinearAcceleration
--
-- Controls the maximum linear acceleration. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear acceleration is in m.s^-2
--
-- ObjC selector: @- maximumLinearAcceleration@
maximumLinearAcceleration :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
maximumLinearAcceleration scnAccelerationConstraint =
  sendMessage scnAccelerationConstraint maximumLinearAccelerationSelector

-- | maximumLinearAcceleration
--
-- Controls the maximum linear acceleration. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear acceleration is in m.s^-2
--
-- ObjC selector: @- setMaximumLinearAcceleration:@
setMaximumLinearAcceleration :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setMaximumLinearAcceleration scnAccelerationConstraint value =
  sendMessage scnAccelerationConstraint setMaximumLinearAccelerationSelector value

-- | maximumLinearVelocity
--
-- Controls the maximum linear velocity. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear velocity is in m.s
--
-- ObjC selector: @- maximumLinearVelocity@
maximumLinearVelocity :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
maximumLinearVelocity scnAccelerationConstraint =
  sendMessage scnAccelerationConstraint maximumLinearVelocitySelector

-- | maximumLinearVelocity
--
-- Controls the maximum linear velocity. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear velocity is in m.s
--
-- ObjC selector: @- setMaximumLinearVelocity:@
setMaximumLinearVelocity :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setMaximumLinearVelocity scnAccelerationConstraint value =
  sendMessage scnAccelerationConstraint setMaximumLinearVelocitySelector value

-- | decelerationDistance
--
-- Controls the distance at which the node should start decelerating. Defaults to 0. Animatable.
--
-- ObjC selector: @- decelerationDistance@
decelerationDistance :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
decelerationDistance scnAccelerationConstraint =
  sendMessage scnAccelerationConstraint decelerationDistanceSelector

-- | decelerationDistance
--
-- Controls the distance at which the node should start decelerating. Defaults to 0. Animatable.
--
-- ObjC selector: @- setDecelerationDistance:@
setDecelerationDistance :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setDecelerationDistance scnAccelerationConstraint value =
  sendMessage scnAccelerationConstraint setDecelerationDistanceSelector value

-- | damping
--
-- Specifies the damping factor of the receiver. Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Animatable.
--
-- ObjC selector: @- damping@
damping :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
damping scnAccelerationConstraint =
  sendMessage scnAccelerationConstraint dampingSelector

-- | damping
--
-- Specifies the damping factor of the receiver. Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Animatable.
--
-- ObjC selector: @- setDamping:@
setDamping :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setDamping scnAccelerationConstraint value =
  sendMessage scnAccelerationConstraint setDampingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accelerationConstraint@
accelerationConstraintSelector :: Selector '[] (Id SCNAccelerationConstraint)
accelerationConstraintSelector = mkSelector "accelerationConstraint"

-- | @Selector@ for @maximumLinearAcceleration@
maximumLinearAccelerationSelector :: Selector '[] CDouble
maximumLinearAccelerationSelector = mkSelector "maximumLinearAcceleration"

-- | @Selector@ for @setMaximumLinearAcceleration:@
setMaximumLinearAccelerationSelector :: Selector '[CDouble] ()
setMaximumLinearAccelerationSelector = mkSelector "setMaximumLinearAcceleration:"

-- | @Selector@ for @maximumLinearVelocity@
maximumLinearVelocitySelector :: Selector '[] CDouble
maximumLinearVelocitySelector = mkSelector "maximumLinearVelocity"

-- | @Selector@ for @setMaximumLinearVelocity:@
setMaximumLinearVelocitySelector :: Selector '[CDouble] ()
setMaximumLinearVelocitySelector = mkSelector "setMaximumLinearVelocity:"

-- | @Selector@ for @decelerationDistance@
decelerationDistanceSelector :: Selector '[] CDouble
decelerationDistanceSelector = mkSelector "decelerationDistance"

-- | @Selector@ for @setDecelerationDistance:@
setDecelerationDistanceSelector :: Selector '[CDouble] ()
setDecelerationDistanceSelector = mkSelector "setDecelerationDistance:"

-- | @Selector@ for @damping@
dampingSelector :: Selector '[] CDouble
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector '[CDouble] ()
setDampingSelector = mkSelector "setDamping:"

