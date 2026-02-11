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
  , maximumLinearAccelerationSelector
  , setMaximumLinearAccelerationSelector
  , maximumLinearVelocitySelector
  , setMaximumLinearVelocitySelector
  , decelerationDistanceSelector
  , setDecelerationDistanceSelector
  , dampingSelector
  , setDampingSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    sendClassMsg cls' (mkSelector "accelerationConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | maximumLinearAcceleration
--
-- Controls the maximum linear acceleration. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear acceleration is in m.s^-2
--
-- ObjC selector: @- maximumLinearAcceleration@
maximumLinearAcceleration :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
maximumLinearAcceleration scnAccelerationConstraint  =
  sendMsg scnAccelerationConstraint (mkSelector "maximumLinearAcceleration") retCDouble []

-- | maximumLinearAcceleration
--
-- Controls the maximum linear acceleration. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear acceleration is in m.s^-2
--
-- ObjC selector: @- setMaximumLinearAcceleration:@
setMaximumLinearAcceleration :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setMaximumLinearAcceleration scnAccelerationConstraint  value =
  sendMsg scnAccelerationConstraint (mkSelector "setMaximumLinearAcceleration:") retVoid [argCDouble (fromIntegral value)]

-- | maximumLinearVelocity
--
-- Controls the maximum linear velocity. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear velocity is in m.s
--
-- ObjC selector: @- maximumLinearVelocity@
maximumLinearVelocity :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
maximumLinearVelocity scnAccelerationConstraint  =
  sendMsg scnAccelerationConstraint (mkSelector "maximumLinearVelocity") retCDouble []

-- | maximumLinearVelocity
--
-- Controls the maximum linear velocity. Defaults to MAXFLOAT. Animatable.
--
-- The maximum linear velocity is in m.s
--
-- ObjC selector: @- setMaximumLinearVelocity:@
setMaximumLinearVelocity :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setMaximumLinearVelocity scnAccelerationConstraint  value =
  sendMsg scnAccelerationConstraint (mkSelector "setMaximumLinearVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | decelerationDistance
--
-- Controls the distance at which the node should start decelerating. Defaults to 0. Animatable.
--
-- ObjC selector: @- decelerationDistance@
decelerationDistance :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
decelerationDistance scnAccelerationConstraint  =
  sendMsg scnAccelerationConstraint (mkSelector "decelerationDistance") retCDouble []

-- | decelerationDistance
--
-- Controls the distance at which the node should start decelerating. Defaults to 0. Animatable.
--
-- ObjC selector: @- setDecelerationDistance:@
setDecelerationDistance :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setDecelerationDistance scnAccelerationConstraint  value =
  sendMsg scnAccelerationConstraint (mkSelector "setDecelerationDistance:") retVoid [argCDouble (fromIntegral value)]

-- | damping
--
-- Specifies the damping factor of the receiver. Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Animatable.
--
-- ObjC selector: @- damping@
damping :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> IO CDouble
damping scnAccelerationConstraint  =
  sendMsg scnAccelerationConstraint (mkSelector "damping") retCDouble []

-- | damping
--
-- Specifies the damping factor of the receiver. Optionally reduce the body's linear velocity each frame to simulate fluid/air friction. Value should be zero or greater. Defaults to 0.1. Animatable.
--
-- ObjC selector: @- setDamping:@
setDamping :: IsSCNAccelerationConstraint scnAccelerationConstraint => scnAccelerationConstraint -> CDouble -> IO ()
setDamping scnAccelerationConstraint  value =
  sendMsg scnAccelerationConstraint (mkSelector "setDamping:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @accelerationConstraint@
accelerationConstraintSelector :: Selector
accelerationConstraintSelector = mkSelector "accelerationConstraint"

-- | @Selector@ for @maximumLinearAcceleration@
maximumLinearAccelerationSelector :: Selector
maximumLinearAccelerationSelector = mkSelector "maximumLinearAcceleration"

-- | @Selector@ for @setMaximumLinearAcceleration:@
setMaximumLinearAccelerationSelector :: Selector
setMaximumLinearAccelerationSelector = mkSelector "setMaximumLinearAcceleration:"

-- | @Selector@ for @maximumLinearVelocity@
maximumLinearVelocitySelector :: Selector
maximumLinearVelocitySelector = mkSelector "maximumLinearVelocity"

-- | @Selector@ for @setMaximumLinearVelocity:@
setMaximumLinearVelocitySelector :: Selector
setMaximumLinearVelocitySelector = mkSelector "setMaximumLinearVelocity:"

-- | @Selector@ for @decelerationDistance@
decelerationDistanceSelector :: Selector
decelerationDistanceSelector = mkSelector "decelerationDistance"

-- | @Selector@ for @setDecelerationDistance:@
setDecelerationDistanceSelector :: Selector
setDecelerationDistanceSelector = mkSelector "setDecelerationDistance:"

-- | @Selector@ for @damping@
dampingSelector :: Selector
dampingSelector = mkSelector "damping"

-- | @Selector@ for @setDamping:@
setDampingSelector :: Selector
setDampingSelector = mkSelector "setDamping:"

