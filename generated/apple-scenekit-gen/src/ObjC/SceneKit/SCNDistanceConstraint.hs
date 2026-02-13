{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNDistanceConstraint
--
-- A SCNDistanceConstraint ensure a minimum/maximum distance with a target node.
--
-- Generated bindings for @SCNDistanceConstraint@.
module ObjC.SceneKit.SCNDistanceConstraint
  ( SCNDistanceConstraint
  , IsSCNDistanceConstraint(..)
  , distanceConstraintWithTarget
  , target
  , setTarget
  , minimumDistance
  , setMinimumDistance
  , maximumDistance
  , setMaximumDistance
  , distanceConstraintWithTargetSelector
  , maximumDistanceSelector
  , minimumDistanceSelector
  , setMaximumDistanceSelector
  , setMinimumDistanceSelector
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
import ObjC.Foundation.Internal.Classes

-- | distanceConstraintWithTarget
--
-- Creates and returns a SCNDistanceConstraint constraint.
--
-- ObjC selector: @+ distanceConstraintWithTarget:@
distanceConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNDistanceConstraint)
distanceConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNDistanceConstraint"
    sendClassMessage cls' distanceConstraintWithTargetSelector (toSCNNode target)

-- | target
--
-- Defines the target node to keep distance with.
--
-- ObjC selector: @- target@
target :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> IO (Id SCNNode)
target scnDistanceConstraint =
  sendMessage scnDistanceConstraint targetSelector

-- | target
--
-- Defines the target node to keep distance with.
--
-- ObjC selector: @- setTarget:@
setTarget :: (IsSCNDistanceConstraint scnDistanceConstraint, IsSCNNode value) => scnDistanceConstraint -> value -> IO ()
setTarget scnDistanceConstraint value =
  sendMessage scnDistanceConstraint setTargetSelector (toSCNNode value)

-- | minimumDistance
--
-- The minimum distance. Defaults to 0. Animatable.
--
-- ObjC selector: @- minimumDistance@
minimumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> IO CDouble
minimumDistance scnDistanceConstraint =
  sendMessage scnDistanceConstraint minimumDistanceSelector

-- | minimumDistance
--
-- The minimum distance. Defaults to 0. Animatable.
--
-- ObjC selector: @- setMinimumDistance:@
setMinimumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> CDouble -> IO ()
setMinimumDistance scnDistanceConstraint value =
  sendMessage scnDistanceConstraint setMinimumDistanceSelector value

-- | maximumDistance
--
-- The minimum distance. Defaults to MAXFLOAT. Animatable.
--
-- ObjC selector: @- maximumDistance@
maximumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> IO CDouble
maximumDistance scnDistanceConstraint =
  sendMessage scnDistanceConstraint maximumDistanceSelector

-- | maximumDistance
--
-- The minimum distance. Defaults to MAXFLOAT. Animatable.
--
-- ObjC selector: @- setMaximumDistance:@
setMaximumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> CDouble -> IO ()
setMaximumDistance scnDistanceConstraint value =
  sendMessage scnDistanceConstraint setMaximumDistanceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @distanceConstraintWithTarget:@
distanceConstraintWithTargetSelector :: Selector '[Id SCNNode] (Id SCNDistanceConstraint)
distanceConstraintWithTargetSelector = mkSelector "distanceConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id SCNNode)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id SCNNode] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @minimumDistance@
minimumDistanceSelector :: Selector '[] CDouble
minimumDistanceSelector = mkSelector "minimumDistance"

-- | @Selector@ for @setMinimumDistance:@
setMinimumDistanceSelector :: Selector '[CDouble] ()
setMinimumDistanceSelector = mkSelector "setMinimumDistance:"

-- | @Selector@ for @maximumDistance@
maximumDistanceSelector :: Selector '[] CDouble
maximumDistanceSelector = mkSelector "maximumDistance"

-- | @Selector@ for @setMaximumDistance:@
setMaximumDistanceSelector :: Selector '[CDouble] ()
setMaximumDistanceSelector = mkSelector "setMaximumDistance:"

