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
  , targetSelector
  , setTargetSelector
  , minimumDistanceSelector
  , setMinimumDistanceSelector
  , maximumDistanceSelector
  , setMaximumDistanceSelector


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

-- | distanceConstraintWithTarget
--
-- Creates and returns a SCNDistanceConstraint constraint.
--
-- ObjC selector: @+ distanceConstraintWithTarget:@
distanceConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNDistanceConstraint)
distanceConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNDistanceConstraint"
    withObjCPtr target $ \raw_target ->
      sendClassMsg cls' (mkSelector "distanceConstraintWithTarget:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ())] >>= retainedObject . castPtr

-- | target
--
-- Defines the target node to keep distance with.
--
-- ObjC selector: @- target@
target :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> IO (Id SCNNode)
target scnDistanceConstraint  =
  sendMsg scnDistanceConstraint (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | target
--
-- Defines the target node to keep distance with.
--
-- ObjC selector: @- setTarget:@
setTarget :: (IsSCNDistanceConstraint scnDistanceConstraint, IsSCNNode value) => scnDistanceConstraint -> value -> IO ()
setTarget scnDistanceConstraint  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnDistanceConstraint (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | minimumDistance
--
-- The minimum distance. Defaults to 0. Animatable.
--
-- ObjC selector: @- minimumDistance@
minimumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> IO CDouble
minimumDistance scnDistanceConstraint  =
  sendMsg scnDistanceConstraint (mkSelector "minimumDistance") retCDouble []

-- | minimumDistance
--
-- The minimum distance. Defaults to 0. Animatable.
--
-- ObjC selector: @- setMinimumDistance:@
setMinimumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> CDouble -> IO ()
setMinimumDistance scnDistanceConstraint  value =
  sendMsg scnDistanceConstraint (mkSelector "setMinimumDistance:") retVoid [argCDouble (fromIntegral value)]

-- | maximumDistance
--
-- The minimum distance. Defaults to MAXFLOAT. Animatable.
--
-- ObjC selector: @- maximumDistance@
maximumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> IO CDouble
maximumDistance scnDistanceConstraint  =
  sendMsg scnDistanceConstraint (mkSelector "maximumDistance") retCDouble []

-- | maximumDistance
--
-- The minimum distance. Defaults to MAXFLOAT. Animatable.
--
-- ObjC selector: @- setMaximumDistance:@
setMaximumDistance :: IsSCNDistanceConstraint scnDistanceConstraint => scnDistanceConstraint -> CDouble -> IO ()
setMaximumDistance scnDistanceConstraint  value =
  sendMsg scnDistanceConstraint (mkSelector "setMaximumDistance:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @distanceConstraintWithTarget:@
distanceConstraintWithTargetSelector :: Selector
distanceConstraintWithTargetSelector = mkSelector "distanceConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @minimumDistance@
minimumDistanceSelector :: Selector
minimumDistanceSelector = mkSelector "minimumDistance"

-- | @Selector@ for @setMinimumDistance:@
setMinimumDistanceSelector :: Selector
setMinimumDistanceSelector = mkSelector "setMinimumDistance:"

-- | @Selector@ for @maximumDistance@
maximumDistanceSelector :: Selector
maximumDistanceSelector = mkSelector "maximumDistance"

-- | @Selector@ for @setMaximumDistance:@
setMaximumDistanceSelector :: Selector
setMaximumDistanceSelector = mkSelector "setMaximumDistance:"

