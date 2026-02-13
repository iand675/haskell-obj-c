{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNTransformConstraint
--
-- A SCNTransformConstraint applies on the transform of a node via a custom block.
--
-- Generated bindings for @SCNTransformConstraint@.
module ObjC.SceneKit.SCNTransformConstraint
  ( SCNTransformConstraint
  , IsSCNTransformConstraint(..)
  , transformConstraintInWorldSpace_withBlock
  , positionConstraintInWorldSpace_withBlock
  , orientationConstraintInWorldSpace_withBlock
  , orientationConstraintInWorldSpace_withBlockSelector
  , positionConstraintInWorldSpace_withBlockSelector
  , transformConstraintInWorldSpace_withBlockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | transformConstraintInWorldSpace:withBlock:
--
-- Creates and returns a SCNTransformConstraint object with the specified parameters.
--
-- @world@ — Determines whether the constraint is evaluated in world or local space.
--
-- @block@ — The custom block to call to evaluate the constraint.
--
-- The node and its transform are passed to the block. The transform returned by the block will be used to render the node.
--
-- ObjC selector: @+ transformConstraintInWorldSpace:withBlock:@
transformConstraintInWorldSpace_withBlock :: Bool -> Ptr () -> IO (Id SCNTransformConstraint)
transformConstraintInWorldSpace_withBlock world block =
  do
    cls' <- getRequiredClass "SCNTransformConstraint"
    sendClassMessage cls' transformConstraintInWorldSpace_withBlockSelector world block

-- | positionConstraintInWorldSpace:withBlock:
--
-- Creates and returns a SCNTransformConstraint object with the specified parameters.
--
-- @world@ — Determines whether the constraint is evaluated in world or local space.
--
-- @block@ — The custom block to call to evaluate the constraint.
--
-- The node and its position are passed to the block. The position returned by the block will be used to render the node.
--
-- ObjC selector: @+ positionConstraintInWorldSpace:withBlock:@
positionConstraintInWorldSpace_withBlock :: Bool -> Ptr () -> IO (Id SCNTransformConstraint)
positionConstraintInWorldSpace_withBlock world block =
  do
    cls' <- getRequiredClass "SCNTransformConstraint"
    sendClassMessage cls' positionConstraintInWorldSpace_withBlockSelector world block

-- | orientationConstraintInWorldSpace:withBlock:
--
-- Creates and returns a SCNTransformConstraint object with the specified parameters.
--
-- @world@ — Determines whether the constraint is evaluated in world or local space.
--
-- @block@ — The custom block to call to evaluate the constraint.
--
-- The node and its quaternion are passed to the block. The quaternion returned by the block will be used to render the node.
--
-- ObjC selector: @+ orientationConstraintInWorldSpace:withBlock:@
orientationConstraintInWorldSpace_withBlock :: Bool -> Ptr () -> IO (Id SCNTransformConstraint)
orientationConstraintInWorldSpace_withBlock world block =
  do
    cls' <- getRequiredClass "SCNTransformConstraint"
    sendClassMessage cls' orientationConstraintInWorldSpace_withBlockSelector world block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transformConstraintInWorldSpace:withBlock:@
transformConstraintInWorldSpace_withBlockSelector :: Selector '[Bool, Ptr ()] (Id SCNTransformConstraint)
transformConstraintInWorldSpace_withBlockSelector = mkSelector "transformConstraintInWorldSpace:withBlock:"

-- | @Selector@ for @positionConstraintInWorldSpace:withBlock:@
positionConstraintInWorldSpace_withBlockSelector :: Selector '[Bool, Ptr ()] (Id SCNTransformConstraint)
positionConstraintInWorldSpace_withBlockSelector = mkSelector "positionConstraintInWorldSpace:withBlock:"

-- | @Selector@ for @orientationConstraintInWorldSpace:withBlock:@
orientationConstraintInWorldSpace_withBlockSelector :: Selector '[Bool, Ptr ()] (Id SCNTransformConstraint)
orientationConstraintInWorldSpace_withBlockSelector = mkSelector "orientationConstraintInWorldSpace:withBlock:"

