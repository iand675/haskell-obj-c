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
  , transformConstraintInWorldSpace_withBlockSelector
  , positionConstraintInWorldSpace_withBlockSelector
  , orientationConstraintInWorldSpace_withBlockSelector


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
    sendClassMsg cls' (mkSelector "transformConstraintInWorldSpace:withBlock:") (retPtr retVoid) [argCULong (if world then 1 else 0), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "positionConstraintInWorldSpace:withBlock:") (retPtr retVoid) [argCULong (if world then 1 else 0), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "orientationConstraintInWorldSpace:withBlock:") (retPtr retVoid) [argCULong (if world then 1 else 0), argPtr (castPtr block :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transformConstraintInWorldSpace:withBlock:@
transformConstraintInWorldSpace_withBlockSelector :: Selector
transformConstraintInWorldSpace_withBlockSelector = mkSelector "transformConstraintInWorldSpace:withBlock:"

-- | @Selector@ for @positionConstraintInWorldSpace:withBlock:@
positionConstraintInWorldSpace_withBlockSelector :: Selector
positionConstraintInWorldSpace_withBlockSelector = mkSelector "positionConstraintInWorldSpace:withBlock:"

-- | @Selector@ for @orientationConstraintInWorldSpace:withBlock:@
orientationConstraintInWorldSpace_withBlockSelector :: Selector
orientationConstraintInWorldSpace_withBlockSelector = mkSelector "orientationConstraintInWorldSpace:withBlock:"

