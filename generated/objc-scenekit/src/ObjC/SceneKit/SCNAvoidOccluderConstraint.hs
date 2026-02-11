{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNAvoidOccluderConstraint
--
-- A SCNAvoidOccluderConstraint constraints place the receiver at a position that prevent nodes with the specified category to occlude the target.
--
-- The target node and it's children are ignored as potential occluders.
--
-- Generated bindings for @SCNAvoidOccluderConstraint@.
module ObjC.SceneKit.SCNAvoidOccluderConstraint
  ( SCNAvoidOccluderConstraint
  , IsSCNAvoidOccluderConstraint(..)
  , avoidOccluderConstraintWithTarget
  , target
  , setTarget
  , occluderCategoryBitMask
  , setOccluderCategoryBitMask
  , bias
  , setBias
  , avoidOccluderConstraintWithTargetSelector
  , targetSelector
  , setTargetSelector
  , occluderCategoryBitMaskSelector
  , setOccluderCategoryBitMaskSelector
  , biasSelector
  , setBiasSelector


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

-- | avoidOccluderConstraintWithTarget
--
-- Creates and returns a SCNAvoidOccluderConstraint object.
--
-- ObjC selector: @+ avoidOccluderConstraintWithTarget:@
avoidOccluderConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNAvoidOccluderConstraint)
avoidOccluderConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNAvoidOccluderConstraint"
    withObjCPtr target $ \raw_target ->
      sendClassMsg cls' (mkSelector "avoidOccluderConstraintWithTarget:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ())] >>= retainedObject . castPtr

-- | target
--
-- Defines the target node
--
-- ObjC selector: @- target@
target :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO (Id SCNNode)
target scnAvoidOccluderConstraint  =
  sendMsg scnAvoidOccluderConstraint (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | target
--
-- Defines the target node
--
-- ObjC selector: @- setTarget:@
setTarget :: (IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint, IsSCNNode value) => scnAvoidOccluderConstraint -> value -> IO ()
setTarget scnAvoidOccluderConstraint  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnAvoidOccluderConstraint (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | occluderCategoryBitMask
--
-- Defines the category of node to consider as occluder. Defaults to 1.
--
-- ObjC selector: @- occluderCategoryBitMask@
occluderCategoryBitMask :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO CULong
occluderCategoryBitMask scnAvoidOccluderConstraint  =
  sendMsg scnAvoidOccluderConstraint (mkSelector "occluderCategoryBitMask") retCULong []

-- | occluderCategoryBitMask
--
-- Defines the category of node to consider as occluder. Defaults to 1.
--
-- ObjC selector: @- setOccluderCategoryBitMask:@
setOccluderCategoryBitMask :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> CULong -> IO ()
setOccluderCategoryBitMask scnAvoidOccluderConstraint  value =
  sendMsg scnAvoidOccluderConstraint (mkSelector "setOccluderCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

-- | bias
--
-- Defines the bias the apply after moving the receiver to avoid occluders. Defaults to 10e-5.
--
-- A positive bias will move the receiver closer to the target.
--
-- ObjC selector: @- bias@
bias :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO CDouble
bias scnAvoidOccluderConstraint  =
  sendMsg scnAvoidOccluderConstraint (mkSelector "bias") retCDouble []

-- | bias
--
-- Defines the bias the apply after moving the receiver to avoid occluders. Defaults to 10e-5.
--
-- A positive bias will move the receiver closer to the target.
--
-- ObjC selector: @- setBias:@
setBias :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> CDouble -> IO ()
setBias scnAvoidOccluderConstraint  value =
  sendMsg scnAvoidOccluderConstraint (mkSelector "setBias:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @avoidOccluderConstraintWithTarget:@
avoidOccluderConstraintWithTargetSelector :: Selector
avoidOccluderConstraintWithTargetSelector = mkSelector "avoidOccluderConstraintWithTarget:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @occluderCategoryBitMask@
occluderCategoryBitMaskSelector :: Selector
occluderCategoryBitMaskSelector = mkSelector "occluderCategoryBitMask"

-- | @Selector@ for @setOccluderCategoryBitMask:@
setOccluderCategoryBitMaskSelector :: Selector
setOccluderCategoryBitMaskSelector = mkSelector "setOccluderCategoryBitMask:"

-- | @Selector@ for @bias@
biasSelector :: Selector
biasSelector = mkSelector "bias"

-- | @Selector@ for @setBias:@
setBiasSelector :: Selector
setBiasSelector = mkSelector "setBias:"

