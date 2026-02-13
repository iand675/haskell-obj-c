{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , target
  , setTarget
  , occluderCategoryBitMask
  , setOccluderCategoryBitMask
  , bias
  , setBias
  , avoidOccluderConstraintWithTargetSelector
  , biasSelector
  , delegateSelector
  , occluderCategoryBitMaskSelector
  , setBiasSelector
  , setDelegateSelector
  , setOccluderCategoryBitMaskSelector
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

-- | avoidOccluderConstraintWithTarget
--
-- Creates and returns a SCNAvoidOccluderConstraint object.
--
-- ObjC selector: @+ avoidOccluderConstraintWithTarget:@
avoidOccluderConstraintWithTarget :: IsSCNNode target => target -> IO (Id SCNAvoidOccluderConstraint)
avoidOccluderConstraintWithTarget target =
  do
    cls' <- getRequiredClass "SCNAvoidOccluderConstraint"
    sendClassMessage cls' avoidOccluderConstraintWithTargetSelector (toSCNNode target)

-- | delegate
--
-- The receiver's delegate
--
-- ObjC selector: @- delegate@
delegate :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO RawId
delegate scnAvoidOccluderConstraint =
  sendMessage scnAvoidOccluderConstraint delegateSelector

-- | delegate
--
-- The receiver's delegate
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> RawId -> IO ()
setDelegate scnAvoidOccluderConstraint value =
  sendMessage scnAvoidOccluderConstraint setDelegateSelector value

-- | target
--
-- Defines the target node
--
-- ObjC selector: @- target@
target :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO (Id SCNNode)
target scnAvoidOccluderConstraint =
  sendMessage scnAvoidOccluderConstraint targetSelector

-- | target
--
-- Defines the target node
--
-- ObjC selector: @- setTarget:@
setTarget :: (IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint, IsSCNNode value) => scnAvoidOccluderConstraint -> value -> IO ()
setTarget scnAvoidOccluderConstraint value =
  sendMessage scnAvoidOccluderConstraint setTargetSelector (toSCNNode value)

-- | occluderCategoryBitMask
--
-- Defines the category of node to consider as occluder. Defaults to 1.
--
-- ObjC selector: @- occluderCategoryBitMask@
occluderCategoryBitMask :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO CULong
occluderCategoryBitMask scnAvoidOccluderConstraint =
  sendMessage scnAvoidOccluderConstraint occluderCategoryBitMaskSelector

-- | occluderCategoryBitMask
--
-- Defines the category of node to consider as occluder. Defaults to 1.
--
-- ObjC selector: @- setOccluderCategoryBitMask:@
setOccluderCategoryBitMask :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> CULong -> IO ()
setOccluderCategoryBitMask scnAvoidOccluderConstraint value =
  sendMessage scnAvoidOccluderConstraint setOccluderCategoryBitMaskSelector value

-- | bias
--
-- Defines the bias the apply after moving the receiver to avoid occluders. Defaults to 10e-5.
--
-- A positive bias will move the receiver closer to the target.
--
-- ObjC selector: @- bias@
bias :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> IO CDouble
bias scnAvoidOccluderConstraint =
  sendMessage scnAvoidOccluderConstraint biasSelector

-- | bias
--
-- Defines the bias the apply after moving the receiver to avoid occluders. Defaults to 10e-5.
--
-- A positive bias will move the receiver closer to the target.
--
-- ObjC selector: @- setBias:@
setBias :: IsSCNAvoidOccluderConstraint scnAvoidOccluderConstraint => scnAvoidOccluderConstraint -> CDouble -> IO ()
setBias scnAvoidOccluderConstraint value =
  sendMessage scnAvoidOccluderConstraint setBiasSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @avoidOccluderConstraintWithTarget:@
avoidOccluderConstraintWithTargetSelector :: Selector '[Id SCNNode] (Id SCNAvoidOccluderConstraint)
avoidOccluderConstraintWithTargetSelector = mkSelector "avoidOccluderConstraintWithTarget:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id SCNNode)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id SCNNode] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @occluderCategoryBitMask@
occluderCategoryBitMaskSelector :: Selector '[] CULong
occluderCategoryBitMaskSelector = mkSelector "occluderCategoryBitMask"

-- | @Selector@ for @setOccluderCategoryBitMask:@
setOccluderCategoryBitMaskSelector :: Selector '[CULong] ()
setOccluderCategoryBitMaskSelector = mkSelector "setOccluderCategoryBitMask:"

-- | @Selector@ for @bias@
biasSelector :: Selector '[] CDouble
biasSelector = mkSelector "bias"

-- | @Selector@ for @setBias:@
setBiasSelector :: Selector '[CDouble] ()
setBiasSelector = mkSelector "setBias:"

