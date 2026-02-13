{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNIKConstraint
--
-- A SCNIKConstraint applies an inverse kinematics constraint
--
-- Generated bindings for @SCNIKConstraint@.
module ObjC.SceneKit.SCNIKConstraint
  ( SCNIKConstraint
  , IsSCNIKConstraint(..)
  , initWithChainRootNode
  , inverseKinematicsConstraintWithChainRootNode
  , setMaxAllowedRotationAngle_forJoint
  , maxAllowedRotationAngleForJoint
  , chainRootNode
  , targetPosition
  , setTargetPosition
  , chainRootNodeSelector
  , initWithChainRootNodeSelector
  , inverseKinematicsConstraintWithChainRootNodeSelector
  , maxAllowedRotationAngleForJointSelector
  , setMaxAllowedRotationAngle_forJointSelector
  , setTargetPositionSelector
  , targetPositionSelector


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

-- | initWithChainRootNode:
--
-- Creates and returns a SCNIKConstraint object with the specified parameter.
--
-- @chainRootNode@ — The root node of the kinematic chain.
--
-- "chainRootNode" must be an ancestor of the node on which the constraint is applied.
--
-- ObjC selector: @- initWithChainRootNode:@
initWithChainRootNode :: (IsSCNIKConstraint scnikConstraint, IsSCNNode chainRootNode) => scnikConstraint -> chainRootNode -> IO (Id SCNIKConstraint)
initWithChainRootNode scnikConstraint chainRootNode =
  sendOwnedMessage scnikConstraint initWithChainRootNodeSelector (toSCNNode chainRootNode)

-- | inverseKinematicsConstraintWithChainRootNode:
--
-- Creates and returns a SCNIKConstraint object with the specified parameter.
--
-- @chainRootNode@ — The root node of the kinematic chain.
--
-- "chainRootNode" must be an ancestor of the node on which the constraint is applied.
--
-- ObjC selector: @+ inverseKinematicsConstraintWithChainRootNode:@
inverseKinematicsConstraintWithChainRootNode :: IsSCNNode chainRootNode => chainRootNode -> IO (Id SCNIKConstraint)
inverseKinematicsConstraintWithChainRootNode chainRootNode =
  do
    cls' <- getRequiredClass "SCNIKConstraint"
    sendClassMessage cls' inverseKinematicsConstraintWithChainRootNodeSelector (toSCNNode chainRootNode)

-- | setMaxAllowedRotationAngle:forJoint:
--
-- Specifies the maximum rotation allowed (in degrees) for the specified joint from its initial orientation. Defaults to 180.
--
-- ObjC selector: @- setMaxAllowedRotationAngle:forJoint:@
setMaxAllowedRotationAngle_forJoint :: (IsSCNIKConstraint scnikConstraint, IsSCNNode node) => scnikConstraint -> CDouble -> node -> IO ()
setMaxAllowedRotationAngle_forJoint scnikConstraint angle node =
  sendMessage scnikConstraint setMaxAllowedRotationAngle_forJointSelector angle (toSCNNode node)

-- | @- maxAllowedRotationAngleForJoint:@
maxAllowedRotationAngleForJoint :: (IsSCNIKConstraint scnikConstraint, IsSCNNode node) => scnikConstraint -> node -> IO CDouble
maxAllowedRotationAngleForJoint scnikConstraint node =
  sendMessage scnikConstraint maxAllowedRotationAngleForJointSelector (toSCNNode node)

-- | chainRootNode
--
-- Specifies the root node of the kinematic chain.
--
-- ObjC selector: @- chainRootNode@
chainRootNode :: IsSCNIKConstraint scnikConstraint => scnikConstraint -> IO (Id SCNNode)
chainRootNode scnikConstraint =
  sendMessage scnikConstraint chainRootNodeSelector

-- | target
--
-- Specifies the target position (in world space coordinates) of the end joint (i.e the node that owns the IK constraint). Defaults to (0,0,0). Animatable.
--
-- ObjC selector: @- targetPosition@
targetPosition :: IsSCNIKConstraint scnikConstraint => scnikConstraint -> IO SCNVector3
targetPosition scnikConstraint =
  sendMessage scnikConstraint targetPositionSelector

-- | target
--
-- Specifies the target position (in world space coordinates) of the end joint (i.e the node that owns the IK constraint). Defaults to (0,0,0). Animatable.
--
-- ObjC selector: @- setTargetPosition:@
setTargetPosition :: IsSCNIKConstraint scnikConstraint => scnikConstraint -> SCNVector3 -> IO ()
setTargetPosition scnikConstraint value =
  sendMessage scnikConstraint setTargetPositionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChainRootNode:@
initWithChainRootNodeSelector :: Selector '[Id SCNNode] (Id SCNIKConstraint)
initWithChainRootNodeSelector = mkSelector "initWithChainRootNode:"

-- | @Selector@ for @inverseKinematicsConstraintWithChainRootNode:@
inverseKinematicsConstraintWithChainRootNodeSelector :: Selector '[Id SCNNode] (Id SCNIKConstraint)
inverseKinematicsConstraintWithChainRootNodeSelector = mkSelector "inverseKinematicsConstraintWithChainRootNode:"

-- | @Selector@ for @setMaxAllowedRotationAngle:forJoint:@
setMaxAllowedRotationAngle_forJointSelector :: Selector '[CDouble, Id SCNNode] ()
setMaxAllowedRotationAngle_forJointSelector = mkSelector "setMaxAllowedRotationAngle:forJoint:"

-- | @Selector@ for @maxAllowedRotationAngleForJoint:@
maxAllowedRotationAngleForJointSelector :: Selector '[Id SCNNode] CDouble
maxAllowedRotationAngleForJointSelector = mkSelector "maxAllowedRotationAngleForJoint:"

-- | @Selector@ for @chainRootNode@
chainRootNodeSelector :: Selector '[] (Id SCNNode)
chainRootNodeSelector = mkSelector "chainRootNode"

-- | @Selector@ for @targetPosition@
targetPositionSelector :: Selector '[] SCNVector3
targetPositionSelector = mkSelector "targetPosition"

-- | @Selector@ for @setTargetPosition:@
setTargetPositionSelector :: Selector '[SCNVector3] ()
setTargetPositionSelector = mkSelector "setTargetPosition:"

