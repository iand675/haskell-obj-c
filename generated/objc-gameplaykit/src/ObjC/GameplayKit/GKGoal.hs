{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines a spatial directive. The various goals cause force to be applied to agents to try to achieve said goal.
--
-- Generated bindings for @GKGoal@.
module ObjC.GameplayKit.GKGoal
  ( GKGoal
  , IsGKGoal(..)
  , goalToSeekAgent
  , goalToFleeAgent
  , goalToAvoidObstacles_maxPredictionTime
  , goalToAvoidAgents_maxPredictionTime
  , goalToSeparateFromAgents_maxDistance_maxAngle
  , goalToAlignWithAgents_maxDistance_maxAngle
  , goalToCohereWithAgents_maxDistance_maxAngle
  , goalToReachTargetSpeed
  , goalToWander
  , goalToInterceptAgent_maxPredictionTime
  , goalToFollowPath_maxPredictionTime_forward
  , goalToStayOnPath_maxPredictionTime
  , goalToSeekAgentSelector
  , goalToFleeAgentSelector
  , goalToAvoidObstacles_maxPredictionTimeSelector
  , goalToAvoidAgents_maxPredictionTimeSelector
  , goalToSeparateFromAgents_maxDistance_maxAngleSelector
  , goalToAlignWithAgents_maxDistance_maxAngleSelector
  , goalToCohereWithAgents_maxDistance_maxAngleSelector
  , goalToReachTargetSpeedSelector
  , goalToWanderSelector
  , goalToInterceptAgent_maxPredictionTimeSelector
  , goalToFollowPath_maxPredictionTime_forwardSelector
  , goalToStayOnPath_maxPredictionTimeSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a goal to move toward the agent
--
-- @agent@ — the agent to seek
--
-- ObjC selector: @+ goalToSeekAgent:@
goalToSeekAgent :: IsGKAgent agent => agent -> IO (Id GKGoal)
goalToSeekAgent agent =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr agent $ \raw_agent ->
      sendClassMsg cls' (mkSelector "goalToSeekAgent:") (retPtr retVoid) [argPtr (castPtr raw_agent :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a goal to move away from the agent
--
-- @agent@ — the agent to flee from
--
-- ObjC selector: @+ goalToFleeAgent:@
goalToFleeAgent :: IsGKAgent agent => agent -> IO (Id GKGoal)
goalToFleeAgent agent =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr agent $ \raw_agent ->
      sendClassMsg cls' (mkSelector "goalToFleeAgent:") (retPtr retVoid) [argPtr (castPtr raw_agent :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a goal to avoid colliding with a group of agents without taking into account those agents' momentum
--
-- @maxPredictionTime@ — how far ahead in the future, in seconds, should we look for potential collisions
--
-- ObjC selector: @+ goalToAvoidObstacles:maxPredictionTime:@
goalToAvoidObstacles_maxPredictionTime :: IsNSArray obstacles => obstacles -> CDouble -> IO (Id GKGoal)
goalToAvoidObstacles_maxPredictionTime obstacles maxPredictionTime =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr obstacles $ \raw_obstacles ->
      sendClassMsg cls' (mkSelector "goalToAvoidObstacles:maxPredictionTime:") (retPtr retVoid) [argPtr (castPtr raw_obstacles :: Ptr ()), argCDouble (fromIntegral maxPredictionTime)] >>= retainedObject . castPtr

-- | Creates a goal to avoid colliding with a group of agents taking into account those agent's momentum
--
-- @maxPredictionTime@ — how far ahead in the future, in seconds, should we look for potential collisions
--
-- ObjC selector: @+ goalToAvoidAgents:maxPredictionTime:@
goalToAvoidAgents_maxPredictionTime :: IsNSArray agents => agents -> CDouble -> IO (Id GKGoal)
goalToAvoidAgents_maxPredictionTime agents maxPredictionTime =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr agents $ \raw_agents ->
      sendClassMsg cls' (mkSelector "goalToAvoidAgents:maxPredictionTime:") (retPtr retVoid) [argPtr (castPtr raw_agents :: Ptr ()), argCDouble (fromIntegral maxPredictionTime)] >>= retainedObject . castPtr

-- | Creates a goal that tries to repel this agent away from the other agents and attempts to prevent overlap
--
-- @maxDistance@ — the distance between agents before repelling happens
--
-- @maxAngle@ — the angle, in radians, between this agent's foward and the vector toward the other agent before the repelling happens
--
-- ObjC selector: @+ goalToSeparateFromAgents:maxDistance:maxAngle:@
goalToSeparateFromAgents_maxDistance_maxAngle :: IsNSArray agents => agents -> CFloat -> CFloat -> IO (Id GKGoal)
goalToSeparateFromAgents_maxDistance_maxAngle agents maxDistance maxAngle =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr agents $ \raw_agents ->
      sendClassMsg cls' (mkSelector "goalToSeparateFromAgents:maxDistance:maxAngle:") (retPtr retVoid) [argPtr (castPtr raw_agents :: Ptr ()), argCFloat (fromIntegral maxDistance), argCFloat (fromIntegral maxAngle)] >>= retainedObject . castPtr

-- | Creates a goal to align this agent's orientation with the average orientation of the group of agents.
--
-- @maxDistance@ — the distance between agents before alignment happens
--
-- @maxAngle@ — the angle, in radians, between this agent's foward and the vector toward the other agent before alignment happens
--
-- ObjC selector: @+ goalToAlignWithAgents:maxDistance:maxAngle:@
goalToAlignWithAgents_maxDistance_maxAngle :: IsNSArray agents => agents -> CFloat -> CFloat -> IO (Id GKGoal)
goalToAlignWithAgents_maxDistance_maxAngle agents maxDistance maxAngle =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr agents $ \raw_agents ->
      sendClassMsg cls' (mkSelector "goalToAlignWithAgents:maxDistance:maxAngle:") (retPtr retVoid) [argPtr (castPtr raw_agents :: Ptr ()), argCFloat (fromIntegral maxDistance), argCFloat (fromIntegral maxAngle)] >>= retainedObject . castPtr

-- | Creates a goal to seek the average position of the group of agents.
--
-- @maxDistance@ — the distance between agents before cohesion happens
--
-- @maxAngle@ — the angle between this agent's foward and the vector toward the other agent before cohesion happens
--
-- ObjC selector: @+ goalToCohereWithAgents:maxDistance:maxAngle:@
goalToCohereWithAgents_maxDistance_maxAngle :: IsNSArray agents => agents -> CFloat -> CFloat -> IO (Id GKGoal)
goalToCohereWithAgents_maxDistance_maxAngle agents maxDistance maxAngle =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr agents $ \raw_agents ->
      sendClassMsg cls' (mkSelector "goalToCohereWithAgents:maxDistance:maxAngle:") (retPtr retVoid) [argPtr (castPtr raw_agents :: Ptr ()), argCFloat (fromIntegral maxDistance), argCFloat (fromIntegral maxAngle)] >>= retainedObject . castPtr

-- | Creates a goal that attempts to change our momentum to reach the target speed
--
-- @targetSpeed@ — the target speed
--
-- ObjC selector: @+ goalToReachTargetSpeed:@
goalToReachTargetSpeed :: CFloat -> IO (Id GKGoal)
goalToReachTargetSpeed targetSpeed =
  do
    cls' <- getRequiredClass "GKGoal"
    sendClassMsg cls' (mkSelector "goalToReachTargetSpeed:") (retPtr retVoid) [argCFloat (fromIntegral targetSpeed)] >>= retainedObject . castPtr

-- | Creates a goal that will make the agent appear to wander, aimlessly moving forward and turning randomly
--
-- @speed@ — the speed at which to wander
--
-- ObjC selector: @+ goalToWander:@
goalToWander :: CFloat -> IO (Id GKGoal)
goalToWander speed =
  do
    cls' <- getRequiredClass "GKGoal"
    sendClassMsg cls' (mkSelector "goalToWander:") (retPtr retVoid) [argCFloat (fromIntegral speed)] >>= retainedObject . castPtr

-- | Creates a goal that will attempt to intercept another target agent taking into account that agent's momentum
--
-- @target@ — agent to intercept
--
-- @maxPredictionTime@ — how far ahead in the future, in seconds, should we look for potential intercepts
--
-- ObjC selector: @+ goalToInterceptAgent:maxPredictionTime:@
goalToInterceptAgent_maxPredictionTime :: IsGKAgent target => target -> CDouble -> IO (Id GKGoal)
goalToInterceptAgent_maxPredictionTime target maxPredictionTime =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr target $ \raw_target ->
      sendClassMsg cls' (mkSelector "goalToInterceptAgent:maxPredictionTime:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ()), argCDouble (fromIntegral maxPredictionTime)] >>= retainedObject . castPtr

-- | Creates a goal that will attempt to follow the given path
--
-- @path@ — the path to follow
--
-- @maxPredictionTime@ — how far ahead in the future, in seconds, should we look for potential intercepts
--
-- @forward@ — direction to follow the path. forward = NO is reverse
--
-- ObjC selector: @+ goalToFollowPath:maxPredictionTime:forward:@
goalToFollowPath_maxPredictionTime_forward :: IsGKPath path => path -> CDouble -> Bool -> IO (Id GKGoal)
goalToFollowPath_maxPredictionTime_forward path maxPredictionTime forward =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "goalToFollowPath:maxPredictionTime:forward:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCDouble (fromIntegral maxPredictionTime), argCULong (if forward then 1 else 0)] >>= retainedObject . castPtr

-- | Creates a goal that will attempt to stay on the given path
--
-- @path@ — the path to follow
--
-- @maxPredictionTime@ — how far ahead in the future, in seconds, should we look for potential intercepts
--
-- ObjC selector: @+ goalToStayOnPath:maxPredictionTime:@
goalToStayOnPath_maxPredictionTime :: IsGKPath path => path -> CDouble -> IO (Id GKGoal)
goalToStayOnPath_maxPredictionTime path maxPredictionTime =
  do
    cls' <- getRequiredClass "GKGoal"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "goalToStayOnPath:maxPredictionTime:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCDouble (fromIntegral maxPredictionTime)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @goalToSeekAgent:@
goalToSeekAgentSelector :: Selector
goalToSeekAgentSelector = mkSelector "goalToSeekAgent:"

-- | @Selector@ for @goalToFleeAgent:@
goalToFleeAgentSelector :: Selector
goalToFleeAgentSelector = mkSelector "goalToFleeAgent:"

-- | @Selector@ for @goalToAvoidObstacles:maxPredictionTime:@
goalToAvoidObstacles_maxPredictionTimeSelector :: Selector
goalToAvoidObstacles_maxPredictionTimeSelector = mkSelector "goalToAvoidObstacles:maxPredictionTime:"

-- | @Selector@ for @goalToAvoidAgents:maxPredictionTime:@
goalToAvoidAgents_maxPredictionTimeSelector :: Selector
goalToAvoidAgents_maxPredictionTimeSelector = mkSelector "goalToAvoidAgents:maxPredictionTime:"

-- | @Selector@ for @goalToSeparateFromAgents:maxDistance:maxAngle:@
goalToSeparateFromAgents_maxDistance_maxAngleSelector :: Selector
goalToSeparateFromAgents_maxDistance_maxAngleSelector = mkSelector "goalToSeparateFromAgents:maxDistance:maxAngle:"

-- | @Selector@ for @goalToAlignWithAgents:maxDistance:maxAngle:@
goalToAlignWithAgents_maxDistance_maxAngleSelector :: Selector
goalToAlignWithAgents_maxDistance_maxAngleSelector = mkSelector "goalToAlignWithAgents:maxDistance:maxAngle:"

-- | @Selector@ for @goalToCohereWithAgents:maxDistance:maxAngle:@
goalToCohereWithAgents_maxDistance_maxAngleSelector :: Selector
goalToCohereWithAgents_maxDistance_maxAngleSelector = mkSelector "goalToCohereWithAgents:maxDistance:maxAngle:"

-- | @Selector@ for @goalToReachTargetSpeed:@
goalToReachTargetSpeedSelector :: Selector
goalToReachTargetSpeedSelector = mkSelector "goalToReachTargetSpeed:"

-- | @Selector@ for @goalToWander:@
goalToWanderSelector :: Selector
goalToWanderSelector = mkSelector "goalToWander:"

-- | @Selector@ for @goalToInterceptAgent:maxPredictionTime:@
goalToInterceptAgent_maxPredictionTimeSelector :: Selector
goalToInterceptAgent_maxPredictionTimeSelector = mkSelector "goalToInterceptAgent:maxPredictionTime:"

-- | @Selector@ for @goalToFollowPath:maxPredictionTime:forward:@
goalToFollowPath_maxPredictionTime_forwardSelector :: Selector
goalToFollowPath_maxPredictionTime_forwardSelector = mkSelector "goalToFollowPath:maxPredictionTime:forward:"

-- | @Selector@ for @goalToStayOnPath:maxPredictionTime:@
goalToStayOnPath_maxPredictionTimeSelector :: Selector
goalToStayOnPath_maxPredictionTimeSelector = mkSelector "goalToStayOnPath:maxPredictionTime:"

