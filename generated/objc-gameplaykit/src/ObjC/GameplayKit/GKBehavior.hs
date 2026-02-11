{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of GKGoals or GKBehaviors with weights that can be applied to a GKAgent The sub-goals or sub-behaviors are summed to produce a total force to be applied to an agent
--
-- Generated bindings for @GKBehavior@.
module ObjC.GameplayKit.GKBehavior
  ( GKBehavior
  , IsGKBehavior(..)
  , behaviorWithGoal_weight
  , behaviorWithGoals
  , behaviorWithGoals_andWeights
  , behaviorWithWeightedGoals
  , setWeight_forGoal
  , weightForGoal
  , removeGoal
  , removeAllGoals
  , objectAtIndexedSubscript
  , setObject_forKeyedSubscript
  , objectForKeyedSubscript
  , goalCount
  , behaviorWithGoal_weightSelector
  , behaviorWithGoalsSelector
  , behaviorWithGoals_andWeightsSelector
  , behaviorWithWeightedGoalsSelector
  , setWeight_forGoalSelector
  , weightForGoalSelector
  , removeGoalSelector
  , removeAllGoalsSelector
  , objectAtIndexedSubscriptSelector
  , setObject_forKeyedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , goalCountSelector


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

-- | Creates a behavior with a single goal and weight
--
-- ObjC selector: @+ behaviorWithGoal:weight:@
behaviorWithGoal_weight :: IsGKGoal goal => goal -> CFloat -> IO (Id GKBehavior)
behaviorWithGoal_weight goal weight =
  do
    cls' <- getRequiredClass "GKBehavior"
    withObjCPtr goal $ \raw_goal ->
      sendClassMsg cls' (mkSelector "behaviorWithGoal:weight:") (retPtr retVoid) [argPtr (castPtr raw_goal :: Ptr ()), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

-- | Creates a behavior with an array of goals.  All weights are set to 1.0f
--
-- ObjC selector: @+ behaviorWithGoals:@
behaviorWithGoals :: IsNSArray goals => goals -> IO (Id GKBehavior)
behaviorWithGoals goals =
  do
    cls' <- getRequiredClass "GKBehavior"
    withObjCPtr goals $ \raw_goals ->
      sendClassMsg cls' (mkSelector "behaviorWithGoals:") (retPtr retVoid) [argPtr (castPtr raw_goals :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a behavior with two associated arrays of goals and weights
--
-- ObjC selector: @+ behaviorWithGoals:andWeights:@
behaviorWithGoals_andWeights :: (IsNSArray goals, IsNSArray weights) => goals -> weights -> IO (Id GKBehavior)
behaviorWithGoals_andWeights goals weights =
  do
    cls' <- getRequiredClass "GKBehavior"
    withObjCPtr goals $ \raw_goals ->
      withObjCPtr weights $ \raw_weights ->
        sendClassMsg cls' (mkSelector "behaviorWithGoals:andWeights:") (retPtr retVoid) [argPtr (castPtr raw_goals :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a behavior with a dictionary of goal/weight pairs
--
-- ObjC selector: @+ behaviorWithWeightedGoals:@
behaviorWithWeightedGoals :: IsNSDictionary weightedGoals => weightedGoals -> IO (Id GKBehavior)
behaviorWithWeightedGoals weightedGoals =
  do
    cls' <- getRequiredClass "GKBehavior"
    withObjCPtr weightedGoals $ \raw_weightedGoals ->
      sendClassMsg cls' (mkSelector "behaviorWithWeightedGoals:") (retPtr retVoid) [argPtr (castPtr raw_weightedGoals :: Ptr ())] >>= retainedObject . castPtr

-- | Adds a new goal or changes the weight of the existing goal in this behavior. If the goal does not exist in this behavior, it is added.
--
-- @weight@ — the weight for this goal
--
-- @goal@ — the goal who's weight to change
--
-- ObjC selector: @- setWeight:forGoal:@
setWeight_forGoal :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> CFloat -> goal -> IO ()
setWeight_forGoal gkBehavior  weight goal =
withObjCPtr goal $ \raw_goal ->
    sendMsg gkBehavior (mkSelector "setWeight:forGoal:") retVoid [argCFloat (fromIntegral weight), argPtr (castPtr raw_goal :: Ptr ())]

-- | Gets the current weight for a given goal.
--
-- Returns: the weight of the goal, or 0 if there is no such goal on this behavior
--
-- ObjC selector: @- weightForGoal:@
weightForGoal :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> goal -> IO CFloat
weightForGoal gkBehavior  goal =
withObjCPtr goal $ \raw_goal ->
    sendMsg gkBehavior (mkSelector "weightForGoal:") retCFloat [argPtr (castPtr raw_goal :: Ptr ())]

-- | Remove the indicated goal from this behavior.
--
-- @goal@ — the goal to be removed
--
-- ObjC selector: @- removeGoal:@
removeGoal :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> goal -> IO ()
removeGoal gkBehavior  goal =
withObjCPtr goal $ \raw_goal ->
    sendMsg gkBehavior (mkSelector "removeGoal:") retVoid [argPtr (castPtr raw_goal :: Ptr ())]

-- | Removes all the goals on the behavior.
--
-- ObjC selector: @- removeAllGoals@
removeAllGoals :: IsGKBehavior gkBehavior => gkBehavior -> IO ()
removeAllGoals gkBehavior  =
  sendMsg gkBehavior (mkSelector "removeAllGoals") retVoid []

-- | Supports getting goals via a [int] subscript.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsGKBehavior gkBehavior => gkBehavior -> CULong -> IO (Id GKGoal)
objectAtIndexedSubscript gkBehavior  idx =
  sendMsg gkBehavior (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)] >>= retainedObject . castPtr

-- | Supports setting a weight via a [goal] subscript.
--
-- ObjC selector: @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: (IsGKBehavior gkBehavior, IsNSNumber weight, IsGKGoal goal) => gkBehavior -> weight -> goal -> IO ()
setObject_forKeyedSubscript gkBehavior  weight goal =
withObjCPtr weight $ \raw_weight ->
  withObjCPtr goal $ \raw_goal ->
      sendMsg gkBehavior (mkSelector "setObject:forKeyedSubscript:") retVoid [argPtr (castPtr raw_weight :: Ptr ()), argPtr (castPtr raw_goal :: Ptr ())]

-- | Supports getting a weight via a [goal] subscript.
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> goal -> IO (Id NSNumber)
objectForKeyedSubscript gkBehavior  goal =
withObjCPtr goal $ \raw_goal ->
    sendMsg gkBehavior (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_goal :: Ptr ())] >>= retainedObject . castPtr

-- | @- goalCount@
goalCount :: IsGKBehavior gkBehavior => gkBehavior -> IO CLong
goalCount gkBehavior  =
  sendMsg gkBehavior (mkSelector "goalCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @behaviorWithGoal:weight:@
behaviorWithGoal_weightSelector :: Selector
behaviorWithGoal_weightSelector = mkSelector "behaviorWithGoal:weight:"

-- | @Selector@ for @behaviorWithGoals:@
behaviorWithGoalsSelector :: Selector
behaviorWithGoalsSelector = mkSelector "behaviorWithGoals:"

-- | @Selector@ for @behaviorWithGoals:andWeights:@
behaviorWithGoals_andWeightsSelector :: Selector
behaviorWithGoals_andWeightsSelector = mkSelector "behaviorWithGoals:andWeights:"

-- | @Selector@ for @behaviorWithWeightedGoals:@
behaviorWithWeightedGoalsSelector :: Selector
behaviorWithWeightedGoalsSelector = mkSelector "behaviorWithWeightedGoals:"

-- | @Selector@ for @setWeight:forGoal:@
setWeight_forGoalSelector :: Selector
setWeight_forGoalSelector = mkSelector "setWeight:forGoal:"

-- | @Selector@ for @weightForGoal:@
weightForGoalSelector :: Selector
weightForGoalSelector = mkSelector "weightForGoal:"

-- | @Selector@ for @removeGoal:@
removeGoalSelector :: Selector
removeGoalSelector = mkSelector "removeGoal:"

-- | @Selector@ for @removeAllGoals@
removeAllGoalsSelector :: Selector
removeAllGoalsSelector = mkSelector "removeAllGoals"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @goalCount@
goalCountSelector :: Selector
goalCountSelector = mkSelector "goalCount"

