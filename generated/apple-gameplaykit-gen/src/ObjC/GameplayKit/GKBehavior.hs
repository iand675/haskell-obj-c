{-# LANGUAGE DataKinds #-}
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
  , goalCountSelector
  , objectAtIndexedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , removeAllGoalsSelector
  , removeGoalSelector
  , setObject_forKeyedSubscriptSelector
  , setWeight_forGoalSelector
  , weightForGoalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' behaviorWithGoal_weightSelector (toGKGoal goal) weight

-- | Creates a behavior with an array of goals.  All weights are set to 1.0f
--
-- ObjC selector: @+ behaviorWithGoals:@
behaviorWithGoals :: IsNSArray goals => goals -> IO (Id GKBehavior)
behaviorWithGoals goals =
  do
    cls' <- getRequiredClass "GKBehavior"
    sendClassMessage cls' behaviorWithGoalsSelector (toNSArray goals)

-- | Creates a behavior with two associated arrays of goals and weights
--
-- ObjC selector: @+ behaviorWithGoals:andWeights:@
behaviorWithGoals_andWeights :: (IsNSArray goals, IsNSArray weights) => goals -> weights -> IO (Id GKBehavior)
behaviorWithGoals_andWeights goals weights =
  do
    cls' <- getRequiredClass "GKBehavior"
    sendClassMessage cls' behaviorWithGoals_andWeightsSelector (toNSArray goals) (toNSArray weights)

-- | Creates a behavior with a dictionary of goal/weight pairs
--
-- ObjC selector: @+ behaviorWithWeightedGoals:@
behaviorWithWeightedGoals :: IsNSDictionary weightedGoals => weightedGoals -> IO (Id GKBehavior)
behaviorWithWeightedGoals weightedGoals =
  do
    cls' <- getRequiredClass "GKBehavior"
    sendClassMessage cls' behaviorWithWeightedGoalsSelector (toNSDictionary weightedGoals)

-- | Adds a new goal or changes the weight of the existing goal in this behavior. If the goal does not exist in this behavior, it is added.
--
-- @weight@ — the weight for this goal
--
-- @goal@ — the goal who's weight to change
--
-- ObjC selector: @- setWeight:forGoal:@
setWeight_forGoal :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> CFloat -> goal -> IO ()
setWeight_forGoal gkBehavior weight goal =
  sendMessage gkBehavior setWeight_forGoalSelector weight (toGKGoal goal)

-- | Gets the current weight for a given goal.
--
-- Returns: the weight of the goal, or 0 if there is no such goal on this behavior
--
-- ObjC selector: @- weightForGoal:@
weightForGoal :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> goal -> IO CFloat
weightForGoal gkBehavior goal =
  sendMessage gkBehavior weightForGoalSelector (toGKGoal goal)

-- | Remove the indicated goal from this behavior.
--
-- @goal@ — the goal to be removed
--
-- ObjC selector: @- removeGoal:@
removeGoal :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> goal -> IO ()
removeGoal gkBehavior goal =
  sendMessage gkBehavior removeGoalSelector (toGKGoal goal)

-- | Removes all the goals on the behavior.
--
-- ObjC selector: @- removeAllGoals@
removeAllGoals :: IsGKBehavior gkBehavior => gkBehavior -> IO ()
removeAllGoals gkBehavior =
  sendMessage gkBehavior removeAllGoalsSelector

-- | Supports getting goals via a [int] subscript.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsGKBehavior gkBehavior => gkBehavior -> CULong -> IO (Id GKGoal)
objectAtIndexedSubscript gkBehavior idx =
  sendMessage gkBehavior objectAtIndexedSubscriptSelector idx

-- | Supports setting a weight via a [goal] subscript.
--
-- ObjC selector: @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: (IsGKBehavior gkBehavior, IsNSNumber weight, IsGKGoal goal) => gkBehavior -> weight -> goal -> IO ()
setObject_forKeyedSubscript gkBehavior weight goal =
  sendMessage gkBehavior setObject_forKeyedSubscriptSelector (toNSNumber weight) (toGKGoal goal)

-- | Supports getting a weight via a [goal] subscript.
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsGKBehavior gkBehavior, IsGKGoal goal) => gkBehavior -> goal -> IO (Id NSNumber)
objectForKeyedSubscript gkBehavior goal =
  sendMessage gkBehavior objectForKeyedSubscriptSelector (toGKGoal goal)

-- | @- goalCount@
goalCount :: IsGKBehavior gkBehavior => gkBehavior -> IO CLong
goalCount gkBehavior =
  sendMessage gkBehavior goalCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @behaviorWithGoal:weight:@
behaviorWithGoal_weightSelector :: Selector '[Id GKGoal, CFloat] (Id GKBehavior)
behaviorWithGoal_weightSelector = mkSelector "behaviorWithGoal:weight:"

-- | @Selector@ for @behaviorWithGoals:@
behaviorWithGoalsSelector :: Selector '[Id NSArray] (Id GKBehavior)
behaviorWithGoalsSelector = mkSelector "behaviorWithGoals:"

-- | @Selector@ for @behaviorWithGoals:andWeights:@
behaviorWithGoals_andWeightsSelector :: Selector '[Id NSArray, Id NSArray] (Id GKBehavior)
behaviorWithGoals_andWeightsSelector = mkSelector "behaviorWithGoals:andWeights:"

-- | @Selector@ for @behaviorWithWeightedGoals:@
behaviorWithWeightedGoalsSelector :: Selector '[Id NSDictionary] (Id GKBehavior)
behaviorWithWeightedGoalsSelector = mkSelector "behaviorWithWeightedGoals:"

-- | @Selector@ for @setWeight:forGoal:@
setWeight_forGoalSelector :: Selector '[CFloat, Id GKGoal] ()
setWeight_forGoalSelector = mkSelector "setWeight:forGoal:"

-- | @Selector@ for @weightForGoal:@
weightForGoalSelector :: Selector '[Id GKGoal] CFloat
weightForGoalSelector = mkSelector "weightForGoal:"

-- | @Selector@ for @removeGoal:@
removeGoalSelector :: Selector '[Id GKGoal] ()
removeGoalSelector = mkSelector "removeGoal:"

-- | @Selector@ for @removeAllGoals@
removeAllGoalsSelector :: Selector '[] ()
removeAllGoalsSelector = mkSelector "removeAllGoals"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id GKGoal)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector '[Id NSNumber, Id GKGoal] ()
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id GKGoal] (Id NSNumber)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @goalCount@
goalCountSelector :: Selector '[] CLong
goalCountSelector = mkSelector "goalCount"

