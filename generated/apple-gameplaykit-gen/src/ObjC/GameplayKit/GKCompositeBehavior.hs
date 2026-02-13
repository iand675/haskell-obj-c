{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKCompositeBehavior@.
module ObjC.GameplayKit.GKCompositeBehavior
  ( GKCompositeBehavior
  , IsGKCompositeBehavior(..)
  , behaviorWithBehaviors
  , behaviorWithBehaviors_andWeights
  , setWeight_forBehavior
  , weightForBehavior
  , removeBehavior
  , removeAllBehaviors
  , objectAtIndexedSubscript
  , setObject_forKeyedSubscript
  , objectForKeyedSubscript
  , behaviorCount
  , behaviorCountSelector
  , behaviorWithBehaviorsSelector
  , behaviorWithBehaviors_andWeightsSelector
  , objectAtIndexedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , removeAllBehaviorsSelector
  , removeBehaviorSelector
  , setObject_forKeyedSubscriptSelector
  , setWeight_forBehaviorSelector
  , weightForBehaviorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a behavior with an array of sub-behaviors
--
-- ObjC selector: @+ behaviorWithBehaviors:@
behaviorWithBehaviors :: IsNSArray behaviors => behaviors -> IO (Id GKCompositeBehavior)
behaviorWithBehaviors behaviors =
  do
    cls' <- getRequiredClass "GKCompositeBehavior"
    sendClassMessage cls' behaviorWithBehaviorsSelector (toNSArray behaviors)

-- | Creates a behavior with two associated arrays of sub-behaviors and weights
--
-- ObjC selector: @+ behaviorWithBehaviors:andWeights:@
behaviorWithBehaviors_andWeights :: (IsNSArray behaviors, IsNSArray weights) => behaviors -> weights -> IO (Id GKCompositeBehavior)
behaviorWithBehaviors_andWeights behaviors weights =
  do
    cls' <- getRequiredClass "GKCompositeBehavior"
    sendClassMessage cls' behaviorWithBehaviors_andWeightsSelector (toNSArray behaviors) (toNSArray weights)

-- | Adds a new sub-behavior or changes the weight of the existing sub-behavior in this behavior. If the sub-behavior  does not exist in this behavior, it is added.
--
-- @weight@ — the weight for this goal
--
-- @behavior@ — the sub-behavior who's weight to change
--
-- ObjC selector: @- setWeight:forBehavior:@
setWeight_forBehavior :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> CFloat -> behavior -> IO ()
setWeight_forBehavior gkCompositeBehavior weight behavior =
  sendMessage gkCompositeBehavior setWeight_forBehaviorSelector weight (toGKBehavior behavior)

-- | Gets the current weight for a given sub-behavior.
--
-- Returns: the weight of the sub-behavior, or 0 if there is no such sub-behavior on this behavior
--
-- ObjC selector: @- weightForBehavior:@
weightForBehavior :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> behavior -> IO CFloat
weightForBehavior gkCompositeBehavior behavior =
  sendMessage gkCompositeBehavior weightForBehaviorSelector (toGKBehavior behavior)

-- | Remove the indicated sub-behavior from this behavior.
--
-- @behavior@ — the sub-behavior to be removed
--
-- ObjC selector: @- removeBehavior:@
removeBehavior :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> behavior -> IO ()
removeBehavior gkCompositeBehavior behavior =
  sendMessage gkCompositeBehavior removeBehaviorSelector (toGKBehavior behavior)

-- | Removes all the sub-behavior on the behavior.
--
-- ObjC selector: @- removeAllBehaviors@
removeAllBehaviors :: IsGKCompositeBehavior gkCompositeBehavior => gkCompositeBehavior -> IO ()
removeAllBehaviors gkCompositeBehavior =
  sendMessage gkCompositeBehavior removeAllBehaviorsSelector

-- | Supports getting behaviors via a [int] subscript.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsGKCompositeBehavior gkCompositeBehavior => gkCompositeBehavior -> CULong -> IO (Id GKBehavior)
objectAtIndexedSubscript gkCompositeBehavior idx =
  sendMessage gkCompositeBehavior objectAtIndexedSubscriptSelector idx

-- | Supports setting a weight via a [behavior] subscript.
--
-- ObjC selector: @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: (IsGKCompositeBehavior gkCompositeBehavior, IsNSNumber weight, IsGKBehavior behavior) => gkCompositeBehavior -> weight -> behavior -> IO ()
setObject_forKeyedSubscript gkCompositeBehavior weight behavior =
  sendMessage gkCompositeBehavior setObject_forKeyedSubscriptSelector (toNSNumber weight) (toGKBehavior behavior)

-- | Supports getting a weight via a [behavior] subscript.
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> behavior -> IO (Id NSNumber)
objectForKeyedSubscript gkCompositeBehavior behavior =
  sendMessage gkCompositeBehavior objectForKeyedSubscriptSelector (toGKBehavior behavior)

-- | Number of sub-behaviors in this behavior
--
-- ObjC selector: @- behaviorCount@
behaviorCount :: IsGKCompositeBehavior gkCompositeBehavior => gkCompositeBehavior -> IO CLong
behaviorCount gkCompositeBehavior =
  sendMessage gkCompositeBehavior behaviorCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @behaviorWithBehaviors:@
behaviorWithBehaviorsSelector :: Selector '[Id NSArray] (Id GKCompositeBehavior)
behaviorWithBehaviorsSelector = mkSelector "behaviorWithBehaviors:"

-- | @Selector@ for @behaviorWithBehaviors:andWeights:@
behaviorWithBehaviors_andWeightsSelector :: Selector '[Id NSArray, Id NSArray] (Id GKCompositeBehavior)
behaviorWithBehaviors_andWeightsSelector = mkSelector "behaviorWithBehaviors:andWeights:"

-- | @Selector@ for @setWeight:forBehavior:@
setWeight_forBehaviorSelector :: Selector '[CFloat, Id GKBehavior] ()
setWeight_forBehaviorSelector = mkSelector "setWeight:forBehavior:"

-- | @Selector@ for @weightForBehavior:@
weightForBehaviorSelector :: Selector '[Id GKBehavior] CFloat
weightForBehaviorSelector = mkSelector "weightForBehavior:"

-- | @Selector@ for @removeBehavior:@
removeBehaviorSelector :: Selector '[Id GKBehavior] ()
removeBehaviorSelector = mkSelector "removeBehavior:"

-- | @Selector@ for @removeAllBehaviors@
removeAllBehaviorsSelector :: Selector '[] ()
removeAllBehaviorsSelector = mkSelector "removeAllBehaviors"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id GKBehavior)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector '[Id NSNumber, Id GKBehavior] ()
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id GKBehavior] (Id NSNumber)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @behaviorCount@
behaviorCountSelector :: Selector '[] CLong
behaviorCountSelector = mkSelector "behaviorCount"

