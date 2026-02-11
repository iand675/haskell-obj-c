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
  , behaviorWithBehaviorsSelector
  , behaviorWithBehaviors_andWeightsSelector
  , setWeight_forBehaviorSelector
  , weightForBehaviorSelector
  , removeBehaviorSelector
  , removeAllBehaviorsSelector
  , objectAtIndexedSubscriptSelector
  , setObject_forKeyedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , behaviorCountSelector


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

-- | Creates a behavior with an array of sub-behaviors
--
-- ObjC selector: @+ behaviorWithBehaviors:@
behaviorWithBehaviors :: IsNSArray behaviors => behaviors -> IO (Id GKCompositeBehavior)
behaviorWithBehaviors behaviors =
  do
    cls' <- getRequiredClass "GKCompositeBehavior"
    withObjCPtr behaviors $ \raw_behaviors ->
      sendClassMsg cls' (mkSelector "behaviorWithBehaviors:") (retPtr retVoid) [argPtr (castPtr raw_behaviors :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a behavior with two associated arrays of sub-behaviors and weights
--
-- ObjC selector: @+ behaviorWithBehaviors:andWeights:@
behaviorWithBehaviors_andWeights :: (IsNSArray behaviors, IsNSArray weights) => behaviors -> weights -> IO (Id GKCompositeBehavior)
behaviorWithBehaviors_andWeights behaviors weights =
  do
    cls' <- getRequiredClass "GKCompositeBehavior"
    withObjCPtr behaviors $ \raw_behaviors ->
      withObjCPtr weights $ \raw_weights ->
        sendClassMsg cls' (mkSelector "behaviorWithBehaviors:andWeights:") (retPtr retVoid) [argPtr (castPtr raw_behaviors :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

-- | Adds a new sub-behavior or changes the weight of the existing sub-behavior in this behavior. If the sub-behavior  does not exist in this behavior, it is added.
--
-- @weight@ — the weight for this goal
--
-- @behavior@ — the sub-behavior who's weight to change
--
-- ObjC selector: @- setWeight:forBehavior:@
setWeight_forBehavior :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> CFloat -> behavior -> IO ()
setWeight_forBehavior gkCompositeBehavior  weight behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg gkCompositeBehavior (mkSelector "setWeight:forBehavior:") retVoid [argCFloat (fromIntegral weight), argPtr (castPtr raw_behavior :: Ptr ())]

-- | Gets the current weight for a given sub-behavior.
--
-- Returns: the weight of the sub-behavior, or 0 if there is no such sub-behavior on this behavior
--
-- ObjC selector: @- weightForBehavior:@
weightForBehavior :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> behavior -> IO CFloat
weightForBehavior gkCompositeBehavior  behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg gkCompositeBehavior (mkSelector "weightForBehavior:") retCFloat [argPtr (castPtr raw_behavior :: Ptr ())]

-- | Remove the indicated sub-behavior from this behavior.
--
-- @behavior@ — the sub-behavior to be removed
--
-- ObjC selector: @- removeBehavior:@
removeBehavior :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> behavior -> IO ()
removeBehavior gkCompositeBehavior  behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg gkCompositeBehavior (mkSelector "removeBehavior:") retVoid [argPtr (castPtr raw_behavior :: Ptr ())]

-- | Removes all the sub-behavior on the behavior.
--
-- ObjC selector: @- removeAllBehaviors@
removeAllBehaviors :: IsGKCompositeBehavior gkCompositeBehavior => gkCompositeBehavior -> IO ()
removeAllBehaviors gkCompositeBehavior  =
  sendMsg gkCompositeBehavior (mkSelector "removeAllBehaviors") retVoid []

-- | Supports getting behaviors via a [int] subscript.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsGKCompositeBehavior gkCompositeBehavior => gkCompositeBehavior -> CULong -> IO (Id GKBehavior)
objectAtIndexedSubscript gkCompositeBehavior  idx =
  sendMsg gkCompositeBehavior (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral idx)] >>= retainedObject . castPtr

-- | Supports setting a weight via a [behavior] subscript.
--
-- ObjC selector: @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: (IsGKCompositeBehavior gkCompositeBehavior, IsNSNumber weight, IsGKBehavior behavior) => gkCompositeBehavior -> weight -> behavior -> IO ()
setObject_forKeyedSubscript gkCompositeBehavior  weight behavior =
withObjCPtr weight $ \raw_weight ->
  withObjCPtr behavior $ \raw_behavior ->
      sendMsg gkCompositeBehavior (mkSelector "setObject:forKeyedSubscript:") retVoid [argPtr (castPtr raw_weight :: Ptr ()), argPtr (castPtr raw_behavior :: Ptr ())]

-- | Supports getting a weight via a [behavior] subscript.
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsGKCompositeBehavior gkCompositeBehavior, IsGKBehavior behavior) => gkCompositeBehavior -> behavior -> IO (Id NSNumber)
objectForKeyedSubscript gkCompositeBehavior  behavior =
withObjCPtr behavior $ \raw_behavior ->
    sendMsg gkCompositeBehavior (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_behavior :: Ptr ())] >>= retainedObject . castPtr

-- | Number of sub-behaviors in this behavior
--
-- ObjC selector: @- behaviorCount@
behaviorCount :: IsGKCompositeBehavior gkCompositeBehavior => gkCompositeBehavior -> IO CLong
behaviorCount gkCompositeBehavior  =
  sendMsg gkCompositeBehavior (mkSelector "behaviorCount") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @behaviorWithBehaviors:@
behaviorWithBehaviorsSelector :: Selector
behaviorWithBehaviorsSelector = mkSelector "behaviorWithBehaviors:"

-- | @Selector@ for @behaviorWithBehaviors:andWeights:@
behaviorWithBehaviors_andWeightsSelector :: Selector
behaviorWithBehaviors_andWeightsSelector = mkSelector "behaviorWithBehaviors:andWeights:"

-- | @Selector@ for @setWeight:forBehavior:@
setWeight_forBehaviorSelector :: Selector
setWeight_forBehaviorSelector = mkSelector "setWeight:forBehavior:"

-- | @Selector@ for @weightForBehavior:@
weightForBehaviorSelector :: Selector
weightForBehaviorSelector = mkSelector "weightForBehavior:"

-- | @Selector@ for @removeBehavior:@
removeBehaviorSelector :: Selector
removeBehaviorSelector = mkSelector "removeBehavior:"

-- | @Selector@ for @removeAllBehaviors@
removeAllBehaviorsSelector :: Selector
removeAllBehaviorsSelector = mkSelector "removeAllBehaviors"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @behaviorCount@
behaviorCountSelector :: Selector
behaviorCountSelector = mkSelector "behaviorCount"

