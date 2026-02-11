{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The concrete class that the GKRuleSystem uses to evaluate the current state and facts with predicated rules. These are sharable between systems, so don't retain any state in the rules themselves. Use the system-provided state storage.
--
-- See: GKRuleSystem.state
--
-- Generated bindings for @GKRule@.
module ObjC.GameplayKit.GKRule
  ( GKRule
  , IsGKRule(..)
  , evaluatePredicateWithSystem
  , performActionWithSystem
  , ruleWithPredicate_assertingFact_grade
  , ruleWithPredicate_retractingFact_grade
  , ruleWithBlockPredicate_action
  , salience
  , setSalience
  , evaluatePredicateWithSystemSelector
  , performActionWithSystemSelector
  , ruleWithPredicate_assertingFact_gradeSelector
  , ruleWithPredicate_retractingFact_gradeSelector
  , ruleWithBlockPredicate_actionSelector
  , salienceSelector
  , setSalienceSelector


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

-- | Called by the rule system when it is this rule's turn to be evaluated. If the predicate returns YES then the action for the rule will be performed. Once the action is performed the rule will move to the system's executed list until the agenda is reset.
--
-- See: performAction
--
-- See: GKRuleSystem.agenda
--
-- See: GKRuleSystem.executed
--
-- See: GKRuleSystem.reset
--
-- Returns: YES is the predicate passes and the action needs to be performed, NO otherwise.
--
-- ObjC selector: @- evaluatePredicateWithSystem:@
evaluatePredicateWithSystem :: (IsGKRule gkRule, IsGKRuleSystem system) => gkRule -> system -> IO Bool
evaluatePredicateWithSystem gkRule  system =
withObjCPtr system $ \raw_system ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkRule (mkSelector "evaluatePredicateWithSystem:") retCULong [argPtr (castPtr raw_system :: Ptr ())]

-- | Performs the action consequence for the rule. This will only be called if the predicate evaluates to YES. Any facts asserted or retracted by the action on the system will cause the system to evaluate the agenda rule set again once the action completes.
--
-- ObjC selector: @- performActionWithSystem:@
performActionWithSystem :: (IsGKRule gkRule, IsGKRuleSystem system) => gkRule -> system -> IO ()
performActionWithSystem gkRule  system =
withObjCPtr system $ \raw_system ->
    sendMsg gkRule (mkSelector "performActionWithSystem:") retVoid [argPtr (castPtr raw_system :: Ptr ())]

-- | Create a data-driven rule that uses NSPredicate and a single assert as the action.
--
-- ObjC selector: @+ ruleWithPredicate:assertingFact:grade:@
ruleWithPredicate_assertingFact_grade :: IsNSPredicate predicate => predicate -> RawId -> CFloat -> IO (Id GKRule)
ruleWithPredicate_assertingFact_grade predicate fact grade =
  do
    cls' <- getRequiredClass "GKRule"
    withObjCPtr predicate $ \raw_predicate ->
      sendClassMsg cls' (mkSelector "ruleWithPredicate:assertingFact:grade:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr (unRawId fact) :: Ptr ()), argCFloat (fromIntegral grade)] >>= retainedObject . castPtr

-- | Short hand for data-driven rule that uses NSPredicate and a single retract as the action.
--
-- ObjC selector: @+ ruleWithPredicate:retractingFact:grade:@
ruleWithPredicate_retractingFact_grade :: IsNSPredicate predicate => predicate -> RawId -> CFloat -> IO (Id GKRule)
ruleWithPredicate_retractingFact_grade predicate fact grade =
  do
    cls' <- getRequiredClass "GKRule"
    withObjCPtr predicate $ \raw_predicate ->
      sendClassMsg cls' (mkSelector "ruleWithPredicate:retractingFact:grade:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr (unRawId fact) :: Ptr ()), argCFloat (fromIntegral grade)] >>= retainedObject . castPtr

-- | Short hand for making a rule that uses blocks for the predicate and action. This rule is not able to be archived using NSKeyedArchiver so use a subclass or NSPredicate based rule if serialization of the rule is needed.
--
-- ObjC selector: @+ ruleWithBlockPredicate:action:@
ruleWithBlockPredicate_action :: Ptr () -> Ptr () -> IO (Id GKRule)
ruleWithBlockPredicate_action predicate action =
  do
    cls' <- getRequiredClass "GKRule"
    sendClassMsg cls' (mkSelector "ruleWithBlockPredicate:action:") (retPtr retVoid) [argPtr (castPtr predicate :: Ptr ()), argPtr (castPtr action :: Ptr ())] >>= retainedObject . castPtr

-- | Salience defines the order in the rule agenda that the system will evaluate. A rule with higher salience will be evaluated before another rule in the agenda that has a lower salience.
--
-- Defaults to 0.
--
-- See: GKRuleSystem.agenda
--
-- ObjC selector: @- salience@
salience :: IsGKRule gkRule => gkRule -> IO CLong
salience gkRule  =
  sendMsg gkRule (mkSelector "salience") retCLong []

-- | Salience defines the order in the rule agenda that the system will evaluate. A rule with higher salience will be evaluated before another rule in the agenda that has a lower salience.
--
-- Defaults to 0.
--
-- See: GKRuleSystem.agenda
--
-- ObjC selector: @- setSalience:@
setSalience :: IsGKRule gkRule => gkRule -> CLong -> IO ()
setSalience gkRule  value =
  sendMsg gkRule (mkSelector "setSalience:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @evaluatePredicateWithSystem:@
evaluatePredicateWithSystemSelector :: Selector
evaluatePredicateWithSystemSelector = mkSelector "evaluatePredicateWithSystem:"

-- | @Selector@ for @performActionWithSystem:@
performActionWithSystemSelector :: Selector
performActionWithSystemSelector = mkSelector "performActionWithSystem:"

-- | @Selector@ for @ruleWithPredicate:assertingFact:grade:@
ruleWithPredicate_assertingFact_gradeSelector :: Selector
ruleWithPredicate_assertingFact_gradeSelector = mkSelector "ruleWithPredicate:assertingFact:grade:"

-- | @Selector@ for @ruleWithPredicate:retractingFact:grade:@
ruleWithPredicate_retractingFact_gradeSelector :: Selector
ruleWithPredicate_retractingFact_gradeSelector = mkSelector "ruleWithPredicate:retractingFact:grade:"

-- | @Selector@ for @ruleWithBlockPredicate:action:@
ruleWithBlockPredicate_actionSelector :: Selector
ruleWithBlockPredicate_actionSelector = mkSelector "ruleWithBlockPredicate:action:"

-- | @Selector@ for @salience@
salienceSelector :: Selector
salienceSelector = mkSelector "salience"

-- | @Selector@ for @setSalience:@
setSalienceSelector :: Selector
setSalienceSelector = mkSelector "setSalience:"

