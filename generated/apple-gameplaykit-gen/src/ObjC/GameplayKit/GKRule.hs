{-# LANGUAGE DataKinds #-}
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
  , ruleWithBlockPredicate_actionSelector
  , ruleWithPredicate_assertingFact_gradeSelector
  , ruleWithPredicate_retractingFact_gradeSelector
  , salienceSelector
  , setSalienceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
evaluatePredicateWithSystem gkRule system =
  sendMessage gkRule evaluatePredicateWithSystemSelector (toGKRuleSystem system)

-- | Performs the action consequence for the rule. This will only be called if the predicate evaluates to YES. Any facts asserted or retracted by the action on the system will cause the system to evaluate the agenda rule set again once the action completes.
--
-- ObjC selector: @- performActionWithSystem:@
performActionWithSystem :: (IsGKRule gkRule, IsGKRuleSystem system) => gkRule -> system -> IO ()
performActionWithSystem gkRule system =
  sendMessage gkRule performActionWithSystemSelector (toGKRuleSystem system)

-- | Create a data-driven rule that uses NSPredicate and a single assert as the action.
--
-- ObjC selector: @+ ruleWithPredicate:assertingFact:grade:@
ruleWithPredicate_assertingFact_grade :: IsNSPredicate predicate => predicate -> RawId -> CFloat -> IO (Id GKRule)
ruleWithPredicate_assertingFact_grade predicate fact grade =
  do
    cls' <- getRequiredClass "GKRule"
    sendClassMessage cls' ruleWithPredicate_assertingFact_gradeSelector (toNSPredicate predicate) fact grade

-- | Short hand for data-driven rule that uses NSPredicate and a single retract as the action.
--
-- ObjC selector: @+ ruleWithPredicate:retractingFact:grade:@
ruleWithPredicate_retractingFact_grade :: IsNSPredicate predicate => predicate -> RawId -> CFloat -> IO (Id GKRule)
ruleWithPredicate_retractingFact_grade predicate fact grade =
  do
    cls' <- getRequiredClass "GKRule"
    sendClassMessage cls' ruleWithPredicate_retractingFact_gradeSelector (toNSPredicate predicate) fact grade

-- | Short hand for making a rule that uses blocks for the predicate and action. This rule is not able to be archived using NSKeyedArchiver so use a subclass or NSPredicate based rule if serialization of the rule is needed.
--
-- ObjC selector: @+ ruleWithBlockPredicate:action:@
ruleWithBlockPredicate_action :: Ptr () -> Ptr () -> IO (Id GKRule)
ruleWithBlockPredicate_action predicate action =
  do
    cls' <- getRequiredClass "GKRule"
    sendClassMessage cls' ruleWithBlockPredicate_actionSelector predicate action

-- | Salience defines the order in the rule agenda that the system will evaluate. A rule with higher salience will be evaluated before another rule in the agenda that has a lower salience.
--
-- Defaults to 0.
--
-- See: GKRuleSystem.agenda
--
-- ObjC selector: @- salience@
salience :: IsGKRule gkRule => gkRule -> IO CLong
salience gkRule =
  sendMessage gkRule salienceSelector

-- | Salience defines the order in the rule agenda that the system will evaluate. A rule with higher salience will be evaluated before another rule in the agenda that has a lower salience.
--
-- Defaults to 0.
--
-- See: GKRuleSystem.agenda
--
-- ObjC selector: @- setSalience:@
setSalience :: IsGKRule gkRule => gkRule -> CLong -> IO ()
setSalience gkRule value =
  sendMessage gkRule setSalienceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @evaluatePredicateWithSystem:@
evaluatePredicateWithSystemSelector :: Selector '[Id GKRuleSystem] Bool
evaluatePredicateWithSystemSelector = mkSelector "evaluatePredicateWithSystem:"

-- | @Selector@ for @performActionWithSystem:@
performActionWithSystemSelector :: Selector '[Id GKRuleSystem] ()
performActionWithSystemSelector = mkSelector "performActionWithSystem:"

-- | @Selector@ for @ruleWithPredicate:assertingFact:grade:@
ruleWithPredicate_assertingFact_gradeSelector :: Selector '[Id NSPredicate, RawId, CFloat] (Id GKRule)
ruleWithPredicate_assertingFact_gradeSelector = mkSelector "ruleWithPredicate:assertingFact:grade:"

-- | @Selector@ for @ruleWithPredicate:retractingFact:grade:@
ruleWithPredicate_retractingFact_gradeSelector :: Selector '[Id NSPredicate, RawId, CFloat] (Id GKRule)
ruleWithPredicate_retractingFact_gradeSelector = mkSelector "ruleWithPredicate:retractingFact:grade:"

-- | @Selector@ for @ruleWithBlockPredicate:action:@
ruleWithBlockPredicate_actionSelector :: Selector '[Ptr (), Ptr ()] (Id GKRule)
ruleWithBlockPredicate_actionSelector = mkSelector "ruleWithBlockPredicate:action:"

-- | @Selector@ for @salience@
salienceSelector :: Selector '[] CLong
salienceSelector = mkSelector "salience"

-- | @Selector@ for @setSalience:@
setSalienceSelector :: Selector '[CLong] ()
setSalienceSelector = mkSelector "setSalience:"

