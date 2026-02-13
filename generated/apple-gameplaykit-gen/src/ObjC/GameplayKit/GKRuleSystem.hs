{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A rule system consists of 3 things: - The current state, which upon creation is considered the inital state. - The current set of rules. - The current set of facts.
--
-- Each time a fact is added to the system, the set of rules are evaluated in order and their actions executed in the system if their predicates are true. Rules can be fuzzy, allowing predicates and facts to be asserted to a degree of confidence instead of just boolean on/off.
--
-- The facts can be any kind of objects as long as they correctly determine equality using isEqual: The simplest approach is to use strings or dictionaries as they provide the most flexibility in defining facts, but user defined classes work just as well and may describe the problem space better.
--
-- The fact set is at all times a fuzzy set, as defined by fact membership in the set being modulated by their grade of membership. The rules may use the grade of membership to predicate their actions and in such a manner create fuzzy logic. The fuzzy logic Zadeh operators are available on the system itself in order to query multiple facts for combined membership grade.
--
-- Generated bindings for @GKRuleSystem@.
module ObjC.GameplayKit.GKRuleSystem
  ( GKRuleSystem
  , IsGKRuleSystem(..)
  , init_
  , evaluate
  , addRule
  , addRulesFromArray
  , removeAllRules
  , gradeForFact
  , minimumGradeForFacts
  , maximumGradeForFacts
  , assertFact
  , assertFact_grade
  , retractFact
  , retractFact_grade
  , reset
  , state
  , rules
  , agenda
  , executed
  , facts
  , addRuleSelector
  , addRulesFromArraySelector
  , agendaSelector
  , assertFactSelector
  , assertFact_gradeSelector
  , evaluateSelector
  , executedSelector
  , factsSelector
  , gradeForFactSelector
  , initSelector
  , maximumGradeForFactsSelector
  , minimumGradeForFactsSelector
  , removeAllRulesSelector
  , resetSelector
  , retractFactSelector
  , retractFact_gradeSelector
  , rulesSelector
  , stateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a clean rule system with no state, rules or facts.
--
-- ObjC selector: @- init@
init_ :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO (Id GKRuleSystem)
init_ gkRuleSystem =
  sendOwnedMessage gkRuleSystem initSelector

-- | Explicitly evaluate the agenda of the rule system based on the current state and the current set of facts.
--
-- This may in turn assert or retract more facts or change the state of the system, including activating more rules in the agenda.
--
-- ObjC selector: @- evaluate@
evaluate :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO ()
evaluate gkRuleSystem =
  sendMessage gkRuleSystem evaluateSelector

-- | Adds a rule to the system. Also adds it to the agenda in salience order.
--
-- ObjC selector: @- addRule:@
addRule :: (IsGKRuleSystem gkRuleSystem, IsGKRule rule) => gkRuleSystem -> rule -> IO ()
addRule gkRuleSystem rule =
  sendMessage gkRuleSystem addRuleSelector (toGKRule rule)

-- | Adds rules to the system. Also adds them to the agenda in salience order.
--
-- ObjC selector: @- addRulesFromArray:@
addRulesFromArray :: (IsGKRuleSystem gkRuleSystem, IsNSArray rules) => gkRuleSystem -> rules -> IO ()
addRulesFromArray gkRuleSystem rules =
  sendMessage gkRuleSystem addRulesFromArraySelector (toNSArray rules)

-- | Removes all rules from the system.  This also removes them from the agenda and executed sets.
--
-- ObjC selector: @- removeAllRules@
removeAllRules :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO ()
removeAllRules gkRuleSystem =
  sendMessage gkRuleSystem removeAllRulesSelector

-- | Returns the current membership grade for the given fact, which is 0.0 if the fact is not a member of the current set of facts.
--
-- Returns: The membership grade of the given fact, in the range [0.0, 1.0].
--
-- ObjC selector: @- gradeForFact:@
gradeForFact :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> RawId -> IO CFloat
gradeForFact gkRuleSystem fact =
  sendMessage gkRuleSystem gradeForFactSelector fact

-- | Returns the combined membership grade for the all the given facts.
--
-- This performs the logical AND operation between the given facts.
--
-- Returns: The membership grade by applying the AND operator on the given facts, in the range [0.0, 1.0].
--
-- ObjC selector: @- minimumGradeForFacts:@
minimumGradeForFacts :: (IsGKRuleSystem gkRuleSystem, IsNSArray facts) => gkRuleSystem -> facts -> IO CFloat
minimumGradeForFacts gkRuleSystem facts =
  sendMessage gkRuleSystem minimumGradeForFactsSelector (toNSArray facts)

-- | Returns the maximum membership grade for the any one of the given facts.
--
-- This performs the logical OR operation between the given facts.
--
-- Returns: The membership grade by applying the OR operator on the given facts, in the range [0.0, 1.0].
--
-- ObjC selector: @- maximumGradeForFacts:@
maximumGradeForFacts :: (IsGKRuleSystem gkRuleSystem, IsNSArray facts) => gkRuleSystem -> facts -> IO CFloat
maximumGradeForFacts gkRuleSystem facts =
  sendMessage gkRuleSystem maximumGradeForFactsSelector (toNSArray facts)

-- | Asserts a fact with membership grade of 1.0.
--
-- This will cause the current rules to be evaluated, which may in turn assert or retract more facts or change the state of the system.
--
-- This is shorthand for calling assertFact:grade: with a grade of 1.0
--
-- See: assertFact:grade:
--
-- See: evaluate
--
-- See: NSObject.isEqual:
--
-- ObjC selector: @- assertFact:@
assertFact :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> RawId -> IO ()
assertFact gkRuleSystem fact =
  sendMessage gkRuleSystem assertFactSelector fact

-- | Asserts a fact with the supplied membership grade.
--
-- This will cause the current rules to be evaluated, which may in turn assert or retract more facts or change the state of the system.
--
-- See: evaluate
--
-- ObjC selector: @- assertFact:grade:@
assertFact_grade :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> RawId -> CFloat -> IO ()
assertFact_grade gkRuleSystem fact grade =
  sendMessage gkRuleSystem assertFact_gradeSelector fact grade

-- | Retracts a fact, setting its membership grade to 0, which also removes it from the fact set.
--
-- This will cause the current rules to be evaluated, which may in turn assert or retract more facts or change the state of the system.
--
-- This is short hand for calling retractFact:grade: with a grade of 1.0
--
-- See: retractFact:grade:
--
-- See: evaluate
--
-- ObjC selector: @- retractFact:@
retractFact :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> RawId -> IO ()
retractFact gkRuleSystem fact =
  sendMessage gkRuleSystem retractFactSelector fact

-- | Retracts a fact, reducing its membership grade by the supplied grade. If this brings the grade to 0 it is also removed from the fact set.
--
-- This will cause the current rules to be evaluated, which may in turn assert or retract more facts or change the state of the system.
--
-- See: evaluate
--
-- ObjC selector: @- retractFact:grade:@
retractFact_grade :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> RawId -> CFloat -> IO ()
retractFact_grade gkRuleSystem fact grade =
  sendMessage gkRuleSystem retractFact_gradeSelector fact grade

-- | Clears the agenda and executed sets and removes all facts currently in the system. It then fills the agenda with rules from the rule set, in salience order.
--
-- See: rules
--
-- See: facts
--
-- ObjC selector: @- reset@
reset :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO ()
reset gkRuleSystem =
  sendMessage gkRuleSystem resetSelector

-- | The implementation-defined state. If any changes are made on this outside the system you must call evaluate to have the system take account of the changes.
--
-- See: evaluate
--
-- ObjC selector: @- state@
state :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO (Id NSMutableDictionary)
state gkRuleSystem =
  sendMessage gkRuleSystem stateSelector

-- | The current set of rules that will be used to set the agenda when rules are first added to the system. They will also be used to refill the agenda whenever it is set.
--
-- This is at all times the union of the agenda and executed sets.
--
-- See: agenda
--
-- See: executed
--
-- ObjC selector: @- rules@
rules :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO (Id NSArray)
rules gkRuleSystem =
  sendMessage gkRuleSystem rulesSelector

-- | The current set of rules to be evaluated, in salience order, where if the salience is equivalent the order of insertion into the agenda is used to decide which is first. Adjust salience of your rules to adjust the order the next time the agenda is reset. Changing salience on a rule currently in the agenda does not change its order in the agenda.
--
-- This is at all times the difference between the rules and executed sets.
--
-- See: rules
--
-- See: executed
--
-- See: reset
--
-- ObjC selector: @- agenda@
agenda :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO (Id NSArray)
agenda gkRuleSystem =
  sendMessage gkRuleSystem agendaSelector

-- | The current set of rules that have already executed. Rules in this set will not be executed again until the system is reset.
--
-- This is at all times the difference between the rules and agenda sets.
--
-- See: rules
--
-- See: agenda
--
-- See: reset
--
-- ObjC selector: @- executed@
executed :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO (Id NSArray)
executed gkRuleSystem =
  sendMessage gkRuleSystem executedSelector

-- | The current set of facts. Facts have a grade of membership that is >= 0.0. Query the system for the individual grades of membership with gradeForFact:
--
-- See: gradeForFact:
--
-- ObjC selector: @- facts@
facts :: IsGKRuleSystem gkRuleSystem => gkRuleSystem -> IO (Id NSArray)
facts gkRuleSystem =
  sendMessage gkRuleSystem factsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKRuleSystem)
initSelector = mkSelector "init"

-- | @Selector@ for @evaluate@
evaluateSelector :: Selector '[] ()
evaluateSelector = mkSelector "evaluate"

-- | @Selector@ for @addRule:@
addRuleSelector :: Selector '[Id GKRule] ()
addRuleSelector = mkSelector "addRule:"

-- | @Selector@ for @addRulesFromArray:@
addRulesFromArraySelector :: Selector '[Id NSArray] ()
addRulesFromArraySelector = mkSelector "addRulesFromArray:"

-- | @Selector@ for @removeAllRules@
removeAllRulesSelector :: Selector '[] ()
removeAllRulesSelector = mkSelector "removeAllRules"

-- | @Selector@ for @gradeForFact:@
gradeForFactSelector :: Selector '[RawId] CFloat
gradeForFactSelector = mkSelector "gradeForFact:"

-- | @Selector@ for @minimumGradeForFacts:@
minimumGradeForFactsSelector :: Selector '[Id NSArray] CFloat
minimumGradeForFactsSelector = mkSelector "minimumGradeForFacts:"

-- | @Selector@ for @maximumGradeForFacts:@
maximumGradeForFactsSelector :: Selector '[Id NSArray] CFloat
maximumGradeForFactsSelector = mkSelector "maximumGradeForFacts:"

-- | @Selector@ for @assertFact:@
assertFactSelector :: Selector '[RawId] ()
assertFactSelector = mkSelector "assertFact:"

-- | @Selector@ for @assertFact:grade:@
assertFact_gradeSelector :: Selector '[RawId, CFloat] ()
assertFact_gradeSelector = mkSelector "assertFact:grade:"

-- | @Selector@ for @retractFact:@
retractFactSelector :: Selector '[RawId] ()
retractFactSelector = mkSelector "retractFact:"

-- | @Selector@ for @retractFact:grade:@
retractFact_gradeSelector :: Selector '[RawId, CFloat] ()
retractFact_gradeSelector = mkSelector "retractFact:grade:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSMutableDictionary)
stateSelector = mkSelector "state"

-- | @Selector@ for @rules@
rulesSelector :: Selector '[] (Id NSArray)
rulesSelector = mkSelector "rules"

-- | @Selector@ for @agenda@
agendaSelector :: Selector '[] (Id NSArray)
agendaSelector = mkSelector "agenda"

-- | @Selector@ for @executed@
executedSelector :: Selector '[] (Id NSArray)
executedSelector = mkSelector "executed"

-- | @Selector@ for @facts@
factsSelector :: Selector '[] (Id NSArray)
factsSelector = mkSelector "facts"

