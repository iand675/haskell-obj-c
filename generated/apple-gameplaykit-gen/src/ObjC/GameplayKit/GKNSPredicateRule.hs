{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A convenient subclass of GKRule that leverages existing NSPRedicate functionality for evaluating the predicate of the rule.
--
-- Generated bindings for @GKNSPredicateRule@.
module ObjC.GameplayKit.GKNSPredicateRule
  ( GKNSPredicateRule
  , IsGKNSPredicateRule(..)
  , initWithPredicate
  , evaluatePredicateWithSystem
  , predicate
  , evaluatePredicateWithSystemSelector
  , initWithPredicateSelector
  , predicateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new rule with the given NSPredicate
--
-- ObjC selector: @- initWithPredicate:@
initWithPredicate :: (IsGKNSPredicateRule gknsPredicateRule, IsNSPredicate predicate) => gknsPredicateRule -> predicate -> IO (Id GKNSPredicateRule)
initWithPredicate gknsPredicateRule predicate =
  sendOwnedMessage gknsPredicateRule initWithPredicateSelector (toNSPredicate predicate)

-- | Overridden here to call the predicate's evaluateWithObject:substitutionVariables:, using sys as the object and the system's state dictionary as the source of the substitution variables.
--
-- Returns: YES if the NSPredicate evaluation passes and the action needs to be performed, NO otherwise.
--
-- ObjC selector: @- evaluatePredicateWithSystem:@
evaluatePredicateWithSystem :: (IsGKNSPredicateRule gknsPredicateRule, IsGKRuleSystem system) => gknsPredicateRule -> system -> IO Bool
evaluatePredicateWithSystem gknsPredicateRule system =
  sendMessage gknsPredicateRule evaluatePredicateWithSystemSelector (toGKRuleSystem system)

-- | The NSPredicate that is used inside this subclass's implementation of evaluatePredicateWithSystem: In order to effectively use this class you must still override performActionWithSystem:
--
-- See: GKRule.evaluatePredicateWithSystem:
--
-- ObjC selector: @- predicate@
predicate :: IsGKNSPredicateRule gknsPredicateRule => gknsPredicateRule -> IO (Id NSPredicate)
predicate gknsPredicateRule =
  sendMessage gknsPredicateRule predicateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPredicate:@
initWithPredicateSelector :: Selector '[Id NSPredicate] (Id GKNSPredicateRule)
initWithPredicateSelector = mkSelector "initWithPredicate:"

-- | @Selector@ for @evaluatePredicateWithSystem:@
evaluatePredicateWithSystemSelector :: Selector '[Id GKRuleSystem] Bool
evaluatePredicateWithSystemSelector = mkSelector "evaluatePredicateWithSystem:"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

