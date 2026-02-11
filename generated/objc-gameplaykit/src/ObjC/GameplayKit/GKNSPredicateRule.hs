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
  , initWithPredicateSelector
  , evaluatePredicateWithSystemSelector
  , predicateSelector


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

-- | Initializes a new rule with the given NSPredicate
--
-- ObjC selector: @- initWithPredicate:@
initWithPredicate :: (IsGKNSPredicateRule gknsPredicateRule, IsNSPredicate predicate) => gknsPredicateRule -> predicate -> IO (Id GKNSPredicateRule)
initWithPredicate gknsPredicateRule  predicate =
withObjCPtr predicate $ \raw_predicate ->
    sendMsg gknsPredicateRule (mkSelector "initWithPredicate:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ())] >>= ownedObject . castPtr

-- | Overridden here to call the predicate's evaluateWithObject:substitutionVariables:, using sys as the object and the system's state dictionary as the source of the substitution variables.
--
-- Returns: YES if the NSPredicate evaluation passes and the action needs to be performed, NO otherwise.
--
-- ObjC selector: @- evaluatePredicateWithSystem:@
evaluatePredicateWithSystem :: (IsGKNSPredicateRule gknsPredicateRule, IsGKRuleSystem system) => gknsPredicateRule -> system -> IO Bool
evaluatePredicateWithSystem gknsPredicateRule  system =
withObjCPtr system $ \raw_system ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gknsPredicateRule (mkSelector "evaluatePredicateWithSystem:") retCULong [argPtr (castPtr raw_system :: Ptr ())]

-- | The NSPredicate that is used inside this subclass's implementation of evaluatePredicateWithSystem: In order to effectively use this class you must still override performActionWithSystem:
--
-- See: GKRule.evaluatePredicateWithSystem:
--
-- ObjC selector: @- predicate@
predicate :: IsGKNSPredicateRule gknsPredicateRule => gknsPredicateRule -> IO (Id NSPredicate)
predicate gknsPredicateRule  =
  sendMsg gknsPredicateRule (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPredicate:@
initWithPredicateSelector :: Selector
initWithPredicateSelector = mkSelector "initWithPredicate:"

-- | @Selector@ for @evaluatePredicateWithSystem:@
evaluatePredicateWithSystemSelector :: Selector
evaluatePredicateWithSystemSelector = mkSelector "evaluatePredicateWithSystem:"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

