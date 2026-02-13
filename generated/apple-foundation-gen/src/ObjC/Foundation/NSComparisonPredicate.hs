{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSComparisonPredicate@.
module ObjC.Foundation.NSComparisonPredicate
  ( NSComparisonPredicate
  , IsNSComparisonPredicate(..)
  , predicateWithLeftExpression_rightExpression_modifier_type_options
  , predicateWithLeftExpression_rightExpression_customSelector
  , initWithLeftExpression_rightExpression_modifier_type_options
  , initWithLeftExpression_rightExpression_customSelector
  , initWithCoder
  , predicateOperatorType
  , comparisonPredicateModifier
  , leftExpression
  , rightExpression
  , customSelector
  , options
  , comparisonPredicateModifierSelector
  , customSelectorSelector
  , initWithCoderSelector
  , initWithLeftExpression_rightExpression_customSelectorSelector
  , initWithLeftExpression_rightExpression_modifier_type_optionsSelector
  , leftExpressionSelector
  , optionsSelector
  , predicateOperatorTypeSelector
  , predicateWithLeftExpression_rightExpression_customSelectorSelector
  , predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector
  , rightExpressionSelector

  -- * Enum types
  , NSComparisonPredicateModifier(NSComparisonPredicateModifier)
  , pattern NSDirectPredicateModifier
  , pattern NSAllPredicateModifier
  , pattern NSAnyPredicateModifier
  , NSComparisonPredicateOptions(NSComparisonPredicateOptions)
  , pattern NSCaseInsensitivePredicateOption
  , pattern NSDiacriticInsensitivePredicateOption
  , pattern NSNormalizedPredicateOption
  , NSPredicateOperatorType(NSPredicateOperatorType)
  , pattern NSLessThanPredicateOperatorType
  , pattern NSLessThanOrEqualToPredicateOperatorType
  , pattern NSGreaterThanPredicateOperatorType
  , pattern NSGreaterThanOrEqualToPredicateOperatorType
  , pattern NSEqualToPredicateOperatorType
  , pattern NSNotEqualToPredicateOperatorType
  , pattern NSMatchesPredicateOperatorType
  , pattern NSLikePredicateOperatorType
  , pattern NSBeginsWithPredicateOperatorType
  , pattern NSEndsWithPredicateOperatorType
  , pattern NSInPredicateOperatorType
  , pattern NSCustomSelectorPredicateOperatorType
  , pattern NSContainsPredicateOperatorType
  , pattern NSBetweenPredicateOperatorType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ predicateWithLeftExpression:rightExpression:modifier:type:options:@
predicateWithLeftExpression_rightExpression_modifier_type_options :: (IsNSExpression lhs, IsNSExpression rhs) => lhs -> rhs -> NSComparisonPredicateModifier -> NSPredicateOperatorType -> NSComparisonPredicateOptions -> IO (Id NSComparisonPredicate)
predicateWithLeftExpression_rightExpression_modifier_type_options lhs rhs modifier type_ options =
  do
    cls' <- getRequiredClass "NSComparisonPredicate"
    sendClassMessage cls' predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector (toNSExpression lhs) (toNSExpression rhs) modifier type_ options

-- | @+ predicateWithLeftExpression:rightExpression:customSelector:@
predicateWithLeftExpression_rightExpression_customSelector :: (IsNSExpression lhs, IsNSExpression rhs) => lhs -> rhs -> Sel -> IO (Id NSComparisonPredicate)
predicateWithLeftExpression_rightExpression_customSelector lhs rhs selector =
  do
    cls' <- getRequiredClass "NSComparisonPredicate"
    sendClassMessage cls' predicateWithLeftExpression_rightExpression_customSelectorSelector (toNSExpression lhs) (toNSExpression rhs) selector

-- | @- initWithLeftExpression:rightExpression:modifier:type:options:@
initWithLeftExpression_rightExpression_modifier_type_options :: (IsNSComparisonPredicate nsComparisonPredicate, IsNSExpression lhs, IsNSExpression rhs) => nsComparisonPredicate -> lhs -> rhs -> NSComparisonPredicateModifier -> NSPredicateOperatorType -> NSComparisonPredicateOptions -> IO (Id NSComparisonPredicate)
initWithLeftExpression_rightExpression_modifier_type_options nsComparisonPredicate lhs rhs modifier type_ options =
  sendOwnedMessage nsComparisonPredicate initWithLeftExpression_rightExpression_modifier_type_optionsSelector (toNSExpression lhs) (toNSExpression rhs) modifier type_ options

-- | @- initWithLeftExpression:rightExpression:customSelector:@
initWithLeftExpression_rightExpression_customSelector :: (IsNSComparisonPredicate nsComparisonPredicate, IsNSExpression lhs, IsNSExpression rhs) => nsComparisonPredicate -> lhs -> rhs -> Sel -> IO (Id NSComparisonPredicate)
initWithLeftExpression_rightExpression_customSelector nsComparisonPredicate lhs rhs selector =
  sendOwnedMessage nsComparisonPredicate initWithLeftExpression_rightExpression_customSelectorSelector (toNSExpression lhs) (toNSExpression rhs) selector

-- | @- initWithCoder:@
initWithCoder :: (IsNSComparisonPredicate nsComparisonPredicate, IsNSCoder coder) => nsComparisonPredicate -> coder -> IO (Id NSComparisonPredicate)
initWithCoder nsComparisonPredicate coder =
  sendOwnedMessage nsComparisonPredicate initWithCoderSelector (toNSCoder coder)

-- | @- predicateOperatorType@
predicateOperatorType :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO NSPredicateOperatorType
predicateOperatorType nsComparisonPredicate =
  sendMessage nsComparisonPredicate predicateOperatorTypeSelector

-- | @- comparisonPredicateModifier@
comparisonPredicateModifier :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO NSComparisonPredicateModifier
comparisonPredicateModifier nsComparisonPredicate =
  sendMessage nsComparisonPredicate comparisonPredicateModifierSelector

-- | @- leftExpression@
leftExpression :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO (Id NSExpression)
leftExpression nsComparisonPredicate =
  sendMessage nsComparisonPredicate leftExpressionSelector

-- | @- rightExpression@
rightExpression :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO (Id NSExpression)
rightExpression nsComparisonPredicate =
  sendMessage nsComparisonPredicate rightExpressionSelector

-- | @- customSelector@
customSelector :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO Sel
customSelector nsComparisonPredicate =
  sendMessage nsComparisonPredicate customSelectorSelector

-- | @- options@
options :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO NSComparisonPredicateOptions
options nsComparisonPredicate =
  sendMessage nsComparisonPredicate optionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithLeftExpression:rightExpression:modifier:type:options:@
predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector :: Selector '[Id NSExpression, Id NSExpression, NSComparisonPredicateModifier, NSPredicateOperatorType, NSComparisonPredicateOptions] (Id NSComparisonPredicate)
predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector = mkSelector "predicateWithLeftExpression:rightExpression:modifier:type:options:"

-- | @Selector@ for @predicateWithLeftExpression:rightExpression:customSelector:@
predicateWithLeftExpression_rightExpression_customSelectorSelector :: Selector '[Id NSExpression, Id NSExpression, Sel] (Id NSComparisonPredicate)
predicateWithLeftExpression_rightExpression_customSelectorSelector = mkSelector "predicateWithLeftExpression:rightExpression:customSelector:"

-- | @Selector@ for @initWithLeftExpression:rightExpression:modifier:type:options:@
initWithLeftExpression_rightExpression_modifier_type_optionsSelector :: Selector '[Id NSExpression, Id NSExpression, NSComparisonPredicateModifier, NSPredicateOperatorType, NSComparisonPredicateOptions] (Id NSComparisonPredicate)
initWithLeftExpression_rightExpression_modifier_type_optionsSelector = mkSelector "initWithLeftExpression:rightExpression:modifier:type:options:"

-- | @Selector@ for @initWithLeftExpression:rightExpression:customSelector:@
initWithLeftExpression_rightExpression_customSelectorSelector :: Selector '[Id NSExpression, Id NSExpression, Sel] (Id NSComparisonPredicate)
initWithLeftExpression_rightExpression_customSelectorSelector = mkSelector "initWithLeftExpression:rightExpression:customSelector:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSComparisonPredicate)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @predicateOperatorType@
predicateOperatorTypeSelector :: Selector '[] NSPredicateOperatorType
predicateOperatorTypeSelector = mkSelector "predicateOperatorType"

-- | @Selector@ for @comparisonPredicateModifier@
comparisonPredicateModifierSelector :: Selector '[] NSComparisonPredicateModifier
comparisonPredicateModifierSelector = mkSelector "comparisonPredicateModifier"

-- | @Selector@ for @leftExpression@
leftExpressionSelector :: Selector '[] (Id NSExpression)
leftExpressionSelector = mkSelector "leftExpression"

-- | @Selector@ for @rightExpression@
rightExpressionSelector :: Selector '[] (Id NSExpression)
rightExpressionSelector = mkSelector "rightExpression"

-- | @Selector@ for @customSelector@
customSelectorSelector :: Selector '[] Sel
customSelectorSelector = mkSelector "customSelector"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] NSComparisonPredicateOptions
optionsSelector = mkSelector "options"

