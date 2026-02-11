{-# LANGUAGE PatternSynonyms #-}
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
  , predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector
  , predicateWithLeftExpression_rightExpression_customSelectorSelector
  , initWithLeftExpression_rightExpression_modifier_type_optionsSelector
  , initWithLeftExpression_rightExpression_customSelectorSelector
  , initWithCoderSelector
  , predicateOperatorTypeSelector
  , comparisonPredicateModifierSelector
  , leftExpressionSelector
  , rightExpressionSelector
  , customSelectorSelector
  , optionsSelector

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ predicateWithLeftExpression:rightExpression:modifier:type:options:@
predicateWithLeftExpression_rightExpression_modifier_type_options :: (IsNSExpression lhs, IsNSExpression rhs) => lhs -> rhs -> NSComparisonPredicateModifier -> NSPredicateOperatorType -> NSComparisonPredicateOptions -> IO (Id NSComparisonPredicate)
predicateWithLeftExpression_rightExpression_modifier_type_options lhs rhs modifier type_ options =
  do
    cls' <- getRequiredClass "NSComparisonPredicate"
    withObjCPtr lhs $ \raw_lhs ->
      withObjCPtr rhs $ \raw_rhs ->
        sendClassMsg cls' (mkSelector "predicateWithLeftExpression:rightExpression:modifier:type:options:") (retPtr retVoid) [argPtr (castPtr raw_lhs :: Ptr ()), argPtr (castPtr raw_rhs :: Ptr ()), argCULong (coerce modifier), argCULong (coerce type_), argCULong (coerce options)] >>= retainedObject . castPtr

-- | @+ predicateWithLeftExpression:rightExpression:customSelector:@
predicateWithLeftExpression_rightExpression_customSelector :: (IsNSExpression lhs, IsNSExpression rhs) => lhs -> rhs -> Selector -> IO (Id NSComparisonPredicate)
predicateWithLeftExpression_rightExpression_customSelector lhs rhs selector =
  do
    cls' <- getRequiredClass "NSComparisonPredicate"
    withObjCPtr lhs $ \raw_lhs ->
      withObjCPtr rhs $ \raw_rhs ->
        sendClassMsg cls' (mkSelector "predicateWithLeftExpression:rightExpression:customSelector:") (retPtr retVoid) [argPtr (castPtr raw_lhs :: Ptr ()), argPtr (castPtr raw_rhs :: Ptr ()), argPtr (unSelector selector)] >>= retainedObject . castPtr

-- | @- initWithLeftExpression:rightExpression:modifier:type:options:@
initWithLeftExpression_rightExpression_modifier_type_options :: (IsNSComparisonPredicate nsComparisonPredicate, IsNSExpression lhs, IsNSExpression rhs) => nsComparisonPredicate -> lhs -> rhs -> NSComparisonPredicateModifier -> NSPredicateOperatorType -> NSComparisonPredicateOptions -> IO (Id NSComparisonPredicate)
initWithLeftExpression_rightExpression_modifier_type_options nsComparisonPredicate  lhs rhs modifier type_ options =
withObjCPtr lhs $ \raw_lhs ->
  withObjCPtr rhs $ \raw_rhs ->
      sendMsg nsComparisonPredicate (mkSelector "initWithLeftExpression:rightExpression:modifier:type:options:") (retPtr retVoid) [argPtr (castPtr raw_lhs :: Ptr ()), argPtr (castPtr raw_rhs :: Ptr ()), argCULong (coerce modifier), argCULong (coerce type_), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- initWithLeftExpression:rightExpression:customSelector:@
initWithLeftExpression_rightExpression_customSelector :: (IsNSComparisonPredicate nsComparisonPredicate, IsNSExpression lhs, IsNSExpression rhs) => nsComparisonPredicate -> lhs -> rhs -> Selector -> IO (Id NSComparisonPredicate)
initWithLeftExpression_rightExpression_customSelector nsComparisonPredicate  lhs rhs selector =
withObjCPtr lhs $ \raw_lhs ->
  withObjCPtr rhs $ \raw_rhs ->
      sendMsg nsComparisonPredicate (mkSelector "initWithLeftExpression:rightExpression:customSelector:") (retPtr retVoid) [argPtr (castPtr raw_lhs :: Ptr ()), argPtr (castPtr raw_rhs :: Ptr ()), argPtr (unSelector selector)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSComparisonPredicate nsComparisonPredicate, IsNSCoder coder) => nsComparisonPredicate -> coder -> IO (Id NSComparisonPredicate)
initWithCoder nsComparisonPredicate  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsComparisonPredicate (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- predicateOperatorType@
predicateOperatorType :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO NSPredicateOperatorType
predicateOperatorType nsComparisonPredicate  =
  fmap (coerce :: CULong -> NSPredicateOperatorType) $ sendMsg nsComparisonPredicate (mkSelector "predicateOperatorType") retCULong []

-- | @- comparisonPredicateModifier@
comparisonPredicateModifier :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO NSComparisonPredicateModifier
comparisonPredicateModifier nsComparisonPredicate  =
  fmap (coerce :: CULong -> NSComparisonPredicateModifier) $ sendMsg nsComparisonPredicate (mkSelector "comparisonPredicateModifier") retCULong []

-- | @- leftExpression@
leftExpression :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO (Id NSExpression)
leftExpression nsComparisonPredicate  =
  sendMsg nsComparisonPredicate (mkSelector "leftExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightExpression@
rightExpression :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO (Id NSExpression)
rightExpression nsComparisonPredicate  =
  sendMsg nsComparisonPredicate (mkSelector "rightExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- customSelector@
customSelector :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO Selector
customSelector nsComparisonPredicate  =
  fmap (Selector . castPtr) $ sendMsg nsComparisonPredicate (mkSelector "customSelector") (retPtr retVoid) []

-- | @- options@
options :: IsNSComparisonPredicate nsComparisonPredicate => nsComparisonPredicate -> IO NSComparisonPredicateOptions
options nsComparisonPredicate  =
  fmap (coerce :: CULong -> NSComparisonPredicateOptions) $ sendMsg nsComparisonPredicate (mkSelector "options") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateWithLeftExpression:rightExpression:modifier:type:options:@
predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector :: Selector
predicateWithLeftExpression_rightExpression_modifier_type_optionsSelector = mkSelector "predicateWithLeftExpression:rightExpression:modifier:type:options:"

-- | @Selector@ for @predicateWithLeftExpression:rightExpression:customSelector:@
predicateWithLeftExpression_rightExpression_customSelectorSelector :: Selector
predicateWithLeftExpression_rightExpression_customSelectorSelector = mkSelector "predicateWithLeftExpression:rightExpression:customSelector:"

-- | @Selector@ for @initWithLeftExpression:rightExpression:modifier:type:options:@
initWithLeftExpression_rightExpression_modifier_type_optionsSelector :: Selector
initWithLeftExpression_rightExpression_modifier_type_optionsSelector = mkSelector "initWithLeftExpression:rightExpression:modifier:type:options:"

-- | @Selector@ for @initWithLeftExpression:rightExpression:customSelector:@
initWithLeftExpression_rightExpression_customSelectorSelector :: Selector
initWithLeftExpression_rightExpression_customSelectorSelector = mkSelector "initWithLeftExpression:rightExpression:customSelector:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @predicateOperatorType@
predicateOperatorTypeSelector :: Selector
predicateOperatorTypeSelector = mkSelector "predicateOperatorType"

-- | @Selector@ for @comparisonPredicateModifier@
comparisonPredicateModifierSelector :: Selector
comparisonPredicateModifierSelector = mkSelector "comparisonPredicateModifier"

-- | @Selector@ for @leftExpression@
leftExpressionSelector :: Selector
leftExpressionSelector = mkSelector "leftExpression"

-- | @Selector@ for @rightExpression@
rightExpressionSelector :: Selector
rightExpressionSelector = mkSelector "rightExpression"

-- | @Selector@ for @customSelector@
customSelectorSelector :: Selector
customSelectorSelector = mkSelector "customSelector"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

