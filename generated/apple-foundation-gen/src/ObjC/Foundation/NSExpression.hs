{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExpression@.
module ObjC.Foundation.NSExpression
  ( NSExpression
  , IsNSExpression(..)
  , expressionWithFormat_argumentArray
  , expressionWithFormat
  , expressionWithFormat_arguments
  , expressionForConstantValue
  , expressionForEvaluatedObject
  , expressionForVariable
  , expressionForKeyPath
  , expressionForFunction_arguments
  , expressionForAggregate
  , expressionForUnionSet_with
  , expressionForIntersectSet_with
  , expressionForMinusSet_with
  , expressionForSubquery_usingIteratorVariable_predicate
  , expressionForFunction_selectorName_arguments
  , expressionForAnyKey
  , expressionForConditional_trueExpression_falseExpression
  , initWithExpressionType
  , initWithCoder
  , expressionValueWithObject_context
  , allowEvaluation
  , expressionType
  , constantValue
  , keyPath
  , function
  , variable
  , operand
  , arguments
  , collection
  , predicate
  , leftExpression
  , rightExpression
  , trueExpression
  , falseExpression
  , allowEvaluationSelector
  , argumentsSelector
  , collectionSelector
  , constantValueSelector
  , expressionForAggregateSelector
  , expressionForAnyKeySelector
  , expressionForConditional_trueExpression_falseExpressionSelector
  , expressionForConstantValueSelector
  , expressionForEvaluatedObjectSelector
  , expressionForFunction_argumentsSelector
  , expressionForFunction_selectorName_argumentsSelector
  , expressionForIntersectSet_withSelector
  , expressionForKeyPathSelector
  , expressionForMinusSet_withSelector
  , expressionForSubquery_usingIteratorVariable_predicateSelector
  , expressionForUnionSet_withSelector
  , expressionForVariableSelector
  , expressionTypeSelector
  , expressionValueWithObject_contextSelector
  , expressionWithFormatSelector
  , expressionWithFormat_argumentArraySelector
  , expressionWithFormat_argumentsSelector
  , falseExpressionSelector
  , functionSelector
  , initWithCoderSelector
  , initWithExpressionTypeSelector
  , keyPathSelector
  , leftExpressionSelector
  , operandSelector
  , predicateSelector
  , rightExpressionSelector
  , trueExpressionSelector
  , variableSelector

  -- * Enum types
  , NSExpressionType(NSExpressionType)
  , pattern NSConstantValueExpressionType
  , pattern NSEvaluatedObjectExpressionType
  , pattern NSVariableExpressionType
  , pattern NSKeyPathExpressionType
  , pattern NSFunctionExpressionType
  , pattern NSUnionSetExpressionType
  , pattern NSIntersectSetExpressionType
  , pattern NSMinusSetExpressionType
  , pattern NSSubqueryExpressionType
  , pattern NSAggregateExpressionType
  , pattern NSAnyKeyExpressionType
  , pattern NSBlockExpressionType
  , pattern NSConditionalExpressionType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ expressionWithFormat:argumentArray:@
expressionWithFormat_argumentArray :: (IsNSString expressionFormat, IsNSArray arguments) => expressionFormat -> arguments -> IO (Id NSExpression)
expressionWithFormat_argumentArray expressionFormat arguments =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionWithFormat_argumentArraySelector (toNSString expressionFormat) (toNSArray arguments)

-- | @+ expressionWithFormat:@
expressionWithFormat :: IsNSString expressionFormat => expressionFormat -> IO (Id NSExpression)
expressionWithFormat expressionFormat =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionWithFormatSelector (toNSString expressionFormat)

-- | @+ expressionWithFormat:arguments:@
expressionWithFormat_arguments :: IsNSString expressionFormat => expressionFormat -> RawId -> IO (Id NSExpression)
expressionWithFormat_arguments expressionFormat argList =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionWithFormat_argumentsSelector (toNSString expressionFormat) argList

-- | @+ expressionForConstantValue:@
expressionForConstantValue :: RawId -> IO (Id NSExpression)
expressionForConstantValue obj_ =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForConstantValueSelector obj_

-- | @+ expressionForEvaluatedObject@
expressionForEvaluatedObject :: IO (Id NSExpression)
expressionForEvaluatedObject  =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForEvaluatedObjectSelector

-- | @+ expressionForVariable:@
expressionForVariable :: IsNSString string => string -> IO (Id NSExpression)
expressionForVariable string =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForVariableSelector (toNSString string)

-- | @+ expressionForKeyPath:@
expressionForKeyPath :: IsNSString keyPath => keyPath -> IO (Id NSExpression)
expressionForKeyPath keyPath =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForKeyPathSelector (toNSString keyPath)

-- | @+ expressionForFunction:arguments:@
expressionForFunction_arguments :: (IsNSString name, IsNSArray parameters) => name -> parameters -> IO (Id NSExpression)
expressionForFunction_arguments name parameters =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForFunction_argumentsSelector (toNSString name) (toNSArray parameters)

-- | @+ expressionForAggregate:@
expressionForAggregate :: IsNSArray subexpressions => subexpressions -> IO (Id NSExpression)
expressionForAggregate subexpressions =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForAggregateSelector (toNSArray subexpressions)

-- | @+ expressionForUnionSet:with:@
expressionForUnionSet_with :: (IsNSExpression left, IsNSExpression right) => left -> right -> IO (Id NSExpression)
expressionForUnionSet_with left right =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForUnionSet_withSelector (toNSExpression left) (toNSExpression right)

-- | @+ expressionForIntersectSet:with:@
expressionForIntersectSet_with :: (IsNSExpression left, IsNSExpression right) => left -> right -> IO (Id NSExpression)
expressionForIntersectSet_with left right =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForIntersectSet_withSelector (toNSExpression left) (toNSExpression right)

-- | @+ expressionForMinusSet:with:@
expressionForMinusSet_with :: (IsNSExpression left, IsNSExpression right) => left -> right -> IO (Id NSExpression)
expressionForMinusSet_with left right =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForMinusSet_withSelector (toNSExpression left) (toNSExpression right)

-- | @+ expressionForSubquery:usingIteratorVariable:predicate:@
expressionForSubquery_usingIteratorVariable_predicate :: (IsNSExpression expression, IsNSString variable, IsNSPredicate predicate) => expression -> variable -> predicate -> IO (Id NSExpression)
expressionForSubquery_usingIteratorVariable_predicate expression variable predicate =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForSubquery_usingIteratorVariable_predicateSelector (toNSExpression expression) (toNSString variable) (toNSPredicate predicate)

-- | @+ expressionForFunction:selectorName:arguments:@
expressionForFunction_selectorName_arguments :: (IsNSExpression target, IsNSString name, IsNSArray parameters) => target -> name -> parameters -> IO (Id NSExpression)
expressionForFunction_selectorName_arguments target name parameters =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForFunction_selectorName_argumentsSelector (toNSExpression target) (toNSString name) (toNSArray parameters)

-- | @+ expressionForAnyKey@
expressionForAnyKey :: IO (Id NSExpression)
expressionForAnyKey  =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForAnyKeySelector

-- | @+ expressionForConditional:trueExpression:falseExpression:@
expressionForConditional_trueExpression_falseExpression :: (IsNSPredicate predicate, IsNSExpression trueExpression, IsNSExpression falseExpression) => predicate -> trueExpression -> falseExpression -> IO (Id NSExpression)
expressionForConditional_trueExpression_falseExpression predicate trueExpression falseExpression =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMessage cls' expressionForConditional_trueExpression_falseExpressionSelector (toNSPredicate predicate) (toNSExpression trueExpression) (toNSExpression falseExpression)

-- | @- initWithExpressionType:@
initWithExpressionType :: IsNSExpression nsExpression => nsExpression -> NSExpressionType -> IO (Id NSExpression)
initWithExpressionType nsExpression type_ =
  sendOwnedMessage nsExpression initWithExpressionTypeSelector type_

-- | @- initWithCoder:@
initWithCoder :: (IsNSExpression nsExpression, IsNSCoder coder) => nsExpression -> coder -> IO (Id NSExpression)
initWithCoder nsExpression coder =
  sendOwnedMessage nsExpression initWithCoderSelector (toNSCoder coder)

-- | @- expressionValueWithObject:context:@
expressionValueWithObject_context :: (IsNSExpression nsExpression, IsNSMutableDictionary context) => nsExpression -> RawId -> context -> IO RawId
expressionValueWithObject_context nsExpression object context =
  sendMessage nsExpression expressionValueWithObject_contextSelector object (toNSMutableDictionary context)

-- | @- allowEvaluation@
allowEvaluation :: IsNSExpression nsExpression => nsExpression -> IO ()
allowEvaluation nsExpression =
  sendMessage nsExpression allowEvaluationSelector

-- | @- expressionType@
expressionType :: IsNSExpression nsExpression => nsExpression -> IO NSExpressionType
expressionType nsExpression =
  sendMessage nsExpression expressionTypeSelector

-- | @- constantValue@
constantValue :: IsNSExpression nsExpression => nsExpression -> IO RawId
constantValue nsExpression =
  sendMessage nsExpression constantValueSelector

-- | @- keyPath@
keyPath :: IsNSExpression nsExpression => nsExpression -> IO (Id NSString)
keyPath nsExpression =
  sendMessage nsExpression keyPathSelector

-- | @- function@
function :: IsNSExpression nsExpression => nsExpression -> IO (Id NSString)
function nsExpression =
  sendMessage nsExpression functionSelector

-- | @- variable@
variable :: IsNSExpression nsExpression => nsExpression -> IO (Id NSString)
variable nsExpression =
  sendMessage nsExpression variableSelector

-- | @- operand@
operand :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
operand nsExpression =
  sendMessage nsExpression operandSelector

-- | @- arguments@
arguments :: IsNSExpression nsExpression => nsExpression -> IO (Id NSArray)
arguments nsExpression =
  sendMessage nsExpression argumentsSelector

-- | @- collection@
collection :: IsNSExpression nsExpression => nsExpression -> IO RawId
collection nsExpression =
  sendMessage nsExpression collectionSelector

-- | @- predicate@
predicate :: IsNSExpression nsExpression => nsExpression -> IO (Id NSPredicate)
predicate nsExpression =
  sendMessage nsExpression predicateSelector

-- | @- leftExpression@
leftExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
leftExpression nsExpression =
  sendMessage nsExpression leftExpressionSelector

-- | @- rightExpression@
rightExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
rightExpression nsExpression =
  sendMessage nsExpression rightExpressionSelector

-- | @- trueExpression@
trueExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
trueExpression nsExpression =
  sendMessage nsExpression trueExpressionSelector

-- | @- falseExpression@
falseExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
falseExpression nsExpression =
  sendMessage nsExpression falseExpressionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expressionWithFormat:argumentArray:@
expressionWithFormat_argumentArraySelector :: Selector '[Id NSString, Id NSArray] (Id NSExpression)
expressionWithFormat_argumentArraySelector = mkSelector "expressionWithFormat:argumentArray:"

-- | @Selector@ for @expressionWithFormat:@
expressionWithFormatSelector :: Selector '[Id NSString] (Id NSExpression)
expressionWithFormatSelector = mkSelector "expressionWithFormat:"

-- | @Selector@ for @expressionWithFormat:arguments:@
expressionWithFormat_argumentsSelector :: Selector '[Id NSString, RawId] (Id NSExpression)
expressionWithFormat_argumentsSelector = mkSelector "expressionWithFormat:arguments:"

-- | @Selector@ for @expressionForConstantValue:@
expressionForConstantValueSelector :: Selector '[RawId] (Id NSExpression)
expressionForConstantValueSelector = mkSelector "expressionForConstantValue:"

-- | @Selector@ for @expressionForEvaluatedObject@
expressionForEvaluatedObjectSelector :: Selector '[] (Id NSExpression)
expressionForEvaluatedObjectSelector = mkSelector "expressionForEvaluatedObject"

-- | @Selector@ for @expressionForVariable:@
expressionForVariableSelector :: Selector '[Id NSString] (Id NSExpression)
expressionForVariableSelector = mkSelector "expressionForVariable:"

-- | @Selector@ for @expressionForKeyPath:@
expressionForKeyPathSelector :: Selector '[Id NSString] (Id NSExpression)
expressionForKeyPathSelector = mkSelector "expressionForKeyPath:"

-- | @Selector@ for @expressionForFunction:arguments:@
expressionForFunction_argumentsSelector :: Selector '[Id NSString, Id NSArray] (Id NSExpression)
expressionForFunction_argumentsSelector = mkSelector "expressionForFunction:arguments:"

-- | @Selector@ for @expressionForAggregate:@
expressionForAggregateSelector :: Selector '[Id NSArray] (Id NSExpression)
expressionForAggregateSelector = mkSelector "expressionForAggregate:"

-- | @Selector@ for @expressionForUnionSet:with:@
expressionForUnionSet_withSelector :: Selector '[Id NSExpression, Id NSExpression] (Id NSExpression)
expressionForUnionSet_withSelector = mkSelector "expressionForUnionSet:with:"

-- | @Selector@ for @expressionForIntersectSet:with:@
expressionForIntersectSet_withSelector :: Selector '[Id NSExpression, Id NSExpression] (Id NSExpression)
expressionForIntersectSet_withSelector = mkSelector "expressionForIntersectSet:with:"

-- | @Selector@ for @expressionForMinusSet:with:@
expressionForMinusSet_withSelector :: Selector '[Id NSExpression, Id NSExpression] (Id NSExpression)
expressionForMinusSet_withSelector = mkSelector "expressionForMinusSet:with:"

-- | @Selector@ for @expressionForSubquery:usingIteratorVariable:predicate:@
expressionForSubquery_usingIteratorVariable_predicateSelector :: Selector '[Id NSExpression, Id NSString, Id NSPredicate] (Id NSExpression)
expressionForSubquery_usingIteratorVariable_predicateSelector = mkSelector "expressionForSubquery:usingIteratorVariable:predicate:"

-- | @Selector@ for @expressionForFunction:selectorName:arguments:@
expressionForFunction_selectorName_argumentsSelector :: Selector '[Id NSExpression, Id NSString, Id NSArray] (Id NSExpression)
expressionForFunction_selectorName_argumentsSelector = mkSelector "expressionForFunction:selectorName:arguments:"

-- | @Selector@ for @expressionForAnyKey@
expressionForAnyKeySelector :: Selector '[] (Id NSExpression)
expressionForAnyKeySelector = mkSelector "expressionForAnyKey"

-- | @Selector@ for @expressionForConditional:trueExpression:falseExpression:@
expressionForConditional_trueExpression_falseExpressionSelector :: Selector '[Id NSPredicate, Id NSExpression, Id NSExpression] (Id NSExpression)
expressionForConditional_trueExpression_falseExpressionSelector = mkSelector "expressionForConditional:trueExpression:falseExpression:"

-- | @Selector@ for @initWithExpressionType:@
initWithExpressionTypeSelector :: Selector '[NSExpressionType] (Id NSExpression)
initWithExpressionTypeSelector = mkSelector "initWithExpressionType:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSExpression)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @expressionValueWithObject:context:@
expressionValueWithObject_contextSelector :: Selector '[RawId, Id NSMutableDictionary] RawId
expressionValueWithObject_contextSelector = mkSelector "expressionValueWithObject:context:"

-- | @Selector@ for @allowEvaluation@
allowEvaluationSelector :: Selector '[] ()
allowEvaluationSelector = mkSelector "allowEvaluation"

-- | @Selector@ for @expressionType@
expressionTypeSelector :: Selector '[] NSExpressionType
expressionTypeSelector = mkSelector "expressionType"

-- | @Selector@ for @constantValue@
constantValueSelector :: Selector '[] RawId
constantValueSelector = mkSelector "constantValue"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector '[] (Id NSString)
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @function@
functionSelector :: Selector '[] (Id NSString)
functionSelector = mkSelector "function"

-- | @Selector@ for @variable@
variableSelector :: Selector '[] (Id NSString)
variableSelector = mkSelector "variable"

-- | @Selector@ for @operand@
operandSelector :: Selector '[] (Id NSExpression)
operandSelector = mkSelector "operand"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @collection@
collectionSelector :: Selector '[] RawId
collectionSelector = mkSelector "collection"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @leftExpression@
leftExpressionSelector :: Selector '[] (Id NSExpression)
leftExpressionSelector = mkSelector "leftExpression"

-- | @Selector@ for @rightExpression@
rightExpressionSelector :: Selector '[] (Id NSExpression)
rightExpressionSelector = mkSelector "rightExpression"

-- | @Selector@ for @trueExpression@
trueExpressionSelector :: Selector '[] (Id NSExpression)
trueExpressionSelector = mkSelector "trueExpression"

-- | @Selector@ for @falseExpression@
falseExpressionSelector :: Selector '[] (Id NSExpression)
falseExpressionSelector = mkSelector "falseExpression"

