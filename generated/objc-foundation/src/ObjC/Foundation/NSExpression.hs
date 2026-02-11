{-# LANGUAGE PatternSynonyms #-}
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
  , expressionWithFormat_argumentArraySelector
  , expressionWithFormatSelector
  , expressionWithFormat_argumentsSelector
  , expressionForConstantValueSelector
  , expressionForEvaluatedObjectSelector
  , expressionForVariableSelector
  , expressionForKeyPathSelector
  , expressionForFunction_argumentsSelector
  , expressionForAggregateSelector
  , expressionForUnionSet_withSelector
  , expressionForIntersectSet_withSelector
  , expressionForMinusSet_withSelector
  , expressionForSubquery_usingIteratorVariable_predicateSelector
  , expressionForFunction_selectorName_argumentsSelector
  , expressionForAnyKeySelector
  , expressionForConditional_trueExpression_falseExpressionSelector
  , initWithExpressionTypeSelector
  , initWithCoderSelector
  , expressionValueWithObject_contextSelector
  , allowEvaluationSelector
  , expressionTypeSelector
  , constantValueSelector
  , keyPathSelector
  , functionSelector
  , variableSelector
  , operandSelector
  , argumentsSelector
  , collectionSelector
  , predicateSelector
  , leftExpressionSelector
  , rightExpressionSelector
  , trueExpressionSelector
  , falseExpressionSelector

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

-- | @+ expressionWithFormat:argumentArray:@
expressionWithFormat_argumentArray :: (IsNSString expressionFormat, IsNSArray arguments) => expressionFormat -> arguments -> IO (Id NSExpression)
expressionWithFormat_argumentArray expressionFormat arguments =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr expressionFormat $ \raw_expressionFormat ->
      withObjCPtr arguments $ \raw_arguments ->
        sendClassMsg cls' (mkSelector "expressionWithFormat:argumentArray:") (retPtr retVoid) [argPtr (castPtr raw_expressionFormat :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionWithFormat:@
expressionWithFormat :: IsNSString expressionFormat => expressionFormat -> IO (Id NSExpression)
expressionWithFormat expressionFormat =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr expressionFormat $ \raw_expressionFormat ->
      sendClassMsg cls' (mkSelector "expressionWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_expressionFormat :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionWithFormat:arguments:@
expressionWithFormat_arguments :: IsNSString expressionFormat => expressionFormat -> RawId -> IO (Id NSExpression)
expressionWithFormat_arguments expressionFormat argList =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr expressionFormat $ \raw_expressionFormat ->
      sendClassMsg cls' (mkSelector "expressionWithFormat:arguments:") (retPtr retVoid) [argPtr (castPtr raw_expressionFormat :: Ptr ()), argPtr (castPtr (unRawId argList) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForConstantValue:@
expressionForConstantValue :: RawId -> IO (Id NSExpression)
expressionForConstantValue obj_ =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMsg cls' (mkSelector "expressionForConstantValue:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForEvaluatedObject@
expressionForEvaluatedObject :: IO (Id NSExpression)
expressionForEvaluatedObject  =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMsg cls' (mkSelector "expressionForEvaluatedObject") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ expressionForVariable:@
expressionForVariable :: IsNSString string => string -> IO (Id NSExpression)
expressionForVariable string =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "expressionForVariable:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForKeyPath:@
expressionForKeyPath :: IsNSString keyPath => keyPath -> IO (Id NSExpression)
expressionForKeyPath keyPath =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr keyPath $ \raw_keyPath ->
      sendClassMsg cls' (mkSelector "expressionForKeyPath:") (retPtr retVoid) [argPtr (castPtr raw_keyPath :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForFunction:arguments:@
expressionForFunction_arguments :: (IsNSString name, IsNSArray parameters) => name -> parameters -> IO (Id NSExpression)
expressionForFunction_arguments name parameters =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr name $ \raw_name ->
      withObjCPtr parameters $ \raw_parameters ->
        sendClassMsg cls' (mkSelector "expressionForFunction:arguments:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_parameters :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForAggregate:@
expressionForAggregate :: IsNSArray subexpressions => subexpressions -> IO (Id NSExpression)
expressionForAggregate subexpressions =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr subexpressions $ \raw_subexpressions ->
      sendClassMsg cls' (mkSelector "expressionForAggregate:") (retPtr retVoid) [argPtr (castPtr raw_subexpressions :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForUnionSet:with:@
expressionForUnionSet_with :: (IsNSExpression left, IsNSExpression right) => left -> right -> IO (Id NSExpression)
expressionForUnionSet_with left right =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr left $ \raw_left ->
      withObjCPtr right $ \raw_right ->
        sendClassMsg cls' (mkSelector "expressionForUnionSet:with:") (retPtr retVoid) [argPtr (castPtr raw_left :: Ptr ()), argPtr (castPtr raw_right :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForIntersectSet:with:@
expressionForIntersectSet_with :: (IsNSExpression left, IsNSExpression right) => left -> right -> IO (Id NSExpression)
expressionForIntersectSet_with left right =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr left $ \raw_left ->
      withObjCPtr right $ \raw_right ->
        sendClassMsg cls' (mkSelector "expressionForIntersectSet:with:") (retPtr retVoid) [argPtr (castPtr raw_left :: Ptr ()), argPtr (castPtr raw_right :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForMinusSet:with:@
expressionForMinusSet_with :: (IsNSExpression left, IsNSExpression right) => left -> right -> IO (Id NSExpression)
expressionForMinusSet_with left right =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr left $ \raw_left ->
      withObjCPtr right $ \raw_right ->
        sendClassMsg cls' (mkSelector "expressionForMinusSet:with:") (retPtr retVoid) [argPtr (castPtr raw_left :: Ptr ()), argPtr (castPtr raw_right :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForSubquery:usingIteratorVariable:predicate:@
expressionForSubquery_usingIteratorVariable_predicate :: (IsNSExpression expression, IsNSString variable, IsNSPredicate predicate) => expression -> variable -> predicate -> IO (Id NSExpression)
expressionForSubquery_usingIteratorVariable_predicate expression variable predicate =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr expression $ \raw_expression ->
      withObjCPtr variable $ \raw_variable ->
        withObjCPtr predicate $ \raw_predicate ->
          sendClassMsg cls' (mkSelector "expressionForSubquery:usingIteratorVariable:predicate:") (retPtr retVoid) [argPtr (castPtr raw_expression :: Ptr ()), argPtr (castPtr raw_variable :: Ptr ()), argPtr (castPtr raw_predicate :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForFunction:selectorName:arguments:@
expressionForFunction_selectorName_arguments :: (IsNSExpression target, IsNSString name, IsNSArray parameters) => target -> name -> parameters -> IO (Id NSExpression)
expressionForFunction_selectorName_arguments target name parameters =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr target $ \raw_target ->
      withObjCPtr name $ \raw_name ->
        withObjCPtr parameters $ \raw_parameters ->
          sendClassMsg cls' (mkSelector "expressionForFunction:selectorName:arguments:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_parameters :: Ptr ())] >>= retainedObject . castPtr

-- | @+ expressionForAnyKey@
expressionForAnyKey :: IO (Id NSExpression)
expressionForAnyKey  =
  do
    cls' <- getRequiredClass "NSExpression"
    sendClassMsg cls' (mkSelector "expressionForAnyKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ expressionForConditional:trueExpression:falseExpression:@
expressionForConditional_trueExpression_falseExpression :: (IsNSPredicate predicate, IsNSExpression trueExpression, IsNSExpression falseExpression) => predicate -> trueExpression -> falseExpression -> IO (Id NSExpression)
expressionForConditional_trueExpression_falseExpression predicate trueExpression falseExpression =
  do
    cls' <- getRequiredClass "NSExpression"
    withObjCPtr predicate $ \raw_predicate ->
      withObjCPtr trueExpression $ \raw_trueExpression ->
        withObjCPtr falseExpression $ \raw_falseExpression ->
          sendClassMsg cls' (mkSelector "expressionForConditional:trueExpression:falseExpression:") (retPtr retVoid) [argPtr (castPtr raw_predicate :: Ptr ()), argPtr (castPtr raw_trueExpression :: Ptr ()), argPtr (castPtr raw_falseExpression :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithExpressionType:@
initWithExpressionType :: IsNSExpression nsExpression => nsExpression -> NSExpressionType -> IO (Id NSExpression)
initWithExpressionType nsExpression  type_ =
  sendMsg nsExpression (mkSelector "initWithExpressionType:") (retPtr retVoid) [argCULong (coerce type_)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSExpression nsExpression, IsNSCoder coder) => nsExpression -> coder -> IO (Id NSExpression)
initWithCoder nsExpression  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsExpression (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- expressionValueWithObject:context:@
expressionValueWithObject_context :: (IsNSExpression nsExpression, IsNSMutableDictionary context) => nsExpression -> RawId -> context -> IO RawId
expressionValueWithObject_context nsExpression  object context =
withObjCPtr context $ \raw_context ->
    fmap (RawId . castPtr) $ sendMsg nsExpression (mkSelector "expressionValueWithObject:context:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())]

-- | @- allowEvaluation@
allowEvaluation :: IsNSExpression nsExpression => nsExpression -> IO ()
allowEvaluation nsExpression  =
  sendMsg nsExpression (mkSelector "allowEvaluation") retVoid []

-- | @- expressionType@
expressionType :: IsNSExpression nsExpression => nsExpression -> IO NSExpressionType
expressionType nsExpression  =
  fmap (coerce :: CULong -> NSExpressionType) $ sendMsg nsExpression (mkSelector "expressionType") retCULong []

-- | @- constantValue@
constantValue :: IsNSExpression nsExpression => nsExpression -> IO RawId
constantValue nsExpression  =
  fmap (RawId . castPtr) $ sendMsg nsExpression (mkSelector "constantValue") (retPtr retVoid) []

-- | @- keyPath@
keyPath :: IsNSExpression nsExpression => nsExpression -> IO (Id NSString)
keyPath nsExpression  =
  sendMsg nsExpression (mkSelector "keyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- function@
function :: IsNSExpression nsExpression => nsExpression -> IO (Id NSString)
function nsExpression  =
  sendMsg nsExpression (mkSelector "function") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- variable@
variable :: IsNSExpression nsExpression => nsExpression -> IO (Id NSString)
variable nsExpression  =
  sendMsg nsExpression (mkSelector "variable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- operand@
operand :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
operand nsExpression  =
  sendMsg nsExpression (mkSelector "operand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arguments@
arguments :: IsNSExpression nsExpression => nsExpression -> IO (Id NSArray)
arguments nsExpression  =
  sendMsg nsExpression (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- collection@
collection :: IsNSExpression nsExpression => nsExpression -> IO RawId
collection nsExpression  =
  fmap (RawId . castPtr) $ sendMsg nsExpression (mkSelector "collection") (retPtr retVoid) []

-- | @- predicate@
predicate :: IsNSExpression nsExpression => nsExpression -> IO (Id NSPredicate)
predicate nsExpression  =
  sendMsg nsExpression (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- leftExpression@
leftExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
leftExpression nsExpression  =
  sendMsg nsExpression (mkSelector "leftExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rightExpression@
rightExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
rightExpression nsExpression  =
  sendMsg nsExpression (mkSelector "rightExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trueExpression@
trueExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
trueExpression nsExpression  =
  sendMsg nsExpression (mkSelector "trueExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- falseExpression@
falseExpression :: IsNSExpression nsExpression => nsExpression -> IO (Id NSExpression)
falseExpression nsExpression  =
  sendMsg nsExpression (mkSelector "falseExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @expressionWithFormat:argumentArray:@
expressionWithFormat_argumentArraySelector :: Selector
expressionWithFormat_argumentArraySelector = mkSelector "expressionWithFormat:argumentArray:"

-- | @Selector@ for @expressionWithFormat:@
expressionWithFormatSelector :: Selector
expressionWithFormatSelector = mkSelector "expressionWithFormat:"

-- | @Selector@ for @expressionWithFormat:arguments:@
expressionWithFormat_argumentsSelector :: Selector
expressionWithFormat_argumentsSelector = mkSelector "expressionWithFormat:arguments:"

-- | @Selector@ for @expressionForConstantValue:@
expressionForConstantValueSelector :: Selector
expressionForConstantValueSelector = mkSelector "expressionForConstantValue:"

-- | @Selector@ for @expressionForEvaluatedObject@
expressionForEvaluatedObjectSelector :: Selector
expressionForEvaluatedObjectSelector = mkSelector "expressionForEvaluatedObject"

-- | @Selector@ for @expressionForVariable:@
expressionForVariableSelector :: Selector
expressionForVariableSelector = mkSelector "expressionForVariable:"

-- | @Selector@ for @expressionForKeyPath:@
expressionForKeyPathSelector :: Selector
expressionForKeyPathSelector = mkSelector "expressionForKeyPath:"

-- | @Selector@ for @expressionForFunction:arguments:@
expressionForFunction_argumentsSelector :: Selector
expressionForFunction_argumentsSelector = mkSelector "expressionForFunction:arguments:"

-- | @Selector@ for @expressionForAggregate:@
expressionForAggregateSelector :: Selector
expressionForAggregateSelector = mkSelector "expressionForAggregate:"

-- | @Selector@ for @expressionForUnionSet:with:@
expressionForUnionSet_withSelector :: Selector
expressionForUnionSet_withSelector = mkSelector "expressionForUnionSet:with:"

-- | @Selector@ for @expressionForIntersectSet:with:@
expressionForIntersectSet_withSelector :: Selector
expressionForIntersectSet_withSelector = mkSelector "expressionForIntersectSet:with:"

-- | @Selector@ for @expressionForMinusSet:with:@
expressionForMinusSet_withSelector :: Selector
expressionForMinusSet_withSelector = mkSelector "expressionForMinusSet:with:"

-- | @Selector@ for @expressionForSubquery:usingIteratorVariable:predicate:@
expressionForSubquery_usingIteratorVariable_predicateSelector :: Selector
expressionForSubquery_usingIteratorVariable_predicateSelector = mkSelector "expressionForSubquery:usingIteratorVariable:predicate:"

-- | @Selector@ for @expressionForFunction:selectorName:arguments:@
expressionForFunction_selectorName_argumentsSelector :: Selector
expressionForFunction_selectorName_argumentsSelector = mkSelector "expressionForFunction:selectorName:arguments:"

-- | @Selector@ for @expressionForAnyKey@
expressionForAnyKeySelector :: Selector
expressionForAnyKeySelector = mkSelector "expressionForAnyKey"

-- | @Selector@ for @expressionForConditional:trueExpression:falseExpression:@
expressionForConditional_trueExpression_falseExpressionSelector :: Selector
expressionForConditional_trueExpression_falseExpressionSelector = mkSelector "expressionForConditional:trueExpression:falseExpression:"

-- | @Selector@ for @initWithExpressionType:@
initWithExpressionTypeSelector :: Selector
initWithExpressionTypeSelector = mkSelector "initWithExpressionType:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @expressionValueWithObject:context:@
expressionValueWithObject_contextSelector :: Selector
expressionValueWithObject_contextSelector = mkSelector "expressionValueWithObject:context:"

-- | @Selector@ for @allowEvaluation@
allowEvaluationSelector :: Selector
allowEvaluationSelector = mkSelector "allowEvaluation"

-- | @Selector@ for @expressionType@
expressionTypeSelector :: Selector
expressionTypeSelector = mkSelector "expressionType"

-- | @Selector@ for @constantValue@
constantValueSelector :: Selector
constantValueSelector = mkSelector "constantValue"

-- | @Selector@ for @keyPath@
keyPathSelector :: Selector
keyPathSelector = mkSelector "keyPath"

-- | @Selector@ for @function@
functionSelector :: Selector
functionSelector = mkSelector "function"

-- | @Selector@ for @variable@
variableSelector :: Selector
variableSelector = mkSelector "variable"

-- | @Selector@ for @operand@
operandSelector :: Selector
operandSelector = mkSelector "operand"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @collection@
collectionSelector :: Selector
collectionSelector = mkSelector "collection"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @leftExpression@
leftExpressionSelector :: Selector
leftExpressionSelector = mkSelector "leftExpression"

-- | @Selector@ for @rightExpression@
rightExpressionSelector :: Selector
rightExpressionSelector = mkSelector "rightExpression"

-- | @Selector@ for @trueExpression@
trueExpressionSelector :: Selector
trueExpressionSelector = mkSelector "trueExpression"

-- | @Selector@ for @falseExpression@
falseExpressionSelector :: Selector
falseExpressionSelector = mkSelector "falseExpression"

