{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionSubSuperscript@.
module ObjC.Accessibility.AXMathExpressionSubSuperscript
  ( AXMathExpressionSubSuperscript
  , IsAXMathExpressionSubSuperscript(..)
  , initWithBaseExpression_subscriptExpressions_superscriptExpressions
  , baseExpression
  , subscriptExpressions
  , superscriptExpressions
  , baseExpressionSelector
  , initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector
  , subscriptExpressionsSelector
  , superscriptExpressionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBaseExpression:subscriptExpressions:superscriptExpressions:@
initWithBaseExpression_subscriptExpressions_superscriptExpressions :: (IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript, IsNSArray baseExpression, IsNSArray subscriptExpressions, IsNSArray superscriptExpressions) => axMathExpressionSubSuperscript -> baseExpression -> subscriptExpressions -> superscriptExpressions -> IO (Id AXMathExpressionSubSuperscript)
initWithBaseExpression_subscriptExpressions_superscriptExpressions axMathExpressionSubSuperscript baseExpression subscriptExpressions superscriptExpressions =
  sendOwnedMessage axMathExpressionSubSuperscript initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector (toNSArray baseExpression) (toNSArray subscriptExpressions) (toNSArray superscriptExpressions)

-- | @- baseExpression@
baseExpression :: IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript => axMathExpressionSubSuperscript -> IO (Id AXMathExpression)
baseExpression axMathExpressionSubSuperscript =
  sendMessage axMathExpressionSubSuperscript baseExpressionSelector

-- | @- subscriptExpressions@
subscriptExpressions :: IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript => axMathExpressionSubSuperscript -> IO (Id NSArray)
subscriptExpressions axMathExpressionSubSuperscript =
  sendMessage axMathExpressionSubSuperscript subscriptExpressionsSelector

-- | @- superscriptExpressions@
superscriptExpressions :: IsAXMathExpressionSubSuperscript axMathExpressionSubSuperscript => axMathExpressionSubSuperscript -> IO (Id NSArray)
superscriptExpressions axMathExpressionSubSuperscript =
  sendMessage axMathExpressionSubSuperscript superscriptExpressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBaseExpression:subscriptExpressions:superscriptExpressions:@
initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray] (Id AXMathExpressionSubSuperscript)
initWithBaseExpression_subscriptExpressions_superscriptExpressionsSelector = mkSelector "initWithBaseExpression:subscriptExpressions:superscriptExpressions:"

-- | @Selector@ for @baseExpression@
baseExpressionSelector :: Selector '[] (Id AXMathExpression)
baseExpressionSelector = mkSelector "baseExpression"

-- | @Selector@ for @subscriptExpressions@
subscriptExpressionsSelector :: Selector '[] (Id NSArray)
subscriptExpressionsSelector = mkSelector "subscriptExpressions"

-- | @Selector@ for @superscriptExpressions@
superscriptExpressionsSelector :: Selector '[] (Id NSArray)
superscriptExpressionsSelector = mkSelector "superscriptExpressions"

