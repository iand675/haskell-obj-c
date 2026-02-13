{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionFraction@.
module ObjC.Accessibility.AXMathExpressionFraction
  ( AXMathExpressionFraction
  , IsAXMathExpressionFraction(..)
  , initWithNumeratorExpression_denimonatorExpression
  , numeratorExpression
  , denimonatorExpression
  , denimonatorExpressionSelector
  , initWithNumeratorExpression_denimonatorExpressionSelector
  , numeratorExpressionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithNumeratorExpression:denimonatorExpression:@
initWithNumeratorExpression_denimonatorExpression :: (IsAXMathExpressionFraction axMathExpressionFraction, IsAXMathExpression numeratorExpression, IsAXMathExpression denimonatorExpression) => axMathExpressionFraction -> numeratorExpression -> denimonatorExpression -> IO (Id AXMathExpressionFraction)
initWithNumeratorExpression_denimonatorExpression axMathExpressionFraction numeratorExpression denimonatorExpression =
  sendOwnedMessage axMathExpressionFraction initWithNumeratorExpression_denimonatorExpressionSelector (toAXMathExpression numeratorExpression) (toAXMathExpression denimonatorExpression)

-- | @- numeratorExpression@
numeratorExpression :: IsAXMathExpressionFraction axMathExpressionFraction => axMathExpressionFraction -> IO (Id AXMathExpression)
numeratorExpression axMathExpressionFraction =
  sendMessage axMathExpressionFraction numeratorExpressionSelector

-- | @- denimonatorExpression@
denimonatorExpression :: IsAXMathExpressionFraction axMathExpressionFraction => axMathExpressionFraction -> IO (Id AXMathExpression)
denimonatorExpression axMathExpressionFraction =
  sendMessage axMathExpressionFraction denimonatorExpressionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNumeratorExpression:denimonatorExpression:@
initWithNumeratorExpression_denimonatorExpressionSelector :: Selector '[Id AXMathExpression, Id AXMathExpression] (Id AXMathExpressionFraction)
initWithNumeratorExpression_denimonatorExpressionSelector = mkSelector "initWithNumeratorExpression:denimonatorExpression:"

-- | @Selector@ for @numeratorExpression@
numeratorExpressionSelector :: Selector '[] (Id AXMathExpression)
numeratorExpressionSelector = mkSelector "numeratorExpression"

-- | @Selector@ for @denimonatorExpression@
denimonatorExpressionSelector :: Selector '[] (Id AXMathExpression)
denimonatorExpressionSelector = mkSelector "denimonatorExpression"

