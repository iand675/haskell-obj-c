{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionUnderOver@.
module ObjC.Accessibility.AXMathExpressionUnderOver
  ( AXMathExpressionUnderOver
  , IsAXMathExpressionUnderOver(..)
  , initWithBaseExpression_underExpression_overExpression
  , baseExpression
  , underExpression
  , overExpression
  , baseExpressionSelector
  , initWithBaseExpression_underExpression_overExpressionSelector
  , overExpressionSelector
  , underExpressionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBaseExpression:underExpression:overExpression:@
initWithBaseExpression_underExpression_overExpression :: (IsAXMathExpressionUnderOver axMathExpressionUnderOver, IsAXMathExpression baseExpression, IsAXMathExpression underExpression, IsAXMathExpression overExpression) => axMathExpressionUnderOver -> baseExpression -> underExpression -> overExpression -> IO (Id AXMathExpressionUnderOver)
initWithBaseExpression_underExpression_overExpression axMathExpressionUnderOver baseExpression underExpression overExpression =
  sendOwnedMessage axMathExpressionUnderOver initWithBaseExpression_underExpression_overExpressionSelector (toAXMathExpression baseExpression) (toAXMathExpression underExpression) (toAXMathExpression overExpression)

-- | @- baseExpression@
baseExpression :: IsAXMathExpressionUnderOver axMathExpressionUnderOver => axMathExpressionUnderOver -> IO (Id AXMathExpression)
baseExpression axMathExpressionUnderOver =
  sendMessage axMathExpressionUnderOver baseExpressionSelector

-- | @- underExpression@
underExpression :: IsAXMathExpressionUnderOver axMathExpressionUnderOver => axMathExpressionUnderOver -> IO (Id AXMathExpression)
underExpression axMathExpressionUnderOver =
  sendMessage axMathExpressionUnderOver underExpressionSelector

-- | @- overExpression@
overExpression :: IsAXMathExpressionUnderOver axMathExpressionUnderOver => axMathExpressionUnderOver -> IO (Id AXMathExpression)
overExpression axMathExpressionUnderOver =
  sendMessage axMathExpressionUnderOver overExpressionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBaseExpression:underExpression:overExpression:@
initWithBaseExpression_underExpression_overExpressionSelector :: Selector '[Id AXMathExpression, Id AXMathExpression, Id AXMathExpression] (Id AXMathExpressionUnderOver)
initWithBaseExpression_underExpression_overExpressionSelector = mkSelector "initWithBaseExpression:underExpression:overExpression:"

-- | @Selector@ for @baseExpression@
baseExpressionSelector :: Selector '[] (Id AXMathExpression)
baseExpressionSelector = mkSelector "baseExpression"

-- | @Selector@ for @underExpression@
underExpressionSelector :: Selector '[] (Id AXMathExpression)
underExpressionSelector = mkSelector "underExpression"

-- | @Selector@ for @overExpression@
overExpressionSelector :: Selector '[] (Id AXMathExpression)
overExpressionSelector = mkSelector "overExpression"

