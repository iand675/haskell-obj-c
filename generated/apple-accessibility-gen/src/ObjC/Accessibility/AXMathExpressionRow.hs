{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionRow@.
module ObjC.Accessibility.AXMathExpressionRow
  ( AXMathExpressionRow
  , IsAXMathExpressionRow(..)
  , initWithExpressions
  , expressions
  , expressionsSelector
  , initWithExpressionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithExpressions:@
initWithExpressions :: (IsAXMathExpressionRow axMathExpressionRow, IsNSArray expressions) => axMathExpressionRow -> expressions -> IO (Id AXMathExpressionRow)
initWithExpressions axMathExpressionRow expressions =
  sendOwnedMessage axMathExpressionRow initWithExpressionsSelector (toNSArray expressions)

-- | @- expressions@
expressions :: IsAXMathExpressionRow axMathExpressionRow => axMathExpressionRow -> IO (Id NSArray)
expressions axMathExpressionRow =
  sendMessage axMathExpressionRow expressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector '[Id NSArray] (Id AXMathExpressionRow)
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector '[] (Id NSArray)
expressionsSelector = mkSelector "expressions"

