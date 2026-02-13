{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionTableRow@.
module ObjC.Accessibility.AXMathExpressionTableRow
  ( AXMathExpressionTableRow
  , IsAXMathExpressionTableRow(..)
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
initWithExpressions :: (IsAXMathExpressionTableRow axMathExpressionTableRow, IsNSArray expressions) => axMathExpressionTableRow -> expressions -> IO (Id AXMathExpressionTableRow)
initWithExpressions axMathExpressionTableRow expressions =
  sendOwnedMessage axMathExpressionTableRow initWithExpressionsSelector (toNSArray expressions)

-- | @- expressions@
expressions :: IsAXMathExpressionTableRow axMathExpressionTableRow => axMathExpressionTableRow -> IO (Id NSArray)
expressions axMathExpressionTableRow =
  sendMessage axMathExpressionTableRow expressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector '[Id NSArray] (Id AXMathExpressionTableRow)
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector '[] (Id NSArray)
expressionsSelector = mkSelector "expressions"

