{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionTableCell@.
module ObjC.Accessibility.AXMathExpressionTableCell
  ( AXMathExpressionTableCell
  , IsAXMathExpressionTableCell(..)
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
initWithExpressions :: (IsAXMathExpressionTableCell axMathExpressionTableCell, IsNSArray expressions) => axMathExpressionTableCell -> expressions -> IO (Id AXMathExpressionTableCell)
initWithExpressions axMathExpressionTableCell expressions =
  sendOwnedMessage axMathExpressionTableCell initWithExpressionsSelector (toNSArray expressions)

-- | @- expressions@
expressions :: IsAXMathExpressionTableCell axMathExpressionTableCell => axMathExpressionTableCell -> IO (Id NSArray)
expressions axMathExpressionTableCell =
  sendMessage axMathExpressionTableCell expressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector '[Id NSArray] (Id AXMathExpressionTableCell)
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector '[] (Id NSArray)
expressionsSelector = mkSelector "expressions"

