{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionTable@.
module ObjC.Accessibility.AXMathExpressionTable
  ( AXMathExpressionTable
  , IsAXMathExpressionTable(..)
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
initWithExpressions :: (IsAXMathExpressionTable axMathExpressionTable, IsNSArray expressions) => axMathExpressionTable -> expressions -> IO (Id AXMathExpressionTable)
initWithExpressions axMathExpressionTable expressions =
  sendOwnedMessage axMathExpressionTable initWithExpressionsSelector (toNSArray expressions)

-- | @- expressions@
expressions :: IsAXMathExpressionTable axMathExpressionTable => axMathExpressionTable -> IO (Id NSArray)
expressions axMathExpressionTable =
  sendMessage axMathExpressionTable expressionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithExpressions:@
initWithExpressionsSelector :: Selector '[Id NSArray] (Id AXMathExpressionTable)
initWithExpressionsSelector = mkSelector "initWithExpressions:"

-- | @Selector@ for @expressions@
expressionsSelector :: Selector '[] (Id NSArray)
expressionsSelector = mkSelector "expressions"

