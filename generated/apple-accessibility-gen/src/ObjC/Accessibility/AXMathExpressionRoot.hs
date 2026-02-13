{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionRoot@.
module ObjC.Accessibility.AXMathExpressionRoot
  ( AXMathExpressionRoot
  , IsAXMathExpressionRoot(..)
  , initWithRadicandExpressions_rootIndexExpression
  , radicandExpressions
  , rootIndexExpression
  , initWithRadicandExpressions_rootIndexExpressionSelector
  , radicandExpressionsSelector
  , rootIndexExpressionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRadicandExpressions:rootIndexExpression:@
initWithRadicandExpressions_rootIndexExpression :: (IsAXMathExpressionRoot axMathExpressionRoot, IsNSArray radicandExpressions, IsAXMathExpression rootIndexExpression) => axMathExpressionRoot -> radicandExpressions -> rootIndexExpression -> IO (Id AXMathExpressionRoot)
initWithRadicandExpressions_rootIndexExpression axMathExpressionRoot radicandExpressions rootIndexExpression =
  sendOwnedMessage axMathExpressionRoot initWithRadicandExpressions_rootIndexExpressionSelector (toNSArray radicandExpressions) (toAXMathExpression rootIndexExpression)

-- | @- radicandExpressions@
radicandExpressions :: IsAXMathExpressionRoot axMathExpressionRoot => axMathExpressionRoot -> IO (Id NSArray)
radicandExpressions axMathExpressionRoot =
  sendMessage axMathExpressionRoot radicandExpressionsSelector

-- | @- rootIndexExpression@
rootIndexExpression :: IsAXMathExpressionRoot axMathExpressionRoot => axMathExpressionRoot -> IO (Id AXMathExpression)
rootIndexExpression axMathExpressionRoot =
  sendMessage axMathExpressionRoot rootIndexExpressionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRadicandExpressions:rootIndexExpression:@
initWithRadicandExpressions_rootIndexExpressionSelector :: Selector '[Id NSArray, Id AXMathExpression] (Id AXMathExpressionRoot)
initWithRadicandExpressions_rootIndexExpressionSelector = mkSelector "initWithRadicandExpressions:rootIndexExpression:"

-- | @Selector@ for @radicandExpressions@
radicandExpressionsSelector :: Selector '[] (Id NSArray)
radicandExpressionsSelector = mkSelector "radicandExpressions"

-- | @Selector@ for @rootIndexExpression@
rootIndexExpressionSelector :: Selector '[] (Id AXMathExpression)
rootIndexExpressionSelector = mkSelector "rootIndexExpression"

