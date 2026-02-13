{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionOperator@.
module ObjC.Accessibility.AXMathExpressionOperator
  ( AXMathExpressionOperator
  , IsAXMathExpressionOperator(..)
  , initWithContent
  , content
  , contentSelector
  , initWithContentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithContent:@
initWithContent :: (IsAXMathExpressionOperator axMathExpressionOperator, IsNSString content) => axMathExpressionOperator -> content -> IO (Id AXMathExpressionOperator)
initWithContent axMathExpressionOperator content =
  sendOwnedMessage axMathExpressionOperator initWithContentSelector (toNSString content)

-- | @- content@
content :: IsAXMathExpressionOperator axMathExpressionOperator => axMathExpressionOperator -> IO (Id NSString)
content axMathExpressionOperator =
  sendMessage axMathExpressionOperator contentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector '[Id NSString] (Id AXMathExpressionOperator)
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

