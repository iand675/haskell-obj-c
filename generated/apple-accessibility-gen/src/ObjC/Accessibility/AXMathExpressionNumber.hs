{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionNumber@.
module ObjC.Accessibility.AXMathExpressionNumber
  ( AXMathExpressionNumber
  , IsAXMathExpressionNumber(..)
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
initWithContent :: (IsAXMathExpressionNumber axMathExpressionNumber, IsNSString content) => axMathExpressionNumber -> content -> IO (Id AXMathExpressionNumber)
initWithContent axMathExpressionNumber content =
  sendOwnedMessage axMathExpressionNumber initWithContentSelector (toNSString content)

-- | @- content@
content :: IsAXMathExpressionNumber axMathExpressionNumber => axMathExpressionNumber -> IO (Id NSString)
content axMathExpressionNumber =
  sendMessage axMathExpressionNumber contentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector '[Id NSString] (Id AXMathExpressionNumber)
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

