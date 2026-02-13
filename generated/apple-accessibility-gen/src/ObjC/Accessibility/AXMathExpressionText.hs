{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionText@.
module ObjC.Accessibility.AXMathExpressionText
  ( AXMathExpressionText
  , IsAXMathExpressionText(..)
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
initWithContent :: (IsAXMathExpressionText axMathExpressionText, IsNSString content) => axMathExpressionText -> content -> IO (Id AXMathExpressionText)
initWithContent axMathExpressionText content =
  sendOwnedMessage axMathExpressionText initWithContentSelector (toNSString content)

-- | @- content@
content :: IsAXMathExpressionText axMathExpressionText => axMathExpressionText -> IO (Id NSString)
content axMathExpressionText =
  sendMessage axMathExpressionText contentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector '[Id NSString] (Id AXMathExpressionText)
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

