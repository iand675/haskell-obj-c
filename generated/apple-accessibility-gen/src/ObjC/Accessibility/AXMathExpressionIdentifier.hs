{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionIdentifier@.
module ObjC.Accessibility.AXMathExpressionIdentifier
  ( AXMathExpressionIdentifier
  , IsAXMathExpressionIdentifier(..)
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
initWithContent :: (IsAXMathExpressionIdentifier axMathExpressionIdentifier, IsNSString content) => axMathExpressionIdentifier -> content -> IO (Id AXMathExpressionIdentifier)
initWithContent axMathExpressionIdentifier content =
  sendOwnedMessage axMathExpressionIdentifier initWithContentSelector (toNSString content)

-- | @- content@
content :: IsAXMathExpressionIdentifier axMathExpressionIdentifier => axMathExpressionIdentifier -> IO (Id NSString)
content axMathExpressionIdentifier =
  sendMessage axMathExpressionIdentifier contentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector '[Id NSString] (Id AXMathExpressionIdentifier)
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

