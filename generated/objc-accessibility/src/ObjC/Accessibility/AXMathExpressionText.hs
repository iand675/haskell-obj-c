{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AXMathExpressionText@.
module ObjC.Accessibility.AXMathExpressionText
  ( AXMathExpressionText
  , IsAXMathExpressionText(..)
  , initWithContent
  , content
  , initWithContentSelector
  , contentSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithContent:@
initWithContent :: (IsAXMathExpressionText axMathExpressionText, IsNSString content) => axMathExpressionText -> content -> IO (Id AXMathExpressionText)
initWithContent axMathExpressionText  content =
withObjCPtr content $ \raw_content ->
    sendMsg axMathExpressionText (mkSelector "initWithContent:") (retPtr retVoid) [argPtr (castPtr raw_content :: Ptr ())] >>= ownedObject . castPtr

-- | @- content@
content :: IsAXMathExpressionText axMathExpressionText => axMathExpressionText -> IO (Id NSString)
content axMathExpressionText  =
  sendMsg axMathExpressionText (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContent:@
initWithContentSelector :: Selector
initWithContentSelector = mkSelector "initWithContent:"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

