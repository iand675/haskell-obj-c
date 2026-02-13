{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHtmlElement@.
module ObjC.WebKit.DOMHTMLHtmlElement
  ( DOMHTMLHtmlElement
  , IsDOMHTMLHtmlElement(..)
  , version
  , setVersion
  , setVersionSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- version@
version :: IsDOMHTMLHtmlElement domhtmlHtmlElement => domhtmlHtmlElement -> IO (Id NSString)
version domhtmlHtmlElement =
  sendMessage domhtmlHtmlElement versionSelector

-- | @- setVersion:@
setVersion :: (IsDOMHTMLHtmlElement domhtmlHtmlElement, IsNSString value) => domhtmlHtmlElement -> value -> IO ()
setVersion domhtmlHtmlElement value =
  sendMessage domhtmlHtmlElement setVersionSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSString] ()
setVersionSelector = mkSelector "setVersion:"

