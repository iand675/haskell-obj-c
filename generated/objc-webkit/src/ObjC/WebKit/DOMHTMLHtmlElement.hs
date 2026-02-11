{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHtmlElement@.
module ObjC.WebKit.DOMHTMLHtmlElement
  ( DOMHTMLHtmlElement
  , IsDOMHTMLHtmlElement(..)
  , version
  , setVersion
  , versionSelector
  , setVersionSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- version@
version :: IsDOMHTMLHtmlElement domhtmlHtmlElement => domhtmlHtmlElement -> IO (Id NSString)
version domhtmlHtmlElement  =
  sendMsg domhtmlHtmlElement (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersion:@
setVersion :: (IsDOMHTMLHtmlElement domhtmlHtmlElement, IsNSString value) => domhtmlHtmlElement -> value -> IO ()
setVersion domhtmlHtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlHtmlElement (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

