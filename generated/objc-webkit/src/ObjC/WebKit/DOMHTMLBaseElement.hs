{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLBaseElement@.
module ObjC.WebKit.DOMHTMLBaseElement
  ( DOMHTMLBaseElement
  , IsDOMHTMLBaseElement(..)
  , href
  , setHref
  , target
  , setTarget
  , hrefSelector
  , setHrefSelector
  , targetSelector
  , setTargetSelector


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

-- | @- href@
href :: IsDOMHTMLBaseElement domhtmlBaseElement => domhtmlBaseElement -> IO (Id NSString)
href domhtmlBaseElement  =
  sendMsg domhtmlBaseElement (mkSelector "href") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHref:@
setHref :: (IsDOMHTMLBaseElement domhtmlBaseElement, IsNSString value) => domhtmlBaseElement -> value -> IO ()
setHref domhtmlBaseElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBaseElement (mkSelector "setHref:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsDOMHTMLBaseElement domhtmlBaseElement => domhtmlBaseElement -> IO (Id NSString)
target domhtmlBaseElement  =
  sendMsg domhtmlBaseElement (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTarget:@
setTarget :: (IsDOMHTMLBaseElement domhtmlBaseElement, IsNSString value) => domhtmlBaseElement -> value -> IO ()
setTarget domhtmlBaseElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlBaseElement (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @href@
hrefSelector :: Selector
hrefSelector = mkSelector "href"

-- | @Selector@ for @setHref:@
setHrefSelector :: Selector
setHrefSelector = mkSelector "setHref:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

