{-# LANGUAGE DataKinds #-}
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
  , setTargetSelector
  , targetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- href@
href :: IsDOMHTMLBaseElement domhtmlBaseElement => domhtmlBaseElement -> IO (Id NSString)
href domhtmlBaseElement =
  sendMessage domhtmlBaseElement hrefSelector

-- | @- setHref:@
setHref :: (IsDOMHTMLBaseElement domhtmlBaseElement, IsNSString value) => domhtmlBaseElement -> value -> IO ()
setHref domhtmlBaseElement value =
  sendMessage domhtmlBaseElement setHrefSelector (toNSString value)

-- | @- target@
target :: IsDOMHTMLBaseElement domhtmlBaseElement => domhtmlBaseElement -> IO (Id NSString)
target domhtmlBaseElement =
  sendMessage domhtmlBaseElement targetSelector

-- | @- setTarget:@
setTarget :: (IsDOMHTMLBaseElement domhtmlBaseElement, IsNSString value) => domhtmlBaseElement -> value -> IO ()
setTarget domhtmlBaseElement value =
  sendMessage domhtmlBaseElement setTargetSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @href@
hrefSelector :: Selector '[] (Id NSString)
hrefSelector = mkSelector "href"

-- | @Selector@ for @setHref:@
setHrefSelector :: Selector '[Id NSString] ()
setHrefSelector = mkSelector "setHref:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id NSString)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id NSString] ()
setTargetSelector = mkSelector "setTarget:"

