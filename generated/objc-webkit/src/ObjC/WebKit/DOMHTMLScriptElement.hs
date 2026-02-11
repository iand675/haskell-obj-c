{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLScriptElement@.
module ObjC.WebKit.DOMHTMLScriptElement
  ( DOMHTMLScriptElement
  , IsDOMHTMLScriptElement(..)
  , text
  , setText
  , htmlFor
  , setHtmlFor
  , event
  , setEvent
  , charset
  , setCharset
  , defer
  , setDefer
  , src
  , setSrc
  , type_
  , setType
  , textSelector
  , setTextSelector
  , htmlForSelector
  , setHtmlForSelector
  , eventSelector
  , setEventSelector
  , charsetSelector
  , setCharsetSelector
  , deferSelector
  , setDeferSelector
  , srcSelector
  , setSrcSelector
  , typeSelector
  , setTypeSelector


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

-- | @- text@
text :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
text domhtmlScriptElement  =
  sendMsg domhtmlScriptElement (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setText:@
setText :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setText domhtmlScriptElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlScriptElement (mkSelector "setText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- htmlFor@
htmlFor :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
htmlFor domhtmlScriptElement  =
  sendMsg domhtmlScriptElement (mkSelector "htmlFor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHtmlFor:@
setHtmlFor :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setHtmlFor domhtmlScriptElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlScriptElement (mkSelector "setHtmlFor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- event@
event :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
event domhtmlScriptElement  =
  sendMsg domhtmlScriptElement (mkSelector "event") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEvent:@
setEvent :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setEvent domhtmlScriptElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlScriptElement (mkSelector "setEvent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- charset@
charset :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
charset domhtmlScriptElement  =
  sendMsg domhtmlScriptElement (mkSelector "charset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharset:@
setCharset :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setCharset domhtmlScriptElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlScriptElement (mkSelector "setCharset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defer@
defer :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO Bool
defer domhtmlScriptElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlScriptElement (mkSelector "defer") retCULong []

-- | @- setDefer:@
setDefer :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> Bool -> IO ()
setDefer domhtmlScriptElement  value =
  sendMsg domhtmlScriptElement (mkSelector "setDefer:") retVoid [argCULong (if value then 1 else 0)]

-- | @- src@
src :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
src domhtmlScriptElement  =
  sendMsg domhtmlScriptElement (mkSelector "src") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSrc:@
setSrc :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setSrc domhtmlScriptElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlScriptElement (mkSelector "setSrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
type_ domhtmlScriptElement  =
  sendMsg domhtmlScriptElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setType domhtmlScriptElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlScriptElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @htmlFor@
htmlForSelector :: Selector
htmlForSelector = mkSelector "htmlFor"

-- | @Selector@ for @setHtmlFor:@
setHtmlForSelector :: Selector
setHtmlForSelector = mkSelector "setHtmlFor:"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

-- | @Selector@ for @setEvent:@
setEventSelector :: Selector
setEventSelector = mkSelector "setEvent:"

-- | @Selector@ for @charset@
charsetSelector :: Selector
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @defer@
deferSelector :: Selector
deferSelector = mkSelector "defer"

-- | @Selector@ for @setDefer:@
setDeferSelector :: Selector
setDeferSelector = mkSelector "setDefer:"

-- | @Selector@ for @src@
srcSelector :: Selector
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

