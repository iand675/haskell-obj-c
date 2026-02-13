{-# LANGUAGE DataKinds #-}
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
  , charsetSelector
  , deferSelector
  , eventSelector
  , htmlForSelector
  , setCharsetSelector
  , setDeferSelector
  , setEventSelector
  , setHtmlForSelector
  , setSrcSelector
  , setTextSelector
  , setTypeSelector
  , srcSelector
  , textSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- text@
text :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
text domhtmlScriptElement =
  sendMessage domhtmlScriptElement textSelector

-- | @- setText:@
setText :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setText domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setTextSelector (toNSString value)

-- | @- htmlFor@
htmlFor :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
htmlFor domhtmlScriptElement =
  sendMessage domhtmlScriptElement htmlForSelector

-- | @- setHtmlFor:@
setHtmlFor :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setHtmlFor domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setHtmlForSelector (toNSString value)

-- | @- event@
event :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
event domhtmlScriptElement =
  sendMessage domhtmlScriptElement eventSelector

-- | @- setEvent:@
setEvent :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setEvent domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setEventSelector (toNSString value)

-- | @- charset@
charset :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
charset domhtmlScriptElement =
  sendMessage domhtmlScriptElement charsetSelector

-- | @- setCharset:@
setCharset :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setCharset domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setCharsetSelector (toNSString value)

-- | @- defer@
defer :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO Bool
defer domhtmlScriptElement =
  sendMessage domhtmlScriptElement deferSelector

-- | @- setDefer:@
setDefer :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> Bool -> IO ()
setDefer domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setDeferSelector value

-- | @- src@
src :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
src domhtmlScriptElement =
  sendMessage domhtmlScriptElement srcSelector

-- | @- setSrc:@
setSrc :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setSrc domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setSrcSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLScriptElement domhtmlScriptElement => domhtmlScriptElement -> IO (Id NSString)
type_ domhtmlScriptElement =
  sendMessage domhtmlScriptElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLScriptElement domhtmlScriptElement, IsNSString value) => domhtmlScriptElement -> value -> IO ()
setType domhtmlScriptElement value =
  sendMessage domhtmlScriptElement setTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector '[Id NSString] ()
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @htmlFor@
htmlForSelector :: Selector '[] (Id NSString)
htmlForSelector = mkSelector "htmlFor"

-- | @Selector@ for @setHtmlFor:@
setHtmlForSelector :: Selector '[Id NSString] ()
setHtmlForSelector = mkSelector "setHtmlFor:"

-- | @Selector@ for @event@
eventSelector :: Selector '[] (Id NSString)
eventSelector = mkSelector "event"

-- | @Selector@ for @setEvent:@
setEventSelector :: Selector '[Id NSString] ()
setEventSelector = mkSelector "setEvent:"

-- | @Selector@ for @charset@
charsetSelector :: Selector '[] (Id NSString)
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector '[Id NSString] ()
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @defer@
deferSelector :: Selector '[] Bool
deferSelector = mkSelector "defer"

-- | @Selector@ for @setDefer:@
setDeferSelector :: Selector '[Bool] ()
setDeferSelector = mkSelector "setDefer:"

-- | @Selector@ for @src@
srcSelector :: Selector '[] (Id NSString)
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector '[Id NSString] ()
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

