{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLFormElement@.
module ObjC.WebKit.DOMHTMLFormElement
  ( DOMHTMLFormElement
  , IsDOMHTMLFormElement(..)
  , submit
  , reset
  , acceptCharset
  , setAcceptCharset
  , action
  , setAction
  , enctype
  , setEnctype
  , encoding
  , setEncoding
  , method
  , setMethod
  , name
  , setName
  , target
  , setTarget
  , elements
  , length_
  , acceptCharsetSelector
  , actionSelector
  , elementsSelector
  , encodingSelector
  , enctypeSelector
  , lengthSelector
  , methodSelector
  , nameSelector
  , resetSelector
  , setAcceptCharsetSelector
  , setActionSelector
  , setEncodingSelector
  , setEnctypeSelector
  , setMethodSelector
  , setNameSelector
  , setTargetSelector
  , submitSelector
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

-- | @- submit@
submit :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO ()
submit domhtmlFormElement =
  sendMessage domhtmlFormElement submitSelector

-- | @- reset@
reset :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO ()
reset domhtmlFormElement =
  sendMessage domhtmlFormElement resetSelector

-- | @- acceptCharset@
acceptCharset :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
acceptCharset domhtmlFormElement =
  sendMessage domhtmlFormElement acceptCharsetSelector

-- | @- setAcceptCharset:@
setAcceptCharset :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setAcceptCharset domhtmlFormElement value =
  sendMessage domhtmlFormElement setAcceptCharsetSelector (toNSString value)

-- | @- action@
action :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
action domhtmlFormElement =
  sendMessage domhtmlFormElement actionSelector

-- | @- setAction:@
setAction :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setAction domhtmlFormElement value =
  sendMessage domhtmlFormElement setActionSelector (toNSString value)

-- | @- enctype@
enctype :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
enctype domhtmlFormElement =
  sendMessage domhtmlFormElement enctypeSelector

-- | @- setEnctype:@
setEnctype :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setEnctype domhtmlFormElement value =
  sendMessage domhtmlFormElement setEnctypeSelector (toNSString value)

-- | @- encoding@
encoding :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
encoding domhtmlFormElement =
  sendMessage domhtmlFormElement encodingSelector

-- | @- setEncoding:@
setEncoding :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setEncoding domhtmlFormElement value =
  sendMessage domhtmlFormElement setEncodingSelector (toNSString value)

-- | @- method@
method :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
method domhtmlFormElement =
  sendMessage domhtmlFormElement methodSelector

-- | @- setMethod:@
setMethod :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setMethod domhtmlFormElement value =
  sendMessage domhtmlFormElement setMethodSelector (toNSString value)

-- | @- name@
name :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
name domhtmlFormElement =
  sendMessage domhtmlFormElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setName domhtmlFormElement value =
  sendMessage domhtmlFormElement setNameSelector (toNSString value)

-- | @- target@
target :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
target domhtmlFormElement =
  sendMessage domhtmlFormElement targetSelector

-- | @- setTarget:@
setTarget :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setTarget domhtmlFormElement value =
  sendMessage domhtmlFormElement setTargetSelector (toNSString value)

-- | @- elements@
elements :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id DOMHTMLCollection)
elements domhtmlFormElement =
  sendMessage domhtmlFormElement elementsSelector

-- | @- length@
length_ :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO CInt
length_ domhtmlFormElement =
  sendMessage domhtmlFormElement lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @submit@
submitSelector :: Selector '[] ()
submitSelector = mkSelector "submit"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @acceptCharset@
acceptCharsetSelector :: Selector '[] (Id NSString)
acceptCharsetSelector = mkSelector "acceptCharset"

-- | @Selector@ for @setAcceptCharset:@
setAcceptCharsetSelector :: Selector '[Id NSString] ()
setAcceptCharsetSelector = mkSelector "setAcceptCharset:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] (Id NSString)
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Id NSString] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @enctype@
enctypeSelector :: Selector '[] (Id NSString)
enctypeSelector = mkSelector "enctype"

-- | @Selector@ for @setEnctype:@
setEnctypeSelector :: Selector '[Id NSString] ()
setEnctypeSelector = mkSelector "setEnctype:"

-- | @Selector@ for @encoding@
encodingSelector :: Selector '[] (Id NSString)
encodingSelector = mkSelector "encoding"

-- | @Selector@ for @setEncoding:@
setEncodingSelector :: Selector '[Id NSString] ()
setEncodingSelector = mkSelector "setEncoding:"

-- | @Selector@ for @method@
methodSelector :: Selector '[] (Id NSString)
methodSelector = mkSelector "method"

-- | @Selector@ for @setMethod:@
setMethodSelector :: Selector '[Id NSString] ()
setMethodSelector = mkSelector "setMethod:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] (Id NSString)
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[Id NSString] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @elements@
elementsSelector :: Selector '[] (Id DOMHTMLCollection)
elementsSelector = mkSelector "elements"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CInt
lengthSelector = mkSelector "length"

