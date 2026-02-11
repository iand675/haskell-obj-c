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
  , submitSelector
  , resetSelector
  , acceptCharsetSelector
  , setAcceptCharsetSelector
  , actionSelector
  , setActionSelector
  , enctypeSelector
  , setEnctypeSelector
  , encodingSelector
  , setEncodingSelector
  , methodSelector
  , setMethodSelector
  , nameSelector
  , setNameSelector
  , targetSelector
  , setTargetSelector
  , elementsSelector
  , lengthSelector


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

-- | @- submit@
submit :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO ()
submit domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "submit") retVoid []

-- | @- reset@
reset :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO ()
reset domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "reset") retVoid []

-- | @- acceptCharset@
acceptCharset :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
acceptCharset domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "acceptCharset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAcceptCharset:@
setAcceptCharset :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setAcceptCharset domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setAcceptCharset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- action@
action :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
action domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "action") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAction:@
setAction :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setAction domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setAction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- enctype@
enctype :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
enctype domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "enctype") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEnctype:@
setEnctype :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setEnctype domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setEnctype:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- encoding@
encoding :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
encoding domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "encoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEncoding:@
setEncoding :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setEncoding domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setEncoding:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- method@
method :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
method domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "method") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMethod:@
setMethod :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setMethod domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setMethod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
name domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setName domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- target@
target :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id NSString)
target domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "target") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTarget:@
setTarget :: (IsDOMHTMLFormElement domhtmlFormElement, IsNSString value) => domhtmlFormElement -> value -> IO ()
setTarget domhtmlFormElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlFormElement (mkSelector "setTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elements@
elements :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO (Id DOMHTMLCollection)
elements domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "elements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMHTMLFormElement domhtmlFormElement => domhtmlFormElement -> IO CInt
length_ domhtmlFormElement  =
  sendMsg domhtmlFormElement (mkSelector "length") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @submit@
submitSelector :: Selector
submitSelector = mkSelector "submit"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @acceptCharset@
acceptCharsetSelector :: Selector
acceptCharsetSelector = mkSelector "acceptCharset"

-- | @Selector@ for @setAcceptCharset:@
setAcceptCharsetSelector :: Selector
setAcceptCharsetSelector = mkSelector "setAcceptCharset:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @enctype@
enctypeSelector :: Selector
enctypeSelector = mkSelector "enctype"

-- | @Selector@ for @setEnctype:@
setEnctypeSelector :: Selector
setEnctypeSelector = mkSelector "setEnctype:"

-- | @Selector@ for @encoding@
encodingSelector :: Selector
encodingSelector = mkSelector "encoding"

-- | @Selector@ for @setEncoding:@
setEncodingSelector :: Selector
setEncodingSelector = mkSelector "setEncoding:"

-- | @Selector@ for @method@
methodSelector :: Selector
methodSelector = mkSelector "method"

-- | @Selector@ for @setMethod:@
setMethodSelector :: Selector
setMethodSelector = mkSelector "setMethod:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @elements@
elementsSelector :: Selector
elementsSelector = mkSelector "elements"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

