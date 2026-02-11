{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLLabelElement@.
module ObjC.WebKit.DOMHTMLLabelElement
  ( DOMHTMLLabelElement
  , IsDOMHTMLLabelElement(..)
  , form
  , htmlFor
  , setHtmlFor
  , accessKey
  , setAccessKey
  , formSelector
  , htmlForSelector
  , setHtmlForSelector
  , accessKeySelector
  , setAccessKeySelector


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

-- | @- form@
form :: IsDOMHTMLLabelElement domhtmlLabelElement => domhtmlLabelElement -> IO (Id DOMHTMLFormElement)
form domhtmlLabelElement  =
  sendMsg domhtmlLabelElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- htmlFor@
htmlFor :: IsDOMHTMLLabelElement domhtmlLabelElement => domhtmlLabelElement -> IO (Id NSString)
htmlFor domhtmlLabelElement  =
  sendMsg domhtmlLabelElement (mkSelector "htmlFor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHtmlFor:@
setHtmlFor :: (IsDOMHTMLLabelElement domhtmlLabelElement, IsNSString value) => domhtmlLabelElement -> value -> IO ()
setHtmlFor domhtmlLabelElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLabelElement (mkSelector "setHtmlFor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessKey@
accessKey :: IsDOMHTMLLabelElement domhtmlLabelElement => domhtmlLabelElement -> IO (Id NSString)
accessKey domhtmlLabelElement  =
  sendMsg domhtmlLabelElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLLabelElement domhtmlLabelElement, IsNSString value) => domhtmlLabelElement -> value -> IO ()
setAccessKey domhtmlLabelElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLabelElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

-- | @Selector@ for @htmlFor@
htmlForSelector :: Selector
htmlForSelector = mkSelector "htmlFor"

-- | @Selector@ for @setHtmlFor:@
setHtmlForSelector :: Selector
setHtmlForSelector = mkSelector "setHtmlFor:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

