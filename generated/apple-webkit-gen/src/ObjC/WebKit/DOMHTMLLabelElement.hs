{-# LANGUAGE DataKinds #-}
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
  , accessKeySelector
  , formSelector
  , htmlForSelector
  , setAccessKeySelector
  , setHtmlForSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- form@
form :: IsDOMHTMLLabelElement domhtmlLabelElement => domhtmlLabelElement -> IO (Id DOMHTMLFormElement)
form domhtmlLabelElement =
  sendMessage domhtmlLabelElement formSelector

-- | @- htmlFor@
htmlFor :: IsDOMHTMLLabelElement domhtmlLabelElement => domhtmlLabelElement -> IO (Id NSString)
htmlFor domhtmlLabelElement =
  sendMessage domhtmlLabelElement htmlForSelector

-- | @- setHtmlFor:@
setHtmlFor :: (IsDOMHTMLLabelElement domhtmlLabelElement, IsNSString value) => domhtmlLabelElement -> value -> IO ()
setHtmlFor domhtmlLabelElement value =
  sendMessage domhtmlLabelElement setHtmlForSelector (toNSString value)

-- | @- accessKey@
accessKey :: IsDOMHTMLLabelElement domhtmlLabelElement => domhtmlLabelElement -> IO (Id NSString)
accessKey domhtmlLabelElement =
  sendMessage domhtmlLabelElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLLabelElement domhtmlLabelElement, IsNSString value) => domhtmlLabelElement -> value -> IO ()
setAccessKey domhtmlLabelElement value =
  sendMessage domhtmlLabelElement setAccessKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector '[] (Id DOMHTMLFormElement)
formSelector = mkSelector "form"

-- | @Selector@ for @htmlFor@
htmlForSelector :: Selector '[] (Id NSString)
htmlForSelector = mkSelector "htmlFor"

-- | @Selector@ for @setHtmlFor:@
setHtmlForSelector :: Selector '[Id NSString] ()
setHtmlForSelector = mkSelector "setHtmlFor:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

