{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLLegendElement@.
module ObjC.WebKit.DOMHTMLLegendElement
  ( DOMHTMLLegendElement
  , IsDOMHTMLLegendElement(..)
  , form
  , align
  , setAlign
  , accessKey
  , setAccessKey
  , formSelector
  , alignSelector
  , setAlignSelector
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
form :: IsDOMHTMLLegendElement domhtmlLegendElement => domhtmlLegendElement -> IO (Id DOMHTMLFormElement)
form domhtmlLegendElement  =
  sendMsg domhtmlLegendElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- align@
align :: IsDOMHTMLLegendElement domhtmlLegendElement => domhtmlLegendElement -> IO (Id NSString)
align domhtmlLegendElement  =
  sendMsg domhtmlLegendElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLLegendElement domhtmlLegendElement, IsNSString value) => domhtmlLegendElement -> value -> IO ()
setAlign domhtmlLegendElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLegendElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessKey@
accessKey :: IsDOMHTMLLegendElement domhtmlLegendElement => domhtmlLegendElement -> IO (Id NSString)
accessKey domhtmlLegendElement  =
  sendMsg domhtmlLegendElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLLegendElement domhtmlLegendElement, IsNSString value) => domhtmlLegendElement -> value -> IO ()
setAccessKey domhtmlLegendElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlLegendElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

