{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLOptionElement@.
module ObjC.WebKit.DOMHTMLOptionElement
  ( DOMHTMLOptionElement
  , IsDOMHTMLOptionElement(..)
  , disabled
  , setDisabled
  , form
  , label
  , setLabel
  , defaultSelected
  , setDefaultSelected
  , selected
  , setSelected
  , value
  , setValue
  , text
  , index
  , disabledSelector
  , setDisabledSelector
  , formSelector
  , labelSelector
  , setLabelSelector
  , defaultSelectedSelector
  , setDefaultSelectedSelector
  , selectedSelector
  , setSelectedSelector
  , valueSelector
  , setValueSelector
  , textSelector
  , indexSelector


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

-- | @- disabled@
disabled :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO Bool
disabled domhtmlOptionElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlOptionElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> Bool -> IO ()
setDisabled domhtmlOptionElement  value =
  sendMsg domhtmlOptionElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- form@
form :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id DOMHTMLFormElement)
form domhtmlOptionElement  =
  sendMsg domhtmlOptionElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- label@
label :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id NSString)
label domhtmlOptionElement  =
  sendMsg domhtmlOptionElement (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsDOMHTMLOptionElement domhtmlOptionElement, IsNSString value) => domhtmlOptionElement -> value -> IO ()
setLabel domhtmlOptionElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlOptionElement (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultSelected@
defaultSelected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO Bool
defaultSelected domhtmlOptionElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlOptionElement (mkSelector "defaultSelected") retCULong []

-- | @- setDefaultSelected:@
setDefaultSelected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> Bool -> IO ()
setDefaultSelected domhtmlOptionElement  value =
  sendMsg domhtmlOptionElement (mkSelector "setDefaultSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- selected@
selected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO Bool
selected domhtmlOptionElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlOptionElement (mkSelector "selected") retCULong []

-- | @- setSelected:@
setSelected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> Bool -> IO ()
setSelected domhtmlOptionElement  value =
  sendMsg domhtmlOptionElement (mkSelector "setSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- value@
value :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id NSString)
value domhtmlOptionElement  =
  sendMsg domhtmlOptionElement (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMHTMLOptionElement domhtmlOptionElement, IsNSString value) => domhtmlOptionElement -> value -> IO ()
setValue domhtmlOptionElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlOptionElement (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- text@
text :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id NSString)
text domhtmlOptionElement  =
  sendMsg domhtmlOptionElement (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- index@
index :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO CInt
index domhtmlOptionElement  =
  sendMsg domhtmlOptionElement (mkSelector "index") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @defaultSelected@
defaultSelectedSelector :: Selector
defaultSelectedSelector = mkSelector "defaultSelected"

-- | @Selector@ for @setDefaultSelected:@
setDefaultSelectedSelector :: Selector
setDefaultSelectedSelector = mkSelector "setDefaultSelected:"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

