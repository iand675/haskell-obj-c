{-# LANGUAGE DataKinds #-}
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
  , defaultSelectedSelector
  , disabledSelector
  , formSelector
  , indexSelector
  , labelSelector
  , selectedSelector
  , setDefaultSelectedSelector
  , setDisabledSelector
  , setLabelSelector
  , setSelectedSelector
  , setValueSelector
  , textSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- disabled@
disabled :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO Bool
disabled domhtmlOptionElement =
  sendMessage domhtmlOptionElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> Bool -> IO ()
setDisabled domhtmlOptionElement value =
  sendMessage domhtmlOptionElement setDisabledSelector value

-- | @- form@
form :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id DOMHTMLFormElement)
form domhtmlOptionElement =
  sendMessage domhtmlOptionElement formSelector

-- | @- label@
label :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id NSString)
label domhtmlOptionElement =
  sendMessage domhtmlOptionElement labelSelector

-- | @- setLabel:@
setLabel :: (IsDOMHTMLOptionElement domhtmlOptionElement, IsNSString value) => domhtmlOptionElement -> value -> IO ()
setLabel domhtmlOptionElement value =
  sendMessage domhtmlOptionElement setLabelSelector (toNSString value)

-- | @- defaultSelected@
defaultSelected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO Bool
defaultSelected domhtmlOptionElement =
  sendMessage domhtmlOptionElement defaultSelectedSelector

-- | @- setDefaultSelected:@
setDefaultSelected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> Bool -> IO ()
setDefaultSelected domhtmlOptionElement value =
  sendMessage domhtmlOptionElement setDefaultSelectedSelector value

-- | @- selected@
selected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO Bool
selected domhtmlOptionElement =
  sendMessage domhtmlOptionElement selectedSelector

-- | @- setSelected:@
setSelected :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> Bool -> IO ()
setSelected domhtmlOptionElement value =
  sendMessage domhtmlOptionElement setSelectedSelector value

-- | @- value@
value :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id NSString)
value domhtmlOptionElement =
  sendMessage domhtmlOptionElement valueSelector

-- | @- setValue:@
setValue :: (IsDOMHTMLOptionElement domhtmlOptionElement, IsNSString value) => domhtmlOptionElement -> value -> IO ()
setValue domhtmlOptionElement value =
  sendMessage domhtmlOptionElement setValueSelector (toNSString value)

-- | @- text@
text :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO (Id NSString)
text domhtmlOptionElement =
  sendMessage domhtmlOptionElement textSelector

-- | @- index@
index :: IsDOMHTMLOptionElement domhtmlOptionElement => domhtmlOptionElement -> IO CInt
index domhtmlOptionElement =
  sendMessage domhtmlOptionElement indexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @form@
formSelector :: Selector '[] (Id DOMHTMLFormElement)
formSelector = mkSelector "form"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @defaultSelected@
defaultSelectedSelector :: Selector '[] Bool
defaultSelectedSelector = mkSelector "defaultSelected"

-- | @Selector@ for @setDefaultSelected:@
setDefaultSelectedSelector :: Selector '[Bool] ()
setDefaultSelectedSelector = mkSelector "setDefaultSelected:"

-- | @Selector@ for @selected@
selectedSelector :: Selector '[] Bool
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector '[Bool] ()
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CInt
indexSelector = mkSelector "index"

