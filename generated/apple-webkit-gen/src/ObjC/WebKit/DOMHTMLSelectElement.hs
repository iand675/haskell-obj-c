{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLSelectElement@.
module ObjC.WebKit.DOMHTMLSelectElement
  ( DOMHTMLSelectElement
  , IsDOMHTMLSelectElement(..)
  , item
  , namedItem
  , add_before
  , remove
  , add
  , autofocus
  , setAutofocus
  , disabled
  , setDisabled
  , form
  , multiple
  , setMultiple
  , name
  , setName
  , size
  , setSize
  , type_
  , options
  , length_
  , selectedIndex
  , setSelectedIndex
  , value
  , setValue
  , willValidate
  , addSelector
  , add_beforeSelector
  , autofocusSelector
  , disabledSelector
  , formSelector
  , itemSelector
  , lengthSelector
  , multipleSelector
  , nameSelector
  , namedItemSelector
  , optionsSelector
  , removeSelector
  , selectedIndexSelector
  , setAutofocusSelector
  , setDisabledSelector
  , setMultipleSelector
  , setNameSelector
  , setSelectedIndexSelector
  , setSizeSelector
  , setValueSelector
  , sizeSelector
  , typeSelector
  , valueSelector
  , willValidateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- item:@
item :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CUInt -> IO (Id DOMNode)
item domhtmlSelectElement index =
  sendMessage domhtmlSelectElement itemSelector index

-- | @- namedItem:@
namedItem :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsNSString name) => domhtmlSelectElement -> name -> IO (Id DOMNode)
namedItem domhtmlSelectElement name =
  sendMessage domhtmlSelectElement namedItemSelector (toNSString name)

-- | @- add:before:@
add_before :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsDOMHTMLElement element, IsDOMHTMLElement before) => domhtmlSelectElement -> element -> before -> IO ()
add_before domhtmlSelectElement element before =
  sendMessage domhtmlSelectElement add_beforeSelector (toDOMHTMLElement element) (toDOMHTMLElement before)

-- | @- remove:@
remove :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CInt -> IO ()
remove domhtmlSelectElement index =
  sendMessage domhtmlSelectElement removeSelector index

-- | @- add::@
add :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsDOMHTMLElement element, IsDOMHTMLElement before) => domhtmlSelectElement -> element -> before -> IO ()
add domhtmlSelectElement element before =
  sendMessage domhtmlSelectElement addSelector (toDOMHTMLElement element) (toDOMHTMLElement before)

-- | @- autofocus@
autofocus :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
autofocus domhtmlSelectElement =
  sendMessage domhtmlSelectElement autofocusSelector

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> Bool -> IO ()
setAutofocus domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setAutofocusSelector value

-- | @- disabled@
disabled :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
disabled domhtmlSelectElement =
  sendMessage domhtmlSelectElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> Bool -> IO ()
setDisabled domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setDisabledSelector value

-- | @- form@
form :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id DOMHTMLFormElement)
form domhtmlSelectElement =
  sendMessage domhtmlSelectElement formSelector

-- | @- multiple@
multiple :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
multiple domhtmlSelectElement =
  sendMessage domhtmlSelectElement multipleSelector

-- | @- setMultiple:@
setMultiple :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> Bool -> IO ()
setMultiple domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setMultipleSelector value

-- | @- name@
name :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id NSString)
name domhtmlSelectElement =
  sendMessage domhtmlSelectElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsNSString value) => domhtmlSelectElement -> value -> IO ()
setName domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setNameSelector (toNSString value)

-- | @- size@
size :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO CInt
size domhtmlSelectElement =
  sendMessage domhtmlSelectElement sizeSelector

-- | @- setSize:@
setSize :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CInt -> IO ()
setSize domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setSizeSelector value

-- | @- type@
type_ :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id NSString)
type_ domhtmlSelectElement =
  sendMessage domhtmlSelectElement typeSelector

-- | @- options@
options :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id DOMHTMLOptionsCollection)
options domhtmlSelectElement =
  sendMessage domhtmlSelectElement optionsSelector

-- | @- length@
length_ :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO CInt
length_ domhtmlSelectElement =
  sendMessage domhtmlSelectElement lengthSelector

-- | @- selectedIndex@
selectedIndex :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO CInt
selectedIndex domhtmlSelectElement =
  sendMessage domhtmlSelectElement selectedIndexSelector

-- | @- setSelectedIndex:@
setSelectedIndex :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CInt -> IO ()
setSelectedIndex domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setSelectedIndexSelector value

-- | @- value@
value :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id NSString)
value domhtmlSelectElement =
  sendMessage domhtmlSelectElement valueSelector

-- | @- setValue:@
setValue :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsNSString value) => domhtmlSelectElement -> value -> IO ()
setValue domhtmlSelectElement value =
  sendMessage domhtmlSelectElement setValueSelector (toNSString value)

-- | @- willValidate@
willValidate :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
willValidate domhtmlSelectElement =
  sendMessage domhtmlSelectElement willValidateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMNode)
itemSelector = mkSelector "item:"

-- | @Selector@ for @namedItem:@
namedItemSelector :: Selector '[Id NSString] (Id DOMNode)
namedItemSelector = mkSelector "namedItem:"

-- | @Selector@ for @add:before:@
add_beforeSelector :: Selector '[Id DOMHTMLElement, Id DOMHTMLElement] ()
add_beforeSelector = mkSelector "add:before:"

-- | @Selector@ for @remove:@
removeSelector :: Selector '[CInt] ()
removeSelector = mkSelector "remove:"

-- | @Selector@ for @add::@
addSelector :: Selector '[Id DOMHTMLElement, Id DOMHTMLElement] ()
addSelector = mkSelector "add::"

-- | @Selector@ for @autofocus@
autofocusSelector :: Selector '[] Bool
autofocusSelector = mkSelector "autofocus"

-- | @Selector@ for @setAutofocus:@
setAutofocusSelector :: Selector '[Bool] ()
setAutofocusSelector = mkSelector "setAutofocus:"

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @form@
formSelector :: Selector '[] (Id DOMHTMLFormElement)
formSelector = mkSelector "form"

-- | @Selector@ for @multiple@
multipleSelector :: Selector '[] Bool
multipleSelector = mkSelector "multiple"

-- | @Selector@ for @setMultiple:@
setMultipleSelector :: Selector '[Bool] ()
setMultipleSelector = mkSelector "setMultiple:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] CInt
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[CInt] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id DOMHTMLOptionsCollection)
optionsSelector = mkSelector "options"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CInt
lengthSelector = mkSelector "length"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector '[] CInt
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector '[CInt] ()
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @willValidate@
willValidateSelector :: Selector '[] Bool
willValidateSelector = mkSelector "willValidate"

