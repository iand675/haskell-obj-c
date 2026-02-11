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
  , itemSelector
  , namedItemSelector
  , add_beforeSelector
  , removeSelector
  , addSelector
  , autofocusSelector
  , setAutofocusSelector
  , disabledSelector
  , setDisabledSelector
  , formSelector
  , multipleSelector
  , setMultipleSelector
  , nameSelector
  , setNameSelector
  , sizeSelector
  , setSizeSelector
  , typeSelector
  , optionsSelector
  , lengthSelector
  , selectedIndexSelector
  , setSelectedIndexSelector
  , valueSelector
  , setValueSelector
  , willValidateSelector


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

-- | @- item:@
item :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CUInt -> IO (Id DOMNode)
item domhtmlSelectElement  index =
  sendMsg domhtmlSelectElement (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- namedItem:@
namedItem :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsNSString name) => domhtmlSelectElement -> name -> IO (Id DOMNode)
namedItem domhtmlSelectElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg domhtmlSelectElement (mkSelector "namedItem:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- add:before:@
add_before :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsDOMHTMLElement element, IsDOMHTMLElement before) => domhtmlSelectElement -> element -> before -> IO ()
add_before domhtmlSelectElement  element before =
withObjCPtr element $ \raw_element ->
  withObjCPtr before $ \raw_before ->
      sendMsg domhtmlSelectElement (mkSelector "add:before:") retVoid [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_before :: Ptr ())]

-- | @- remove:@
remove :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CInt -> IO ()
remove domhtmlSelectElement  index =
  sendMsg domhtmlSelectElement (mkSelector "remove:") retVoid [argCInt (fromIntegral index)]

-- | @- add::@
add :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsDOMHTMLElement element, IsDOMHTMLElement before) => domhtmlSelectElement -> element -> before -> IO ()
add domhtmlSelectElement  element before =
withObjCPtr element $ \raw_element ->
  withObjCPtr before $ \raw_before ->
      sendMsg domhtmlSelectElement (mkSelector "add::") retVoid [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_before :: Ptr ())]

-- | @- autofocus@
autofocus :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
autofocus domhtmlSelectElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlSelectElement (mkSelector "autofocus") retCULong []

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> Bool -> IO ()
setAutofocus domhtmlSelectElement  value =
  sendMsg domhtmlSelectElement (mkSelector "setAutofocus:") retVoid [argCULong (if value then 1 else 0)]

-- | @- disabled@
disabled :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
disabled domhtmlSelectElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlSelectElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> Bool -> IO ()
setDisabled domhtmlSelectElement  value =
  sendMsg domhtmlSelectElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- form@
form :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id DOMHTMLFormElement)
form domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- multiple@
multiple :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
multiple domhtmlSelectElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlSelectElement (mkSelector "multiple") retCULong []

-- | @- setMultiple:@
setMultiple :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> Bool -> IO ()
setMultiple domhtmlSelectElement  value =
  sendMsg domhtmlSelectElement (mkSelector "setMultiple:") retVoid [argCULong (if value then 1 else 0)]

-- | @- name@
name :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id NSString)
name domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsNSString value) => domhtmlSelectElement -> value -> IO ()
setName domhtmlSelectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlSelectElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- size@
size :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO CInt
size domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "size") retCInt []

-- | @- setSize:@
setSize :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CInt -> IO ()
setSize domhtmlSelectElement  value =
  sendMsg domhtmlSelectElement (mkSelector "setSize:") retVoid [argCInt (fromIntegral value)]

-- | @- type@
type_ :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id NSString)
type_ domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- options@
options :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id DOMHTMLOptionsCollection)
options domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO CInt
length_ domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "length") retCInt []

-- | @- selectedIndex@
selectedIndex :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO CInt
selectedIndex domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "selectedIndex") retCInt []

-- | @- setSelectedIndex:@
setSelectedIndex :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> CInt -> IO ()
setSelectedIndex domhtmlSelectElement  value =
  sendMsg domhtmlSelectElement (mkSelector "setSelectedIndex:") retVoid [argCInt (fromIntegral value)]

-- | @- value@
value :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO (Id NSString)
value domhtmlSelectElement  =
  sendMsg domhtmlSelectElement (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMHTMLSelectElement domhtmlSelectElement, IsNSString value) => domhtmlSelectElement -> value -> IO ()
setValue domhtmlSelectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlSelectElement (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- willValidate@
willValidate :: IsDOMHTMLSelectElement domhtmlSelectElement => domhtmlSelectElement -> IO Bool
willValidate domhtmlSelectElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlSelectElement (mkSelector "willValidate") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @namedItem:@
namedItemSelector :: Selector
namedItemSelector = mkSelector "namedItem:"

-- | @Selector@ for @add:before:@
add_beforeSelector :: Selector
add_beforeSelector = mkSelector "add:before:"

-- | @Selector@ for @remove:@
removeSelector :: Selector
removeSelector = mkSelector "remove:"

-- | @Selector@ for @add::@
addSelector :: Selector
addSelector = mkSelector "add::"

-- | @Selector@ for @autofocus@
autofocusSelector :: Selector
autofocusSelector = mkSelector "autofocus"

-- | @Selector@ for @setAutofocus:@
setAutofocusSelector :: Selector
setAutofocusSelector = mkSelector "setAutofocus:"

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

-- | @Selector@ for @multiple@
multipleSelector :: Selector
multipleSelector = mkSelector "multiple"

-- | @Selector@ for @setMultiple:@
setMultipleSelector :: Selector
setMultipleSelector = mkSelector "setMultiple:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @selectedIndex@
selectedIndexSelector :: Selector
selectedIndexSelector = mkSelector "selectedIndex"

-- | @Selector@ for @setSelectedIndex:@
setSelectedIndexSelector :: Selector
setSelectedIndexSelector = mkSelector "setSelectedIndex:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @willValidate@
willValidateSelector :: Selector
willValidateSelector = mkSelector "willValidate"

