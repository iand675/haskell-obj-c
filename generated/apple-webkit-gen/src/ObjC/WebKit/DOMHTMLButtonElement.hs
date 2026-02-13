{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLButtonElement@.
module ObjC.WebKit.DOMHTMLButtonElement
  ( DOMHTMLButtonElement
  , IsDOMHTMLButtonElement(..)
  , click
  , autofocus
  , setAutofocus
  , disabled
  , setDisabled
  , form
  , type_
  , setType
  , name
  , setName
  , value
  , setValue
  , willValidate
  , accessKey
  , setAccessKey
  , accessKeySelector
  , autofocusSelector
  , clickSelector
  , disabledSelector
  , formSelector
  , nameSelector
  , setAccessKeySelector
  , setAutofocusSelector
  , setDisabledSelector
  , setNameSelector
  , setTypeSelector
  , setValueSelector
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

-- | @- click@
click :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO ()
click domhtmlButtonElement =
  sendMessage domhtmlButtonElement clickSelector

-- | @- autofocus@
autofocus :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO Bool
autofocus domhtmlButtonElement =
  sendMessage domhtmlButtonElement autofocusSelector

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> Bool -> IO ()
setAutofocus domhtmlButtonElement value =
  sendMessage domhtmlButtonElement setAutofocusSelector value

-- | @- disabled@
disabled :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO Bool
disabled domhtmlButtonElement =
  sendMessage domhtmlButtonElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> Bool -> IO ()
setDisabled domhtmlButtonElement value =
  sendMessage domhtmlButtonElement setDisabledSelector value

-- | @- form@
form :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id DOMHTMLFormElement)
form domhtmlButtonElement =
  sendMessage domhtmlButtonElement formSelector

-- | @- type@
type_ :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
type_ domhtmlButtonElement =
  sendMessage domhtmlButtonElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setType domhtmlButtonElement value =
  sendMessage domhtmlButtonElement setTypeSelector (toNSString value)

-- | @- name@
name :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
name domhtmlButtonElement =
  sendMessage domhtmlButtonElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setName domhtmlButtonElement value =
  sendMessage domhtmlButtonElement setNameSelector (toNSString value)

-- | @- value@
value :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
value domhtmlButtonElement =
  sendMessage domhtmlButtonElement valueSelector

-- | @- setValue:@
setValue :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setValue domhtmlButtonElement value =
  sendMessage domhtmlButtonElement setValueSelector (toNSString value)

-- | @- willValidate@
willValidate :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO Bool
willValidate domhtmlButtonElement =
  sendMessage domhtmlButtonElement willValidateSelector

-- | @- accessKey@
accessKey :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
accessKey domhtmlButtonElement =
  sendMessage domhtmlButtonElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setAccessKey domhtmlButtonElement value =
  sendMessage domhtmlButtonElement setAccessKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @click@
clickSelector :: Selector '[] ()
clickSelector = mkSelector "click"

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

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @willValidate@
willValidateSelector :: Selector '[] Bool
willValidateSelector = mkSelector "willValidate"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

