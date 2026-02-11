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
  , clickSelector
  , autofocusSelector
  , setAutofocusSelector
  , disabledSelector
  , setDisabledSelector
  , formSelector
  , typeSelector
  , setTypeSelector
  , nameSelector
  , setNameSelector
  , valueSelector
  , setValueSelector
  , willValidateSelector
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

-- | @- click@
click :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO ()
click domhtmlButtonElement  =
  sendMsg domhtmlButtonElement (mkSelector "click") retVoid []

-- | @- autofocus@
autofocus :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO Bool
autofocus domhtmlButtonElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlButtonElement (mkSelector "autofocus") retCULong []

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> Bool -> IO ()
setAutofocus domhtmlButtonElement  value =
  sendMsg domhtmlButtonElement (mkSelector "setAutofocus:") retVoid [argCULong (if value then 1 else 0)]

-- | @- disabled@
disabled :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO Bool
disabled domhtmlButtonElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlButtonElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> Bool -> IO ()
setDisabled domhtmlButtonElement  value =
  sendMsg domhtmlButtonElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- form@
form :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id DOMHTMLFormElement)
form domhtmlButtonElement  =
  sendMsg domhtmlButtonElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
type_ domhtmlButtonElement  =
  sendMsg domhtmlButtonElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setType domhtmlButtonElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlButtonElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
name domhtmlButtonElement  =
  sendMsg domhtmlButtonElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setName domhtmlButtonElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlButtonElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
value domhtmlButtonElement  =
  sendMsg domhtmlButtonElement (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setValue domhtmlButtonElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlButtonElement (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- willValidate@
willValidate :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO Bool
willValidate domhtmlButtonElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlButtonElement (mkSelector "willValidate") retCULong []

-- | @- accessKey@
accessKey :: IsDOMHTMLButtonElement domhtmlButtonElement => domhtmlButtonElement -> IO (Id NSString)
accessKey domhtmlButtonElement  =
  sendMsg domhtmlButtonElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLButtonElement domhtmlButtonElement, IsNSString value) => domhtmlButtonElement -> value -> IO ()
setAccessKey domhtmlButtonElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlButtonElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @click@
clickSelector :: Selector
clickSelector = mkSelector "click"

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

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @willValidate@
willValidateSelector :: Selector
willValidateSelector = mkSelector "willValidate"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

