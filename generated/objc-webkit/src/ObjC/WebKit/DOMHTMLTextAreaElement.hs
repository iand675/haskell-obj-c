{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLTextAreaElement@.
module ObjC.WebKit.DOMHTMLTextAreaElement
  ( DOMHTMLTextAreaElement
  , IsDOMHTMLTextAreaElement(..)
  , select
  , setSelectionRange_end
  , autofocus
  , setAutofocus
  , disabled
  , setDisabled
  , form
  , name
  , setName
  , readOnly
  , setReadOnly
  , rows
  , setRows
  , cols
  , setCols
  , type_
  , defaultValue
  , setDefaultValue
  , value
  , setValue
  , willValidate
  , selectionStart
  , setSelectionStart
  , selectionEnd
  , setSelectionEnd
  , accessKey
  , setAccessKey
  , selectSelector
  , setSelectionRange_endSelector
  , autofocusSelector
  , setAutofocusSelector
  , disabledSelector
  , setDisabledSelector
  , formSelector
  , nameSelector
  , setNameSelector
  , readOnlySelector
  , setReadOnlySelector
  , rowsSelector
  , setRowsSelector
  , colsSelector
  , setColsSelector
  , typeSelector
  , defaultValueSelector
  , setDefaultValueSelector
  , valueSelector
  , setValueSelector
  , willValidateSelector
  , selectionStartSelector
  , setSelectionStartSelector
  , selectionEndSelector
  , setSelectionEndSelector
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

-- | @- select@
select :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO ()
select domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "select") retVoid []

-- | @- setSelectionRange:end:@
setSelectionRange_end :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> CInt -> IO ()
setSelectionRange_end domhtmlTextAreaElement  start end =
  sendMsg domhtmlTextAreaElement (mkSelector "setSelectionRange:end:") retVoid [argCInt (fromIntegral start), argCInt (fromIntegral end)]

-- | @- autofocus@
autofocus :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
autofocus domhtmlTextAreaElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlTextAreaElement (mkSelector "autofocus") retCULong []

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> Bool -> IO ()
setAutofocus domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setAutofocus:") retVoid [argCULong (if value then 1 else 0)]

-- | @- disabled@
disabled :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
disabled domhtmlTextAreaElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlTextAreaElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> Bool -> IO ()
setDisabled domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- form@
form :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id DOMHTMLFormElement)
form domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
name domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setName domhtmlTextAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTextAreaElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- readOnly@
readOnly :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
readOnly domhtmlTextAreaElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlTextAreaElement (mkSelector "readOnly") retCULong []

-- | @- setReadOnly:@
setReadOnly :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> Bool -> IO ()
setReadOnly domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setReadOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- rows@
rows :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
rows domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "rows") retCInt []

-- | @- setRows:@
setRows :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setRows domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setRows:") retVoid [argCInt (fromIntegral value)]

-- | @- cols@
cols :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
cols domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "cols") retCInt []

-- | @- setCols:@
setCols :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setCols domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setCols:") retVoid [argCInt (fromIntegral value)]

-- | @- type@
type_ :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
type_ domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- defaultValue@
defaultValue :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
defaultValue domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "defaultValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultValue:@
setDefaultValue :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setDefaultValue domhtmlTextAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTextAreaElement (mkSelector "setDefaultValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
value domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setValue domhtmlTextAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTextAreaElement (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- willValidate@
willValidate :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
willValidate domhtmlTextAreaElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlTextAreaElement (mkSelector "willValidate") retCULong []

-- | @- selectionStart@
selectionStart :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
selectionStart domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "selectionStart") retCInt []

-- | @- setSelectionStart:@
setSelectionStart :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setSelectionStart domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setSelectionStart:") retVoid [argCInt (fromIntegral value)]

-- | @- selectionEnd@
selectionEnd :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
selectionEnd domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "selectionEnd") retCInt []

-- | @- setSelectionEnd:@
setSelectionEnd :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setSelectionEnd domhtmlTextAreaElement  value =
  sendMsg domhtmlTextAreaElement (mkSelector "setSelectionEnd:") retVoid [argCInt (fromIntegral value)]

-- | @- accessKey@
accessKey :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
accessKey domhtmlTextAreaElement  =
  sendMsg domhtmlTextAreaElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setAccessKey domhtmlTextAreaElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlTextAreaElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @select@
selectSelector :: Selector
selectSelector = mkSelector "select"

-- | @Selector@ for @setSelectionRange:end:@
setSelectionRange_endSelector :: Selector
setSelectionRange_endSelector = mkSelector "setSelectionRange:end:"

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

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @rows@
rowsSelector :: Selector
rowsSelector = mkSelector "rows"

-- | @Selector@ for @setRows:@
setRowsSelector :: Selector
setRowsSelector = mkSelector "setRows:"

-- | @Selector@ for @cols@
colsSelector :: Selector
colsSelector = mkSelector "cols"

-- | @Selector@ for @setCols:@
setColsSelector :: Selector
setColsSelector = mkSelector "setCols:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @setDefaultValue:@
setDefaultValueSelector :: Selector
setDefaultValueSelector = mkSelector "setDefaultValue:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @willValidate@
willValidateSelector :: Selector
willValidateSelector = mkSelector "willValidate"

-- | @Selector@ for @selectionStart@
selectionStartSelector :: Selector
selectionStartSelector = mkSelector "selectionStart"

-- | @Selector@ for @setSelectionStart:@
setSelectionStartSelector :: Selector
setSelectionStartSelector = mkSelector "setSelectionStart:"

-- | @Selector@ for @selectionEnd@
selectionEndSelector :: Selector
selectionEndSelector = mkSelector "selectionEnd"

-- | @Selector@ for @setSelectionEnd:@
setSelectionEndSelector :: Selector
setSelectionEndSelector = mkSelector "setSelectionEnd:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

