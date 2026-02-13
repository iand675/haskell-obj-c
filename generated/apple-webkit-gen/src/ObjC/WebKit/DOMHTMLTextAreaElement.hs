{-# LANGUAGE DataKinds #-}
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
  , accessKeySelector
  , autofocusSelector
  , colsSelector
  , defaultValueSelector
  , disabledSelector
  , formSelector
  , nameSelector
  , readOnlySelector
  , rowsSelector
  , selectSelector
  , selectionEndSelector
  , selectionStartSelector
  , setAccessKeySelector
  , setAutofocusSelector
  , setColsSelector
  , setDefaultValueSelector
  , setDisabledSelector
  , setNameSelector
  , setReadOnlySelector
  , setRowsSelector
  , setSelectionEndSelector
  , setSelectionRange_endSelector
  , setSelectionStartSelector
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

-- | @- select@
select :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO ()
select domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement selectSelector

-- | @- setSelectionRange:end:@
setSelectionRange_end :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> CInt -> IO ()
setSelectionRange_end domhtmlTextAreaElement start end =
  sendMessage domhtmlTextAreaElement setSelectionRange_endSelector start end

-- | @- autofocus@
autofocus :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
autofocus domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement autofocusSelector

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> Bool -> IO ()
setAutofocus domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setAutofocusSelector value

-- | @- disabled@
disabled :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
disabled domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> Bool -> IO ()
setDisabled domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setDisabledSelector value

-- | @- form@
form :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id DOMHTMLFormElement)
form domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement formSelector

-- | @- name@
name :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
name domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setName domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setNameSelector (toNSString value)

-- | @- readOnly@
readOnly :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
readOnly domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement readOnlySelector

-- | @- setReadOnly:@
setReadOnly :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> Bool -> IO ()
setReadOnly domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setReadOnlySelector value

-- | @- rows@
rows :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
rows domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement rowsSelector

-- | @- setRows:@
setRows :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setRows domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setRowsSelector value

-- | @- cols@
cols :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
cols domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement colsSelector

-- | @- setCols:@
setCols :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setCols domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setColsSelector value

-- | @- type@
type_ :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
type_ domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement typeSelector

-- | @- defaultValue@
defaultValue :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
defaultValue domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement defaultValueSelector

-- | @- setDefaultValue:@
setDefaultValue :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setDefaultValue domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setDefaultValueSelector (toNSString value)

-- | @- value@
value :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
value domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement valueSelector

-- | @- setValue:@
setValue :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setValue domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setValueSelector (toNSString value)

-- | @- willValidate@
willValidate :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO Bool
willValidate domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement willValidateSelector

-- | @- selectionStart@
selectionStart :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
selectionStart domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement selectionStartSelector

-- | @- setSelectionStart:@
setSelectionStart :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setSelectionStart domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setSelectionStartSelector value

-- | @- selectionEnd@
selectionEnd :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO CInt
selectionEnd domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement selectionEndSelector

-- | @- setSelectionEnd:@
setSelectionEnd :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> CInt -> IO ()
setSelectionEnd domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setSelectionEndSelector value

-- | @- accessKey@
accessKey :: IsDOMHTMLTextAreaElement domhtmlTextAreaElement => domhtmlTextAreaElement -> IO (Id NSString)
accessKey domhtmlTextAreaElement =
  sendMessage domhtmlTextAreaElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLTextAreaElement domhtmlTextAreaElement, IsNSString value) => domhtmlTextAreaElement -> value -> IO ()
setAccessKey domhtmlTextAreaElement value =
  sendMessage domhtmlTextAreaElement setAccessKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @select@
selectSelector :: Selector '[] ()
selectSelector = mkSelector "select"

-- | @Selector@ for @setSelectionRange:end:@
setSelectionRange_endSelector :: Selector '[CInt, CInt] ()
setSelectionRange_endSelector = mkSelector "setSelectionRange:end:"

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

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector '[Bool] ()
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @rows@
rowsSelector :: Selector '[] CInt
rowsSelector = mkSelector "rows"

-- | @Selector@ for @setRows:@
setRowsSelector :: Selector '[CInt] ()
setRowsSelector = mkSelector "setRows:"

-- | @Selector@ for @cols@
colsSelector :: Selector '[] CInt
colsSelector = mkSelector "cols"

-- | @Selector@ for @setCols:@
setColsSelector :: Selector '[CInt] ()
setColsSelector = mkSelector "setCols:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector '[] (Id NSString)
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @setDefaultValue:@
setDefaultValueSelector :: Selector '[Id NSString] ()
setDefaultValueSelector = mkSelector "setDefaultValue:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @willValidate@
willValidateSelector :: Selector '[] Bool
willValidateSelector = mkSelector "willValidate"

-- | @Selector@ for @selectionStart@
selectionStartSelector :: Selector '[] CInt
selectionStartSelector = mkSelector "selectionStart"

-- | @Selector@ for @setSelectionStart:@
setSelectionStartSelector :: Selector '[CInt] ()
setSelectionStartSelector = mkSelector "setSelectionStart:"

-- | @Selector@ for @selectionEnd@
selectionEndSelector :: Selector '[] CInt
selectionEndSelector = mkSelector "selectionEnd"

-- | @Selector@ for @setSelectionEnd:@
setSelectionEndSelector :: Selector '[CInt] ()
setSelectionEndSelector = mkSelector "setSelectionEnd:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

