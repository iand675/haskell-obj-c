{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLInputElement@.
module ObjC.WebKit.DOMHTMLInputElement
  ( DOMHTMLInputElement
  , IsDOMHTMLInputElement(..)
  , select
  , setSelectionRange_end
  , click
  , accept
  , setAccept
  , alt
  , setAlt
  , autofocus
  , setAutofocus
  , defaultChecked
  , setDefaultChecked
  , checked
  , setChecked
  , disabled
  , setDisabled
  , form
  , files
  , setFiles
  , indeterminate
  , setIndeterminate
  , maxLength
  , setMaxLength
  , multiple
  , setMultiple
  , name
  , setName
  , readOnly
  , setReadOnly
  , size
  , setSize
  , src
  , setSrc
  , type_
  , setType
  , defaultValue
  , setDefaultValue
  , value
  , setValue
  , willValidate
  , selectionStart
  , setSelectionStart
  , selectionEnd
  , setSelectionEnd
  , align
  , setAlign
  , useMap
  , setUseMap
  , accessKey
  , setAccessKey
  , altDisplayString
  , absoluteImageURL
  , absoluteImageURLSelector
  , acceptSelector
  , accessKeySelector
  , alignSelector
  , altDisplayStringSelector
  , altSelector
  , autofocusSelector
  , checkedSelector
  , clickSelector
  , defaultCheckedSelector
  , defaultValueSelector
  , disabledSelector
  , filesSelector
  , formSelector
  , indeterminateSelector
  , maxLengthSelector
  , multipleSelector
  , nameSelector
  , readOnlySelector
  , selectSelector
  , selectionEndSelector
  , selectionStartSelector
  , setAcceptSelector
  , setAccessKeySelector
  , setAlignSelector
  , setAltSelector
  , setAutofocusSelector
  , setCheckedSelector
  , setDefaultCheckedSelector
  , setDefaultValueSelector
  , setDisabledSelector
  , setFilesSelector
  , setIndeterminateSelector
  , setMaxLengthSelector
  , setMultipleSelector
  , setNameSelector
  , setReadOnlySelector
  , setSelectionEndSelector
  , setSelectionRange_endSelector
  , setSelectionStartSelector
  , setSizeSelector
  , setSrcSelector
  , setTypeSelector
  , setUseMapSelector
  , setValueSelector
  , sizeSelector
  , srcSelector
  , typeSelector
  , useMapSelector
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
select :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO ()
select domhtmlInputElement =
  sendMessage domhtmlInputElement selectSelector

-- | @- setSelectionRange:end:@
setSelectionRange_end :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> CInt -> IO ()
setSelectionRange_end domhtmlInputElement start end =
  sendMessage domhtmlInputElement setSelectionRange_endSelector start end

-- | @- click@
click :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO ()
click domhtmlInputElement =
  sendMessage domhtmlInputElement clickSelector

-- | @- accept@
accept :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
accept domhtmlInputElement =
  sendMessage domhtmlInputElement acceptSelector

-- | @- setAccept:@
setAccept :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAccept domhtmlInputElement value =
  sendMessage domhtmlInputElement setAcceptSelector (toNSString value)

-- | @- alt@
alt :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
alt domhtmlInputElement =
  sendMessage domhtmlInputElement altSelector

-- | @- setAlt:@
setAlt :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAlt domhtmlInputElement value =
  sendMessage domhtmlInputElement setAltSelector (toNSString value)

-- | @- autofocus@
autofocus :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
autofocus domhtmlInputElement =
  sendMessage domhtmlInputElement autofocusSelector

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setAutofocus domhtmlInputElement value =
  sendMessage domhtmlInputElement setAutofocusSelector value

-- | @- defaultChecked@
defaultChecked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
defaultChecked domhtmlInputElement =
  sendMessage domhtmlInputElement defaultCheckedSelector

-- | @- setDefaultChecked:@
setDefaultChecked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setDefaultChecked domhtmlInputElement value =
  sendMessage domhtmlInputElement setDefaultCheckedSelector value

-- | @- checked@
checked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
checked domhtmlInputElement =
  sendMessage domhtmlInputElement checkedSelector

-- | @- setChecked:@
setChecked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setChecked domhtmlInputElement value =
  sendMessage domhtmlInputElement setCheckedSelector value

-- | @- disabled@
disabled :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
disabled domhtmlInputElement =
  sendMessage domhtmlInputElement disabledSelector

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setDisabled domhtmlInputElement value =
  sendMessage domhtmlInputElement setDisabledSelector value

-- | @- form@
form :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id DOMHTMLFormElement)
form domhtmlInputElement =
  sendMessage domhtmlInputElement formSelector

-- | @- files@
files :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id DOMFileList)
files domhtmlInputElement =
  sendMessage domhtmlInputElement filesSelector

-- | @- setFiles:@
setFiles :: (IsDOMHTMLInputElement domhtmlInputElement, IsDOMFileList value) => domhtmlInputElement -> value -> IO ()
setFiles domhtmlInputElement value =
  sendMessage domhtmlInputElement setFilesSelector (toDOMFileList value)

-- | @- indeterminate@
indeterminate :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
indeterminate domhtmlInputElement =
  sendMessage domhtmlInputElement indeterminateSelector

-- | @- setIndeterminate:@
setIndeterminate :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setIndeterminate domhtmlInputElement value =
  sendMessage domhtmlInputElement setIndeterminateSelector value

-- | @- maxLength@
maxLength :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO CInt
maxLength domhtmlInputElement =
  sendMessage domhtmlInputElement maxLengthSelector

-- | @- setMaxLength:@
setMaxLength :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> IO ()
setMaxLength domhtmlInputElement value =
  sendMessage domhtmlInputElement setMaxLengthSelector value

-- | @- multiple@
multiple :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
multiple domhtmlInputElement =
  sendMessage domhtmlInputElement multipleSelector

-- | @- setMultiple:@
setMultiple :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setMultiple domhtmlInputElement value =
  sendMessage domhtmlInputElement setMultipleSelector value

-- | @- name@
name :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
name domhtmlInputElement =
  sendMessage domhtmlInputElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setName domhtmlInputElement value =
  sendMessage domhtmlInputElement setNameSelector (toNSString value)

-- | @- readOnly@
readOnly :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
readOnly domhtmlInputElement =
  sendMessage domhtmlInputElement readOnlySelector

-- | @- setReadOnly:@
setReadOnly :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setReadOnly domhtmlInputElement value =
  sendMessage domhtmlInputElement setReadOnlySelector value

-- | @- size@
size :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
size domhtmlInputElement =
  sendMessage domhtmlInputElement sizeSelector

-- | @- setSize:@
setSize :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setSize domhtmlInputElement value =
  sendMessage domhtmlInputElement setSizeSelector (toNSString value)

-- | @- src@
src :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
src domhtmlInputElement =
  sendMessage domhtmlInputElement srcSelector

-- | @- setSrc:@
setSrc :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setSrc domhtmlInputElement value =
  sendMessage domhtmlInputElement setSrcSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
type_ domhtmlInputElement =
  sendMessage domhtmlInputElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setType domhtmlInputElement value =
  sendMessage domhtmlInputElement setTypeSelector (toNSString value)

-- | @- defaultValue@
defaultValue :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
defaultValue domhtmlInputElement =
  sendMessage domhtmlInputElement defaultValueSelector

-- | @- setDefaultValue:@
setDefaultValue :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setDefaultValue domhtmlInputElement value =
  sendMessage domhtmlInputElement setDefaultValueSelector (toNSString value)

-- | @- value@
value :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
value domhtmlInputElement =
  sendMessage domhtmlInputElement valueSelector

-- | @- setValue:@
setValue :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setValue domhtmlInputElement value =
  sendMessage domhtmlInputElement setValueSelector (toNSString value)

-- | @- willValidate@
willValidate :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
willValidate domhtmlInputElement =
  sendMessage domhtmlInputElement willValidateSelector

-- | @- selectionStart@
selectionStart :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO CInt
selectionStart domhtmlInputElement =
  sendMessage domhtmlInputElement selectionStartSelector

-- | @- setSelectionStart:@
setSelectionStart :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> IO ()
setSelectionStart domhtmlInputElement value =
  sendMessage domhtmlInputElement setSelectionStartSelector value

-- | @- selectionEnd@
selectionEnd :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO CInt
selectionEnd domhtmlInputElement =
  sendMessage domhtmlInputElement selectionEndSelector

-- | @- setSelectionEnd:@
setSelectionEnd :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> IO ()
setSelectionEnd domhtmlInputElement value =
  sendMessage domhtmlInputElement setSelectionEndSelector value

-- | @- align@
align :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
align domhtmlInputElement =
  sendMessage domhtmlInputElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAlign domhtmlInputElement value =
  sendMessage domhtmlInputElement setAlignSelector (toNSString value)

-- | @- useMap@
useMap :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
useMap domhtmlInputElement =
  sendMessage domhtmlInputElement useMapSelector

-- | @- setUseMap:@
setUseMap :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setUseMap domhtmlInputElement value =
  sendMessage domhtmlInputElement setUseMapSelector (toNSString value)

-- | @- accessKey@
accessKey :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
accessKey domhtmlInputElement =
  sendMessage domhtmlInputElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAccessKey domhtmlInputElement value =
  sendMessage domhtmlInputElement setAccessKeySelector (toNSString value)

-- | @- altDisplayString@
altDisplayString :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
altDisplayString domhtmlInputElement =
  sendMessage domhtmlInputElement altDisplayStringSelector

-- | @- absoluteImageURL@
absoluteImageURL :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSURL)
absoluteImageURL domhtmlInputElement =
  sendMessage domhtmlInputElement absoluteImageURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @select@
selectSelector :: Selector '[] ()
selectSelector = mkSelector "select"

-- | @Selector@ for @setSelectionRange:end:@
setSelectionRange_endSelector :: Selector '[CInt, CInt] ()
setSelectionRange_endSelector = mkSelector "setSelectionRange:end:"

-- | @Selector@ for @click@
clickSelector :: Selector '[] ()
clickSelector = mkSelector "click"

-- | @Selector@ for @accept@
acceptSelector :: Selector '[] (Id NSString)
acceptSelector = mkSelector "accept"

-- | @Selector@ for @setAccept:@
setAcceptSelector :: Selector '[Id NSString] ()
setAcceptSelector = mkSelector "setAccept:"

-- | @Selector@ for @alt@
altSelector :: Selector '[] (Id NSString)
altSelector = mkSelector "alt"

-- | @Selector@ for @setAlt:@
setAltSelector :: Selector '[Id NSString] ()
setAltSelector = mkSelector "setAlt:"

-- | @Selector@ for @autofocus@
autofocusSelector :: Selector '[] Bool
autofocusSelector = mkSelector "autofocus"

-- | @Selector@ for @setAutofocus:@
setAutofocusSelector :: Selector '[Bool] ()
setAutofocusSelector = mkSelector "setAutofocus:"

-- | @Selector@ for @defaultChecked@
defaultCheckedSelector :: Selector '[] Bool
defaultCheckedSelector = mkSelector "defaultChecked"

-- | @Selector@ for @setDefaultChecked:@
setDefaultCheckedSelector :: Selector '[Bool] ()
setDefaultCheckedSelector = mkSelector "setDefaultChecked:"

-- | @Selector@ for @checked@
checkedSelector :: Selector '[] Bool
checkedSelector = mkSelector "checked"

-- | @Selector@ for @setChecked:@
setCheckedSelector :: Selector '[Bool] ()
setCheckedSelector = mkSelector "setChecked:"

-- | @Selector@ for @disabled@
disabledSelector :: Selector '[] Bool
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector '[Bool] ()
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @form@
formSelector :: Selector '[] (Id DOMHTMLFormElement)
formSelector = mkSelector "form"

-- | @Selector@ for @files@
filesSelector :: Selector '[] (Id DOMFileList)
filesSelector = mkSelector "files"

-- | @Selector@ for @setFiles:@
setFilesSelector :: Selector '[Id DOMFileList] ()
setFilesSelector = mkSelector "setFiles:"

-- | @Selector@ for @indeterminate@
indeterminateSelector :: Selector '[] Bool
indeterminateSelector = mkSelector "indeterminate"

-- | @Selector@ for @setIndeterminate:@
setIndeterminateSelector :: Selector '[Bool] ()
setIndeterminateSelector = mkSelector "setIndeterminate:"

-- | @Selector@ for @maxLength@
maxLengthSelector :: Selector '[] CInt
maxLengthSelector = mkSelector "maxLength"

-- | @Selector@ for @setMaxLength:@
setMaxLengthSelector :: Selector '[CInt] ()
setMaxLengthSelector = mkSelector "setMaxLength:"

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

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector '[Bool] ()
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] (Id NSString)
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[Id NSString] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @src@
srcSelector :: Selector '[] (Id NSString)
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector '[Id NSString] ()
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

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

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @useMap@
useMapSelector :: Selector '[] (Id NSString)
useMapSelector = mkSelector "useMap"

-- | @Selector@ for @setUseMap:@
setUseMapSelector :: Selector '[Id NSString] ()
setUseMapSelector = mkSelector "setUseMap:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

-- | @Selector@ for @altDisplayString@
altDisplayStringSelector :: Selector '[] (Id NSString)
altDisplayStringSelector = mkSelector "altDisplayString"

-- | @Selector@ for @absoluteImageURL@
absoluteImageURLSelector :: Selector '[] (Id NSURL)
absoluteImageURLSelector = mkSelector "absoluteImageURL"

