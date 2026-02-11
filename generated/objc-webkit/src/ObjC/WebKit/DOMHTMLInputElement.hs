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
  , selectSelector
  , setSelectionRange_endSelector
  , clickSelector
  , acceptSelector
  , setAcceptSelector
  , altSelector
  , setAltSelector
  , autofocusSelector
  , setAutofocusSelector
  , defaultCheckedSelector
  , setDefaultCheckedSelector
  , checkedSelector
  , setCheckedSelector
  , disabledSelector
  , setDisabledSelector
  , formSelector
  , filesSelector
  , setFilesSelector
  , indeterminateSelector
  , setIndeterminateSelector
  , maxLengthSelector
  , setMaxLengthSelector
  , multipleSelector
  , setMultipleSelector
  , nameSelector
  , setNameSelector
  , readOnlySelector
  , setReadOnlySelector
  , sizeSelector
  , setSizeSelector
  , srcSelector
  , setSrcSelector
  , typeSelector
  , setTypeSelector
  , defaultValueSelector
  , setDefaultValueSelector
  , valueSelector
  , setValueSelector
  , willValidateSelector
  , selectionStartSelector
  , setSelectionStartSelector
  , selectionEndSelector
  , setSelectionEndSelector
  , alignSelector
  , setAlignSelector
  , useMapSelector
  , setUseMapSelector
  , accessKeySelector
  , setAccessKeySelector
  , altDisplayStringSelector
  , absoluteImageURLSelector


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
select :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO ()
select domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "select") retVoid []

-- | @- setSelectionRange:end:@
setSelectionRange_end :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> CInt -> IO ()
setSelectionRange_end domhtmlInputElement  start end =
  sendMsg domhtmlInputElement (mkSelector "setSelectionRange:end:") retVoid [argCInt (fromIntegral start), argCInt (fromIntegral end)]

-- | @- click@
click :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO ()
click domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "click") retVoid []

-- | @- accept@
accept :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
accept domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "accept") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccept:@
setAccept :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAccept domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setAccept:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alt@
alt :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
alt domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "alt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlt:@
setAlt :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAlt domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setAlt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- autofocus@
autofocus :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
autofocus domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "autofocus") retCULong []

-- | @- setAutofocus:@
setAutofocus :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setAutofocus domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setAutofocus:") retVoid [argCULong (if value then 1 else 0)]

-- | @- defaultChecked@
defaultChecked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
defaultChecked domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "defaultChecked") retCULong []

-- | @- setDefaultChecked:@
setDefaultChecked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setDefaultChecked domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setDefaultChecked:") retVoid [argCULong (if value then 1 else 0)]

-- | @- checked@
checked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
checked domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "checked") retCULong []

-- | @- setChecked:@
setChecked :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setChecked domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setChecked:") retVoid [argCULong (if value then 1 else 0)]

-- | @- disabled@
disabled :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
disabled domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "disabled") retCULong []

-- | @- setDisabled:@
setDisabled :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setDisabled domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- form@
form :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id DOMHTMLFormElement)
form domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- files@
files :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id DOMFileList)
files domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "files") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFiles:@
setFiles :: (IsDOMHTMLInputElement domhtmlInputElement, IsDOMFileList value) => domhtmlInputElement -> value -> IO ()
setFiles domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setFiles:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- indeterminate@
indeterminate :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
indeterminate domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "indeterminate") retCULong []

-- | @- setIndeterminate:@
setIndeterminate :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setIndeterminate domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setIndeterminate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- maxLength@
maxLength :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO CInt
maxLength domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "maxLength") retCInt []

-- | @- setMaxLength:@
setMaxLength :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> IO ()
setMaxLength domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setMaxLength:") retVoid [argCInt (fromIntegral value)]

-- | @- multiple@
multiple :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
multiple domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "multiple") retCULong []

-- | @- setMultiple:@
setMultiple :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setMultiple domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setMultiple:") retVoid [argCULong (if value then 1 else 0)]

-- | @- name@
name :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
name domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setName domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- readOnly@
readOnly :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
readOnly domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "readOnly") retCULong []

-- | @- setReadOnly:@
setReadOnly :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> Bool -> IO ()
setReadOnly domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setReadOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- size@
size :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
size domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "size") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSize:@
setSize :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setSize domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setSize:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- src@
src :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
src domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "src") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSrc:@
setSrc :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setSrc domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setSrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
type_ domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setType domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultValue@
defaultValue :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
defaultValue domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "defaultValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultValue:@
setDefaultValue :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setDefaultValue domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setDefaultValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
value domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setValue domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- willValidate@
willValidate :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO Bool
willValidate domhtmlInputElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlInputElement (mkSelector "willValidate") retCULong []

-- | @- selectionStart@
selectionStart :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO CInt
selectionStart domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "selectionStart") retCInt []

-- | @- setSelectionStart:@
setSelectionStart :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> IO ()
setSelectionStart domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setSelectionStart:") retVoid [argCInt (fromIntegral value)]

-- | @- selectionEnd@
selectionEnd :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO CInt
selectionEnd domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "selectionEnd") retCInt []

-- | @- setSelectionEnd:@
setSelectionEnd :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> CInt -> IO ()
setSelectionEnd domhtmlInputElement  value =
  sendMsg domhtmlInputElement (mkSelector "setSelectionEnd:") retVoid [argCInt (fromIntegral value)]

-- | @- align@
align :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
align domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAlign domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- useMap@
useMap :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
useMap domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "useMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUseMap:@
setUseMap :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setUseMap domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setUseMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessKey@
accessKey :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
accessKey domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLInputElement domhtmlInputElement, IsNSString value) => domhtmlInputElement -> value -> IO ()
setAccessKey domhtmlInputElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlInputElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- altDisplayString@
altDisplayString :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSString)
altDisplayString domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "altDisplayString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteImageURL@
absoluteImageURL :: IsDOMHTMLInputElement domhtmlInputElement => domhtmlInputElement -> IO (Id NSURL)
absoluteImageURL domhtmlInputElement  =
  sendMsg domhtmlInputElement (mkSelector "absoluteImageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @select@
selectSelector :: Selector
selectSelector = mkSelector "select"

-- | @Selector@ for @setSelectionRange:end:@
setSelectionRange_endSelector :: Selector
setSelectionRange_endSelector = mkSelector "setSelectionRange:end:"

-- | @Selector@ for @click@
clickSelector :: Selector
clickSelector = mkSelector "click"

-- | @Selector@ for @accept@
acceptSelector :: Selector
acceptSelector = mkSelector "accept"

-- | @Selector@ for @setAccept:@
setAcceptSelector :: Selector
setAcceptSelector = mkSelector "setAccept:"

-- | @Selector@ for @alt@
altSelector :: Selector
altSelector = mkSelector "alt"

-- | @Selector@ for @setAlt:@
setAltSelector :: Selector
setAltSelector = mkSelector "setAlt:"

-- | @Selector@ for @autofocus@
autofocusSelector :: Selector
autofocusSelector = mkSelector "autofocus"

-- | @Selector@ for @setAutofocus:@
setAutofocusSelector :: Selector
setAutofocusSelector = mkSelector "setAutofocus:"

-- | @Selector@ for @defaultChecked@
defaultCheckedSelector :: Selector
defaultCheckedSelector = mkSelector "defaultChecked"

-- | @Selector@ for @setDefaultChecked:@
setDefaultCheckedSelector :: Selector
setDefaultCheckedSelector = mkSelector "setDefaultChecked:"

-- | @Selector@ for @checked@
checkedSelector :: Selector
checkedSelector = mkSelector "checked"

-- | @Selector@ for @setChecked:@
setCheckedSelector :: Selector
setCheckedSelector = mkSelector "setChecked:"

-- | @Selector@ for @disabled@
disabledSelector :: Selector
disabledSelector = mkSelector "disabled"

-- | @Selector@ for @setDisabled:@
setDisabledSelector :: Selector
setDisabledSelector = mkSelector "setDisabled:"

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

-- | @Selector@ for @files@
filesSelector :: Selector
filesSelector = mkSelector "files"

-- | @Selector@ for @setFiles:@
setFilesSelector :: Selector
setFilesSelector = mkSelector "setFiles:"

-- | @Selector@ for @indeterminate@
indeterminateSelector :: Selector
indeterminateSelector = mkSelector "indeterminate"

-- | @Selector@ for @setIndeterminate:@
setIndeterminateSelector :: Selector
setIndeterminateSelector = mkSelector "setIndeterminate:"

-- | @Selector@ for @maxLength@
maxLengthSelector :: Selector
maxLengthSelector = mkSelector "maxLength"

-- | @Selector@ for @setMaxLength:@
setMaxLengthSelector :: Selector
setMaxLengthSelector = mkSelector "setMaxLength:"

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

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @src@
srcSelector :: Selector
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector
setSrcSelector = mkSelector "setSrc:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

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

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @useMap@
useMapSelector :: Selector
useMapSelector = mkSelector "useMap"

-- | @Selector@ for @setUseMap:@
setUseMapSelector :: Selector
setUseMapSelector = mkSelector "setUseMap:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

-- | @Selector@ for @altDisplayString@
altDisplayStringSelector :: Selector
altDisplayStringSelector = mkSelector "altDisplayString"

-- | @Selector@ for @absoluteImageURL@
absoluteImageURLSelector :: Selector
absoluteImageURLSelector = mkSelector "absoluteImageURL"

