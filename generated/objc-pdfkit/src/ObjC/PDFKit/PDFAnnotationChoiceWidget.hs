{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationChoiceWidget@.
module ObjC.PDFKit.PDFAnnotationChoiceWidget
  ( PDFAnnotationChoiceWidget
  , IsPDFAnnotationChoiceWidget(..)
  , stringValue
  , setStringValue
  , backgroundColor
  , setBackgroundColor
  , font
  , setFont
  , fontColor
  , setFontColor
  , fieldName
  , setFieldName
  , isListChoice
  , setIsListChoice
  , choices
  , setChoices
  , stringValueSelector
  , setStringValueSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , fontSelector
  , setFontSelector
  , fontColorSelector
  , setFontColorSelector
  , fieldNameSelector
  , setFieldNameSelector
  , isListChoiceSelector
  , setIsListChoiceSelector
  , choicesSelector
  , setChoicesSelector


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

import ObjC.PDFKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stringValue@
stringValue :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSString)
stringValue pdfAnnotationChoiceWidget  =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStringValue:@
setStringValue :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSString value) => pdfAnnotationChoiceWidget -> value -> IO ()
setStringValue pdfAnnotationChoiceWidget  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAnnotationChoiceWidget (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSColor)
backgroundColor pdfAnnotationChoiceWidget  =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSColor color) => pdfAnnotationChoiceWidget -> color -> IO ()
setBackgroundColor pdfAnnotationChoiceWidget  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationChoiceWidget (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- font@
font :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSFont)
font pdfAnnotationChoiceWidget  =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSFont font) => pdfAnnotationChoiceWidget -> font -> IO ()
setFont pdfAnnotationChoiceWidget  font =
withObjCPtr font $ \raw_font ->
    sendMsg pdfAnnotationChoiceWidget (mkSelector "setFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @- fontColor@
fontColor :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSColor)
fontColor pdfAnnotationChoiceWidget  =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "fontColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSColor color) => pdfAnnotationChoiceWidget -> color -> IO ()
setFontColor pdfAnnotationChoiceWidget  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationChoiceWidget (mkSelector "setFontColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- fieldName@
fieldName :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSString)
fieldName pdfAnnotationChoiceWidget  =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "fieldName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSString name) => pdfAnnotationChoiceWidget -> name -> IO ()
setFieldName pdfAnnotationChoiceWidget  name =
withObjCPtr name $ \raw_name ->
    sendMsg pdfAnnotationChoiceWidget (mkSelector "setFieldName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- isListChoice@
isListChoice :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO Bool
isListChoice pdfAnnotationChoiceWidget  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotationChoiceWidget (mkSelector "isListChoice") retCULong []

-- | @- setIsListChoice:@
setIsListChoice :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> Bool -> IO ()
setIsListChoice pdfAnnotationChoiceWidget  isList =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "setIsListChoice:") retVoid [argCULong (if isList then 1 else 0)]

-- | @- choices@
choices :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSArray)
choices pdfAnnotationChoiceWidget  =
  sendMsg pdfAnnotationChoiceWidget (mkSelector "choices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChoices:@
setChoices :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSArray options) => pdfAnnotationChoiceWidget -> options -> IO ()
setChoices pdfAnnotationChoiceWidget  options =
withObjCPtr options $ \raw_options ->
    sendMsg pdfAnnotationChoiceWidget (mkSelector "setChoices:") retVoid [argPtr (castPtr raw_options :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @isListChoice@
isListChoiceSelector :: Selector
isListChoiceSelector = mkSelector "isListChoice"

-- | @Selector@ for @setIsListChoice:@
setIsListChoiceSelector :: Selector
setIsListChoiceSelector = mkSelector "setIsListChoice:"

-- | @Selector@ for @choices@
choicesSelector :: Selector
choicesSelector = mkSelector "choices"

-- | @Selector@ for @setChoices:@
setChoicesSelector :: Selector
setChoicesSelector = mkSelector "setChoices:"

