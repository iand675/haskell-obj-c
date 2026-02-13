{-# LANGUAGE DataKinds #-}
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
  , backgroundColorSelector
  , choicesSelector
  , fieldNameSelector
  , fontColorSelector
  , fontSelector
  , isListChoiceSelector
  , setBackgroundColorSelector
  , setChoicesSelector
  , setFieldNameSelector
  , setFontColorSelector
  , setFontSelector
  , setIsListChoiceSelector
  , setStringValueSelector
  , stringValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stringValue@
stringValue :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSString)
stringValue pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget stringValueSelector

-- | @- setStringValue:@
setStringValue :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSString value) => pdfAnnotationChoiceWidget -> value -> IO ()
setStringValue pdfAnnotationChoiceWidget value =
  sendMessage pdfAnnotationChoiceWidget setStringValueSelector (toNSString value)

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSColor)
backgroundColor pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSColor color) => pdfAnnotationChoiceWidget -> color -> IO ()
setBackgroundColor pdfAnnotationChoiceWidget color =
  sendMessage pdfAnnotationChoiceWidget setBackgroundColorSelector (toNSColor color)

-- | @- font@
font :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSFont)
font pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget fontSelector

-- | @- setFont:@
setFont :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSFont font) => pdfAnnotationChoiceWidget -> font -> IO ()
setFont pdfAnnotationChoiceWidget font =
  sendMessage pdfAnnotationChoiceWidget setFontSelector (toNSFont font)

-- | @- fontColor@
fontColor :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSColor)
fontColor pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget fontColorSelector

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSColor color) => pdfAnnotationChoiceWidget -> color -> IO ()
setFontColor pdfAnnotationChoiceWidget color =
  sendMessage pdfAnnotationChoiceWidget setFontColorSelector (toNSColor color)

-- | @- fieldName@
fieldName :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSString)
fieldName pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget fieldNameSelector

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSString name) => pdfAnnotationChoiceWidget -> name -> IO ()
setFieldName pdfAnnotationChoiceWidget name =
  sendMessage pdfAnnotationChoiceWidget setFieldNameSelector (toNSString name)

-- | @- isListChoice@
isListChoice :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO Bool
isListChoice pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget isListChoiceSelector

-- | @- setIsListChoice:@
setIsListChoice :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> Bool -> IO ()
setIsListChoice pdfAnnotationChoiceWidget isList =
  sendMessage pdfAnnotationChoiceWidget setIsListChoiceSelector isList

-- | @- choices@
choices :: IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget => pdfAnnotationChoiceWidget -> IO (Id NSArray)
choices pdfAnnotationChoiceWidget =
  sendMessage pdfAnnotationChoiceWidget choicesSelector

-- | @- setChoices:@
setChoices :: (IsPDFAnnotationChoiceWidget pdfAnnotationChoiceWidget, IsNSArray options) => pdfAnnotationChoiceWidget -> options -> IO ()
setChoices pdfAnnotationChoiceWidget options =
  sendMessage pdfAnnotationChoiceWidget setChoicesSelector (toNSArray options)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @fontColor@
fontColorSelector :: Selector '[] (Id NSColor)
fontColorSelector = mkSelector "fontColor"

-- | @Selector@ for @setFontColor:@
setFontColorSelector :: Selector '[Id NSColor] ()
setFontColorSelector = mkSelector "setFontColor:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector '[] (Id NSString)
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector '[Id NSString] ()
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @isListChoice@
isListChoiceSelector :: Selector '[] Bool
isListChoiceSelector = mkSelector "isListChoice"

-- | @Selector@ for @setIsListChoice:@
setIsListChoiceSelector :: Selector '[Bool] ()
setIsListChoiceSelector = mkSelector "setIsListChoice:"

-- | @Selector@ for @choices@
choicesSelector :: Selector '[] (Id NSArray)
choicesSelector = mkSelector "choices"

-- | @Selector@ for @setChoices:@
setChoicesSelector :: Selector '[Id NSArray] ()
setChoicesSelector = mkSelector "setChoices:"

