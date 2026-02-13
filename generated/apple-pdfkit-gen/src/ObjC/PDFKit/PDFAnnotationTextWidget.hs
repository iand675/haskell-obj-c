{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationTextWidget@.
module ObjC.PDFKit.PDFAnnotationTextWidget
  ( PDFAnnotationTextWidget
  , IsPDFAnnotationTextWidget(..)
  , stringValue
  , setStringValue
  , attributedStringValue
  , setAttributedStringValue
  , backgroundColor
  , setBackgroundColor
  , rotation
  , setRotation
  , font
  , setFont
  , fontColor
  , setFontColor
  , alignment
  , setAlignment
  , maximumLength
  , setMaximumLength
  , fieldName
  , setFieldName
  , isMultiline
  , setIsMultiline
  , alignmentSelector
  , attributedStringValueSelector
  , backgroundColorSelector
  , fieldNameSelector
  , fontColorSelector
  , fontSelector
  , isMultilineSelector
  , maximumLengthSelector
  , rotationSelector
  , setAlignmentSelector
  , setAttributedStringValueSelector
  , setBackgroundColorSelector
  , setFieldNameSelector
  , setFontColorSelector
  , setFontSelector
  , setIsMultilineSelector
  , setMaximumLengthSelector
  , setRotationSelector
  , setStringValueSelector
  , stringValueSelector

  -- * Enum types
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stringValue@
stringValue :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSString)
stringValue pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget stringValueSelector

-- | @- setStringValue:@
setStringValue :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSString value) => pdfAnnotationTextWidget -> value -> IO ()
setStringValue pdfAnnotationTextWidget value =
  sendMessage pdfAnnotationTextWidget setStringValueSelector (toNSString value)

-- | @- attributedStringValue@
attributedStringValue :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSAttributedString)
attributedStringValue pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget attributedStringValueSelector

-- | @- setAttributedStringValue:@
setAttributedStringValue :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSAttributedString value) => pdfAnnotationTextWidget -> value -> IO ()
setAttributedStringValue pdfAnnotationTextWidget value =
  sendMessage pdfAnnotationTextWidget setAttributedStringValueSelector (toNSAttributedString value)

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSColor)
backgroundColor pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSColor color) => pdfAnnotationTextWidget -> color -> IO ()
setBackgroundColor pdfAnnotationTextWidget color =
  sendMessage pdfAnnotationTextWidget setBackgroundColorSelector (toNSColor color)

-- | @- rotation@
rotation :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO CLong
rotation pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget rotationSelector

-- | @- setRotation:@
setRotation :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> CInt -> IO ()
setRotation pdfAnnotationTextWidget rotation =
  sendMessage pdfAnnotationTextWidget setRotationSelector rotation

-- | @- font@
font :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSFont)
font pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget fontSelector

-- | @- setFont:@
setFont :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSFont font) => pdfAnnotationTextWidget -> font -> IO ()
setFont pdfAnnotationTextWidget font =
  sendMessage pdfAnnotationTextWidget setFontSelector (toNSFont font)

-- | @- fontColor@
fontColor :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSColor)
fontColor pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget fontColorSelector

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSColor color) => pdfAnnotationTextWidget -> color -> IO ()
setFontColor pdfAnnotationTextWidget color =
  sendMessage pdfAnnotationTextWidget setFontColorSelector (toNSColor color)

-- | @- alignment@
alignment :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO NSTextAlignment
alignment pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> NSTextAlignment -> IO ()
setAlignment pdfAnnotationTextWidget alignment =
  sendMessage pdfAnnotationTextWidget setAlignmentSelector alignment

-- | @- maximumLength@
maximumLength :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO CULong
maximumLength pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget maximumLengthSelector

-- | @- setMaximumLength:@
setMaximumLength :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> CULong -> IO ()
setMaximumLength pdfAnnotationTextWidget maxLen =
  sendMessage pdfAnnotationTextWidget setMaximumLengthSelector maxLen

-- | @- fieldName@
fieldName :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSString)
fieldName pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget fieldNameSelector

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSString name) => pdfAnnotationTextWidget -> name -> IO ()
setFieldName pdfAnnotationTextWidget name =
  sendMessage pdfAnnotationTextWidget setFieldNameSelector (toNSString name)

-- | @- isMultiline@
isMultiline :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO Bool
isMultiline pdfAnnotationTextWidget =
  sendMessage pdfAnnotationTextWidget isMultilineSelector

-- | @- setIsMultiline:@
setIsMultiline :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> Bool -> IO ()
setIsMultiline pdfAnnotationTextWidget multiline =
  sendMessage pdfAnnotationTextWidget setIsMultilineSelector multiline

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @attributedStringValue@
attributedStringValueSelector :: Selector '[] (Id NSAttributedString)
attributedStringValueSelector = mkSelector "attributedStringValue"

-- | @Selector@ for @setAttributedStringValue:@
setAttributedStringValueSelector :: Selector '[Id NSAttributedString] ()
setAttributedStringValueSelector = mkSelector "setAttributedStringValue:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector '[] CLong
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector '[CInt] ()
setRotationSelector = mkSelector "setRotation:"

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

-- | @Selector@ for @alignment@
alignmentSelector :: Selector '[] NSTextAlignment
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector '[NSTextAlignment] ()
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @maximumLength@
maximumLengthSelector :: Selector '[] CULong
maximumLengthSelector = mkSelector "maximumLength"

-- | @Selector@ for @setMaximumLength:@
setMaximumLengthSelector :: Selector '[CULong] ()
setMaximumLengthSelector = mkSelector "setMaximumLength:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector '[] (Id NSString)
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector '[Id NSString] ()
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @isMultiline@
isMultilineSelector :: Selector '[] Bool
isMultilineSelector = mkSelector "isMultiline"

-- | @Selector@ for @setIsMultiline:@
setIsMultilineSelector :: Selector '[Bool] ()
setIsMultilineSelector = mkSelector "setIsMultiline:"

