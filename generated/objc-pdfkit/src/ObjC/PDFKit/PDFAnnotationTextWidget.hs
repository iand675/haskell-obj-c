{-# LANGUAGE PatternSynonyms #-}
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
  , stringValueSelector
  , setStringValueSelector
  , attributedStringValueSelector
  , setAttributedStringValueSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , rotationSelector
  , setRotationSelector
  , fontSelector
  , setFontSelector
  , fontColorSelector
  , setFontColorSelector
  , alignmentSelector
  , setAlignmentSelector
  , maximumLengthSelector
  , setMaximumLengthSelector
  , fieldNameSelector
  , setFieldNameSelector
  , isMultilineSelector
  , setIsMultilineSelector

  -- * Enum types
  , NSTextAlignment(NSTextAlignment)
  , pattern NSTextAlignmentLeft
  , pattern NSTextAlignmentCenter
  , pattern NSTextAlignmentRight
  , pattern NSTextAlignmentJustified
  , pattern NSTextAlignmentNatural

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
import ObjC.AppKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stringValue@
stringValue :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSString)
stringValue pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStringValue:@
setStringValue :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSString value) => pdfAnnotationTextWidget -> value -> IO ()
setStringValue pdfAnnotationTextWidget  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAnnotationTextWidget (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedStringValue@
attributedStringValue :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSAttributedString)
attributedStringValue pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "attributedStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedStringValue:@
setAttributedStringValue :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSAttributedString value) => pdfAnnotationTextWidget -> value -> IO ()
setAttributedStringValue pdfAnnotationTextWidget  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfAnnotationTextWidget (mkSelector "setAttributedStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSColor)
backgroundColor pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSColor color) => pdfAnnotationTextWidget -> color -> IO ()
setBackgroundColor pdfAnnotationTextWidget  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationTextWidget (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- rotation@
rotation :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO CLong
rotation pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "rotation") retCLong []

-- | @- setRotation:@
setRotation :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> CInt -> IO ()
setRotation pdfAnnotationTextWidget  rotation =
  sendMsg pdfAnnotationTextWidget (mkSelector "setRotation:") retVoid [argCInt (fromIntegral rotation)]

-- | @- font@
font :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSFont)
font pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSFont font) => pdfAnnotationTextWidget -> font -> IO ()
setFont pdfAnnotationTextWidget  font =
withObjCPtr font $ \raw_font ->
    sendMsg pdfAnnotationTextWidget (mkSelector "setFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @- fontColor@
fontColor :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSColor)
fontColor pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "fontColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSColor color) => pdfAnnotationTextWidget -> color -> IO ()
setFontColor pdfAnnotationTextWidget  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationTextWidget (mkSelector "setFontColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- alignment@
alignment :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO NSTextAlignment
alignment pdfAnnotationTextWidget  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg pdfAnnotationTextWidget (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> NSTextAlignment -> IO ()
setAlignment pdfAnnotationTextWidget  alignment =
  sendMsg pdfAnnotationTextWidget (mkSelector "setAlignment:") retVoid [argCLong (coerce alignment)]

-- | @- maximumLength@
maximumLength :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO CULong
maximumLength pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "maximumLength") retCULong []

-- | @- setMaximumLength:@
setMaximumLength :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> CULong -> IO ()
setMaximumLength pdfAnnotationTextWidget  maxLen =
  sendMsg pdfAnnotationTextWidget (mkSelector "setMaximumLength:") retVoid [argCULong (fromIntegral maxLen)]

-- | @- fieldName@
fieldName :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO (Id NSString)
fieldName pdfAnnotationTextWidget  =
  sendMsg pdfAnnotationTextWidget (mkSelector "fieldName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFieldName:@
setFieldName :: (IsPDFAnnotationTextWidget pdfAnnotationTextWidget, IsNSString name) => pdfAnnotationTextWidget -> name -> IO ()
setFieldName pdfAnnotationTextWidget  name =
withObjCPtr name $ \raw_name ->
    sendMsg pdfAnnotationTextWidget (mkSelector "setFieldName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- isMultiline@
isMultiline :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> IO Bool
isMultiline pdfAnnotationTextWidget  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfAnnotationTextWidget (mkSelector "isMultiline") retCULong []

-- | @- setIsMultiline:@
setIsMultiline :: IsPDFAnnotationTextWidget pdfAnnotationTextWidget => pdfAnnotationTextWidget -> Bool -> IO ()
setIsMultiline pdfAnnotationTextWidget  multiline =
  sendMsg pdfAnnotationTextWidget (mkSelector "setIsMultiline:") retVoid [argCULong (if multiline then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @attributedStringValue@
attributedStringValueSelector :: Selector
attributedStringValueSelector = mkSelector "attributedStringValue"

-- | @Selector@ for @setAttributedStringValue:@
setAttributedStringValueSelector :: Selector
setAttributedStringValueSelector = mkSelector "setAttributedStringValue:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @setRotation:@
setRotationSelector :: Selector
setRotationSelector = mkSelector "setRotation:"

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

-- | @Selector@ for @alignment@
alignmentSelector :: Selector
alignmentSelector = mkSelector "alignment"

-- | @Selector@ for @setAlignment:@
setAlignmentSelector :: Selector
setAlignmentSelector = mkSelector "setAlignment:"

-- | @Selector@ for @maximumLength@
maximumLengthSelector :: Selector
maximumLengthSelector = mkSelector "maximumLength"

-- | @Selector@ for @setMaximumLength:@
setMaximumLengthSelector :: Selector
setMaximumLengthSelector = mkSelector "setMaximumLength:"

-- | @Selector@ for @fieldName@
fieldNameSelector :: Selector
fieldNameSelector = mkSelector "fieldName"

-- | @Selector@ for @setFieldName:@
setFieldNameSelector :: Selector
setFieldNameSelector = mkSelector "setFieldName:"

-- | @Selector@ for @isMultiline@
isMultilineSelector :: Selector
isMultilineSelector = mkSelector "isMultiline"

-- | @Selector@ for @setIsMultiline:@
setIsMultilineSelector :: Selector
setIsMultilineSelector = mkSelector "setIsMultiline:"

