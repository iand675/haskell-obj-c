{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationFreeText@.
module ObjC.PDFKit.PDFAnnotationFreeText
  ( PDFAnnotationFreeText
  , IsPDFAnnotationFreeText(..)
  , font
  , setFont
  , fontColor
  , setFontColor
  , alignment
  , setAlignment
  , fontSelector
  , setFontSelector
  , fontColorSelector
  , setFontColorSelector
  , alignmentSelector
  , setAlignmentSelector

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

-- | @- font@
font :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> IO (Id NSFont)
font pdfAnnotationFreeText  =
  sendMsg pdfAnnotationFreeText (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsPDFAnnotationFreeText pdfAnnotationFreeText, IsNSFont font) => pdfAnnotationFreeText -> font -> IO ()
setFont pdfAnnotationFreeText  font =
withObjCPtr font $ \raw_font ->
    sendMsg pdfAnnotationFreeText (mkSelector "setFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @- fontColor@
fontColor :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> IO (Id NSColor)
fontColor pdfAnnotationFreeText  =
  sendMsg pdfAnnotationFreeText (mkSelector "fontColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationFreeText pdfAnnotationFreeText, IsNSColor color) => pdfAnnotationFreeText -> color -> IO ()
setFontColor pdfAnnotationFreeText  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationFreeText (mkSelector "setFontColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- | @- alignment@
alignment :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> IO NSTextAlignment
alignment pdfAnnotationFreeText  =
  fmap (coerce :: CLong -> NSTextAlignment) $ sendMsg pdfAnnotationFreeText (mkSelector "alignment") retCLong []

-- | @- setAlignment:@
setAlignment :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> NSTextAlignment -> IO ()
setAlignment pdfAnnotationFreeText  alignment =
  sendMsg pdfAnnotationFreeText (mkSelector "setAlignment:") retVoid [argCLong (coerce alignment)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

