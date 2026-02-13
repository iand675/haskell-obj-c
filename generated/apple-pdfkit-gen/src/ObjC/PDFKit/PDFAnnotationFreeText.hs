{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , alignmentSelector
  , fontColorSelector
  , fontSelector
  , setAlignmentSelector
  , setFontColorSelector
  , setFontSelector

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

-- | @- font@
font :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> IO (Id NSFont)
font pdfAnnotationFreeText =
  sendMessage pdfAnnotationFreeText fontSelector

-- | @- setFont:@
setFont :: (IsPDFAnnotationFreeText pdfAnnotationFreeText, IsNSFont font) => pdfAnnotationFreeText -> font -> IO ()
setFont pdfAnnotationFreeText font =
  sendMessage pdfAnnotationFreeText setFontSelector (toNSFont font)

-- | @- fontColor@
fontColor :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> IO (Id NSColor)
fontColor pdfAnnotationFreeText =
  sendMessage pdfAnnotationFreeText fontColorSelector

-- | @- setFontColor:@
setFontColor :: (IsPDFAnnotationFreeText pdfAnnotationFreeText, IsNSColor color) => pdfAnnotationFreeText -> color -> IO ()
setFontColor pdfAnnotationFreeText color =
  sendMessage pdfAnnotationFreeText setFontColorSelector (toNSColor color)

-- | @- alignment@
alignment :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> IO NSTextAlignment
alignment pdfAnnotationFreeText =
  sendMessage pdfAnnotationFreeText alignmentSelector

-- | @- setAlignment:@
setAlignment :: IsPDFAnnotationFreeText pdfAnnotationFreeText => pdfAnnotationFreeText -> NSTextAlignment -> IO ()
setAlignment pdfAnnotationFreeText alignment =
  sendMessage pdfAnnotationFreeText setAlignmentSelector alignment

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

