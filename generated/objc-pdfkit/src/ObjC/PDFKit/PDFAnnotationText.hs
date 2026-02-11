{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationText@.
module ObjC.PDFKit.PDFAnnotationText
  ( PDFAnnotationText
  , IsPDFAnnotationText(..)
  , iconType
  , setIconType
  , iconTypeSelector
  , setIconTypeSelector

  -- * Enum types
  , PDFTextAnnotationIconType(PDFTextAnnotationIconType)
  , pattern KPDFTextAnnotationIconComment
  , pattern KPDFTextAnnotationIconKey
  , pattern KPDFTextAnnotationIconNote
  , pattern KPDFTextAnnotationIconHelp
  , pattern KPDFTextAnnotationIconNewParagraph
  , pattern KPDFTextAnnotationIconParagraph
  , pattern KPDFTextAnnotationIconInsert

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
import ObjC.PDFKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- iconType@
iconType :: IsPDFAnnotationText pdfAnnotationText => pdfAnnotationText -> IO PDFTextAnnotationIconType
iconType pdfAnnotationText  =
  fmap (coerce :: CLong -> PDFTextAnnotationIconType) $ sendMsg pdfAnnotationText (mkSelector "iconType") retCLong []

-- | @- setIconType:@
setIconType :: IsPDFAnnotationText pdfAnnotationText => pdfAnnotationText -> PDFTextAnnotationIconType -> IO ()
setIconType pdfAnnotationText  type_ =
  sendMsg pdfAnnotationText (mkSelector "setIconType:") retVoid [argCLong (coerce type_)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @iconType@
iconTypeSelector :: Selector
iconTypeSelector = mkSelector "iconType"

-- | @Selector@ for @setIconType:@
setIconTypeSelector :: Selector
setIconTypeSelector = mkSelector "setIconType:"

