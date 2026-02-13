{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.PDFKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- iconType@
iconType :: IsPDFAnnotationText pdfAnnotationText => pdfAnnotationText -> IO PDFTextAnnotationIconType
iconType pdfAnnotationText =
  sendMessage pdfAnnotationText iconTypeSelector

-- | @- setIconType:@
setIconType :: IsPDFAnnotationText pdfAnnotationText => pdfAnnotationText -> PDFTextAnnotationIconType -> IO ()
setIconType pdfAnnotationText type_ =
  sendMessage pdfAnnotationText setIconTypeSelector type_

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @iconType@
iconTypeSelector :: Selector '[] PDFTextAnnotationIconType
iconTypeSelector = mkSelector "iconType"

-- | @Selector@ for @setIconType:@
setIconTypeSelector :: Selector '[PDFTextAnnotationIconType] ()
setIconTypeSelector = mkSelector "setIconType:"

