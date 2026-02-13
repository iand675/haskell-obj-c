{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationMarkup@.
module ObjC.PDFKit.PDFAnnotationMarkup
  ( PDFAnnotationMarkup
  , IsPDFAnnotationMarkup(..)
  , quadrilateralPoints
  , setQuadrilateralPoints
  , markupType
  , setMarkupType
  , markupTypeSelector
  , quadrilateralPointsSelector
  , setMarkupTypeSelector
  , setQuadrilateralPointsSelector

  -- * Enum types
  , PDFMarkupType(PDFMarkupType)
  , pattern KPDFMarkupTypeHighlight
  , pattern KPDFMarkupTypeStrikeOut
  , pattern KPDFMarkupTypeUnderline
  , pattern KPDFMarkupTypeRedact

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

-- | @- quadrilateralPoints@
quadrilateralPoints :: IsPDFAnnotationMarkup pdfAnnotationMarkup => pdfAnnotationMarkup -> IO (Id NSArray)
quadrilateralPoints pdfAnnotationMarkup =
  sendMessage pdfAnnotationMarkup quadrilateralPointsSelector

-- | @- setQuadrilateralPoints:@
setQuadrilateralPoints :: (IsPDFAnnotationMarkup pdfAnnotationMarkup, IsNSArray points) => pdfAnnotationMarkup -> points -> IO ()
setQuadrilateralPoints pdfAnnotationMarkup points =
  sendMessage pdfAnnotationMarkup setQuadrilateralPointsSelector (toNSArray points)

-- | @- markupType@
markupType :: IsPDFAnnotationMarkup pdfAnnotationMarkup => pdfAnnotationMarkup -> IO PDFMarkupType
markupType pdfAnnotationMarkup =
  sendMessage pdfAnnotationMarkup markupTypeSelector

-- | @- setMarkupType:@
setMarkupType :: IsPDFAnnotationMarkup pdfAnnotationMarkup => pdfAnnotationMarkup -> PDFMarkupType -> IO ()
setMarkupType pdfAnnotationMarkup type_ =
  sendMessage pdfAnnotationMarkup setMarkupTypeSelector type_

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quadrilateralPoints@
quadrilateralPointsSelector :: Selector '[] (Id NSArray)
quadrilateralPointsSelector = mkSelector "quadrilateralPoints"

-- | @Selector@ for @setQuadrilateralPoints:@
setQuadrilateralPointsSelector :: Selector '[Id NSArray] ()
setQuadrilateralPointsSelector = mkSelector "setQuadrilateralPoints:"

-- | @Selector@ for @markupType@
markupTypeSelector :: Selector '[] PDFMarkupType
markupTypeSelector = mkSelector "markupType"

-- | @Selector@ for @setMarkupType:@
setMarkupTypeSelector :: Selector '[PDFMarkupType] ()
setMarkupTypeSelector = mkSelector "setMarkupType:"

