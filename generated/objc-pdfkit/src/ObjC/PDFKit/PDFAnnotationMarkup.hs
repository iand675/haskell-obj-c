{-# LANGUAGE PatternSynonyms #-}
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
  , quadrilateralPointsSelector
  , setQuadrilateralPointsSelector
  , markupTypeSelector
  , setMarkupTypeSelector

  -- * Enum types
  , PDFMarkupType(PDFMarkupType)
  , pattern KPDFMarkupTypeHighlight
  , pattern KPDFMarkupTypeStrikeOut
  , pattern KPDFMarkupTypeUnderline
  , pattern KPDFMarkupTypeRedact

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

-- | @- quadrilateralPoints@
quadrilateralPoints :: IsPDFAnnotationMarkup pdfAnnotationMarkup => pdfAnnotationMarkup -> IO (Id NSArray)
quadrilateralPoints pdfAnnotationMarkup  =
  sendMsg pdfAnnotationMarkup (mkSelector "quadrilateralPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuadrilateralPoints:@
setQuadrilateralPoints :: (IsPDFAnnotationMarkup pdfAnnotationMarkup, IsNSArray points) => pdfAnnotationMarkup -> points -> IO ()
setQuadrilateralPoints pdfAnnotationMarkup  points =
withObjCPtr points $ \raw_points ->
    sendMsg pdfAnnotationMarkup (mkSelector "setQuadrilateralPoints:") retVoid [argPtr (castPtr raw_points :: Ptr ())]

-- | @- markupType@
markupType :: IsPDFAnnotationMarkup pdfAnnotationMarkup => pdfAnnotationMarkup -> IO PDFMarkupType
markupType pdfAnnotationMarkup  =
  fmap (coerce :: CLong -> PDFMarkupType) $ sendMsg pdfAnnotationMarkup (mkSelector "markupType") retCLong []

-- | @- setMarkupType:@
setMarkupType :: IsPDFAnnotationMarkup pdfAnnotationMarkup => pdfAnnotationMarkup -> PDFMarkupType -> IO ()
setMarkupType pdfAnnotationMarkup  type_ =
  sendMsg pdfAnnotationMarkup (mkSelector "setMarkupType:") retVoid [argCLong (coerce type_)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quadrilateralPoints@
quadrilateralPointsSelector :: Selector
quadrilateralPointsSelector = mkSelector "quadrilateralPoints"

-- | @Selector@ for @setQuadrilateralPoints:@
setQuadrilateralPointsSelector :: Selector
setQuadrilateralPointsSelector = mkSelector "setQuadrilateralPoints:"

-- | @Selector@ for @markupType@
markupTypeSelector :: Selector
markupTypeSelector = mkSelector "markupType"

-- | @Selector@ for @setMarkupType:@
setMarkupTypeSelector :: Selector
setMarkupTypeSelector = mkSelector "setMarkupType:"

