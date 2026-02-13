{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationCircle@.
module ObjC.PDFKit.PDFAnnotationCircle
  ( PDFAnnotationCircle
  , IsPDFAnnotationCircle(..)
  , interiorColor
  , setInteriorColor
  , interiorColorSelector
  , setInteriorColorSelector


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

-- | @- interiorColor@
interiorColor :: IsPDFAnnotationCircle pdfAnnotationCircle => pdfAnnotationCircle -> IO (Id NSColor)
interiorColor pdfAnnotationCircle =
  sendMessage pdfAnnotationCircle interiorColorSelector

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotationCircle pdfAnnotationCircle, IsNSColor color) => pdfAnnotationCircle -> color -> IO ()
setInteriorColor pdfAnnotationCircle color =
  sendMessage pdfAnnotationCircle setInteriorColorSelector (toNSColor color)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector '[] (Id NSColor)
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector '[Id NSColor] ()
setInteriorColorSelector = mkSelector "setInteriorColor:"

