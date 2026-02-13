{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationSquare@.
module ObjC.PDFKit.PDFAnnotationSquare
  ( PDFAnnotationSquare
  , IsPDFAnnotationSquare(..)
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
interiorColor :: IsPDFAnnotationSquare pdfAnnotationSquare => pdfAnnotationSquare -> IO (Id NSColor)
interiorColor pdfAnnotationSquare =
  sendMessage pdfAnnotationSquare interiorColorSelector

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotationSquare pdfAnnotationSquare, IsNSColor color) => pdfAnnotationSquare -> color -> IO ()
setInteriorColor pdfAnnotationSquare color =
  sendMessage pdfAnnotationSquare setInteriorColorSelector (toNSColor color)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector '[] (Id NSColor)
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector '[Id NSColor] ()
setInteriorColorSelector = mkSelector "setInteriorColor:"

