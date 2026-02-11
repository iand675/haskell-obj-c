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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- interiorColor@
interiorColor :: IsPDFAnnotationCircle pdfAnnotationCircle => pdfAnnotationCircle -> IO (Id NSColor)
interiorColor pdfAnnotationCircle  =
  sendMsg pdfAnnotationCircle (mkSelector "interiorColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotationCircle pdfAnnotationCircle, IsNSColor color) => pdfAnnotationCircle -> color -> IO ()
setInteriorColor pdfAnnotationCircle  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationCircle (mkSelector "setInteriorColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector
setInteriorColorSelector = mkSelector "setInteriorColor:"

