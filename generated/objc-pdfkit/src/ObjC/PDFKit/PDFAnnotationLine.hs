{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFAnnotationLine@.
module ObjC.PDFKit.PDFAnnotationLine
  ( PDFAnnotationLine
  , IsPDFAnnotationLine(..)
  , startPoint
  , setStartPoint
  , endPoint
  , setEndPoint
  , startLineStyle
  , setStartLineStyle
  , endLineStyle
  , setEndLineStyle
  , interiorColor
  , setInteriorColor
  , startPointSelector
  , setStartPointSelector
  , endPointSelector
  , setEndPointSelector
  , startLineStyleSelector
  , setStartLineStyleSelector
  , endLineStyleSelector
  , setEndLineStyleSelector
  , interiorColorSelector
  , setInteriorColorSelector

  -- * Enum types
  , PDFLineStyle(PDFLineStyle)
  , pattern KPDFLineStyleNone
  , pattern KPDFLineStyleSquare
  , pattern KPDFLineStyleCircle
  , pattern KPDFLineStyleDiamond
  , pattern KPDFLineStyleOpenArrow
  , pattern KPDFLineStyleClosedArrow

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startPoint@
startPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO NSPoint
startPoint pdfAnnotationLine  =
  sendMsgStret pdfAnnotationLine (mkSelector "startPoint") retNSPoint []

-- | @- setStartPoint:@
setStartPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> NSPoint -> IO ()
setStartPoint pdfAnnotationLine  point =
  sendMsg pdfAnnotationLine (mkSelector "setStartPoint:") retVoid [argNSPoint point]

-- | @- endPoint@
endPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO NSPoint
endPoint pdfAnnotationLine  =
  sendMsgStret pdfAnnotationLine (mkSelector "endPoint") retNSPoint []

-- | @- setEndPoint:@
setEndPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> NSPoint -> IO ()
setEndPoint pdfAnnotationLine  point =
  sendMsg pdfAnnotationLine (mkSelector "setEndPoint:") retVoid [argNSPoint point]

-- | @- startLineStyle@
startLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO PDFLineStyle
startLineStyle pdfAnnotationLine  =
  fmap (coerce :: CLong -> PDFLineStyle) $ sendMsg pdfAnnotationLine (mkSelector "startLineStyle") retCLong []

-- | @- setStartLineStyle:@
setStartLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> PDFLineStyle -> IO ()
setStartLineStyle pdfAnnotationLine  style =
  sendMsg pdfAnnotationLine (mkSelector "setStartLineStyle:") retVoid [argCLong (coerce style)]

-- | @- endLineStyle@
endLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO PDFLineStyle
endLineStyle pdfAnnotationLine  =
  fmap (coerce :: CLong -> PDFLineStyle) $ sendMsg pdfAnnotationLine (mkSelector "endLineStyle") retCLong []

-- | @- setEndLineStyle:@
setEndLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> PDFLineStyle -> IO ()
setEndLineStyle pdfAnnotationLine  style =
  sendMsg pdfAnnotationLine (mkSelector "setEndLineStyle:") retVoid [argCLong (coerce style)]

-- | @- interiorColor@
interiorColor :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO (Id NSColor)
interiorColor pdfAnnotationLine  =
  sendMsg pdfAnnotationLine (mkSelector "interiorColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotationLine pdfAnnotationLine, IsNSColor color) => pdfAnnotationLine -> color -> IO ()
setInteriorColor pdfAnnotationLine  color =
withObjCPtr color $ \raw_color ->
    sendMsg pdfAnnotationLine (mkSelector "setInteriorColor:") retVoid [argPtr (castPtr raw_color :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startPoint@
startPointSelector :: Selector
startPointSelector = mkSelector "startPoint"

-- | @Selector@ for @setStartPoint:@
setStartPointSelector :: Selector
setStartPointSelector = mkSelector "setStartPoint:"

-- | @Selector@ for @endPoint@
endPointSelector :: Selector
endPointSelector = mkSelector "endPoint"

-- | @Selector@ for @setEndPoint:@
setEndPointSelector :: Selector
setEndPointSelector = mkSelector "setEndPoint:"

-- | @Selector@ for @startLineStyle@
startLineStyleSelector :: Selector
startLineStyleSelector = mkSelector "startLineStyle"

-- | @Selector@ for @setStartLineStyle:@
setStartLineStyleSelector :: Selector
setStartLineStyleSelector = mkSelector "setStartLineStyle:"

-- | @Selector@ for @endLineStyle@
endLineStyleSelector :: Selector
endLineStyleSelector = mkSelector "endLineStyle"

-- | @Selector@ for @setEndLineStyle:@
setEndLineStyleSelector :: Selector
setEndLineStyleSelector = mkSelector "setEndLineStyle:"

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector
setInteriorColorSelector = mkSelector "setInteriorColor:"

