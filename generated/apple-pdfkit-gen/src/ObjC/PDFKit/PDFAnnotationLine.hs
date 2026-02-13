{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , endLineStyleSelector
  , endPointSelector
  , interiorColorSelector
  , setEndLineStyleSelector
  , setEndPointSelector
  , setInteriorColorSelector
  , setStartLineStyleSelector
  , setStartPointSelector
  , startLineStyleSelector
  , startPointSelector

  -- * Enum types
  , PDFLineStyle(PDFLineStyle)
  , pattern KPDFLineStyleNone
  , pattern KPDFLineStyleSquare
  , pattern KPDFLineStyleCircle
  , pattern KPDFLineStyleDiamond
  , pattern KPDFLineStyleOpenArrow
  , pattern KPDFLineStyleClosedArrow

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- startPoint@
startPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO NSPoint
startPoint pdfAnnotationLine =
  sendMessage pdfAnnotationLine startPointSelector

-- | @- setStartPoint:@
setStartPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> NSPoint -> IO ()
setStartPoint pdfAnnotationLine point =
  sendMessage pdfAnnotationLine setStartPointSelector point

-- | @- endPoint@
endPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO NSPoint
endPoint pdfAnnotationLine =
  sendMessage pdfAnnotationLine endPointSelector

-- | @- setEndPoint:@
setEndPoint :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> NSPoint -> IO ()
setEndPoint pdfAnnotationLine point =
  sendMessage pdfAnnotationLine setEndPointSelector point

-- | @- startLineStyle@
startLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO PDFLineStyle
startLineStyle pdfAnnotationLine =
  sendMessage pdfAnnotationLine startLineStyleSelector

-- | @- setStartLineStyle:@
setStartLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> PDFLineStyle -> IO ()
setStartLineStyle pdfAnnotationLine style =
  sendMessage pdfAnnotationLine setStartLineStyleSelector style

-- | @- endLineStyle@
endLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO PDFLineStyle
endLineStyle pdfAnnotationLine =
  sendMessage pdfAnnotationLine endLineStyleSelector

-- | @- setEndLineStyle:@
setEndLineStyle :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> PDFLineStyle -> IO ()
setEndLineStyle pdfAnnotationLine style =
  sendMessage pdfAnnotationLine setEndLineStyleSelector style

-- | @- interiorColor@
interiorColor :: IsPDFAnnotationLine pdfAnnotationLine => pdfAnnotationLine -> IO (Id NSColor)
interiorColor pdfAnnotationLine =
  sendMessage pdfAnnotationLine interiorColorSelector

-- | @- setInteriorColor:@
setInteriorColor :: (IsPDFAnnotationLine pdfAnnotationLine, IsNSColor color) => pdfAnnotationLine -> color -> IO ()
setInteriorColor pdfAnnotationLine color =
  sendMessage pdfAnnotationLine setInteriorColorSelector (toNSColor color)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startPoint@
startPointSelector :: Selector '[] NSPoint
startPointSelector = mkSelector "startPoint"

-- | @Selector@ for @setStartPoint:@
setStartPointSelector :: Selector '[NSPoint] ()
setStartPointSelector = mkSelector "setStartPoint:"

-- | @Selector@ for @endPoint@
endPointSelector :: Selector '[] NSPoint
endPointSelector = mkSelector "endPoint"

-- | @Selector@ for @setEndPoint:@
setEndPointSelector :: Selector '[NSPoint] ()
setEndPointSelector = mkSelector "setEndPoint:"

-- | @Selector@ for @startLineStyle@
startLineStyleSelector :: Selector '[] PDFLineStyle
startLineStyleSelector = mkSelector "startLineStyle"

-- | @Selector@ for @setStartLineStyle:@
setStartLineStyleSelector :: Selector '[PDFLineStyle] ()
setStartLineStyleSelector = mkSelector "setStartLineStyle:"

-- | @Selector@ for @endLineStyle@
endLineStyleSelector :: Selector '[] PDFLineStyle
endLineStyleSelector = mkSelector "endLineStyle"

-- | @Selector@ for @setEndLineStyle:@
setEndLineStyleSelector :: Selector '[PDFLineStyle] ()
setEndLineStyleSelector = mkSelector "setEndLineStyle:"

-- | @Selector@ for @interiorColor@
interiorColorSelector :: Selector '[] (Id NSColor)
interiorColorSelector = mkSelector "interiorColor"

-- | @Selector@ for @setInteriorColor:@
setInteriorColorSelector :: Selector '[Id NSColor] ()
setInteriorColorSelector = mkSelector "setInteriorColor:"

