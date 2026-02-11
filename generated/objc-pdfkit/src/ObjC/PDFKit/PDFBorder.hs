{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFBorder@.
module ObjC.PDFKit.PDFBorder
  ( PDFBorder
  , IsPDFBorder(..)
  , drawInRect
  , style
  , setStyle
  , lineWidth
  , setLineWidth
  , dashPattern
  , setDashPattern
  , borderKeyValues
  , drawInRectSelector
  , styleSelector
  , setStyleSelector
  , lineWidthSelector
  , setLineWidthSelector
  , dashPatternSelector
  , setDashPatternSelector
  , borderKeyValuesSelector

  -- * Enum types
  , PDFBorderStyle(PDFBorderStyle)
  , pattern KPDFBorderStyleSolid
  , pattern KPDFBorderStyleDashed
  , pattern KPDFBorderStyleBeveled
  , pattern KPDFBorderStyleInset
  , pattern KPDFBorderStyleUnderline

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
import ObjC.Foundation.Internal.Structs
import ObjC.PDFKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- drawInRect:@
drawInRect :: IsPDFBorder pdfBorder => pdfBorder -> NSRect -> IO ()
drawInRect pdfBorder  rect =
  sendMsg pdfBorder (mkSelector "drawInRect:") retVoid [argNSRect rect]

-- | @- style@
style :: IsPDFBorder pdfBorder => pdfBorder -> IO PDFBorderStyle
style pdfBorder  =
  fmap (coerce :: CLong -> PDFBorderStyle) $ sendMsg pdfBorder (mkSelector "style") retCLong []

-- | @- setStyle:@
setStyle :: IsPDFBorder pdfBorder => pdfBorder -> PDFBorderStyle -> IO ()
setStyle pdfBorder  value =
  sendMsg pdfBorder (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- | @- lineWidth@
lineWidth :: IsPDFBorder pdfBorder => pdfBorder -> IO CDouble
lineWidth pdfBorder  =
  sendMsg pdfBorder (mkSelector "lineWidth") retCDouble []

-- | @- setLineWidth:@
setLineWidth :: IsPDFBorder pdfBorder => pdfBorder -> CDouble -> IO ()
setLineWidth pdfBorder  value =
  sendMsg pdfBorder (mkSelector "setLineWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- dashPattern@
dashPattern :: IsPDFBorder pdfBorder => pdfBorder -> IO (Id NSArray)
dashPattern pdfBorder  =
  sendMsg pdfBorder (mkSelector "dashPattern") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDashPattern:@
setDashPattern :: (IsPDFBorder pdfBorder, IsNSArray value) => pdfBorder -> value -> IO ()
setDashPattern pdfBorder  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfBorder (mkSelector "setDashPattern:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- borderKeyValues@
borderKeyValues :: IsPDFBorder pdfBorder => pdfBorder -> IO (Id NSDictionary)
borderKeyValues pdfBorder  =
  sendMsg pdfBorder (mkSelector "borderKeyValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawInRect:@
drawInRectSelector :: Selector
drawInRectSelector = mkSelector "drawInRect:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @dashPattern@
dashPatternSelector :: Selector
dashPatternSelector = mkSelector "dashPattern"

-- | @Selector@ for @setDashPattern:@
setDashPatternSelector :: Selector
setDashPatternSelector = mkSelector "setDashPattern:"

-- | @Selector@ for @borderKeyValues@
borderKeyValuesSelector :: Selector
borderKeyValuesSelector = mkSelector "borderKeyValues"

