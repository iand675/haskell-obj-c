{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , borderKeyValuesSelector
  , dashPatternSelector
  , drawInRectSelector
  , lineWidthSelector
  , setDashPatternSelector
  , setLineWidthSelector
  , setStyleSelector
  , styleSelector

  -- * Enum types
  , PDFBorderStyle(PDFBorderStyle)
  , pattern KPDFBorderStyleSolid
  , pattern KPDFBorderStyleDashed
  , pattern KPDFBorderStyleBeveled
  , pattern KPDFBorderStyleInset
  , pattern KPDFBorderStyleUnderline

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
import ObjC.Foundation.Internal.Classes

-- | @- drawInRect:@
drawInRect :: IsPDFBorder pdfBorder => pdfBorder -> NSRect -> IO ()
drawInRect pdfBorder rect =
  sendMessage pdfBorder drawInRectSelector rect

-- | @- style@
style :: IsPDFBorder pdfBorder => pdfBorder -> IO PDFBorderStyle
style pdfBorder =
  sendMessage pdfBorder styleSelector

-- | @- setStyle:@
setStyle :: IsPDFBorder pdfBorder => pdfBorder -> PDFBorderStyle -> IO ()
setStyle pdfBorder value =
  sendMessage pdfBorder setStyleSelector value

-- | @- lineWidth@
lineWidth :: IsPDFBorder pdfBorder => pdfBorder -> IO CDouble
lineWidth pdfBorder =
  sendMessage pdfBorder lineWidthSelector

-- | @- setLineWidth:@
setLineWidth :: IsPDFBorder pdfBorder => pdfBorder -> CDouble -> IO ()
setLineWidth pdfBorder value =
  sendMessage pdfBorder setLineWidthSelector value

-- | @- dashPattern@
dashPattern :: IsPDFBorder pdfBorder => pdfBorder -> IO (Id NSArray)
dashPattern pdfBorder =
  sendMessage pdfBorder dashPatternSelector

-- | @- setDashPattern:@
setDashPattern :: (IsPDFBorder pdfBorder, IsNSArray value) => pdfBorder -> value -> IO ()
setDashPattern pdfBorder value =
  sendMessage pdfBorder setDashPatternSelector (toNSArray value)

-- | @- borderKeyValues@
borderKeyValues :: IsPDFBorder pdfBorder => pdfBorder -> IO (Id NSDictionary)
borderKeyValues pdfBorder =
  sendMessage pdfBorder borderKeyValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawInRect:@
drawInRectSelector :: Selector '[NSRect] ()
drawInRectSelector = mkSelector "drawInRect:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] PDFBorderStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[PDFBorderStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @lineWidth@
lineWidthSelector :: Selector '[] CDouble
lineWidthSelector = mkSelector "lineWidth"

-- | @Selector@ for @setLineWidth:@
setLineWidthSelector :: Selector '[CDouble] ()
setLineWidthSelector = mkSelector "setLineWidth:"

-- | @Selector@ for @dashPattern@
dashPatternSelector :: Selector '[] (Id NSArray)
dashPatternSelector = mkSelector "dashPattern"

-- | @Selector@ for @setDashPattern:@
setDashPatternSelector :: Selector '[Id NSArray] ()
setDashPatternSelector = mkSelector "setDashPattern:"

-- | @Selector@ for @borderKeyValues@
borderKeyValuesSelector :: Selector '[] (Id NSDictionary)
borderKeyValuesSelector = mkSelector "borderKeyValues"

