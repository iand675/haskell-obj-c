{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFThumbnailView@.
module ObjC.PDFKit.PDFThumbnailView
  ( PDFThumbnailView
  , IsPDFThumbnailView(..)
  , pdfView
  , setPDFView
  , backgroundColor
  , setBackgroundColor
  , selectedPages
  , thumbnailSize
  , setThumbnailSize
  , maximumNumberOfColumns
  , setMaximumNumberOfColumns
  , labelFont
  , setLabelFont
  , allowsDragging
  , setAllowsDragging
  , allowsMultipleSelection
  , setAllowsMultipleSelection
  , pdfViewSelector
  , setPDFViewSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , selectedPagesSelector
  , thumbnailSizeSelector
  , setThumbnailSizeSelector
  , maximumNumberOfColumnsSelector
  , setMaximumNumberOfColumnsSelector
  , labelFontSelector
  , setLabelFontSelector
  , allowsDraggingSelector
  , setAllowsDraggingSelector
  , allowsMultipleSelectionSelector
  , setAllowsMultipleSelectionSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- PDFView@
pdfView :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id PDFView)
pdfView pdfThumbnailView  =
  sendMsg pdfThumbnailView (mkSelector "PDFView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPDFView:@
setPDFView :: (IsPDFThumbnailView pdfThumbnailView, IsPDFView value) => pdfThumbnailView -> value -> IO ()
setPDFView pdfThumbnailView  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfThumbnailView (mkSelector "setPDFView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id NSColor)
backgroundColor pdfThumbnailView  =
  sendMsg pdfThumbnailView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFThumbnailView pdfThumbnailView, IsNSColor value) => pdfThumbnailView -> value -> IO ()
setBackgroundColor pdfThumbnailView  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfThumbnailView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- selectedPages@
selectedPages :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id NSArray)
selectedPages pdfThumbnailView  =
  sendMsg pdfThumbnailView (mkSelector "selectedPages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- thumbnailSize@
thumbnailSize :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO NSSize
thumbnailSize pdfThumbnailView  =
  sendMsgStret pdfThumbnailView (mkSelector "thumbnailSize") retNSSize []

-- | @- setThumbnailSize:@
setThumbnailSize :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> NSSize -> IO ()
setThumbnailSize pdfThumbnailView  value =
  sendMsg pdfThumbnailView (mkSelector "setThumbnailSize:") retVoid [argNSSize value]

-- | @- maximumNumberOfColumns@
maximumNumberOfColumns :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO CULong
maximumNumberOfColumns pdfThumbnailView  =
  sendMsg pdfThumbnailView (mkSelector "maximumNumberOfColumns") retCULong []

-- | @- setMaximumNumberOfColumns:@
setMaximumNumberOfColumns :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> CULong -> IO ()
setMaximumNumberOfColumns pdfThumbnailView  value =
  sendMsg pdfThumbnailView (mkSelector "setMaximumNumberOfColumns:") retVoid [argCULong (fromIntegral value)]

-- | @- labelFont@
labelFont :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id NSFont)
labelFont pdfThumbnailView  =
  sendMsg pdfThumbnailView (mkSelector "labelFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabelFont:@
setLabelFont :: (IsPDFThumbnailView pdfThumbnailView, IsNSFont value) => pdfThumbnailView -> value -> IO ()
setLabelFont pdfThumbnailView  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfThumbnailView (mkSelector "setLabelFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsDragging@
allowsDragging :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO Bool
allowsDragging pdfThumbnailView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfThumbnailView (mkSelector "allowsDragging") retCULong []

-- | @- setAllowsDragging:@
setAllowsDragging :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> Bool -> IO ()
setAllowsDragging pdfThumbnailView  value =
  sendMsg pdfThumbnailView (mkSelector "setAllowsDragging:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO Bool
allowsMultipleSelection pdfThumbnailView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfThumbnailView (mkSelector "allowsMultipleSelection") retCULong []

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> Bool -> IO ()
setAllowsMultipleSelection pdfThumbnailView  value =
  sendMsg pdfThumbnailView (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PDFView@
pdfViewSelector :: Selector
pdfViewSelector = mkSelector "PDFView"

-- | @Selector@ for @setPDFView:@
setPDFViewSelector :: Selector
setPDFViewSelector = mkSelector "setPDFView:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @selectedPages@
selectedPagesSelector :: Selector
selectedPagesSelector = mkSelector "selectedPages"

-- | @Selector@ for @thumbnailSize@
thumbnailSizeSelector :: Selector
thumbnailSizeSelector = mkSelector "thumbnailSize"

-- | @Selector@ for @setThumbnailSize:@
setThumbnailSizeSelector :: Selector
setThumbnailSizeSelector = mkSelector "setThumbnailSize:"

-- | @Selector@ for @maximumNumberOfColumns@
maximumNumberOfColumnsSelector :: Selector
maximumNumberOfColumnsSelector = mkSelector "maximumNumberOfColumns"

-- | @Selector@ for @setMaximumNumberOfColumns:@
setMaximumNumberOfColumnsSelector :: Selector
setMaximumNumberOfColumnsSelector = mkSelector "setMaximumNumberOfColumns:"

-- | @Selector@ for @labelFont@
labelFontSelector :: Selector
labelFontSelector = mkSelector "labelFont"

-- | @Selector@ for @setLabelFont:@
setLabelFontSelector :: Selector
setLabelFontSelector = mkSelector "setLabelFont:"

-- | @Selector@ for @allowsDragging@
allowsDraggingSelector :: Selector
allowsDraggingSelector = mkSelector "allowsDragging"

-- | @Selector@ for @setAllowsDragging:@
setAllowsDraggingSelector :: Selector
setAllowsDraggingSelector = mkSelector "setAllowsDragging:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

