{-# LANGUAGE DataKinds #-}
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
  , allowsDraggingSelector
  , allowsMultipleSelectionSelector
  , backgroundColorSelector
  , labelFontSelector
  , maximumNumberOfColumnsSelector
  , pdfViewSelector
  , selectedPagesSelector
  , setAllowsDraggingSelector
  , setAllowsMultipleSelectionSelector
  , setBackgroundColorSelector
  , setLabelFontSelector
  , setMaximumNumberOfColumnsSelector
  , setPDFViewSelector
  , setThumbnailSizeSelector
  , thumbnailSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PDFKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- PDFView@
pdfView :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id PDFView)
pdfView pdfThumbnailView =
  sendMessage pdfThumbnailView pdfViewSelector

-- | @- setPDFView:@
setPDFView :: (IsPDFThumbnailView pdfThumbnailView, IsPDFView value) => pdfThumbnailView -> value -> IO ()
setPDFView pdfThumbnailView value =
  sendMessage pdfThumbnailView setPDFViewSelector (toPDFView value)

-- | @- backgroundColor@
backgroundColor :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id NSColor)
backgroundColor pdfThumbnailView =
  sendMessage pdfThumbnailView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFThumbnailView pdfThumbnailView, IsNSColor value) => pdfThumbnailView -> value -> IO ()
setBackgroundColor pdfThumbnailView value =
  sendMessage pdfThumbnailView setBackgroundColorSelector (toNSColor value)

-- | @- selectedPages@
selectedPages :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id NSArray)
selectedPages pdfThumbnailView =
  sendMessage pdfThumbnailView selectedPagesSelector

-- | @- thumbnailSize@
thumbnailSize :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO NSSize
thumbnailSize pdfThumbnailView =
  sendMessage pdfThumbnailView thumbnailSizeSelector

-- | @- setThumbnailSize:@
setThumbnailSize :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> NSSize -> IO ()
setThumbnailSize pdfThumbnailView value =
  sendMessage pdfThumbnailView setThumbnailSizeSelector value

-- | @- maximumNumberOfColumns@
maximumNumberOfColumns :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO CULong
maximumNumberOfColumns pdfThumbnailView =
  sendMessage pdfThumbnailView maximumNumberOfColumnsSelector

-- | @- setMaximumNumberOfColumns:@
setMaximumNumberOfColumns :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> CULong -> IO ()
setMaximumNumberOfColumns pdfThumbnailView value =
  sendMessage pdfThumbnailView setMaximumNumberOfColumnsSelector value

-- | @- labelFont@
labelFont :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO (Id NSFont)
labelFont pdfThumbnailView =
  sendMessage pdfThumbnailView labelFontSelector

-- | @- setLabelFont:@
setLabelFont :: (IsPDFThumbnailView pdfThumbnailView, IsNSFont value) => pdfThumbnailView -> value -> IO ()
setLabelFont pdfThumbnailView value =
  sendMessage pdfThumbnailView setLabelFontSelector (toNSFont value)

-- | @- allowsDragging@
allowsDragging :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO Bool
allowsDragging pdfThumbnailView =
  sendMessage pdfThumbnailView allowsDraggingSelector

-- | @- setAllowsDragging:@
setAllowsDragging :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> Bool -> IO ()
setAllowsDragging pdfThumbnailView value =
  sendMessage pdfThumbnailView setAllowsDraggingSelector value

-- | @- allowsMultipleSelection@
allowsMultipleSelection :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> IO Bool
allowsMultipleSelection pdfThumbnailView =
  sendMessage pdfThumbnailView allowsMultipleSelectionSelector

-- | @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsPDFThumbnailView pdfThumbnailView => pdfThumbnailView -> Bool -> IO ()
setAllowsMultipleSelection pdfThumbnailView value =
  sendMessage pdfThumbnailView setAllowsMultipleSelectionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @PDFView@
pdfViewSelector :: Selector '[] (Id PDFView)
pdfViewSelector = mkSelector "PDFView"

-- | @Selector@ for @setPDFView:@
setPDFViewSelector :: Selector '[Id PDFView] ()
setPDFViewSelector = mkSelector "setPDFView:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @selectedPages@
selectedPagesSelector :: Selector '[] (Id NSArray)
selectedPagesSelector = mkSelector "selectedPages"

-- | @Selector@ for @thumbnailSize@
thumbnailSizeSelector :: Selector '[] NSSize
thumbnailSizeSelector = mkSelector "thumbnailSize"

-- | @Selector@ for @setThumbnailSize:@
setThumbnailSizeSelector :: Selector '[NSSize] ()
setThumbnailSizeSelector = mkSelector "setThumbnailSize:"

-- | @Selector@ for @maximumNumberOfColumns@
maximumNumberOfColumnsSelector :: Selector '[] CULong
maximumNumberOfColumnsSelector = mkSelector "maximumNumberOfColumns"

-- | @Selector@ for @setMaximumNumberOfColumns:@
setMaximumNumberOfColumnsSelector :: Selector '[CULong] ()
setMaximumNumberOfColumnsSelector = mkSelector "setMaximumNumberOfColumns:"

-- | @Selector@ for @labelFont@
labelFontSelector :: Selector '[] (Id NSFont)
labelFontSelector = mkSelector "labelFont"

-- | @Selector@ for @setLabelFont:@
setLabelFontSelector :: Selector '[Id NSFont] ()
setLabelFontSelector = mkSelector "setLabelFont:"

-- | @Selector@ for @allowsDragging@
allowsDraggingSelector :: Selector '[] Bool
allowsDraggingSelector = mkSelector "allowsDragging"

-- | @Selector@ for @setAllowsDragging:@
setAllowsDraggingSelector :: Selector '[Bool] ()
setAllowsDraggingSelector = mkSelector "setAllowsDragging:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

