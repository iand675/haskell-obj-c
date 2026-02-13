{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFSelection@.
module ObjC.PDFKit.PDFSelection
  ( PDFSelection
  , IsPDFSelection(..)
  , initWithDocument
  , boundsForPage
  , numberOfTextRangesOnPage
  , rangeAtIndex_onPage
  , selectionsByLine
  , addSelection
  , addSelections
  , extendSelectionAtEnd
  , extendSelectionAtStart
  , extendSelectionForLineBoundaries
  , drawForPage_active
  , drawForPage_withBox_active
  , pages
  , color
  , setColor
  , string
  , attributedString
  , addSelectionSelector
  , addSelectionsSelector
  , attributedStringSelector
  , boundsForPageSelector
  , colorSelector
  , drawForPage_activeSelector
  , drawForPage_withBox_activeSelector
  , extendSelectionAtEndSelector
  , extendSelectionAtStartSelector
  , extendSelectionForLineBoundariesSelector
  , initWithDocumentSelector
  , numberOfTextRangesOnPageSelector
  , pagesSelector
  , rangeAtIndex_onPageSelector
  , selectionsByLineSelector
  , setColorSelector
  , stringSelector

  -- * Enum types
  , PDFDisplayBox(PDFDisplayBox)
  , pattern KPDFDisplayBoxMediaBox
  , pattern KPDFDisplayBoxCropBox
  , pattern KPDFDisplayBoxBleedBox
  , pattern KPDFDisplayBoxTrimBox
  , pattern KPDFDisplayBoxArtBox

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

-- | @- initWithDocument:@
initWithDocument :: (IsPDFSelection pdfSelection, IsPDFDocument document) => pdfSelection -> document -> IO (Id PDFSelection)
initWithDocument pdfSelection document =
  sendOwnedMessage pdfSelection initWithDocumentSelector (toPDFDocument document)

-- | @- boundsForPage:@
boundsForPage :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> IO NSRect
boundsForPage pdfSelection page =
  sendMessage pdfSelection boundsForPageSelector (toPDFPage page)

-- | @- numberOfTextRangesOnPage:@
numberOfTextRangesOnPage :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> IO CULong
numberOfTextRangesOnPage pdfSelection page =
  sendMessage pdfSelection numberOfTextRangesOnPageSelector (toPDFPage page)

-- | @- rangeAtIndex:onPage:@
rangeAtIndex_onPage :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> CULong -> page -> IO NSRange
rangeAtIndex_onPage pdfSelection index page =
  sendMessage pdfSelection rangeAtIndex_onPageSelector index (toPDFPage page)

-- | @- selectionsByLine@
selectionsByLine :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSArray)
selectionsByLine pdfSelection =
  sendMessage pdfSelection selectionsByLineSelector

-- | @- addSelection:@
addSelection :: (IsPDFSelection pdfSelection, IsPDFSelection selection) => pdfSelection -> selection -> IO ()
addSelection pdfSelection selection =
  sendMessage pdfSelection addSelectionSelector (toPDFSelection selection)

-- | @- addSelections:@
addSelections :: (IsPDFSelection pdfSelection, IsNSArray selections) => pdfSelection -> selections -> IO ()
addSelections pdfSelection selections =
  sendMessage pdfSelection addSelectionsSelector (toNSArray selections)

-- | @- extendSelectionAtEnd:@
extendSelectionAtEnd :: IsPDFSelection pdfSelection => pdfSelection -> CLong -> IO ()
extendSelectionAtEnd pdfSelection succeed =
  sendMessage pdfSelection extendSelectionAtEndSelector succeed

-- | @- extendSelectionAtStart:@
extendSelectionAtStart :: IsPDFSelection pdfSelection => pdfSelection -> CLong -> IO ()
extendSelectionAtStart pdfSelection precede =
  sendMessage pdfSelection extendSelectionAtStartSelector precede

-- | @- extendSelectionForLineBoundaries@
extendSelectionForLineBoundaries :: IsPDFSelection pdfSelection => pdfSelection -> IO ()
extendSelectionForLineBoundaries pdfSelection =
  sendMessage pdfSelection extendSelectionForLineBoundariesSelector

-- | @- drawForPage:active:@
drawForPage_active :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> Bool -> IO ()
drawForPage_active pdfSelection page active =
  sendMessage pdfSelection drawForPage_activeSelector (toPDFPage page) active

-- | @- drawForPage:withBox:active:@
drawForPage_withBox_active :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> PDFDisplayBox -> Bool -> IO ()
drawForPage_withBox_active pdfSelection page box active =
  sendMessage pdfSelection drawForPage_withBox_activeSelector (toPDFPage page) box active

-- | @- pages@
pages :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSArray)
pages pdfSelection =
  sendMessage pdfSelection pagesSelector

-- | @- color@
color :: IsPDFSelection pdfSelection => pdfSelection -> IO RawId
color pdfSelection =
  sendMessage pdfSelection colorSelector

-- | @- setColor:@
setColor :: IsPDFSelection pdfSelection => pdfSelection -> RawId -> IO ()
setColor pdfSelection value =
  sendMessage pdfSelection setColorSelector value

-- | @- string@
string :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSString)
string pdfSelection =
  sendMessage pdfSelection stringSelector

-- | @- attributedString@
attributedString :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSAttributedString)
attributedString pdfSelection =
  sendMessage pdfSelection attributedStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDocument:@
initWithDocumentSelector :: Selector '[Id PDFDocument] (Id PDFSelection)
initWithDocumentSelector = mkSelector "initWithDocument:"

-- | @Selector@ for @boundsForPage:@
boundsForPageSelector :: Selector '[Id PDFPage] NSRect
boundsForPageSelector = mkSelector "boundsForPage:"

-- | @Selector@ for @numberOfTextRangesOnPage:@
numberOfTextRangesOnPageSelector :: Selector '[Id PDFPage] CULong
numberOfTextRangesOnPageSelector = mkSelector "numberOfTextRangesOnPage:"

-- | @Selector@ for @rangeAtIndex:onPage:@
rangeAtIndex_onPageSelector :: Selector '[CULong, Id PDFPage] NSRange
rangeAtIndex_onPageSelector = mkSelector "rangeAtIndex:onPage:"

-- | @Selector@ for @selectionsByLine@
selectionsByLineSelector :: Selector '[] (Id NSArray)
selectionsByLineSelector = mkSelector "selectionsByLine"

-- | @Selector@ for @addSelection:@
addSelectionSelector :: Selector '[Id PDFSelection] ()
addSelectionSelector = mkSelector "addSelection:"

-- | @Selector@ for @addSelections:@
addSelectionsSelector :: Selector '[Id NSArray] ()
addSelectionsSelector = mkSelector "addSelections:"

-- | @Selector@ for @extendSelectionAtEnd:@
extendSelectionAtEndSelector :: Selector '[CLong] ()
extendSelectionAtEndSelector = mkSelector "extendSelectionAtEnd:"

-- | @Selector@ for @extendSelectionAtStart:@
extendSelectionAtStartSelector :: Selector '[CLong] ()
extendSelectionAtStartSelector = mkSelector "extendSelectionAtStart:"

-- | @Selector@ for @extendSelectionForLineBoundaries@
extendSelectionForLineBoundariesSelector :: Selector '[] ()
extendSelectionForLineBoundariesSelector = mkSelector "extendSelectionForLineBoundaries"

-- | @Selector@ for @drawForPage:active:@
drawForPage_activeSelector :: Selector '[Id PDFPage, Bool] ()
drawForPage_activeSelector = mkSelector "drawForPage:active:"

-- | @Selector@ for @drawForPage:withBox:active:@
drawForPage_withBox_activeSelector :: Selector '[Id PDFPage, PDFDisplayBox, Bool] ()
drawForPage_withBox_activeSelector = mkSelector "drawForPage:withBox:active:"

-- | @Selector@ for @pages@
pagesSelector :: Selector '[] (Id NSArray)
pagesSelector = mkSelector "pages"

-- | @Selector@ for @color@
colorSelector :: Selector '[] RawId
colorSelector = mkSelector "color"

-- | @Selector@ for @setColor:@
setColorSelector :: Selector '[RawId] ()
setColorSelector = mkSelector "setColor:"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

