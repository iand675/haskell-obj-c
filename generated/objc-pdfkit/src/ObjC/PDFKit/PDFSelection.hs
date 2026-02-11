{-# LANGUAGE PatternSynonyms #-}
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
  , string
  , attributedString
  , initWithDocumentSelector
  , boundsForPageSelector
  , numberOfTextRangesOnPageSelector
  , rangeAtIndex_onPageSelector
  , selectionsByLineSelector
  , addSelectionSelector
  , addSelectionsSelector
  , extendSelectionAtEndSelector
  , extendSelectionAtStartSelector
  , extendSelectionForLineBoundariesSelector
  , drawForPage_activeSelector
  , drawForPage_withBox_activeSelector
  , pagesSelector
  , stringSelector
  , attributedStringSelector

  -- * Enum types
  , PDFDisplayBox(PDFDisplayBox)
  , pattern KPDFDisplayBoxMediaBox
  , pattern KPDFDisplayBoxCropBox
  , pattern KPDFDisplayBoxBleedBox
  , pattern KPDFDisplayBoxTrimBox
  , pattern KPDFDisplayBoxArtBox

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
import ObjC.Foundation.Internal.Classes

-- | @- initWithDocument:@
initWithDocument :: (IsPDFSelection pdfSelection, IsPDFDocument document) => pdfSelection -> document -> IO (Id PDFSelection)
initWithDocument pdfSelection  document =
withObjCPtr document $ \raw_document ->
    sendMsg pdfSelection (mkSelector "initWithDocument:") (retPtr retVoid) [argPtr (castPtr raw_document :: Ptr ())] >>= ownedObject . castPtr

-- | @- boundsForPage:@
boundsForPage :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> IO NSRect
boundsForPage pdfSelection  page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfSelection (mkSelector "boundsForPage:") retNSRect [argPtr (castPtr raw_page :: Ptr ())]

-- | @- numberOfTextRangesOnPage:@
numberOfTextRangesOnPage :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> IO CULong
numberOfTextRangesOnPage pdfSelection  page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfSelection (mkSelector "numberOfTextRangesOnPage:") retCULong [argPtr (castPtr raw_page :: Ptr ())]

-- | @- rangeAtIndex:onPage:@
rangeAtIndex_onPage :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> CULong -> page -> IO NSRange
rangeAtIndex_onPage pdfSelection  index page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfSelection (mkSelector "rangeAtIndex:onPage:") retNSRange [argCULong (fromIntegral index), argPtr (castPtr raw_page :: Ptr ())]

-- | @- selectionsByLine@
selectionsByLine :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSArray)
selectionsByLine pdfSelection  =
  sendMsg pdfSelection (mkSelector "selectionsByLine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addSelection:@
addSelection :: (IsPDFSelection pdfSelection, IsPDFSelection selection) => pdfSelection -> selection -> IO ()
addSelection pdfSelection  selection =
withObjCPtr selection $ \raw_selection ->
    sendMsg pdfSelection (mkSelector "addSelection:") retVoid [argPtr (castPtr raw_selection :: Ptr ())]

-- | @- addSelections:@
addSelections :: (IsPDFSelection pdfSelection, IsNSArray selections) => pdfSelection -> selections -> IO ()
addSelections pdfSelection  selections =
withObjCPtr selections $ \raw_selections ->
    sendMsg pdfSelection (mkSelector "addSelections:") retVoid [argPtr (castPtr raw_selections :: Ptr ())]

-- | @- extendSelectionAtEnd:@
extendSelectionAtEnd :: IsPDFSelection pdfSelection => pdfSelection -> CLong -> IO ()
extendSelectionAtEnd pdfSelection  succeed =
  sendMsg pdfSelection (mkSelector "extendSelectionAtEnd:") retVoid [argCLong (fromIntegral succeed)]

-- | @- extendSelectionAtStart:@
extendSelectionAtStart :: IsPDFSelection pdfSelection => pdfSelection -> CLong -> IO ()
extendSelectionAtStart pdfSelection  precede =
  sendMsg pdfSelection (mkSelector "extendSelectionAtStart:") retVoid [argCLong (fromIntegral precede)]

-- | @- extendSelectionForLineBoundaries@
extendSelectionForLineBoundaries :: IsPDFSelection pdfSelection => pdfSelection -> IO ()
extendSelectionForLineBoundaries pdfSelection  =
  sendMsg pdfSelection (mkSelector "extendSelectionForLineBoundaries") retVoid []

-- | @- drawForPage:active:@
drawForPage_active :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> Bool -> IO ()
drawForPage_active pdfSelection  page active =
withObjCPtr page $ \raw_page ->
    sendMsg pdfSelection (mkSelector "drawForPage:active:") retVoid [argPtr (castPtr raw_page :: Ptr ()), argCULong (if active then 1 else 0)]

-- | @- drawForPage:withBox:active:@
drawForPage_withBox_active :: (IsPDFSelection pdfSelection, IsPDFPage page) => pdfSelection -> page -> PDFDisplayBox -> Bool -> IO ()
drawForPage_withBox_active pdfSelection  page box active =
withObjCPtr page $ \raw_page ->
    sendMsg pdfSelection (mkSelector "drawForPage:withBox:active:") retVoid [argPtr (castPtr raw_page :: Ptr ()), argCLong (coerce box), argCULong (if active then 1 else 0)]

-- | @- pages@
pages :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSArray)
pages pdfSelection  =
  sendMsg pdfSelection (mkSelector "pages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- string@
string :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSString)
string pdfSelection  =
  sendMsg pdfSelection (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributedString@
attributedString :: IsPDFSelection pdfSelection => pdfSelection -> IO (Id NSAttributedString)
attributedString pdfSelection  =
  sendMsg pdfSelection (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDocument:@
initWithDocumentSelector :: Selector
initWithDocumentSelector = mkSelector "initWithDocument:"

-- | @Selector@ for @boundsForPage:@
boundsForPageSelector :: Selector
boundsForPageSelector = mkSelector "boundsForPage:"

-- | @Selector@ for @numberOfTextRangesOnPage:@
numberOfTextRangesOnPageSelector :: Selector
numberOfTextRangesOnPageSelector = mkSelector "numberOfTextRangesOnPage:"

-- | @Selector@ for @rangeAtIndex:onPage:@
rangeAtIndex_onPageSelector :: Selector
rangeAtIndex_onPageSelector = mkSelector "rangeAtIndex:onPage:"

-- | @Selector@ for @selectionsByLine@
selectionsByLineSelector :: Selector
selectionsByLineSelector = mkSelector "selectionsByLine"

-- | @Selector@ for @addSelection:@
addSelectionSelector :: Selector
addSelectionSelector = mkSelector "addSelection:"

-- | @Selector@ for @addSelections:@
addSelectionsSelector :: Selector
addSelectionsSelector = mkSelector "addSelections:"

-- | @Selector@ for @extendSelectionAtEnd:@
extendSelectionAtEndSelector :: Selector
extendSelectionAtEndSelector = mkSelector "extendSelectionAtEnd:"

-- | @Selector@ for @extendSelectionAtStart:@
extendSelectionAtStartSelector :: Selector
extendSelectionAtStartSelector = mkSelector "extendSelectionAtStart:"

-- | @Selector@ for @extendSelectionForLineBoundaries@
extendSelectionForLineBoundariesSelector :: Selector
extendSelectionForLineBoundariesSelector = mkSelector "extendSelectionForLineBoundaries"

-- | @Selector@ for @drawForPage:active:@
drawForPage_activeSelector :: Selector
drawForPage_activeSelector = mkSelector "drawForPage:active:"

-- | @Selector@ for @drawForPage:withBox:active:@
drawForPage_withBox_activeSelector :: Selector
drawForPage_withBox_activeSelector = mkSelector "drawForPage:withBox:active:"

-- | @Selector@ for @pages@
pagesSelector :: Selector
pagesSelector = mkSelector "pages"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

