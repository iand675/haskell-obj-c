{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PDFView@.
module ObjC.PDFKit.PDFView
  ( PDFView
  , IsPDFView(..)
  , goToFirstPage
  , goToLastPage
  , goToNextPage
  , goToPreviousPage
  , goBack
  , goForward
  , goToPage
  , goToDestination
  , goToSelection
  , goToRect_onPage
  , zoomIn
  , zoomOut
  , areaOfInterestForMouse
  , areaOfInterestForPoint
  , setCursorForAreaOfInterest
  , performAction
  , setCurrentSelection_animate
  , clearSelection
  , selectAll
  , scrollSelectionToVisible
  , drawPage_toContext
  , drawPagePost_toContext
  , copy
  , printWithInfo_autoRotate
  , printWithInfo_autoRotate_pageScaling
  , pageForPoint_nearest
  , convertPoint_toPage
  , convertRect_toPage
  , convertPoint_fromPage
  , convertRect_fromPage
  , layoutDocumentView
  , annotationsChangedOnPage
  , rowSizeForPage
  , takePasswordFrom
  , drawPage
  , drawPagePost
  , takeBackgroundColorFrom
  , document
  , setDocument
  , canGoToFirstPage
  , canGoToLastPage
  , canGoToNextPage
  , canGoToPreviousPage
  , canGoBack
  , canGoForward
  , currentPage
  , currentDestination
  , displayMode
  , setDisplayMode
  , displayDirection
  , setDisplayDirection
  , displaysPageBreaks
  , setDisplaysPageBreaks
  , pageBreakMargins
  , setPageBreakMargins
  , displayBox
  , setDisplayBox
  , displaysAsBook
  , setDisplaysAsBook
  , displaysRTL
  , setDisplaysRTL
  , backgroundColor
  , setBackgroundColor
  , interpolationQuality
  , setInterpolationQuality
  , pageShadowsEnabled
  , setPageShadowsEnabled
  , scaleFactor
  , setScaleFactor
  , minScaleFactor
  , setMinScaleFactor
  , maxScaleFactor
  , setMaxScaleFactor
  , autoScales
  , setAutoScales
  , scaleFactorForSizeToFit
  , canZoomIn
  , canZoomOut
  , currentSelection
  , setCurrentSelection
  , documentView
  , acceptsDraggedFiles
  , setAcceptsDraggedFiles
  , enableDataDetectors
  , setEnableDataDetectors
  , inMarkupMode
  , setInMarkupMode
  , shouldAntiAlias
  , setShouldAntiAlias
  , greekingThreshold
  , setGreekingThreshold
  , allowsDragging
  , setAllowsDragging
  , goToFirstPageSelector
  , goToLastPageSelector
  , goToNextPageSelector
  , goToPreviousPageSelector
  , goBackSelector
  , goForwardSelector
  , goToPageSelector
  , goToDestinationSelector
  , goToSelectionSelector
  , goToRect_onPageSelector
  , zoomInSelector
  , zoomOutSelector
  , areaOfInterestForMouseSelector
  , areaOfInterestForPointSelector
  , setCursorForAreaOfInterestSelector
  , performActionSelector
  , setCurrentSelection_animateSelector
  , clearSelectionSelector
  , selectAllSelector
  , scrollSelectionToVisibleSelector
  , drawPage_toContextSelector
  , drawPagePost_toContextSelector
  , copySelector
  , printWithInfo_autoRotateSelector
  , printWithInfo_autoRotate_pageScalingSelector
  , pageForPoint_nearestSelector
  , convertPoint_toPageSelector
  , convertRect_toPageSelector
  , convertPoint_fromPageSelector
  , convertRect_fromPageSelector
  , layoutDocumentViewSelector
  , annotationsChangedOnPageSelector
  , rowSizeForPageSelector
  , takePasswordFromSelector
  , drawPageSelector
  , drawPagePostSelector
  , takeBackgroundColorFromSelector
  , documentSelector
  , setDocumentSelector
  , canGoToFirstPageSelector
  , canGoToLastPageSelector
  , canGoToNextPageSelector
  , canGoToPreviousPageSelector
  , canGoBackSelector
  , canGoForwardSelector
  , currentPageSelector
  , currentDestinationSelector
  , displayModeSelector
  , setDisplayModeSelector
  , displayDirectionSelector
  , setDisplayDirectionSelector
  , displaysPageBreaksSelector
  , setDisplaysPageBreaksSelector
  , pageBreakMarginsSelector
  , setPageBreakMarginsSelector
  , displayBoxSelector
  , setDisplayBoxSelector
  , displaysAsBookSelector
  , setDisplaysAsBookSelector
  , displaysRTLSelector
  , setDisplaysRTLSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , interpolationQualitySelector
  , setInterpolationQualitySelector
  , pageShadowsEnabledSelector
  , setPageShadowsEnabledSelector
  , scaleFactorSelector
  , setScaleFactorSelector
  , minScaleFactorSelector
  , setMinScaleFactorSelector
  , maxScaleFactorSelector
  , setMaxScaleFactorSelector
  , autoScalesSelector
  , setAutoScalesSelector
  , scaleFactorForSizeToFitSelector
  , canZoomInSelector
  , canZoomOutSelector
  , currentSelectionSelector
  , setCurrentSelectionSelector
  , documentViewSelector
  , acceptsDraggedFilesSelector
  , setAcceptsDraggedFilesSelector
  , enableDataDetectorsSelector
  , setEnableDataDetectorsSelector
  , inMarkupModeSelector
  , setInMarkupModeSelector
  , shouldAntiAliasSelector
  , setShouldAntiAliasSelector
  , greekingThresholdSelector
  , setGreekingThresholdSelector
  , allowsDraggingSelector
  , setAllowsDraggingSelector

  -- * Enum types
  , PDFAreaOfInterest(PDFAreaOfInterest)
  , pattern KPDFNoArea
  , pattern KPDFPageArea
  , pattern KPDFTextArea
  , pattern KPDFAnnotationArea
  , pattern KPDFLinkArea
  , pattern KPDFControlArea
  , pattern KPDFTextFieldArea
  , pattern KPDFIconArea
  , pattern KPDFPopupArea
  , pattern KPDFImageArea
  , pattern KPDFAnyArea
  , PDFDisplayBox(PDFDisplayBox)
  , pattern KPDFDisplayBoxMediaBox
  , pattern KPDFDisplayBoxCropBox
  , pattern KPDFDisplayBoxBleedBox
  , pattern KPDFDisplayBoxTrimBox
  , pattern KPDFDisplayBoxArtBox
  , PDFDisplayDirection(PDFDisplayDirection)
  , pattern KPDFDisplayDirectionVertical
  , pattern KPDFDisplayDirectionHorizontal
  , PDFDisplayMode(PDFDisplayMode)
  , pattern KPDFDisplaySinglePage
  , pattern KPDFDisplaySinglePageContinuous
  , pattern KPDFDisplayTwoUp
  , pattern KPDFDisplayTwoUpContinuous
  , PDFInterpolationQuality(PDFInterpolationQuality)
  , pattern KPDFInterpolationQualityNone
  , pattern KPDFInterpolationQualityLow
  , pattern KPDFInterpolationQualityHigh
  , PDFPrintScalingMode(PDFPrintScalingMode)
  , pattern KPDFPrintPageScaleNone
  , pattern KPDFPrintPageScaleToFit
  , pattern KPDFPrintPageScaleDownToFit

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

-- | @- goToFirstPage:@
goToFirstPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToFirstPage pdfView  sender =
  sendMsg pdfView (mkSelector "goToFirstPage:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- goToLastPage:@
goToLastPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToLastPage pdfView  sender =
  sendMsg pdfView (mkSelector "goToLastPage:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- goToNextPage:@
goToNextPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToNextPage pdfView  sender =
  sendMsg pdfView (mkSelector "goToNextPage:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- goToPreviousPage:@
goToPreviousPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToPreviousPage pdfView  sender =
  sendMsg pdfView (mkSelector "goToPreviousPage:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- goBack:@
goBack :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goBack pdfView  sender =
  sendMsg pdfView (mkSelector "goBack:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- goForward:@
goForward :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goForward pdfView  sender =
  sendMsg pdfView (mkSelector "goForward:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- goToPage:@
goToPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
goToPage pdfView  page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "goToPage:") retVoid [argPtr (castPtr raw_page :: Ptr ())]

-- | @- goToDestination:@
goToDestination :: (IsPDFView pdfView, IsPDFDestination destination) => pdfView -> destination -> IO ()
goToDestination pdfView  destination =
withObjCPtr destination $ \raw_destination ->
    sendMsg pdfView (mkSelector "goToDestination:") retVoid [argPtr (castPtr raw_destination :: Ptr ())]

-- | @- goToSelection:@
goToSelection :: (IsPDFView pdfView, IsPDFSelection selection) => pdfView -> selection -> IO ()
goToSelection pdfView  selection =
withObjCPtr selection $ \raw_selection ->
    sendMsg pdfView (mkSelector "goToSelection:") retVoid [argPtr (castPtr raw_selection :: Ptr ())]

-- | @- goToRect:onPage:@
goToRect_onPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSRect -> page -> IO ()
goToRect_onPage pdfView  rect page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "goToRect:onPage:") retVoid [argNSRect rect, argPtr (castPtr raw_page :: Ptr ())]

-- | @- zoomIn:@
zoomIn :: IsPDFView pdfView => pdfView -> RawId -> IO ()
zoomIn pdfView  sender =
  sendMsg pdfView (mkSelector "zoomIn:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- zoomOut:@
zoomOut :: IsPDFView pdfView => pdfView -> RawId -> IO ()
zoomOut pdfView  sender =
  sendMsg pdfView (mkSelector "zoomOut:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- areaOfInterestForMouse:@
areaOfInterestForMouse :: (IsPDFView pdfView, IsNSEvent event) => pdfView -> event -> IO PDFAreaOfInterest
areaOfInterestForMouse pdfView  event =
withObjCPtr event $ \raw_event ->
    fmap (coerce :: CLong -> PDFAreaOfInterest) $ sendMsg pdfView (mkSelector "areaOfInterestForMouse:") retCLong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- areaOfInterestForPoint:@
areaOfInterestForPoint :: IsPDFView pdfView => pdfView -> NSPoint -> IO PDFAreaOfInterest
areaOfInterestForPoint pdfView  cursorLocation =
  fmap (coerce :: CLong -> PDFAreaOfInterest) $ sendMsg pdfView (mkSelector "areaOfInterestForPoint:") retCLong [argNSPoint cursorLocation]

-- | @- setCursorForAreaOfInterest:@
setCursorForAreaOfInterest :: IsPDFView pdfView => pdfView -> PDFAreaOfInterest -> IO ()
setCursorForAreaOfInterest pdfView  area =
  sendMsg pdfView (mkSelector "setCursorForAreaOfInterest:") retVoid [argCLong (coerce area)]

-- | @- performAction:@
performAction :: (IsPDFView pdfView, IsPDFAction action) => pdfView -> action -> IO ()
performAction pdfView  action =
withObjCPtr action $ \raw_action ->
    sendMsg pdfView (mkSelector "performAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- | @- setCurrentSelection:animate:@
setCurrentSelection_animate :: (IsPDFView pdfView, IsPDFSelection selection) => pdfView -> selection -> Bool -> IO ()
setCurrentSelection_animate pdfView  selection animate =
withObjCPtr selection $ \raw_selection ->
    sendMsg pdfView (mkSelector "setCurrentSelection:animate:") retVoid [argPtr (castPtr raw_selection :: Ptr ()), argCULong (if animate then 1 else 0)]

-- | @- clearSelection@
clearSelection :: IsPDFView pdfView => pdfView -> IO ()
clearSelection pdfView  =
  sendMsg pdfView (mkSelector "clearSelection") retVoid []

-- | @- selectAll:@
selectAll :: IsPDFView pdfView => pdfView -> RawId -> IO ()
selectAll pdfView  sender =
  sendMsg pdfView (mkSelector "selectAll:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- scrollSelectionToVisible:@
scrollSelectionToVisible :: IsPDFView pdfView => pdfView -> RawId -> IO ()
scrollSelectionToVisible pdfView  sender =
  sendMsg pdfView (mkSelector "scrollSelectionToVisible:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- drawPage:toContext:@
drawPage_toContext :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> Ptr () -> IO ()
drawPage_toContext pdfView  page context =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "drawPage:toContext:") retVoid [argPtr (castPtr raw_page :: Ptr ()), argPtr context]

-- | @- drawPagePost:toContext:@
drawPagePost_toContext :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> Ptr () -> IO ()
drawPagePost_toContext pdfView  page context =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "drawPagePost:toContext:") retVoid [argPtr (castPtr raw_page :: Ptr ()), argPtr context]

-- | @- copy:@
copy :: IsPDFView pdfView => pdfView -> RawId -> IO ()
copy pdfView  sender =
  sendMsg pdfView (mkSelector "copy:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- printWithInfo:autoRotate:@
printWithInfo_autoRotate :: (IsPDFView pdfView, IsNSPrintInfo printInfo) => pdfView -> printInfo -> Bool -> IO ()
printWithInfo_autoRotate pdfView  printInfo doRotate =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg pdfView (mkSelector "printWithInfo:autoRotate:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argCULong (if doRotate then 1 else 0)]

-- | @- printWithInfo:autoRotate:pageScaling:@
printWithInfo_autoRotate_pageScaling :: (IsPDFView pdfView, IsNSPrintInfo printInfo) => pdfView -> printInfo -> Bool -> PDFPrintScalingMode -> IO ()
printWithInfo_autoRotate_pageScaling pdfView  printInfo doRotate scale =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg pdfView (mkSelector "printWithInfo:autoRotate:pageScaling:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argCULong (if doRotate then 1 else 0), argCLong (coerce scale)]

-- | @- pageForPoint:nearest:@
pageForPoint_nearest :: IsPDFView pdfView => pdfView -> NSPoint -> Bool -> IO (Id PDFPage)
pageForPoint_nearest pdfView  point nearest =
  sendMsg pdfView (mkSelector "pageForPoint:nearest:") (retPtr retVoid) [argNSPoint point, argCULong (if nearest then 1 else 0)] >>= retainedObject . castPtr

-- | @- convertPoint:toPage:@
convertPoint_toPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSPoint -> page -> IO NSPoint
convertPoint_toPage pdfView  point page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfView (mkSelector "convertPoint:toPage:") retNSPoint [argNSPoint point, argPtr (castPtr raw_page :: Ptr ())]

-- | @- convertRect:toPage:@
convertRect_toPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSRect -> page -> IO NSRect
convertRect_toPage pdfView  rect page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfView (mkSelector "convertRect:toPage:") retNSRect [argNSRect rect, argPtr (castPtr raw_page :: Ptr ())]

-- | @- convertPoint:fromPage:@
convertPoint_fromPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSPoint -> page -> IO NSPoint
convertPoint_fromPage pdfView  point page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfView (mkSelector "convertPoint:fromPage:") retNSPoint [argNSPoint point, argPtr (castPtr raw_page :: Ptr ())]

-- | @- convertRect:fromPage:@
convertRect_fromPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSRect -> page -> IO NSRect
convertRect_fromPage pdfView  rect page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfView (mkSelector "convertRect:fromPage:") retNSRect [argNSRect rect, argPtr (castPtr raw_page :: Ptr ())]

-- | @- layoutDocumentView@
layoutDocumentView :: IsPDFView pdfView => pdfView -> IO ()
layoutDocumentView pdfView  =
  sendMsg pdfView (mkSelector "layoutDocumentView") retVoid []

-- | @- annotationsChangedOnPage:@
annotationsChangedOnPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
annotationsChangedOnPage pdfView  page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "annotationsChangedOnPage:") retVoid [argPtr (castPtr raw_page :: Ptr ())]

-- | @- rowSizeForPage:@
rowSizeForPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO NSSize
rowSizeForPage pdfView  page =
withObjCPtr page $ \raw_page ->
    sendMsgStret pdfView (mkSelector "rowSizeForPage:") retNSSize [argPtr (castPtr raw_page :: Ptr ())]

-- | @- takePasswordFrom:@
takePasswordFrom :: IsPDFView pdfView => pdfView -> RawId -> IO ()
takePasswordFrom pdfView  sender =
  sendMsg pdfView (mkSelector "takePasswordFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- drawPage:@
drawPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
drawPage pdfView  page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "drawPage:") retVoid [argPtr (castPtr raw_page :: Ptr ())]

-- | @- drawPagePost:@
drawPagePost :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
drawPagePost pdfView  page =
withObjCPtr page $ \raw_page ->
    sendMsg pdfView (mkSelector "drawPagePost:") retVoid [argPtr (castPtr raw_page :: Ptr ())]

-- | @- takeBackgroundColorFrom:@
takeBackgroundColorFrom :: IsPDFView pdfView => pdfView -> RawId -> IO ()
takeBackgroundColorFrom pdfView  sender =
  sendMsg pdfView (mkSelector "takeBackgroundColorFrom:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- document@
document :: IsPDFView pdfView => pdfView -> IO (Id PDFDocument)
document pdfView  =
  sendMsg pdfView (mkSelector "document") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocument:@
setDocument :: (IsPDFView pdfView, IsPDFDocument value) => pdfView -> value -> IO ()
setDocument pdfView  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfView (mkSelector "setDocument:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- canGoToFirstPage@
canGoToFirstPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToFirstPage pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canGoToFirstPage") retCULong []

-- | @- canGoToLastPage@
canGoToLastPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToLastPage pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canGoToLastPage") retCULong []

-- | @- canGoToNextPage@
canGoToNextPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToNextPage pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canGoToNextPage") retCULong []

-- | @- canGoToPreviousPage@
canGoToPreviousPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToPreviousPage pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canGoToPreviousPage") retCULong []

-- | @- canGoBack@
canGoBack :: IsPDFView pdfView => pdfView -> IO Bool
canGoBack pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canGoBack") retCULong []

-- | @- canGoForward@
canGoForward :: IsPDFView pdfView => pdfView -> IO Bool
canGoForward pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canGoForward") retCULong []

-- | @- currentPage@
currentPage :: IsPDFView pdfView => pdfView -> IO (Id PDFPage)
currentPage pdfView  =
  sendMsg pdfView (mkSelector "currentPage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- currentDestination@
currentDestination :: IsPDFView pdfView => pdfView -> IO (Id PDFDestination)
currentDestination pdfView  =
  sendMsg pdfView (mkSelector "currentDestination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayMode@
displayMode :: IsPDFView pdfView => pdfView -> IO PDFDisplayMode
displayMode pdfView  =
  fmap (coerce :: CLong -> PDFDisplayMode) $ sendMsg pdfView (mkSelector "displayMode") retCLong []

-- | @- setDisplayMode:@
setDisplayMode :: IsPDFView pdfView => pdfView -> PDFDisplayMode -> IO ()
setDisplayMode pdfView  value =
  sendMsg pdfView (mkSelector "setDisplayMode:") retVoid [argCLong (coerce value)]

-- | @- displayDirection@
displayDirection :: IsPDFView pdfView => pdfView -> IO PDFDisplayDirection
displayDirection pdfView  =
  fmap (coerce :: CLong -> PDFDisplayDirection) $ sendMsg pdfView (mkSelector "displayDirection") retCLong []

-- | @- setDisplayDirection:@
setDisplayDirection :: IsPDFView pdfView => pdfView -> PDFDisplayDirection -> IO ()
setDisplayDirection pdfView  value =
  sendMsg pdfView (mkSelector "setDisplayDirection:") retVoid [argCLong (coerce value)]

-- | @- displaysPageBreaks@
displaysPageBreaks :: IsPDFView pdfView => pdfView -> IO Bool
displaysPageBreaks pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "displaysPageBreaks") retCULong []

-- | @- setDisplaysPageBreaks:@
setDisplaysPageBreaks :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setDisplaysPageBreaks pdfView  value =
  sendMsg pdfView (mkSelector "setDisplaysPageBreaks:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pageBreakMargins@
pageBreakMargins :: IsPDFView pdfView => pdfView -> IO NSEdgeInsets
pageBreakMargins pdfView  =
  sendMsgStret pdfView (mkSelector "pageBreakMargins") retNSEdgeInsets []

-- | @- setPageBreakMargins:@
setPageBreakMargins :: IsPDFView pdfView => pdfView -> NSEdgeInsets -> IO ()
setPageBreakMargins pdfView  value =
  sendMsg pdfView (mkSelector "setPageBreakMargins:") retVoid [argNSEdgeInsets value]

-- | @- displayBox@
displayBox :: IsPDFView pdfView => pdfView -> IO PDFDisplayBox
displayBox pdfView  =
  fmap (coerce :: CLong -> PDFDisplayBox) $ sendMsg pdfView (mkSelector "displayBox") retCLong []

-- | @- setDisplayBox:@
setDisplayBox :: IsPDFView pdfView => pdfView -> PDFDisplayBox -> IO ()
setDisplayBox pdfView  value =
  sendMsg pdfView (mkSelector "setDisplayBox:") retVoid [argCLong (coerce value)]

-- | @- displaysAsBook@
displaysAsBook :: IsPDFView pdfView => pdfView -> IO Bool
displaysAsBook pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "displaysAsBook") retCULong []

-- | @- setDisplaysAsBook:@
setDisplaysAsBook :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setDisplaysAsBook pdfView  value =
  sendMsg pdfView (mkSelector "setDisplaysAsBook:") retVoid [argCULong (if value then 1 else 0)]

-- | @- displaysRTL@
displaysRTL :: IsPDFView pdfView => pdfView -> IO Bool
displaysRTL pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "displaysRTL") retCULong []

-- | @- setDisplaysRTL:@
setDisplaysRTL :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setDisplaysRTL pdfView  value =
  sendMsg pdfView (mkSelector "setDisplaysRTL:") retVoid [argCULong (if value then 1 else 0)]

-- | @- backgroundColor@
backgroundColor :: IsPDFView pdfView => pdfView -> IO (Id NSColor)
backgroundColor pdfView  =
  sendMsg pdfView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFView pdfView, IsNSColor value) => pdfView -> value -> IO ()
setBackgroundColor pdfView  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- interpolationQuality@
interpolationQuality :: IsPDFView pdfView => pdfView -> IO PDFInterpolationQuality
interpolationQuality pdfView  =
  fmap (coerce :: CLong -> PDFInterpolationQuality) $ sendMsg pdfView (mkSelector "interpolationQuality") retCLong []

-- | @- setInterpolationQuality:@
setInterpolationQuality :: IsPDFView pdfView => pdfView -> PDFInterpolationQuality -> IO ()
setInterpolationQuality pdfView  value =
  sendMsg pdfView (mkSelector "setInterpolationQuality:") retVoid [argCLong (coerce value)]

-- | @- pageShadowsEnabled@
pageShadowsEnabled :: IsPDFView pdfView => pdfView -> IO Bool
pageShadowsEnabled pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "pageShadowsEnabled") retCULong []

-- | @- setPageShadowsEnabled:@
setPageShadowsEnabled :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setPageShadowsEnabled pdfView  value =
  sendMsg pdfView (mkSelector "setPageShadowsEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scaleFactor@
scaleFactor :: IsPDFView pdfView => pdfView -> IO CDouble
scaleFactor pdfView  =
  sendMsg pdfView (mkSelector "scaleFactor") retCDouble []

-- | @- setScaleFactor:@
setScaleFactor :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setScaleFactor pdfView  value =
  sendMsg pdfView (mkSelector "setScaleFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- minScaleFactor@
minScaleFactor :: IsPDFView pdfView => pdfView -> IO CDouble
minScaleFactor pdfView  =
  sendMsg pdfView (mkSelector "minScaleFactor") retCDouble []

-- | @- setMinScaleFactor:@
setMinScaleFactor :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setMinScaleFactor pdfView  value =
  sendMsg pdfView (mkSelector "setMinScaleFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxScaleFactor@
maxScaleFactor :: IsPDFView pdfView => pdfView -> IO CDouble
maxScaleFactor pdfView  =
  sendMsg pdfView (mkSelector "maxScaleFactor") retCDouble []

-- | @- setMaxScaleFactor:@
setMaxScaleFactor :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setMaxScaleFactor pdfView  value =
  sendMsg pdfView (mkSelector "setMaxScaleFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- autoScales@
autoScales :: IsPDFView pdfView => pdfView -> IO Bool
autoScales pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "autoScales") retCULong []

-- | @- setAutoScales:@
setAutoScales :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setAutoScales pdfView  value =
  sendMsg pdfView (mkSelector "setAutoScales:") retVoid [argCULong (if value then 1 else 0)]

-- | @- scaleFactorForSizeToFit@
scaleFactorForSizeToFit :: IsPDFView pdfView => pdfView -> IO CDouble
scaleFactorForSizeToFit pdfView  =
  sendMsg pdfView (mkSelector "scaleFactorForSizeToFit") retCDouble []

-- | @- canZoomIn@
canZoomIn :: IsPDFView pdfView => pdfView -> IO Bool
canZoomIn pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canZoomIn") retCULong []

-- | @- canZoomOut@
canZoomOut :: IsPDFView pdfView => pdfView -> IO Bool
canZoomOut pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "canZoomOut") retCULong []

-- | @- currentSelection@
currentSelection :: IsPDFView pdfView => pdfView -> IO (Id PDFSelection)
currentSelection pdfView  =
  sendMsg pdfView (mkSelector "currentSelection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrentSelection:@
setCurrentSelection :: (IsPDFView pdfView, IsPDFSelection value) => pdfView -> value -> IO ()
setCurrentSelection pdfView  value =
withObjCPtr value $ \raw_value ->
    sendMsg pdfView (mkSelector "setCurrentSelection:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- documentView@
documentView :: IsPDFView pdfView => pdfView -> IO (Id NSView)
documentView pdfView  =
  sendMsg pdfView (mkSelector "documentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- acceptsDraggedFiles@
acceptsDraggedFiles :: IsPDFView pdfView => pdfView -> IO Bool
acceptsDraggedFiles pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "acceptsDraggedFiles") retCULong []

-- | @- setAcceptsDraggedFiles:@
setAcceptsDraggedFiles :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setAcceptsDraggedFiles pdfView  value =
  sendMsg pdfView (mkSelector "setAcceptsDraggedFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | @- enableDataDetectors@
enableDataDetectors :: IsPDFView pdfView => pdfView -> IO Bool
enableDataDetectors pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "enableDataDetectors") retCULong []

-- | @- setEnableDataDetectors:@
setEnableDataDetectors :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setEnableDataDetectors pdfView  value =
  sendMsg pdfView (mkSelector "setEnableDataDetectors:") retVoid [argCULong (if value then 1 else 0)]

-- | @- inMarkupMode@
inMarkupMode :: IsPDFView pdfView => pdfView -> IO Bool
inMarkupMode pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "inMarkupMode") retCULong []

-- | @- setInMarkupMode:@
setInMarkupMode :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setInMarkupMode pdfView  value =
  sendMsg pdfView (mkSelector "setInMarkupMode:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldAntiAlias@
shouldAntiAlias :: IsPDFView pdfView => pdfView -> IO Bool
shouldAntiAlias pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "shouldAntiAlias") retCULong []

-- | @- setShouldAntiAlias:@
setShouldAntiAlias :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setShouldAntiAlias pdfView  value =
  sendMsg pdfView (mkSelector "setShouldAntiAlias:") retVoid [argCULong (if value then 1 else 0)]

-- | @- greekingThreshold@
greekingThreshold :: IsPDFView pdfView => pdfView -> IO CDouble
greekingThreshold pdfView  =
  sendMsg pdfView (mkSelector "greekingThreshold") retCDouble []

-- | @- setGreekingThreshold:@
setGreekingThreshold :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setGreekingThreshold pdfView  value =
  sendMsg pdfView (mkSelector "setGreekingThreshold:") retVoid [argCDouble (fromIntegral value)]

-- | @- allowsDragging@
allowsDragging :: IsPDFView pdfView => pdfView -> IO Bool
allowsDragging pdfView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pdfView (mkSelector "allowsDragging") retCULong []

-- | @- setAllowsDragging:@
setAllowsDragging :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setAllowsDragging pdfView  value =
  sendMsg pdfView (mkSelector "setAllowsDragging:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @goToFirstPage:@
goToFirstPageSelector :: Selector
goToFirstPageSelector = mkSelector "goToFirstPage:"

-- | @Selector@ for @goToLastPage:@
goToLastPageSelector :: Selector
goToLastPageSelector = mkSelector "goToLastPage:"

-- | @Selector@ for @goToNextPage:@
goToNextPageSelector :: Selector
goToNextPageSelector = mkSelector "goToNextPage:"

-- | @Selector@ for @goToPreviousPage:@
goToPreviousPageSelector :: Selector
goToPreviousPageSelector = mkSelector "goToPreviousPage:"

-- | @Selector@ for @goBack:@
goBackSelector :: Selector
goBackSelector = mkSelector "goBack:"

-- | @Selector@ for @goForward:@
goForwardSelector :: Selector
goForwardSelector = mkSelector "goForward:"

-- | @Selector@ for @goToPage:@
goToPageSelector :: Selector
goToPageSelector = mkSelector "goToPage:"

-- | @Selector@ for @goToDestination:@
goToDestinationSelector :: Selector
goToDestinationSelector = mkSelector "goToDestination:"

-- | @Selector@ for @goToSelection:@
goToSelectionSelector :: Selector
goToSelectionSelector = mkSelector "goToSelection:"

-- | @Selector@ for @goToRect:onPage:@
goToRect_onPageSelector :: Selector
goToRect_onPageSelector = mkSelector "goToRect:onPage:"

-- | @Selector@ for @zoomIn:@
zoomInSelector :: Selector
zoomInSelector = mkSelector "zoomIn:"

-- | @Selector@ for @zoomOut:@
zoomOutSelector :: Selector
zoomOutSelector = mkSelector "zoomOut:"

-- | @Selector@ for @areaOfInterestForMouse:@
areaOfInterestForMouseSelector :: Selector
areaOfInterestForMouseSelector = mkSelector "areaOfInterestForMouse:"

-- | @Selector@ for @areaOfInterestForPoint:@
areaOfInterestForPointSelector :: Selector
areaOfInterestForPointSelector = mkSelector "areaOfInterestForPoint:"

-- | @Selector@ for @setCursorForAreaOfInterest:@
setCursorForAreaOfInterestSelector :: Selector
setCursorForAreaOfInterestSelector = mkSelector "setCursorForAreaOfInterest:"

-- | @Selector@ for @performAction:@
performActionSelector :: Selector
performActionSelector = mkSelector "performAction:"

-- | @Selector@ for @setCurrentSelection:animate:@
setCurrentSelection_animateSelector :: Selector
setCurrentSelection_animateSelector = mkSelector "setCurrentSelection:animate:"

-- | @Selector@ for @clearSelection@
clearSelectionSelector :: Selector
clearSelectionSelector = mkSelector "clearSelection"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @scrollSelectionToVisible:@
scrollSelectionToVisibleSelector :: Selector
scrollSelectionToVisibleSelector = mkSelector "scrollSelectionToVisible:"

-- | @Selector@ for @drawPage:toContext:@
drawPage_toContextSelector :: Selector
drawPage_toContextSelector = mkSelector "drawPage:toContext:"

-- | @Selector@ for @drawPagePost:toContext:@
drawPagePost_toContextSelector :: Selector
drawPagePost_toContextSelector = mkSelector "drawPagePost:toContext:"

-- | @Selector@ for @copy:@
copySelector :: Selector
copySelector = mkSelector "copy:"

-- | @Selector@ for @printWithInfo:autoRotate:@
printWithInfo_autoRotateSelector :: Selector
printWithInfo_autoRotateSelector = mkSelector "printWithInfo:autoRotate:"

-- | @Selector@ for @printWithInfo:autoRotate:pageScaling:@
printWithInfo_autoRotate_pageScalingSelector :: Selector
printWithInfo_autoRotate_pageScalingSelector = mkSelector "printWithInfo:autoRotate:pageScaling:"

-- | @Selector@ for @pageForPoint:nearest:@
pageForPoint_nearestSelector :: Selector
pageForPoint_nearestSelector = mkSelector "pageForPoint:nearest:"

-- | @Selector@ for @convertPoint:toPage:@
convertPoint_toPageSelector :: Selector
convertPoint_toPageSelector = mkSelector "convertPoint:toPage:"

-- | @Selector@ for @convertRect:toPage:@
convertRect_toPageSelector :: Selector
convertRect_toPageSelector = mkSelector "convertRect:toPage:"

-- | @Selector@ for @convertPoint:fromPage:@
convertPoint_fromPageSelector :: Selector
convertPoint_fromPageSelector = mkSelector "convertPoint:fromPage:"

-- | @Selector@ for @convertRect:fromPage:@
convertRect_fromPageSelector :: Selector
convertRect_fromPageSelector = mkSelector "convertRect:fromPage:"

-- | @Selector@ for @layoutDocumentView@
layoutDocumentViewSelector :: Selector
layoutDocumentViewSelector = mkSelector "layoutDocumentView"

-- | @Selector@ for @annotationsChangedOnPage:@
annotationsChangedOnPageSelector :: Selector
annotationsChangedOnPageSelector = mkSelector "annotationsChangedOnPage:"

-- | @Selector@ for @rowSizeForPage:@
rowSizeForPageSelector :: Selector
rowSizeForPageSelector = mkSelector "rowSizeForPage:"

-- | @Selector@ for @takePasswordFrom:@
takePasswordFromSelector :: Selector
takePasswordFromSelector = mkSelector "takePasswordFrom:"

-- | @Selector@ for @drawPage:@
drawPageSelector :: Selector
drawPageSelector = mkSelector "drawPage:"

-- | @Selector@ for @drawPagePost:@
drawPagePostSelector :: Selector
drawPagePostSelector = mkSelector "drawPagePost:"

-- | @Selector@ for @takeBackgroundColorFrom:@
takeBackgroundColorFromSelector :: Selector
takeBackgroundColorFromSelector = mkSelector "takeBackgroundColorFrom:"

-- | @Selector@ for @document@
documentSelector :: Selector
documentSelector = mkSelector "document"

-- | @Selector@ for @setDocument:@
setDocumentSelector :: Selector
setDocumentSelector = mkSelector "setDocument:"

-- | @Selector@ for @canGoToFirstPage@
canGoToFirstPageSelector :: Selector
canGoToFirstPageSelector = mkSelector "canGoToFirstPage"

-- | @Selector@ for @canGoToLastPage@
canGoToLastPageSelector :: Selector
canGoToLastPageSelector = mkSelector "canGoToLastPage"

-- | @Selector@ for @canGoToNextPage@
canGoToNextPageSelector :: Selector
canGoToNextPageSelector = mkSelector "canGoToNextPage"

-- | @Selector@ for @canGoToPreviousPage@
canGoToPreviousPageSelector :: Selector
canGoToPreviousPageSelector = mkSelector "canGoToPreviousPage"

-- | @Selector@ for @canGoBack@
canGoBackSelector :: Selector
canGoBackSelector = mkSelector "canGoBack"

-- | @Selector@ for @canGoForward@
canGoForwardSelector :: Selector
canGoForwardSelector = mkSelector "canGoForward"

-- | @Selector@ for @currentPage@
currentPageSelector :: Selector
currentPageSelector = mkSelector "currentPage"

-- | @Selector@ for @currentDestination@
currentDestinationSelector :: Selector
currentDestinationSelector = mkSelector "currentDestination"

-- | @Selector@ for @displayMode@
displayModeSelector :: Selector
displayModeSelector = mkSelector "displayMode"

-- | @Selector@ for @setDisplayMode:@
setDisplayModeSelector :: Selector
setDisplayModeSelector = mkSelector "setDisplayMode:"

-- | @Selector@ for @displayDirection@
displayDirectionSelector :: Selector
displayDirectionSelector = mkSelector "displayDirection"

-- | @Selector@ for @setDisplayDirection:@
setDisplayDirectionSelector :: Selector
setDisplayDirectionSelector = mkSelector "setDisplayDirection:"

-- | @Selector@ for @displaysPageBreaks@
displaysPageBreaksSelector :: Selector
displaysPageBreaksSelector = mkSelector "displaysPageBreaks"

-- | @Selector@ for @setDisplaysPageBreaks:@
setDisplaysPageBreaksSelector :: Selector
setDisplaysPageBreaksSelector = mkSelector "setDisplaysPageBreaks:"

-- | @Selector@ for @pageBreakMargins@
pageBreakMarginsSelector :: Selector
pageBreakMarginsSelector = mkSelector "pageBreakMargins"

-- | @Selector@ for @setPageBreakMargins:@
setPageBreakMarginsSelector :: Selector
setPageBreakMarginsSelector = mkSelector "setPageBreakMargins:"

-- | @Selector@ for @displayBox@
displayBoxSelector :: Selector
displayBoxSelector = mkSelector "displayBox"

-- | @Selector@ for @setDisplayBox:@
setDisplayBoxSelector :: Selector
setDisplayBoxSelector = mkSelector "setDisplayBox:"

-- | @Selector@ for @displaysAsBook@
displaysAsBookSelector :: Selector
displaysAsBookSelector = mkSelector "displaysAsBook"

-- | @Selector@ for @setDisplaysAsBook:@
setDisplaysAsBookSelector :: Selector
setDisplaysAsBookSelector = mkSelector "setDisplaysAsBook:"

-- | @Selector@ for @displaysRTL@
displaysRTLSelector :: Selector
displaysRTLSelector = mkSelector "displaysRTL"

-- | @Selector@ for @setDisplaysRTL:@
setDisplaysRTLSelector :: Selector
setDisplaysRTLSelector = mkSelector "setDisplaysRTL:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @interpolationQuality@
interpolationQualitySelector :: Selector
interpolationQualitySelector = mkSelector "interpolationQuality"

-- | @Selector@ for @setInterpolationQuality:@
setInterpolationQualitySelector :: Selector
setInterpolationQualitySelector = mkSelector "setInterpolationQuality:"

-- | @Selector@ for @pageShadowsEnabled@
pageShadowsEnabledSelector :: Selector
pageShadowsEnabledSelector = mkSelector "pageShadowsEnabled"

-- | @Selector@ for @setPageShadowsEnabled:@
setPageShadowsEnabledSelector :: Selector
setPageShadowsEnabledSelector = mkSelector "setPageShadowsEnabled:"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @minScaleFactor@
minScaleFactorSelector :: Selector
minScaleFactorSelector = mkSelector "minScaleFactor"

-- | @Selector@ for @setMinScaleFactor:@
setMinScaleFactorSelector :: Selector
setMinScaleFactorSelector = mkSelector "setMinScaleFactor:"

-- | @Selector@ for @maxScaleFactor@
maxScaleFactorSelector :: Selector
maxScaleFactorSelector = mkSelector "maxScaleFactor"

-- | @Selector@ for @setMaxScaleFactor:@
setMaxScaleFactorSelector :: Selector
setMaxScaleFactorSelector = mkSelector "setMaxScaleFactor:"

-- | @Selector@ for @autoScales@
autoScalesSelector :: Selector
autoScalesSelector = mkSelector "autoScales"

-- | @Selector@ for @setAutoScales:@
setAutoScalesSelector :: Selector
setAutoScalesSelector = mkSelector "setAutoScales:"

-- | @Selector@ for @scaleFactorForSizeToFit@
scaleFactorForSizeToFitSelector :: Selector
scaleFactorForSizeToFitSelector = mkSelector "scaleFactorForSizeToFit"

-- | @Selector@ for @canZoomIn@
canZoomInSelector :: Selector
canZoomInSelector = mkSelector "canZoomIn"

-- | @Selector@ for @canZoomOut@
canZoomOutSelector :: Selector
canZoomOutSelector = mkSelector "canZoomOut"

-- | @Selector@ for @currentSelection@
currentSelectionSelector :: Selector
currentSelectionSelector = mkSelector "currentSelection"

-- | @Selector@ for @setCurrentSelection:@
setCurrentSelectionSelector :: Selector
setCurrentSelectionSelector = mkSelector "setCurrentSelection:"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @acceptsDraggedFiles@
acceptsDraggedFilesSelector :: Selector
acceptsDraggedFilesSelector = mkSelector "acceptsDraggedFiles"

-- | @Selector@ for @setAcceptsDraggedFiles:@
setAcceptsDraggedFilesSelector :: Selector
setAcceptsDraggedFilesSelector = mkSelector "setAcceptsDraggedFiles:"

-- | @Selector@ for @enableDataDetectors@
enableDataDetectorsSelector :: Selector
enableDataDetectorsSelector = mkSelector "enableDataDetectors"

-- | @Selector@ for @setEnableDataDetectors:@
setEnableDataDetectorsSelector :: Selector
setEnableDataDetectorsSelector = mkSelector "setEnableDataDetectors:"

-- | @Selector@ for @inMarkupMode@
inMarkupModeSelector :: Selector
inMarkupModeSelector = mkSelector "inMarkupMode"

-- | @Selector@ for @setInMarkupMode:@
setInMarkupModeSelector :: Selector
setInMarkupModeSelector = mkSelector "setInMarkupMode:"

-- | @Selector@ for @shouldAntiAlias@
shouldAntiAliasSelector :: Selector
shouldAntiAliasSelector = mkSelector "shouldAntiAlias"

-- | @Selector@ for @setShouldAntiAlias:@
setShouldAntiAliasSelector :: Selector
setShouldAntiAliasSelector = mkSelector "setShouldAntiAlias:"

-- | @Selector@ for @greekingThreshold@
greekingThresholdSelector :: Selector
greekingThresholdSelector = mkSelector "greekingThreshold"

-- | @Selector@ for @setGreekingThreshold:@
setGreekingThresholdSelector :: Selector
setGreekingThresholdSelector = mkSelector "setGreekingThreshold:"

-- | @Selector@ for @allowsDragging@
allowsDraggingSelector :: Selector
allowsDraggingSelector = mkSelector "allowsDragging"

-- | @Selector@ for @setAllowsDragging:@
setAllowsDraggingSelector :: Selector
setAllowsDraggingSelector = mkSelector "setAllowsDragging:"

