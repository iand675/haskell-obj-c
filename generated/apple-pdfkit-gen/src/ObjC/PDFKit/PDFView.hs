{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , acceptsDraggedFilesSelector
  , allowsDraggingSelector
  , annotationsChangedOnPageSelector
  , areaOfInterestForMouseSelector
  , areaOfInterestForPointSelector
  , autoScalesSelector
  , backgroundColorSelector
  , canGoBackSelector
  , canGoForwardSelector
  , canGoToFirstPageSelector
  , canGoToLastPageSelector
  , canGoToNextPageSelector
  , canGoToPreviousPageSelector
  , canZoomInSelector
  , canZoomOutSelector
  , clearSelectionSelector
  , convertPoint_fromPageSelector
  , convertPoint_toPageSelector
  , convertRect_fromPageSelector
  , convertRect_toPageSelector
  , copySelector
  , currentDestinationSelector
  , currentPageSelector
  , currentSelectionSelector
  , delegateSelector
  , displayBoxSelector
  , displayDirectionSelector
  , displayModeSelector
  , displaysAsBookSelector
  , displaysPageBreaksSelector
  , displaysRTLSelector
  , documentSelector
  , documentViewSelector
  , drawPagePostSelector
  , drawPagePost_toContextSelector
  , drawPageSelector
  , drawPage_toContextSelector
  , enableDataDetectorsSelector
  , goBackSelector
  , goForwardSelector
  , goToDestinationSelector
  , goToFirstPageSelector
  , goToLastPageSelector
  , goToNextPageSelector
  , goToPageSelector
  , goToPreviousPageSelector
  , goToRect_onPageSelector
  , goToSelectionSelector
  , greekingThresholdSelector
  , inMarkupModeSelector
  , interpolationQualitySelector
  , layoutDocumentViewSelector
  , maxScaleFactorSelector
  , minScaleFactorSelector
  , pageBreakMarginsSelector
  , pageForPoint_nearestSelector
  , pageShadowsEnabledSelector
  , performActionSelector
  , printWithInfo_autoRotateSelector
  , printWithInfo_autoRotate_pageScalingSelector
  , rowSizeForPageSelector
  , scaleFactorForSizeToFitSelector
  , scaleFactorSelector
  , scrollSelectionToVisibleSelector
  , selectAllSelector
  , setAcceptsDraggedFilesSelector
  , setAllowsDraggingSelector
  , setAutoScalesSelector
  , setBackgroundColorSelector
  , setCurrentSelectionSelector
  , setCurrentSelection_animateSelector
  , setCursorForAreaOfInterestSelector
  , setDelegateSelector
  , setDisplayBoxSelector
  , setDisplayDirectionSelector
  , setDisplayModeSelector
  , setDisplaysAsBookSelector
  , setDisplaysPageBreaksSelector
  , setDisplaysRTLSelector
  , setDocumentSelector
  , setEnableDataDetectorsSelector
  , setGreekingThresholdSelector
  , setInMarkupModeSelector
  , setInterpolationQualitySelector
  , setMaxScaleFactorSelector
  , setMinScaleFactorSelector
  , setPageBreakMarginsSelector
  , setPageShadowsEnabledSelector
  , setScaleFactorSelector
  , setShouldAntiAliasSelector
  , shouldAntiAliasSelector
  , takeBackgroundColorFromSelector
  , takePasswordFromSelector
  , zoomInSelector
  , zoomOutSelector

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

-- | @- goToFirstPage:@
goToFirstPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToFirstPage pdfView sender =
  sendMessage pdfView goToFirstPageSelector sender

-- | @- goToLastPage:@
goToLastPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToLastPage pdfView sender =
  sendMessage pdfView goToLastPageSelector sender

-- | @- goToNextPage:@
goToNextPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToNextPage pdfView sender =
  sendMessage pdfView goToNextPageSelector sender

-- | @- goToPreviousPage:@
goToPreviousPage :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goToPreviousPage pdfView sender =
  sendMessage pdfView goToPreviousPageSelector sender

-- | @- goBack:@
goBack :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goBack pdfView sender =
  sendMessage pdfView goBackSelector sender

-- | @- goForward:@
goForward :: IsPDFView pdfView => pdfView -> RawId -> IO ()
goForward pdfView sender =
  sendMessage pdfView goForwardSelector sender

-- | @- goToPage:@
goToPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
goToPage pdfView page =
  sendMessage pdfView goToPageSelector (toPDFPage page)

-- | @- goToDestination:@
goToDestination :: (IsPDFView pdfView, IsPDFDestination destination) => pdfView -> destination -> IO ()
goToDestination pdfView destination =
  sendMessage pdfView goToDestinationSelector (toPDFDestination destination)

-- | @- goToSelection:@
goToSelection :: (IsPDFView pdfView, IsPDFSelection selection) => pdfView -> selection -> IO ()
goToSelection pdfView selection =
  sendMessage pdfView goToSelectionSelector (toPDFSelection selection)

-- | @- goToRect:onPage:@
goToRect_onPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSRect -> page -> IO ()
goToRect_onPage pdfView rect page =
  sendMessage pdfView goToRect_onPageSelector rect (toPDFPage page)

-- | @- zoomIn:@
zoomIn :: IsPDFView pdfView => pdfView -> RawId -> IO ()
zoomIn pdfView sender =
  sendMessage pdfView zoomInSelector sender

-- | @- zoomOut:@
zoomOut :: IsPDFView pdfView => pdfView -> RawId -> IO ()
zoomOut pdfView sender =
  sendMessage pdfView zoomOutSelector sender

-- | @- areaOfInterestForMouse:@
areaOfInterestForMouse :: (IsPDFView pdfView, IsNSEvent event) => pdfView -> event -> IO PDFAreaOfInterest
areaOfInterestForMouse pdfView event =
  sendMessage pdfView areaOfInterestForMouseSelector (toNSEvent event)

-- | @- areaOfInterestForPoint:@
areaOfInterestForPoint :: IsPDFView pdfView => pdfView -> NSPoint -> IO PDFAreaOfInterest
areaOfInterestForPoint pdfView cursorLocation =
  sendMessage pdfView areaOfInterestForPointSelector cursorLocation

-- | @- setCursorForAreaOfInterest:@
setCursorForAreaOfInterest :: IsPDFView pdfView => pdfView -> PDFAreaOfInterest -> IO ()
setCursorForAreaOfInterest pdfView area =
  sendMessage pdfView setCursorForAreaOfInterestSelector area

-- | @- performAction:@
performAction :: (IsPDFView pdfView, IsPDFAction action) => pdfView -> action -> IO ()
performAction pdfView action =
  sendMessage pdfView performActionSelector (toPDFAction action)

-- | @- setCurrentSelection:animate:@
setCurrentSelection_animate :: (IsPDFView pdfView, IsPDFSelection selection) => pdfView -> selection -> Bool -> IO ()
setCurrentSelection_animate pdfView selection animate =
  sendMessage pdfView setCurrentSelection_animateSelector (toPDFSelection selection) animate

-- | @- clearSelection@
clearSelection :: IsPDFView pdfView => pdfView -> IO ()
clearSelection pdfView =
  sendMessage pdfView clearSelectionSelector

-- | @- selectAll:@
selectAll :: IsPDFView pdfView => pdfView -> RawId -> IO ()
selectAll pdfView sender =
  sendMessage pdfView selectAllSelector sender

-- | @- scrollSelectionToVisible:@
scrollSelectionToVisible :: IsPDFView pdfView => pdfView -> RawId -> IO ()
scrollSelectionToVisible pdfView sender =
  sendMessage pdfView scrollSelectionToVisibleSelector sender

-- | @- drawPage:toContext:@
drawPage_toContext :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> Ptr () -> IO ()
drawPage_toContext pdfView page context =
  sendMessage pdfView drawPage_toContextSelector (toPDFPage page) context

-- | @- drawPagePost:toContext:@
drawPagePost_toContext :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> Ptr () -> IO ()
drawPagePost_toContext pdfView page context =
  sendMessage pdfView drawPagePost_toContextSelector (toPDFPage page) context

-- | @- copy:@
copy :: IsPDFView pdfView => pdfView -> RawId -> IO ()
copy pdfView sender =
  sendOwnedMessage pdfView copySelector sender

-- | @- printWithInfo:autoRotate:@
printWithInfo_autoRotate :: (IsPDFView pdfView, IsNSPrintInfo printInfo) => pdfView -> printInfo -> Bool -> IO ()
printWithInfo_autoRotate pdfView printInfo doRotate =
  sendMessage pdfView printWithInfo_autoRotateSelector (toNSPrintInfo printInfo) doRotate

-- | @- printWithInfo:autoRotate:pageScaling:@
printWithInfo_autoRotate_pageScaling :: (IsPDFView pdfView, IsNSPrintInfo printInfo) => pdfView -> printInfo -> Bool -> PDFPrintScalingMode -> IO ()
printWithInfo_autoRotate_pageScaling pdfView printInfo doRotate scale =
  sendMessage pdfView printWithInfo_autoRotate_pageScalingSelector (toNSPrintInfo printInfo) doRotate scale

-- | @- pageForPoint:nearest:@
pageForPoint_nearest :: IsPDFView pdfView => pdfView -> NSPoint -> Bool -> IO (Id PDFPage)
pageForPoint_nearest pdfView point nearest =
  sendMessage pdfView pageForPoint_nearestSelector point nearest

-- | @- convertPoint:toPage:@
convertPoint_toPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSPoint -> page -> IO NSPoint
convertPoint_toPage pdfView point page =
  sendMessage pdfView convertPoint_toPageSelector point (toPDFPage page)

-- | @- convertRect:toPage:@
convertRect_toPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSRect -> page -> IO NSRect
convertRect_toPage pdfView rect page =
  sendMessage pdfView convertRect_toPageSelector rect (toPDFPage page)

-- | @- convertPoint:fromPage:@
convertPoint_fromPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSPoint -> page -> IO NSPoint
convertPoint_fromPage pdfView point page =
  sendMessage pdfView convertPoint_fromPageSelector point (toPDFPage page)

-- | @- convertRect:fromPage:@
convertRect_fromPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> NSRect -> page -> IO NSRect
convertRect_fromPage pdfView rect page =
  sendMessage pdfView convertRect_fromPageSelector rect (toPDFPage page)

-- | @- layoutDocumentView@
layoutDocumentView :: IsPDFView pdfView => pdfView -> IO ()
layoutDocumentView pdfView =
  sendMessage pdfView layoutDocumentViewSelector

-- | @- annotationsChangedOnPage:@
annotationsChangedOnPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
annotationsChangedOnPage pdfView page =
  sendMessage pdfView annotationsChangedOnPageSelector (toPDFPage page)

-- | @- rowSizeForPage:@
rowSizeForPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO NSSize
rowSizeForPage pdfView page =
  sendMessage pdfView rowSizeForPageSelector (toPDFPage page)

-- | @- takePasswordFrom:@
takePasswordFrom :: IsPDFView pdfView => pdfView -> RawId -> IO ()
takePasswordFrom pdfView sender =
  sendMessage pdfView takePasswordFromSelector sender

-- | @- drawPage:@
drawPage :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
drawPage pdfView page =
  sendMessage pdfView drawPageSelector (toPDFPage page)

-- | @- drawPagePost:@
drawPagePost :: (IsPDFView pdfView, IsPDFPage page) => pdfView -> page -> IO ()
drawPagePost pdfView page =
  sendMessage pdfView drawPagePostSelector (toPDFPage page)

-- | @- takeBackgroundColorFrom:@
takeBackgroundColorFrom :: IsPDFView pdfView => pdfView -> RawId -> IO ()
takeBackgroundColorFrom pdfView sender =
  sendMessage pdfView takeBackgroundColorFromSelector sender

-- | @- document@
document :: IsPDFView pdfView => pdfView -> IO (Id PDFDocument)
document pdfView =
  sendMessage pdfView documentSelector

-- | @- setDocument:@
setDocument :: (IsPDFView pdfView, IsPDFDocument value) => pdfView -> value -> IO ()
setDocument pdfView value =
  sendMessage pdfView setDocumentSelector (toPDFDocument value)

-- | @- canGoToFirstPage@
canGoToFirstPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToFirstPage pdfView =
  sendMessage pdfView canGoToFirstPageSelector

-- | @- canGoToLastPage@
canGoToLastPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToLastPage pdfView =
  sendMessage pdfView canGoToLastPageSelector

-- | @- canGoToNextPage@
canGoToNextPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToNextPage pdfView =
  sendMessage pdfView canGoToNextPageSelector

-- | @- canGoToPreviousPage@
canGoToPreviousPage :: IsPDFView pdfView => pdfView -> IO Bool
canGoToPreviousPage pdfView =
  sendMessage pdfView canGoToPreviousPageSelector

-- | @- canGoBack@
canGoBack :: IsPDFView pdfView => pdfView -> IO Bool
canGoBack pdfView =
  sendMessage pdfView canGoBackSelector

-- | @- canGoForward@
canGoForward :: IsPDFView pdfView => pdfView -> IO Bool
canGoForward pdfView =
  sendMessage pdfView canGoForwardSelector

-- | @- currentPage@
currentPage :: IsPDFView pdfView => pdfView -> IO (Id PDFPage)
currentPage pdfView =
  sendMessage pdfView currentPageSelector

-- | @- currentDestination@
currentDestination :: IsPDFView pdfView => pdfView -> IO (Id PDFDestination)
currentDestination pdfView =
  sendMessage pdfView currentDestinationSelector

-- | @- displayMode@
displayMode :: IsPDFView pdfView => pdfView -> IO PDFDisplayMode
displayMode pdfView =
  sendMessage pdfView displayModeSelector

-- | @- setDisplayMode:@
setDisplayMode :: IsPDFView pdfView => pdfView -> PDFDisplayMode -> IO ()
setDisplayMode pdfView value =
  sendMessage pdfView setDisplayModeSelector value

-- | @- displayDirection@
displayDirection :: IsPDFView pdfView => pdfView -> IO PDFDisplayDirection
displayDirection pdfView =
  sendMessage pdfView displayDirectionSelector

-- | @- setDisplayDirection:@
setDisplayDirection :: IsPDFView pdfView => pdfView -> PDFDisplayDirection -> IO ()
setDisplayDirection pdfView value =
  sendMessage pdfView setDisplayDirectionSelector value

-- | @- displaysPageBreaks@
displaysPageBreaks :: IsPDFView pdfView => pdfView -> IO Bool
displaysPageBreaks pdfView =
  sendMessage pdfView displaysPageBreaksSelector

-- | @- setDisplaysPageBreaks:@
setDisplaysPageBreaks :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setDisplaysPageBreaks pdfView value =
  sendMessage pdfView setDisplaysPageBreaksSelector value

-- | @- pageBreakMargins@
pageBreakMargins :: IsPDFView pdfView => pdfView -> IO NSEdgeInsets
pageBreakMargins pdfView =
  sendMessage pdfView pageBreakMarginsSelector

-- | @- setPageBreakMargins:@
setPageBreakMargins :: IsPDFView pdfView => pdfView -> NSEdgeInsets -> IO ()
setPageBreakMargins pdfView value =
  sendMessage pdfView setPageBreakMarginsSelector value

-- | @- displayBox@
displayBox :: IsPDFView pdfView => pdfView -> IO PDFDisplayBox
displayBox pdfView =
  sendMessage pdfView displayBoxSelector

-- | @- setDisplayBox:@
setDisplayBox :: IsPDFView pdfView => pdfView -> PDFDisplayBox -> IO ()
setDisplayBox pdfView value =
  sendMessage pdfView setDisplayBoxSelector value

-- | @- displaysAsBook@
displaysAsBook :: IsPDFView pdfView => pdfView -> IO Bool
displaysAsBook pdfView =
  sendMessage pdfView displaysAsBookSelector

-- | @- setDisplaysAsBook:@
setDisplaysAsBook :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setDisplaysAsBook pdfView value =
  sendMessage pdfView setDisplaysAsBookSelector value

-- | @- displaysRTL@
displaysRTL :: IsPDFView pdfView => pdfView -> IO Bool
displaysRTL pdfView =
  sendMessage pdfView displaysRTLSelector

-- | @- setDisplaysRTL:@
setDisplaysRTL :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setDisplaysRTL pdfView value =
  sendMessage pdfView setDisplaysRTLSelector value

-- | @- backgroundColor@
backgroundColor :: IsPDFView pdfView => pdfView -> IO (Id NSColor)
backgroundColor pdfView =
  sendMessage pdfView backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsPDFView pdfView, IsNSColor value) => pdfView -> value -> IO ()
setBackgroundColor pdfView value =
  sendMessage pdfView setBackgroundColorSelector (toNSColor value)

-- | @- interpolationQuality@
interpolationQuality :: IsPDFView pdfView => pdfView -> IO PDFInterpolationQuality
interpolationQuality pdfView =
  sendMessage pdfView interpolationQualitySelector

-- | @- setInterpolationQuality:@
setInterpolationQuality :: IsPDFView pdfView => pdfView -> PDFInterpolationQuality -> IO ()
setInterpolationQuality pdfView value =
  sendMessage pdfView setInterpolationQualitySelector value

-- | @- pageShadowsEnabled@
pageShadowsEnabled :: IsPDFView pdfView => pdfView -> IO Bool
pageShadowsEnabled pdfView =
  sendMessage pdfView pageShadowsEnabledSelector

-- | @- setPageShadowsEnabled:@
setPageShadowsEnabled :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setPageShadowsEnabled pdfView value =
  sendMessage pdfView setPageShadowsEnabledSelector value

-- | @- delegate@
delegate :: IsPDFView pdfView => pdfView -> IO RawId
delegate pdfView =
  sendMessage pdfView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsPDFView pdfView => pdfView -> RawId -> IO ()
setDelegate pdfView value =
  sendMessage pdfView setDelegateSelector value

-- | @- scaleFactor@
scaleFactor :: IsPDFView pdfView => pdfView -> IO CDouble
scaleFactor pdfView =
  sendMessage pdfView scaleFactorSelector

-- | @- setScaleFactor:@
setScaleFactor :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setScaleFactor pdfView value =
  sendMessage pdfView setScaleFactorSelector value

-- | @- minScaleFactor@
minScaleFactor :: IsPDFView pdfView => pdfView -> IO CDouble
minScaleFactor pdfView =
  sendMessage pdfView minScaleFactorSelector

-- | @- setMinScaleFactor:@
setMinScaleFactor :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setMinScaleFactor pdfView value =
  sendMessage pdfView setMinScaleFactorSelector value

-- | @- maxScaleFactor@
maxScaleFactor :: IsPDFView pdfView => pdfView -> IO CDouble
maxScaleFactor pdfView =
  sendMessage pdfView maxScaleFactorSelector

-- | @- setMaxScaleFactor:@
setMaxScaleFactor :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setMaxScaleFactor pdfView value =
  sendMessage pdfView setMaxScaleFactorSelector value

-- | @- autoScales@
autoScales :: IsPDFView pdfView => pdfView -> IO Bool
autoScales pdfView =
  sendMessage pdfView autoScalesSelector

-- | @- setAutoScales:@
setAutoScales :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setAutoScales pdfView value =
  sendMessage pdfView setAutoScalesSelector value

-- | @- scaleFactorForSizeToFit@
scaleFactorForSizeToFit :: IsPDFView pdfView => pdfView -> IO CDouble
scaleFactorForSizeToFit pdfView =
  sendMessage pdfView scaleFactorForSizeToFitSelector

-- | @- canZoomIn@
canZoomIn :: IsPDFView pdfView => pdfView -> IO Bool
canZoomIn pdfView =
  sendMessage pdfView canZoomInSelector

-- | @- canZoomOut@
canZoomOut :: IsPDFView pdfView => pdfView -> IO Bool
canZoomOut pdfView =
  sendMessage pdfView canZoomOutSelector

-- | @- currentSelection@
currentSelection :: IsPDFView pdfView => pdfView -> IO (Id PDFSelection)
currentSelection pdfView =
  sendMessage pdfView currentSelectionSelector

-- | @- setCurrentSelection:@
setCurrentSelection :: (IsPDFView pdfView, IsPDFSelection value) => pdfView -> value -> IO ()
setCurrentSelection pdfView value =
  sendMessage pdfView setCurrentSelectionSelector (toPDFSelection value)

-- | @- documentView@
documentView :: IsPDFView pdfView => pdfView -> IO (Id NSView)
documentView pdfView =
  sendMessage pdfView documentViewSelector

-- | @- acceptsDraggedFiles@
acceptsDraggedFiles :: IsPDFView pdfView => pdfView -> IO Bool
acceptsDraggedFiles pdfView =
  sendMessage pdfView acceptsDraggedFilesSelector

-- | @- setAcceptsDraggedFiles:@
setAcceptsDraggedFiles :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setAcceptsDraggedFiles pdfView value =
  sendMessage pdfView setAcceptsDraggedFilesSelector value

-- | @- enableDataDetectors@
enableDataDetectors :: IsPDFView pdfView => pdfView -> IO Bool
enableDataDetectors pdfView =
  sendMessage pdfView enableDataDetectorsSelector

-- | @- setEnableDataDetectors:@
setEnableDataDetectors :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setEnableDataDetectors pdfView value =
  sendMessage pdfView setEnableDataDetectorsSelector value

-- | @- inMarkupMode@
inMarkupMode :: IsPDFView pdfView => pdfView -> IO Bool
inMarkupMode pdfView =
  sendMessage pdfView inMarkupModeSelector

-- | @- setInMarkupMode:@
setInMarkupMode :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setInMarkupMode pdfView value =
  sendMessage pdfView setInMarkupModeSelector value

-- | @- shouldAntiAlias@
shouldAntiAlias :: IsPDFView pdfView => pdfView -> IO Bool
shouldAntiAlias pdfView =
  sendMessage pdfView shouldAntiAliasSelector

-- | @- setShouldAntiAlias:@
setShouldAntiAlias :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setShouldAntiAlias pdfView value =
  sendMessage pdfView setShouldAntiAliasSelector value

-- | @- greekingThreshold@
greekingThreshold :: IsPDFView pdfView => pdfView -> IO CDouble
greekingThreshold pdfView =
  sendMessage pdfView greekingThresholdSelector

-- | @- setGreekingThreshold:@
setGreekingThreshold :: IsPDFView pdfView => pdfView -> CDouble -> IO ()
setGreekingThreshold pdfView value =
  sendMessage pdfView setGreekingThresholdSelector value

-- | @- allowsDragging@
allowsDragging :: IsPDFView pdfView => pdfView -> IO Bool
allowsDragging pdfView =
  sendMessage pdfView allowsDraggingSelector

-- | @- setAllowsDragging:@
setAllowsDragging :: IsPDFView pdfView => pdfView -> Bool -> IO ()
setAllowsDragging pdfView value =
  sendMessage pdfView setAllowsDraggingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @goToFirstPage:@
goToFirstPageSelector :: Selector '[RawId] ()
goToFirstPageSelector = mkSelector "goToFirstPage:"

-- | @Selector@ for @goToLastPage:@
goToLastPageSelector :: Selector '[RawId] ()
goToLastPageSelector = mkSelector "goToLastPage:"

-- | @Selector@ for @goToNextPage:@
goToNextPageSelector :: Selector '[RawId] ()
goToNextPageSelector = mkSelector "goToNextPage:"

-- | @Selector@ for @goToPreviousPage:@
goToPreviousPageSelector :: Selector '[RawId] ()
goToPreviousPageSelector = mkSelector "goToPreviousPage:"

-- | @Selector@ for @goBack:@
goBackSelector :: Selector '[RawId] ()
goBackSelector = mkSelector "goBack:"

-- | @Selector@ for @goForward:@
goForwardSelector :: Selector '[RawId] ()
goForwardSelector = mkSelector "goForward:"

-- | @Selector@ for @goToPage:@
goToPageSelector :: Selector '[Id PDFPage] ()
goToPageSelector = mkSelector "goToPage:"

-- | @Selector@ for @goToDestination:@
goToDestinationSelector :: Selector '[Id PDFDestination] ()
goToDestinationSelector = mkSelector "goToDestination:"

-- | @Selector@ for @goToSelection:@
goToSelectionSelector :: Selector '[Id PDFSelection] ()
goToSelectionSelector = mkSelector "goToSelection:"

-- | @Selector@ for @goToRect:onPage:@
goToRect_onPageSelector :: Selector '[NSRect, Id PDFPage] ()
goToRect_onPageSelector = mkSelector "goToRect:onPage:"

-- | @Selector@ for @zoomIn:@
zoomInSelector :: Selector '[RawId] ()
zoomInSelector = mkSelector "zoomIn:"

-- | @Selector@ for @zoomOut:@
zoomOutSelector :: Selector '[RawId] ()
zoomOutSelector = mkSelector "zoomOut:"

-- | @Selector@ for @areaOfInterestForMouse:@
areaOfInterestForMouseSelector :: Selector '[Id NSEvent] PDFAreaOfInterest
areaOfInterestForMouseSelector = mkSelector "areaOfInterestForMouse:"

-- | @Selector@ for @areaOfInterestForPoint:@
areaOfInterestForPointSelector :: Selector '[NSPoint] PDFAreaOfInterest
areaOfInterestForPointSelector = mkSelector "areaOfInterestForPoint:"

-- | @Selector@ for @setCursorForAreaOfInterest:@
setCursorForAreaOfInterestSelector :: Selector '[PDFAreaOfInterest] ()
setCursorForAreaOfInterestSelector = mkSelector "setCursorForAreaOfInterest:"

-- | @Selector@ for @performAction:@
performActionSelector :: Selector '[Id PDFAction] ()
performActionSelector = mkSelector "performAction:"

-- | @Selector@ for @setCurrentSelection:animate:@
setCurrentSelection_animateSelector :: Selector '[Id PDFSelection, Bool] ()
setCurrentSelection_animateSelector = mkSelector "setCurrentSelection:animate:"

-- | @Selector@ for @clearSelection@
clearSelectionSelector :: Selector '[] ()
clearSelectionSelector = mkSelector "clearSelection"

-- | @Selector@ for @selectAll:@
selectAllSelector :: Selector '[RawId] ()
selectAllSelector = mkSelector "selectAll:"

-- | @Selector@ for @scrollSelectionToVisible:@
scrollSelectionToVisibleSelector :: Selector '[RawId] ()
scrollSelectionToVisibleSelector = mkSelector "scrollSelectionToVisible:"

-- | @Selector@ for @drawPage:toContext:@
drawPage_toContextSelector :: Selector '[Id PDFPage, Ptr ()] ()
drawPage_toContextSelector = mkSelector "drawPage:toContext:"

-- | @Selector@ for @drawPagePost:toContext:@
drawPagePost_toContextSelector :: Selector '[Id PDFPage, Ptr ()] ()
drawPagePost_toContextSelector = mkSelector "drawPagePost:toContext:"

-- | @Selector@ for @copy:@
copySelector :: Selector '[RawId] ()
copySelector = mkSelector "copy:"

-- | @Selector@ for @printWithInfo:autoRotate:@
printWithInfo_autoRotateSelector :: Selector '[Id NSPrintInfo, Bool] ()
printWithInfo_autoRotateSelector = mkSelector "printWithInfo:autoRotate:"

-- | @Selector@ for @printWithInfo:autoRotate:pageScaling:@
printWithInfo_autoRotate_pageScalingSelector :: Selector '[Id NSPrintInfo, Bool, PDFPrintScalingMode] ()
printWithInfo_autoRotate_pageScalingSelector = mkSelector "printWithInfo:autoRotate:pageScaling:"

-- | @Selector@ for @pageForPoint:nearest:@
pageForPoint_nearestSelector :: Selector '[NSPoint, Bool] (Id PDFPage)
pageForPoint_nearestSelector = mkSelector "pageForPoint:nearest:"

-- | @Selector@ for @convertPoint:toPage:@
convertPoint_toPageSelector :: Selector '[NSPoint, Id PDFPage] NSPoint
convertPoint_toPageSelector = mkSelector "convertPoint:toPage:"

-- | @Selector@ for @convertRect:toPage:@
convertRect_toPageSelector :: Selector '[NSRect, Id PDFPage] NSRect
convertRect_toPageSelector = mkSelector "convertRect:toPage:"

-- | @Selector@ for @convertPoint:fromPage:@
convertPoint_fromPageSelector :: Selector '[NSPoint, Id PDFPage] NSPoint
convertPoint_fromPageSelector = mkSelector "convertPoint:fromPage:"

-- | @Selector@ for @convertRect:fromPage:@
convertRect_fromPageSelector :: Selector '[NSRect, Id PDFPage] NSRect
convertRect_fromPageSelector = mkSelector "convertRect:fromPage:"

-- | @Selector@ for @layoutDocumentView@
layoutDocumentViewSelector :: Selector '[] ()
layoutDocumentViewSelector = mkSelector "layoutDocumentView"

-- | @Selector@ for @annotationsChangedOnPage:@
annotationsChangedOnPageSelector :: Selector '[Id PDFPage] ()
annotationsChangedOnPageSelector = mkSelector "annotationsChangedOnPage:"

-- | @Selector@ for @rowSizeForPage:@
rowSizeForPageSelector :: Selector '[Id PDFPage] NSSize
rowSizeForPageSelector = mkSelector "rowSizeForPage:"

-- | @Selector@ for @takePasswordFrom:@
takePasswordFromSelector :: Selector '[RawId] ()
takePasswordFromSelector = mkSelector "takePasswordFrom:"

-- | @Selector@ for @drawPage:@
drawPageSelector :: Selector '[Id PDFPage] ()
drawPageSelector = mkSelector "drawPage:"

-- | @Selector@ for @drawPagePost:@
drawPagePostSelector :: Selector '[Id PDFPage] ()
drawPagePostSelector = mkSelector "drawPagePost:"

-- | @Selector@ for @takeBackgroundColorFrom:@
takeBackgroundColorFromSelector :: Selector '[RawId] ()
takeBackgroundColorFromSelector = mkSelector "takeBackgroundColorFrom:"

-- | @Selector@ for @document@
documentSelector :: Selector '[] (Id PDFDocument)
documentSelector = mkSelector "document"

-- | @Selector@ for @setDocument:@
setDocumentSelector :: Selector '[Id PDFDocument] ()
setDocumentSelector = mkSelector "setDocument:"

-- | @Selector@ for @canGoToFirstPage@
canGoToFirstPageSelector :: Selector '[] Bool
canGoToFirstPageSelector = mkSelector "canGoToFirstPage"

-- | @Selector@ for @canGoToLastPage@
canGoToLastPageSelector :: Selector '[] Bool
canGoToLastPageSelector = mkSelector "canGoToLastPage"

-- | @Selector@ for @canGoToNextPage@
canGoToNextPageSelector :: Selector '[] Bool
canGoToNextPageSelector = mkSelector "canGoToNextPage"

-- | @Selector@ for @canGoToPreviousPage@
canGoToPreviousPageSelector :: Selector '[] Bool
canGoToPreviousPageSelector = mkSelector "canGoToPreviousPage"

-- | @Selector@ for @canGoBack@
canGoBackSelector :: Selector '[] Bool
canGoBackSelector = mkSelector "canGoBack"

-- | @Selector@ for @canGoForward@
canGoForwardSelector :: Selector '[] Bool
canGoForwardSelector = mkSelector "canGoForward"

-- | @Selector@ for @currentPage@
currentPageSelector :: Selector '[] (Id PDFPage)
currentPageSelector = mkSelector "currentPage"

-- | @Selector@ for @currentDestination@
currentDestinationSelector :: Selector '[] (Id PDFDestination)
currentDestinationSelector = mkSelector "currentDestination"

-- | @Selector@ for @displayMode@
displayModeSelector :: Selector '[] PDFDisplayMode
displayModeSelector = mkSelector "displayMode"

-- | @Selector@ for @setDisplayMode:@
setDisplayModeSelector :: Selector '[PDFDisplayMode] ()
setDisplayModeSelector = mkSelector "setDisplayMode:"

-- | @Selector@ for @displayDirection@
displayDirectionSelector :: Selector '[] PDFDisplayDirection
displayDirectionSelector = mkSelector "displayDirection"

-- | @Selector@ for @setDisplayDirection:@
setDisplayDirectionSelector :: Selector '[PDFDisplayDirection] ()
setDisplayDirectionSelector = mkSelector "setDisplayDirection:"

-- | @Selector@ for @displaysPageBreaks@
displaysPageBreaksSelector :: Selector '[] Bool
displaysPageBreaksSelector = mkSelector "displaysPageBreaks"

-- | @Selector@ for @setDisplaysPageBreaks:@
setDisplaysPageBreaksSelector :: Selector '[Bool] ()
setDisplaysPageBreaksSelector = mkSelector "setDisplaysPageBreaks:"

-- | @Selector@ for @pageBreakMargins@
pageBreakMarginsSelector :: Selector '[] NSEdgeInsets
pageBreakMarginsSelector = mkSelector "pageBreakMargins"

-- | @Selector@ for @setPageBreakMargins:@
setPageBreakMarginsSelector :: Selector '[NSEdgeInsets] ()
setPageBreakMarginsSelector = mkSelector "setPageBreakMargins:"

-- | @Selector@ for @displayBox@
displayBoxSelector :: Selector '[] PDFDisplayBox
displayBoxSelector = mkSelector "displayBox"

-- | @Selector@ for @setDisplayBox:@
setDisplayBoxSelector :: Selector '[PDFDisplayBox] ()
setDisplayBoxSelector = mkSelector "setDisplayBox:"

-- | @Selector@ for @displaysAsBook@
displaysAsBookSelector :: Selector '[] Bool
displaysAsBookSelector = mkSelector "displaysAsBook"

-- | @Selector@ for @setDisplaysAsBook:@
setDisplaysAsBookSelector :: Selector '[Bool] ()
setDisplaysAsBookSelector = mkSelector "setDisplaysAsBook:"

-- | @Selector@ for @displaysRTL@
displaysRTLSelector :: Selector '[] Bool
displaysRTLSelector = mkSelector "displaysRTL"

-- | @Selector@ for @setDisplaysRTL:@
setDisplaysRTLSelector :: Selector '[Bool] ()
setDisplaysRTLSelector = mkSelector "setDisplaysRTL:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @interpolationQuality@
interpolationQualitySelector :: Selector '[] PDFInterpolationQuality
interpolationQualitySelector = mkSelector "interpolationQuality"

-- | @Selector@ for @setInterpolationQuality:@
setInterpolationQualitySelector :: Selector '[PDFInterpolationQuality] ()
setInterpolationQualitySelector = mkSelector "setInterpolationQuality:"

-- | @Selector@ for @pageShadowsEnabled@
pageShadowsEnabledSelector :: Selector '[] Bool
pageShadowsEnabledSelector = mkSelector "pageShadowsEnabled"

-- | @Selector@ for @setPageShadowsEnabled:@
setPageShadowsEnabledSelector :: Selector '[Bool] ()
setPageShadowsEnabledSelector = mkSelector "setPageShadowsEnabled:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector '[] CDouble
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector '[CDouble] ()
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @minScaleFactor@
minScaleFactorSelector :: Selector '[] CDouble
minScaleFactorSelector = mkSelector "minScaleFactor"

-- | @Selector@ for @setMinScaleFactor:@
setMinScaleFactorSelector :: Selector '[CDouble] ()
setMinScaleFactorSelector = mkSelector "setMinScaleFactor:"

-- | @Selector@ for @maxScaleFactor@
maxScaleFactorSelector :: Selector '[] CDouble
maxScaleFactorSelector = mkSelector "maxScaleFactor"

-- | @Selector@ for @setMaxScaleFactor:@
setMaxScaleFactorSelector :: Selector '[CDouble] ()
setMaxScaleFactorSelector = mkSelector "setMaxScaleFactor:"

-- | @Selector@ for @autoScales@
autoScalesSelector :: Selector '[] Bool
autoScalesSelector = mkSelector "autoScales"

-- | @Selector@ for @setAutoScales:@
setAutoScalesSelector :: Selector '[Bool] ()
setAutoScalesSelector = mkSelector "setAutoScales:"

-- | @Selector@ for @scaleFactorForSizeToFit@
scaleFactorForSizeToFitSelector :: Selector '[] CDouble
scaleFactorForSizeToFitSelector = mkSelector "scaleFactorForSizeToFit"

-- | @Selector@ for @canZoomIn@
canZoomInSelector :: Selector '[] Bool
canZoomInSelector = mkSelector "canZoomIn"

-- | @Selector@ for @canZoomOut@
canZoomOutSelector :: Selector '[] Bool
canZoomOutSelector = mkSelector "canZoomOut"

-- | @Selector@ for @currentSelection@
currentSelectionSelector :: Selector '[] (Id PDFSelection)
currentSelectionSelector = mkSelector "currentSelection"

-- | @Selector@ for @setCurrentSelection:@
setCurrentSelectionSelector :: Selector '[Id PDFSelection] ()
setCurrentSelectionSelector = mkSelector "setCurrentSelection:"

-- | @Selector@ for @documentView@
documentViewSelector :: Selector '[] (Id NSView)
documentViewSelector = mkSelector "documentView"

-- | @Selector@ for @acceptsDraggedFiles@
acceptsDraggedFilesSelector :: Selector '[] Bool
acceptsDraggedFilesSelector = mkSelector "acceptsDraggedFiles"

-- | @Selector@ for @setAcceptsDraggedFiles:@
setAcceptsDraggedFilesSelector :: Selector '[Bool] ()
setAcceptsDraggedFilesSelector = mkSelector "setAcceptsDraggedFiles:"

-- | @Selector@ for @enableDataDetectors@
enableDataDetectorsSelector :: Selector '[] Bool
enableDataDetectorsSelector = mkSelector "enableDataDetectors"

-- | @Selector@ for @setEnableDataDetectors:@
setEnableDataDetectorsSelector :: Selector '[Bool] ()
setEnableDataDetectorsSelector = mkSelector "setEnableDataDetectors:"

-- | @Selector@ for @inMarkupMode@
inMarkupModeSelector :: Selector '[] Bool
inMarkupModeSelector = mkSelector "inMarkupMode"

-- | @Selector@ for @setInMarkupMode:@
setInMarkupModeSelector :: Selector '[Bool] ()
setInMarkupModeSelector = mkSelector "setInMarkupMode:"

-- | @Selector@ for @shouldAntiAlias@
shouldAntiAliasSelector :: Selector '[] Bool
shouldAntiAliasSelector = mkSelector "shouldAntiAlias"

-- | @Selector@ for @setShouldAntiAlias:@
setShouldAntiAliasSelector :: Selector '[Bool] ()
setShouldAntiAliasSelector = mkSelector "setShouldAntiAlias:"

-- | @Selector@ for @greekingThreshold@
greekingThresholdSelector :: Selector '[] CDouble
greekingThresholdSelector = mkSelector "greekingThreshold"

-- | @Selector@ for @setGreekingThreshold:@
setGreekingThresholdSelector :: Selector '[CDouble] ()
setGreekingThresholdSelector = mkSelector "setGreekingThreshold:"

-- | @Selector@ for @allowsDragging@
allowsDraggingSelector :: Selector '[] Bool
allowsDraggingSelector = mkSelector "allowsDragging"

-- | @Selector@ for @setAllowsDragging:@
setAllowsDraggingSelector :: Selector '[Bool] ()
setAllowsDraggingSelector = mkSelector "setAllowsDragging:"

