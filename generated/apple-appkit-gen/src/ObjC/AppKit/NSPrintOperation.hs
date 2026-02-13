{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPrintOperation@.
module ObjC.AppKit.NSPrintOperation
  ( NSPrintOperation
  , IsNSPrintOperation(..)
  , printOperationWithView_printInfo
  , pdfOperationWithView_insideRect_toData_printInfo
  , pdfOperationWithView_insideRect_toPath_printInfo
  , epsOperationWithView_insideRect_toData_printInfo
  , epsOperationWithView_insideRect_toPath_printInfo
  , printOperationWithView
  , pdfOperationWithView_insideRect_toData
  , epsOperationWithView_insideRect_toData
  , runOperationModalForWindow_delegate_didRunSelector_contextInfo
  , runOperation
  , createContext
  , destroyContext
  , deliverResult
  , cleanUpOperation
  , setAccessoryView
  , accessoryView
  , setJobStyleHint
  , jobStyleHint
  , setShowPanels
  , showPanels
  , currentOperation
  , setCurrentOperation
  , copyingOperation
  , preferredRenderingQuality
  , jobTitle
  , setJobTitle
  , showsPrintPanel
  , setShowsPrintPanel
  , showsProgressPanel
  , setShowsProgressPanel
  , printPanel
  , setPrintPanel
  , pdfPanel
  , setPDFPanel
  , canSpawnSeparateThread
  , setCanSpawnSeparateThread
  , pageOrder
  , setPageOrder
  , view
  , printInfo
  , setPrintInfo
  , context
  , pageRange
  , currentPage
  , accessoryViewSelector
  , canSpawnSeparateThreadSelector
  , cleanUpOperationSelector
  , contextSelector
  , copyingOperationSelector
  , createContextSelector
  , currentOperationSelector
  , currentPageSelector
  , deliverResultSelector
  , destroyContextSelector
  , epsOperationWithView_insideRect_toDataSelector
  , epsOperationWithView_insideRect_toData_printInfoSelector
  , epsOperationWithView_insideRect_toPath_printInfoSelector
  , jobStyleHintSelector
  , jobTitleSelector
  , pageOrderSelector
  , pageRangeSelector
  , pdfOperationWithView_insideRect_toDataSelector
  , pdfOperationWithView_insideRect_toData_printInfoSelector
  , pdfOperationWithView_insideRect_toPath_printInfoSelector
  , pdfPanelSelector
  , preferredRenderingQualitySelector
  , printInfoSelector
  , printOperationWithViewSelector
  , printOperationWithView_printInfoSelector
  , printPanelSelector
  , runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector
  , runOperationSelector
  , setAccessoryViewSelector
  , setCanSpawnSeparateThreadSelector
  , setCurrentOperationSelector
  , setJobStyleHintSelector
  , setJobTitleSelector
  , setPDFPanelSelector
  , setPageOrderSelector
  , setPrintInfoSelector
  , setPrintPanelSelector
  , setShowPanelsSelector
  , setShowsPrintPanelSelector
  , setShowsProgressPanelSelector
  , showPanelsSelector
  , showsPrintPanelSelector
  , showsProgressPanelSelector
  , viewSelector

  -- * Enum types
  , NSPrintRenderingQuality(NSPrintRenderingQuality)
  , pattern NSPrintRenderingQualityBest
  , pattern NSPrintRenderingQualityResponsive
  , NSPrintingPageOrder(NSPrintingPageOrder)
  , pattern NSDescendingPageOrder
  , pattern NSSpecialPageOrder
  , pattern NSAscendingPageOrder
  , pattern NSUnknownPageOrder

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ printOperationWithView:printInfo:@
printOperationWithView_printInfo :: (IsNSView view, IsNSPrintInfo printInfo) => view -> printInfo -> IO (Id NSPrintOperation)
printOperationWithView_printInfo view printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' printOperationWithView_printInfoSelector (toNSView view) (toNSPrintInfo printInfo)

-- | @+ PDFOperationWithView:insideRect:toData:printInfo:@
pdfOperationWithView_insideRect_toData_printInfo :: (IsNSView view, IsNSMutableData data_, IsNSPrintInfo printInfo) => view -> NSRect -> data_ -> printInfo -> IO (Id NSPrintOperation)
pdfOperationWithView_insideRect_toData_printInfo view rect data_ printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' pdfOperationWithView_insideRect_toData_printInfoSelector (toNSView view) rect (toNSMutableData data_) (toNSPrintInfo printInfo)

-- | @+ PDFOperationWithView:insideRect:toPath:printInfo:@
pdfOperationWithView_insideRect_toPath_printInfo :: (IsNSView view, IsNSString path, IsNSPrintInfo printInfo) => view -> NSRect -> path -> printInfo -> IO (Id NSPrintOperation)
pdfOperationWithView_insideRect_toPath_printInfo view rect path printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' pdfOperationWithView_insideRect_toPath_printInfoSelector (toNSView view) rect (toNSString path) (toNSPrintInfo printInfo)

-- | @+ EPSOperationWithView:insideRect:toData:printInfo:@
epsOperationWithView_insideRect_toData_printInfo :: (IsNSView view, IsNSMutableData data_, IsNSPrintInfo printInfo) => view -> NSRect -> data_ -> printInfo -> IO (Id NSPrintOperation)
epsOperationWithView_insideRect_toData_printInfo view rect data_ printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' epsOperationWithView_insideRect_toData_printInfoSelector (toNSView view) rect (toNSMutableData data_) (toNSPrintInfo printInfo)

-- | @+ EPSOperationWithView:insideRect:toPath:printInfo:@
epsOperationWithView_insideRect_toPath_printInfo :: (IsNSView view, IsNSString path, IsNSPrintInfo printInfo) => view -> NSRect -> path -> printInfo -> IO (Id NSPrintOperation)
epsOperationWithView_insideRect_toPath_printInfo view rect path printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' epsOperationWithView_insideRect_toPath_printInfoSelector (toNSView view) rect (toNSString path) (toNSPrintInfo printInfo)

-- | @+ printOperationWithView:@
printOperationWithView :: IsNSView view => view -> IO (Id NSPrintOperation)
printOperationWithView view =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' printOperationWithViewSelector (toNSView view)

-- | @+ PDFOperationWithView:insideRect:toData:@
pdfOperationWithView_insideRect_toData :: (IsNSView view, IsNSMutableData data_) => view -> NSRect -> data_ -> IO (Id NSPrintOperation)
pdfOperationWithView_insideRect_toData view rect data_ =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' pdfOperationWithView_insideRect_toDataSelector (toNSView view) rect (toNSMutableData data_)

-- | @+ EPSOperationWithView:insideRect:toData:@
epsOperationWithView_insideRect_toData :: (IsNSView view, IsNSMutableData data_) => view -> NSRect -> data_ -> IO (Id NSPrintOperation)
epsOperationWithView_insideRect_toData view rect data_ =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' epsOperationWithView_insideRect_toDataSelector (toNSView view) rect (toNSMutableData data_)

-- | @- runOperationModalForWindow:delegate:didRunSelector:contextInfo:@
runOperationModalForWindow_delegate_didRunSelector_contextInfo :: (IsNSPrintOperation nsPrintOperation, IsNSWindow docWindow) => nsPrintOperation -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
runOperationModalForWindow_delegate_didRunSelector_contextInfo nsPrintOperation docWindow delegate didRunSelector contextInfo =
  sendMessage nsPrintOperation runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector (toNSWindow docWindow) delegate didRunSelector contextInfo

-- | @- runOperation@
runOperation :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
runOperation nsPrintOperation =
  sendMessage nsPrintOperation runOperationSelector

-- | @- createContext@
createContext :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSGraphicsContext)
createContext nsPrintOperation =
  sendMessage nsPrintOperation createContextSelector

-- | @- destroyContext@
destroyContext :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO ()
destroyContext nsPrintOperation =
  sendMessage nsPrintOperation destroyContextSelector

-- | @- deliverResult@
deliverResult :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
deliverResult nsPrintOperation =
  sendMessage nsPrintOperation deliverResultSelector

-- | @- cleanUpOperation@
cleanUpOperation :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO ()
cleanUpOperation nsPrintOperation =
  sendMessage nsPrintOperation cleanUpOperationSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSPrintOperation nsPrintOperation, IsNSView view) => nsPrintOperation -> view -> IO ()
setAccessoryView nsPrintOperation view =
  sendMessage nsPrintOperation setAccessoryViewSelector (toNSView view)

-- | @- accessoryView@
accessoryView :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSView)
accessoryView nsPrintOperation =
  sendMessage nsPrintOperation accessoryViewSelector

-- | @- setJobStyleHint:@
setJobStyleHint :: (IsNSPrintOperation nsPrintOperation, IsNSString hint) => nsPrintOperation -> hint -> IO ()
setJobStyleHint nsPrintOperation hint =
  sendMessage nsPrintOperation setJobStyleHintSelector (toNSString hint)

-- | @- jobStyleHint@
jobStyleHint :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSString)
jobStyleHint nsPrintOperation =
  sendMessage nsPrintOperation jobStyleHintSelector

-- | @- setShowPanels:@
setShowPanels :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setShowPanels nsPrintOperation flag =
  sendMessage nsPrintOperation setShowPanelsSelector flag

-- | @- showPanels@
showPanels :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
showPanels nsPrintOperation =
  sendMessage nsPrintOperation showPanelsSelector

-- | @+ currentOperation@
currentOperation :: IO (Id NSPrintOperation)
currentOperation  =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' currentOperationSelector

-- | @+ setCurrentOperation:@
setCurrentOperation :: IsNSPrintOperation value => value -> IO ()
setCurrentOperation value =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMessage cls' setCurrentOperationSelector (toNSPrintOperation value)

-- | @- copyingOperation@
copyingOperation :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
copyingOperation nsPrintOperation =
  sendOwnedMessage nsPrintOperation copyingOperationSelector

-- | @- preferredRenderingQuality@
preferredRenderingQuality :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO NSPrintRenderingQuality
preferredRenderingQuality nsPrintOperation =
  sendMessage nsPrintOperation preferredRenderingQualitySelector

-- | @- jobTitle@
jobTitle :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSString)
jobTitle nsPrintOperation =
  sendMessage nsPrintOperation jobTitleSelector

-- | @- setJobTitle:@
setJobTitle :: (IsNSPrintOperation nsPrintOperation, IsNSString value) => nsPrintOperation -> value -> IO ()
setJobTitle nsPrintOperation value =
  sendMessage nsPrintOperation setJobTitleSelector (toNSString value)

-- | @- showsPrintPanel@
showsPrintPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
showsPrintPanel nsPrintOperation =
  sendMessage nsPrintOperation showsPrintPanelSelector

-- | @- setShowsPrintPanel:@
setShowsPrintPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setShowsPrintPanel nsPrintOperation value =
  sendMessage nsPrintOperation setShowsPrintPanelSelector value

-- | @- showsProgressPanel@
showsProgressPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
showsProgressPanel nsPrintOperation =
  sendMessage nsPrintOperation showsProgressPanelSelector

-- | @- setShowsProgressPanel:@
setShowsProgressPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setShowsProgressPanel nsPrintOperation value =
  sendMessage nsPrintOperation setShowsProgressPanelSelector value

-- | @- printPanel@
printPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSPrintPanel)
printPanel nsPrintOperation =
  sendMessage nsPrintOperation printPanelSelector

-- | @- setPrintPanel:@
setPrintPanel :: (IsNSPrintOperation nsPrintOperation, IsNSPrintPanel value) => nsPrintOperation -> value -> IO ()
setPrintPanel nsPrintOperation value =
  sendMessage nsPrintOperation setPrintPanelSelector (toNSPrintPanel value)

-- | @- PDFPanel@
pdfPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSPDFPanel)
pdfPanel nsPrintOperation =
  sendMessage nsPrintOperation pdfPanelSelector

-- | @- setPDFPanel:@
setPDFPanel :: (IsNSPrintOperation nsPrintOperation, IsNSPDFPanel value) => nsPrintOperation -> value -> IO ()
setPDFPanel nsPrintOperation value =
  sendMessage nsPrintOperation setPDFPanelSelector (toNSPDFPanel value)

-- | @- canSpawnSeparateThread@
canSpawnSeparateThread :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
canSpawnSeparateThread nsPrintOperation =
  sendMessage nsPrintOperation canSpawnSeparateThreadSelector

-- | @- setCanSpawnSeparateThread:@
setCanSpawnSeparateThread :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setCanSpawnSeparateThread nsPrintOperation value =
  sendMessage nsPrintOperation setCanSpawnSeparateThreadSelector value

-- | @- pageOrder@
pageOrder :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO NSPrintingPageOrder
pageOrder nsPrintOperation =
  sendMessage nsPrintOperation pageOrderSelector

-- | @- setPageOrder:@
setPageOrder :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> NSPrintingPageOrder -> IO ()
setPageOrder nsPrintOperation value =
  sendMessage nsPrintOperation setPageOrderSelector value

-- | @- view@
view :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSView)
view nsPrintOperation =
  sendMessage nsPrintOperation viewSelector

-- | @- printInfo@
printInfo :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSPrintInfo)
printInfo nsPrintOperation =
  sendMessage nsPrintOperation printInfoSelector

-- | @- setPrintInfo:@
setPrintInfo :: (IsNSPrintOperation nsPrintOperation, IsNSPrintInfo value) => nsPrintOperation -> value -> IO ()
setPrintInfo nsPrintOperation value =
  sendMessage nsPrintOperation setPrintInfoSelector (toNSPrintInfo value)

-- | @- context@
context :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSGraphicsContext)
context nsPrintOperation =
  sendMessage nsPrintOperation contextSelector

-- | @- pageRange@
pageRange :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO NSRange
pageRange nsPrintOperation =
  sendMessage nsPrintOperation pageRangeSelector

-- | @- currentPage@
currentPage :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO CLong
currentPage nsPrintOperation =
  sendMessage nsPrintOperation currentPageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printOperationWithView:printInfo:@
printOperationWithView_printInfoSelector :: Selector '[Id NSView, Id NSPrintInfo] (Id NSPrintOperation)
printOperationWithView_printInfoSelector = mkSelector "printOperationWithView:printInfo:"

-- | @Selector@ for @PDFOperationWithView:insideRect:toData:printInfo:@
pdfOperationWithView_insideRect_toData_printInfoSelector :: Selector '[Id NSView, NSRect, Id NSMutableData, Id NSPrintInfo] (Id NSPrintOperation)
pdfOperationWithView_insideRect_toData_printInfoSelector = mkSelector "PDFOperationWithView:insideRect:toData:printInfo:"

-- | @Selector@ for @PDFOperationWithView:insideRect:toPath:printInfo:@
pdfOperationWithView_insideRect_toPath_printInfoSelector :: Selector '[Id NSView, NSRect, Id NSString, Id NSPrintInfo] (Id NSPrintOperation)
pdfOperationWithView_insideRect_toPath_printInfoSelector = mkSelector "PDFOperationWithView:insideRect:toPath:printInfo:"

-- | @Selector@ for @EPSOperationWithView:insideRect:toData:printInfo:@
epsOperationWithView_insideRect_toData_printInfoSelector :: Selector '[Id NSView, NSRect, Id NSMutableData, Id NSPrintInfo] (Id NSPrintOperation)
epsOperationWithView_insideRect_toData_printInfoSelector = mkSelector "EPSOperationWithView:insideRect:toData:printInfo:"

-- | @Selector@ for @EPSOperationWithView:insideRect:toPath:printInfo:@
epsOperationWithView_insideRect_toPath_printInfoSelector :: Selector '[Id NSView, NSRect, Id NSString, Id NSPrintInfo] (Id NSPrintOperation)
epsOperationWithView_insideRect_toPath_printInfoSelector = mkSelector "EPSOperationWithView:insideRect:toPath:printInfo:"

-- | @Selector@ for @printOperationWithView:@
printOperationWithViewSelector :: Selector '[Id NSView] (Id NSPrintOperation)
printOperationWithViewSelector = mkSelector "printOperationWithView:"

-- | @Selector@ for @PDFOperationWithView:insideRect:toData:@
pdfOperationWithView_insideRect_toDataSelector :: Selector '[Id NSView, NSRect, Id NSMutableData] (Id NSPrintOperation)
pdfOperationWithView_insideRect_toDataSelector = mkSelector "PDFOperationWithView:insideRect:toData:"

-- | @Selector@ for @EPSOperationWithView:insideRect:toData:@
epsOperationWithView_insideRect_toDataSelector :: Selector '[Id NSView, NSRect, Id NSMutableData] (Id NSPrintOperation)
epsOperationWithView_insideRect_toDataSelector = mkSelector "EPSOperationWithView:insideRect:toData:"

-- | @Selector@ for @runOperationModalForWindow:delegate:didRunSelector:contextInfo:@
runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector :: Selector '[Id NSWindow, RawId, Sel, Ptr ()] ()
runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector = mkSelector "runOperationModalForWindow:delegate:didRunSelector:contextInfo:"

-- | @Selector@ for @runOperation@
runOperationSelector :: Selector '[] Bool
runOperationSelector = mkSelector "runOperation"

-- | @Selector@ for @createContext@
createContextSelector :: Selector '[] (Id NSGraphicsContext)
createContextSelector = mkSelector "createContext"

-- | @Selector@ for @destroyContext@
destroyContextSelector :: Selector '[] ()
destroyContextSelector = mkSelector "destroyContext"

-- | @Selector@ for @deliverResult@
deliverResultSelector :: Selector '[] Bool
deliverResultSelector = mkSelector "deliverResult"

-- | @Selector@ for @cleanUpOperation@
cleanUpOperationSelector :: Selector '[] ()
cleanUpOperationSelector = mkSelector "cleanUpOperation"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setJobStyleHint:@
setJobStyleHintSelector :: Selector '[Id NSString] ()
setJobStyleHintSelector = mkSelector "setJobStyleHint:"

-- | @Selector@ for @jobStyleHint@
jobStyleHintSelector :: Selector '[] (Id NSString)
jobStyleHintSelector = mkSelector "jobStyleHint"

-- | @Selector@ for @setShowPanels:@
setShowPanelsSelector :: Selector '[Bool] ()
setShowPanelsSelector = mkSelector "setShowPanels:"

-- | @Selector@ for @showPanels@
showPanelsSelector :: Selector '[] Bool
showPanelsSelector = mkSelector "showPanels"

-- | @Selector@ for @currentOperation@
currentOperationSelector :: Selector '[] (Id NSPrintOperation)
currentOperationSelector = mkSelector "currentOperation"

-- | @Selector@ for @setCurrentOperation:@
setCurrentOperationSelector :: Selector '[Id NSPrintOperation] ()
setCurrentOperationSelector = mkSelector "setCurrentOperation:"

-- | @Selector@ for @copyingOperation@
copyingOperationSelector :: Selector '[] Bool
copyingOperationSelector = mkSelector "copyingOperation"

-- | @Selector@ for @preferredRenderingQuality@
preferredRenderingQualitySelector :: Selector '[] NSPrintRenderingQuality
preferredRenderingQualitySelector = mkSelector "preferredRenderingQuality"

-- | @Selector@ for @jobTitle@
jobTitleSelector :: Selector '[] (Id NSString)
jobTitleSelector = mkSelector "jobTitle"

-- | @Selector@ for @setJobTitle:@
setJobTitleSelector :: Selector '[Id NSString] ()
setJobTitleSelector = mkSelector "setJobTitle:"

-- | @Selector@ for @showsPrintPanel@
showsPrintPanelSelector :: Selector '[] Bool
showsPrintPanelSelector = mkSelector "showsPrintPanel"

-- | @Selector@ for @setShowsPrintPanel:@
setShowsPrintPanelSelector :: Selector '[Bool] ()
setShowsPrintPanelSelector = mkSelector "setShowsPrintPanel:"

-- | @Selector@ for @showsProgressPanel@
showsProgressPanelSelector :: Selector '[] Bool
showsProgressPanelSelector = mkSelector "showsProgressPanel"

-- | @Selector@ for @setShowsProgressPanel:@
setShowsProgressPanelSelector :: Selector '[Bool] ()
setShowsProgressPanelSelector = mkSelector "setShowsProgressPanel:"

-- | @Selector@ for @printPanel@
printPanelSelector :: Selector '[] (Id NSPrintPanel)
printPanelSelector = mkSelector "printPanel"

-- | @Selector@ for @setPrintPanel:@
setPrintPanelSelector :: Selector '[Id NSPrintPanel] ()
setPrintPanelSelector = mkSelector "setPrintPanel:"

-- | @Selector@ for @PDFPanel@
pdfPanelSelector :: Selector '[] (Id NSPDFPanel)
pdfPanelSelector = mkSelector "PDFPanel"

-- | @Selector@ for @setPDFPanel:@
setPDFPanelSelector :: Selector '[Id NSPDFPanel] ()
setPDFPanelSelector = mkSelector "setPDFPanel:"

-- | @Selector@ for @canSpawnSeparateThread@
canSpawnSeparateThreadSelector :: Selector '[] Bool
canSpawnSeparateThreadSelector = mkSelector "canSpawnSeparateThread"

-- | @Selector@ for @setCanSpawnSeparateThread:@
setCanSpawnSeparateThreadSelector :: Selector '[Bool] ()
setCanSpawnSeparateThreadSelector = mkSelector "setCanSpawnSeparateThread:"

-- | @Selector@ for @pageOrder@
pageOrderSelector :: Selector '[] NSPrintingPageOrder
pageOrderSelector = mkSelector "pageOrder"

-- | @Selector@ for @setPageOrder:@
setPageOrderSelector :: Selector '[NSPrintingPageOrder] ()
setPageOrderSelector = mkSelector "setPageOrder:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector '[] (Id NSPrintInfo)
printInfoSelector = mkSelector "printInfo"

-- | @Selector@ for @setPrintInfo:@
setPrintInfoSelector :: Selector '[Id NSPrintInfo] ()
setPrintInfoSelector = mkSelector "setPrintInfo:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id NSGraphicsContext)
contextSelector = mkSelector "context"

-- | @Selector@ for @pageRange@
pageRangeSelector :: Selector '[] NSRange
pageRangeSelector = mkSelector "pageRange"

-- | @Selector@ for @currentPage@
currentPageSelector :: Selector '[] CLong
currentPageSelector = mkSelector "currentPage"

