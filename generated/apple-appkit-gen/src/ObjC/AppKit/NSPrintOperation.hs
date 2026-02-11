{-# LANGUAGE PatternSynonyms #-}
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
  , printOperationWithView_printInfoSelector
  , pdfOperationWithView_insideRect_toData_printInfoSelector
  , pdfOperationWithView_insideRect_toPath_printInfoSelector
  , epsOperationWithView_insideRect_toData_printInfoSelector
  , epsOperationWithView_insideRect_toPath_printInfoSelector
  , printOperationWithViewSelector
  , pdfOperationWithView_insideRect_toDataSelector
  , epsOperationWithView_insideRect_toDataSelector
  , runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector
  , runOperationSelector
  , createContextSelector
  , destroyContextSelector
  , deliverResultSelector
  , cleanUpOperationSelector
  , setAccessoryViewSelector
  , accessoryViewSelector
  , setJobStyleHintSelector
  , jobStyleHintSelector
  , setShowPanelsSelector
  , showPanelsSelector
  , currentOperationSelector
  , setCurrentOperationSelector
  , copyingOperationSelector
  , preferredRenderingQualitySelector
  , jobTitleSelector
  , setJobTitleSelector
  , showsPrintPanelSelector
  , setShowsPrintPanelSelector
  , showsProgressPanelSelector
  , setShowsProgressPanelSelector
  , printPanelSelector
  , setPrintPanelSelector
  , pdfPanelSelector
  , setPDFPanelSelector
  , canSpawnSeparateThreadSelector
  , setCanSpawnSeparateThreadSelector
  , pageOrderSelector
  , setPageOrderSelector
  , viewSelector
  , printInfoSelector
  , setPrintInfoSelector
  , contextSelector
  , pageRangeSelector
  , currentPageSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ printOperationWithView:printInfo:@
printOperationWithView_printInfo :: (IsNSView view, IsNSPrintInfo printInfo) => view -> printInfo -> IO (Id NSPrintOperation)
printOperationWithView_printInfo view printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr printInfo $ \raw_printInfo ->
        sendClassMsg cls' (mkSelector "printOperationWithView:printInfo:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @+ PDFOperationWithView:insideRect:toData:printInfo:@
pdfOperationWithView_insideRect_toData_printInfo :: (IsNSView view, IsNSMutableData data_, IsNSPrintInfo printInfo) => view -> NSRect -> data_ -> printInfo -> IO (Id NSPrintOperation)
pdfOperationWithView_insideRect_toData_printInfo view rect data_ printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr printInfo $ \raw_printInfo ->
          sendClassMsg cls' (mkSelector "PDFOperationWithView:insideRect:toData:printInfo:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSRect rect, argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @+ PDFOperationWithView:insideRect:toPath:printInfo:@
pdfOperationWithView_insideRect_toPath_printInfo :: (IsNSView view, IsNSString path, IsNSPrintInfo printInfo) => view -> NSRect -> path -> printInfo -> IO (Id NSPrintOperation)
pdfOperationWithView_insideRect_toPath_printInfo view rect path printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr path $ \raw_path ->
        withObjCPtr printInfo $ \raw_printInfo ->
          sendClassMsg cls' (mkSelector "PDFOperationWithView:insideRect:toPath:printInfo:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSRect rect, argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @+ EPSOperationWithView:insideRect:toData:printInfo:@
epsOperationWithView_insideRect_toData_printInfo :: (IsNSView view, IsNSMutableData data_, IsNSPrintInfo printInfo) => view -> NSRect -> data_ -> printInfo -> IO (Id NSPrintOperation)
epsOperationWithView_insideRect_toData_printInfo view rect data_ printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr data_ $ \raw_data_ ->
        withObjCPtr printInfo $ \raw_printInfo ->
          sendClassMsg cls' (mkSelector "EPSOperationWithView:insideRect:toData:printInfo:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSRect rect, argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @+ EPSOperationWithView:insideRect:toPath:printInfo:@
epsOperationWithView_insideRect_toPath_printInfo :: (IsNSView view, IsNSString path, IsNSPrintInfo printInfo) => view -> NSRect -> path -> printInfo -> IO (Id NSPrintOperation)
epsOperationWithView_insideRect_toPath_printInfo view rect path printInfo =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr path $ \raw_path ->
        withObjCPtr printInfo $ \raw_printInfo ->
          sendClassMsg cls' (mkSelector "EPSOperationWithView:insideRect:toPath:printInfo:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSRect rect, argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_printInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @+ printOperationWithView:@
printOperationWithView :: IsNSView view => view -> IO (Id NSPrintOperation)
printOperationWithView view =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      sendClassMsg cls' (mkSelector "printOperationWithView:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @+ PDFOperationWithView:insideRect:toData:@
pdfOperationWithView_insideRect_toData :: (IsNSView view, IsNSMutableData data_) => view -> NSRect -> data_ -> IO (Id NSPrintOperation)
pdfOperationWithView_insideRect_toData view rect data_ =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr data_ $ \raw_data_ ->
        sendClassMsg cls' (mkSelector "PDFOperationWithView:insideRect:toData:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSRect rect, argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ EPSOperationWithView:insideRect:toData:@
epsOperationWithView_insideRect_toData :: (IsNSView view, IsNSMutableData data_) => view -> NSRect -> data_ -> IO (Id NSPrintOperation)
epsOperationWithView_insideRect_toData view rect data_ =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr view $ \raw_view ->
      withObjCPtr data_ $ \raw_data_ ->
        sendClassMsg cls' (mkSelector "EPSOperationWithView:insideRect:toData:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ()), argNSRect rect, argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- runOperationModalForWindow:delegate:didRunSelector:contextInfo:@
runOperationModalForWindow_delegate_didRunSelector_contextInfo :: (IsNSPrintOperation nsPrintOperation, IsNSWindow docWindow) => nsPrintOperation -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
runOperationModalForWindow_delegate_didRunSelector_contextInfo nsPrintOperation  docWindow delegate didRunSelector contextInfo =
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg nsPrintOperation (mkSelector "runOperationModalForWindow:delegate:didRunSelector:contextInfo:") retVoid [argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didRunSelector), argPtr contextInfo]

-- | @- runOperation@
runOperation :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
runOperation nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "runOperation") retCULong []

-- | @- createContext@
createContext :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSGraphicsContext)
createContext nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "createContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destroyContext@
destroyContext :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO ()
destroyContext nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "destroyContext") retVoid []

-- | @- deliverResult@
deliverResult :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
deliverResult nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "deliverResult") retCULong []

-- | @- cleanUpOperation@
cleanUpOperation :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO ()
cleanUpOperation nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "cleanUpOperation") retVoid []

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSPrintOperation nsPrintOperation, IsNSView view) => nsPrintOperation -> view -> IO ()
setAccessoryView nsPrintOperation  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsPrintOperation (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | @- accessoryView@
accessoryView :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSView)
accessoryView nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setJobStyleHint:@
setJobStyleHint :: (IsNSPrintOperation nsPrintOperation, IsNSString hint) => nsPrintOperation -> hint -> IO ()
setJobStyleHint nsPrintOperation  hint =
  withObjCPtr hint $ \raw_hint ->
      sendMsg nsPrintOperation (mkSelector "setJobStyleHint:") retVoid [argPtr (castPtr raw_hint :: Ptr ())]

-- | @- jobStyleHint@
jobStyleHint :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSString)
jobStyleHint nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "jobStyleHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShowPanels:@
setShowPanels :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setShowPanels nsPrintOperation  flag =
    sendMsg nsPrintOperation (mkSelector "setShowPanels:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- showPanels@
showPanels :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
showPanels nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "showPanels") retCULong []

-- | @+ currentOperation@
currentOperation :: IO (Id NSPrintOperation)
currentOperation  =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    sendClassMsg cls' (mkSelector "currentOperation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setCurrentOperation:@
setCurrentOperation :: IsNSPrintOperation value => value -> IO ()
setCurrentOperation value =
  do
    cls' <- getRequiredClass "NSPrintOperation"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setCurrentOperation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- copyingOperation@
copyingOperation :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
copyingOperation nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "copyingOperation") retCULong []

-- | @- preferredRenderingQuality@
preferredRenderingQuality :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO NSPrintRenderingQuality
preferredRenderingQuality nsPrintOperation  =
    fmap (coerce :: CLong -> NSPrintRenderingQuality) $ sendMsg nsPrintOperation (mkSelector "preferredRenderingQuality") retCLong []

-- | @- jobTitle@
jobTitle :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSString)
jobTitle nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "jobTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setJobTitle:@
setJobTitle :: (IsNSPrintOperation nsPrintOperation, IsNSString value) => nsPrintOperation -> value -> IO ()
setJobTitle nsPrintOperation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintOperation (mkSelector "setJobTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- showsPrintPanel@
showsPrintPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
showsPrintPanel nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "showsPrintPanel") retCULong []

-- | @- setShowsPrintPanel:@
setShowsPrintPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setShowsPrintPanel nsPrintOperation  value =
    sendMsg nsPrintOperation (mkSelector "setShowsPrintPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsProgressPanel@
showsProgressPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
showsProgressPanel nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "showsProgressPanel") retCULong []

-- | @- setShowsProgressPanel:@
setShowsProgressPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setShowsProgressPanel nsPrintOperation  value =
    sendMsg nsPrintOperation (mkSelector "setShowsProgressPanel:") retVoid [argCULong (if value then 1 else 0)]

-- | @- printPanel@
printPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSPrintPanel)
printPanel nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "printPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrintPanel:@
setPrintPanel :: (IsNSPrintOperation nsPrintOperation, IsNSPrintPanel value) => nsPrintOperation -> value -> IO ()
setPrintPanel nsPrintOperation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintOperation (mkSelector "setPrintPanel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- PDFPanel@
pdfPanel :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSPDFPanel)
pdfPanel nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "PDFPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPDFPanel:@
setPDFPanel :: (IsNSPrintOperation nsPrintOperation, IsNSPDFPanel value) => nsPrintOperation -> value -> IO ()
setPDFPanel nsPrintOperation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintOperation (mkSelector "setPDFPanel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- canSpawnSeparateThread@
canSpawnSeparateThread :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO Bool
canSpawnSeparateThread nsPrintOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPrintOperation (mkSelector "canSpawnSeparateThread") retCULong []

-- | @- setCanSpawnSeparateThread:@
setCanSpawnSeparateThread :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> Bool -> IO ()
setCanSpawnSeparateThread nsPrintOperation  value =
    sendMsg nsPrintOperation (mkSelector "setCanSpawnSeparateThread:") retVoid [argCULong (if value then 1 else 0)]

-- | @- pageOrder@
pageOrder :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO NSPrintingPageOrder
pageOrder nsPrintOperation  =
    fmap (coerce :: CLong -> NSPrintingPageOrder) $ sendMsg nsPrintOperation (mkSelector "pageOrder") retCLong []

-- | @- setPageOrder:@
setPageOrder :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> NSPrintingPageOrder -> IO ()
setPageOrder nsPrintOperation  value =
    sendMsg nsPrintOperation (mkSelector "setPageOrder:") retVoid [argCLong (coerce value)]

-- | @- view@
view :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSView)
view nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- printInfo@
printInfo :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSPrintInfo)
printInfo nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "printInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrintInfo:@
setPrintInfo :: (IsNSPrintOperation nsPrintOperation, IsNSPrintInfo value) => nsPrintOperation -> value -> IO ()
setPrintInfo nsPrintOperation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsPrintOperation (mkSelector "setPrintInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- context@
context :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO (Id NSGraphicsContext)
context nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pageRange@
pageRange :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO NSRange
pageRange nsPrintOperation  =
    sendMsgStret nsPrintOperation (mkSelector "pageRange") retNSRange []

-- | @- currentPage@
currentPage :: IsNSPrintOperation nsPrintOperation => nsPrintOperation -> IO CLong
currentPage nsPrintOperation  =
    sendMsg nsPrintOperation (mkSelector "currentPage") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printOperationWithView:printInfo:@
printOperationWithView_printInfoSelector :: Selector
printOperationWithView_printInfoSelector = mkSelector "printOperationWithView:printInfo:"

-- | @Selector@ for @PDFOperationWithView:insideRect:toData:printInfo:@
pdfOperationWithView_insideRect_toData_printInfoSelector :: Selector
pdfOperationWithView_insideRect_toData_printInfoSelector = mkSelector "PDFOperationWithView:insideRect:toData:printInfo:"

-- | @Selector@ for @PDFOperationWithView:insideRect:toPath:printInfo:@
pdfOperationWithView_insideRect_toPath_printInfoSelector :: Selector
pdfOperationWithView_insideRect_toPath_printInfoSelector = mkSelector "PDFOperationWithView:insideRect:toPath:printInfo:"

-- | @Selector@ for @EPSOperationWithView:insideRect:toData:printInfo:@
epsOperationWithView_insideRect_toData_printInfoSelector :: Selector
epsOperationWithView_insideRect_toData_printInfoSelector = mkSelector "EPSOperationWithView:insideRect:toData:printInfo:"

-- | @Selector@ for @EPSOperationWithView:insideRect:toPath:printInfo:@
epsOperationWithView_insideRect_toPath_printInfoSelector :: Selector
epsOperationWithView_insideRect_toPath_printInfoSelector = mkSelector "EPSOperationWithView:insideRect:toPath:printInfo:"

-- | @Selector@ for @printOperationWithView:@
printOperationWithViewSelector :: Selector
printOperationWithViewSelector = mkSelector "printOperationWithView:"

-- | @Selector@ for @PDFOperationWithView:insideRect:toData:@
pdfOperationWithView_insideRect_toDataSelector :: Selector
pdfOperationWithView_insideRect_toDataSelector = mkSelector "PDFOperationWithView:insideRect:toData:"

-- | @Selector@ for @EPSOperationWithView:insideRect:toData:@
epsOperationWithView_insideRect_toDataSelector :: Selector
epsOperationWithView_insideRect_toDataSelector = mkSelector "EPSOperationWithView:insideRect:toData:"

-- | @Selector@ for @runOperationModalForWindow:delegate:didRunSelector:contextInfo:@
runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector :: Selector
runOperationModalForWindow_delegate_didRunSelector_contextInfoSelector = mkSelector "runOperationModalForWindow:delegate:didRunSelector:contextInfo:"

-- | @Selector@ for @runOperation@
runOperationSelector :: Selector
runOperationSelector = mkSelector "runOperation"

-- | @Selector@ for @createContext@
createContextSelector :: Selector
createContextSelector = mkSelector "createContext"

-- | @Selector@ for @destroyContext@
destroyContextSelector :: Selector
destroyContextSelector = mkSelector "destroyContext"

-- | @Selector@ for @deliverResult@
deliverResultSelector :: Selector
deliverResultSelector = mkSelector "deliverResult"

-- | @Selector@ for @cleanUpOperation@
cleanUpOperationSelector :: Selector
cleanUpOperationSelector = mkSelector "cleanUpOperation"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setJobStyleHint:@
setJobStyleHintSelector :: Selector
setJobStyleHintSelector = mkSelector "setJobStyleHint:"

-- | @Selector@ for @jobStyleHint@
jobStyleHintSelector :: Selector
jobStyleHintSelector = mkSelector "jobStyleHint"

-- | @Selector@ for @setShowPanels:@
setShowPanelsSelector :: Selector
setShowPanelsSelector = mkSelector "setShowPanels:"

-- | @Selector@ for @showPanels@
showPanelsSelector :: Selector
showPanelsSelector = mkSelector "showPanels"

-- | @Selector@ for @currentOperation@
currentOperationSelector :: Selector
currentOperationSelector = mkSelector "currentOperation"

-- | @Selector@ for @setCurrentOperation:@
setCurrentOperationSelector :: Selector
setCurrentOperationSelector = mkSelector "setCurrentOperation:"

-- | @Selector@ for @copyingOperation@
copyingOperationSelector :: Selector
copyingOperationSelector = mkSelector "copyingOperation"

-- | @Selector@ for @preferredRenderingQuality@
preferredRenderingQualitySelector :: Selector
preferredRenderingQualitySelector = mkSelector "preferredRenderingQuality"

-- | @Selector@ for @jobTitle@
jobTitleSelector :: Selector
jobTitleSelector = mkSelector "jobTitle"

-- | @Selector@ for @setJobTitle:@
setJobTitleSelector :: Selector
setJobTitleSelector = mkSelector "setJobTitle:"

-- | @Selector@ for @showsPrintPanel@
showsPrintPanelSelector :: Selector
showsPrintPanelSelector = mkSelector "showsPrintPanel"

-- | @Selector@ for @setShowsPrintPanel:@
setShowsPrintPanelSelector :: Selector
setShowsPrintPanelSelector = mkSelector "setShowsPrintPanel:"

-- | @Selector@ for @showsProgressPanel@
showsProgressPanelSelector :: Selector
showsProgressPanelSelector = mkSelector "showsProgressPanel"

-- | @Selector@ for @setShowsProgressPanel:@
setShowsProgressPanelSelector :: Selector
setShowsProgressPanelSelector = mkSelector "setShowsProgressPanel:"

-- | @Selector@ for @printPanel@
printPanelSelector :: Selector
printPanelSelector = mkSelector "printPanel"

-- | @Selector@ for @setPrintPanel:@
setPrintPanelSelector :: Selector
setPrintPanelSelector = mkSelector "setPrintPanel:"

-- | @Selector@ for @PDFPanel@
pdfPanelSelector :: Selector
pdfPanelSelector = mkSelector "PDFPanel"

-- | @Selector@ for @setPDFPanel:@
setPDFPanelSelector :: Selector
setPDFPanelSelector = mkSelector "setPDFPanel:"

-- | @Selector@ for @canSpawnSeparateThread@
canSpawnSeparateThreadSelector :: Selector
canSpawnSeparateThreadSelector = mkSelector "canSpawnSeparateThread"

-- | @Selector@ for @setCanSpawnSeparateThread:@
setCanSpawnSeparateThreadSelector :: Selector
setCanSpawnSeparateThreadSelector = mkSelector "setCanSpawnSeparateThread:"

-- | @Selector@ for @pageOrder@
pageOrderSelector :: Selector
pageOrderSelector = mkSelector "pageOrder"

-- | @Selector@ for @setPageOrder:@
setPageOrderSelector :: Selector
setPageOrderSelector = mkSelector "setPageOrder:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector
printInfoSelector = mkSelector "printInfo"

-- | @Selector@ for @setPrintInfo:@
setPrintInfoSelector :: Selector
setPrintInfoSelector = mkSelector "setPrintInfo:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @pageRange@
pageRangeSelector :: Selector
pageRangeSelector = mkSelector "pageRange"

-- | @Selector@ for @currentPage@
currentPageSelector :: Selector
currentPageSelector = mkSelector "currentPage"

