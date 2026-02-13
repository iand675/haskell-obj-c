{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPageLayout@.
module ObjC.AppKit.NSPageLayout
  ( NSPageLayout
  , IsNSPageLayout(..)
  , pageLayout
  , addAccessoryController
  , removeAccessoryController
  , beginSheetUsingPrintInfo_onWindow_completionHandler
  , beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo
  , runModalWithPrintInfo
  , runModal
  , setAccessoryView
  , accessoryView
  , readPrintInfo
  , writePrintInfo
  , accessoryControllers
  , printInfo
  , accessoryControllersSelector
  , accessoryViewSelector
  , addAccessoryControllerSelector
  , beginSheetUsingPrintInfo_onWindow_completionHandlerSelector
  , beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector
  , pageLayoutSelector
  , printInfoSelector
  , readPrintInfoSelector
  , removeAccessoryControllerSelector
  , runModalSelector
  , runModalWithPrintInfoSelector
  , setAccessoryViewSelector
  , writePrintInfoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ pageLayout@
pageLayout :: IO (Id NSPageLayout)
pageLayout  =
  do
    cls' <- getRequiredClass "NSPageLayout"
    sendClassMessage cls' pageLayoutSelector

-- | @- addAccessoryController:@
addAccessoryController :: (IsNSPageLayout nsPageLayout, IsNSViewController accessoryController) => nsPageLayout -> accessoryController -> IO ()
addAccessoryController nsPageLayout accessoryController =
  sendMessage nsPageLayout addAccessoryControllerSelector (toNSViewController accessoryController)

-- | @- removeAccessoryController:@
removeAccessoryController :: (IsNSPageLayout nsPageLayout, IsNSViewController accessoryController) => nsPageLayout -> accessoryController -> IO ()
removeAccessoryController nsPageLayout accessoryController =
  sendMessage nsPageLayout removeAccessoryControllerSelector (toNSViewController accessoryController)

-- | @- beginSheetUsingPrintInfo:onWindow:completionHandler:@
beginSheetUsingPrintInfo_onWindow_completionHandler :: (IsNSPageLayout nsPageLayout, IsNSPrintInfo printInfo, IsNSWindow parentWindow) => nsPageLayout -> printInfo -> parentWindow -> Ptr () -> IO ()
beginSheetUsingPrintInfo_onWindow_completionHandler nsPageLayout printInfo parentWindow handler =
  sendMessage nsPageLayout beginSheetUsingPrintInfo_onWindow_completionHandlerSelector (toNSPrintInfo printInfo) (toNSWindow parentWindow) handler

-- | @- beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:@
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo :: (IsNSPageLayout nsPageLayout, IsNSPrintInfo printInfo, IsNSWindow docWindow) => nsPageLayout -> printInfo -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo nsPageLayout printInfo docWindow delegate didEndSelector contextInfo =
  sendMessage nsPageLayout beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector (toNSPrintInfo printInfo) (toNSWindow docWindow) delegate didEndSelector contextInfo

-- | @- runModalWithPrintInfo:@
runModalWithPrintInfo :: (IsNSPageLayout nsPageLayout, IsNSPrintInfo printInfo) => nsPageLayout -> printInfo -> IO CLong
runModalWithPrintInfo nsPageLayout printInfo =
  sendMessage nsPageLayout runModalWithPrintInfoSelector (toNSPrintInfo printInfo)

-- | @- runModal@
runModal :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO CLong
runModal nsPageLayout =
  sendMessage nsPageLayout runModalSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSPageLayout nsPageLayout, IsNSView accessoryView) => nsPageLayout -> accessoryView -> IO ()
setAccessoryView nsPageLayout accessoryView =
  sendMessage nsPageLayout setAccessoryViewSelector (toNSView accessoryView)

-- | @- accessoryView@
accessoryView :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO (Id NSView)
accessoryView nsPageLayout =
  sendMessage nsPageLayout accessoryViewSelector

-- | @- readPrintInfo@
readPrintInfo :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO ()
readPrintInfo nsPageLayout =
  sendMessage nsPageLayout readPrintInfoSelector

-- | @- writePrintInfo@
writePrintInfo :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO ()
writePrintInfo nsPageLayout =
  sendMessage nsPageLayout writePrintInfoSelector

-- | @- accessoryControllers@
accessoryControllers :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO (Id NSArray)
accessoryControllers nsPageLayout =
  sendMessage nsPageLayout accessoryControllersSelector

-- | @- printInfo@
printInfo :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO (Id NSPrintInfo)
printInfo nsPageLayout =
  sendMessage nsPageLayout printInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pageLayout@
pageLayoutSelector :: Selector '[] (Id NSPageLayout)
pageLayoutSelector = mkSelector "pageLayout"

-- | @Selector@ for @addAccessoryController:@
addAccessoryControllerSelector :: Selector '[Id NSViewController] ()
addAccessoryControllerSelector = mkSelector "addAccessoryController:"

-- | @Selector@ for @removeAccessoryController:@
removeAccessoryControllerSelector :: Selector '[Id NSViewController] ()
removeAccessoryControllerSelector = mkSelector "removeAccessoryController:"

-- | @Selector@ for @beginSheetUsingPrintInfo:onWindow:completionHandler:@
beginSheetUsingPrintInfo_onWindow_completionHandlerSelector :: Selector '[Id NSPrintInfo, Id NSWindow, Ptr ()] ()
beginSheetUsingPrintInfo_onWindow_completionHandlerSelector = mkSelector "beginSheetUsingPrintInfo:onWindow:completionHandler:"

-- | @Selector@ for @beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:@
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector :: Selector '[Id NSPrintInfo, Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalWithPrintInfo:@
runModalWithPrintInfoSelector :: Selector '[Id NSPrintInfo] CLong
runModalWithPrintInfoSelector = mkSelector "runModalWithPrintInfo:"

-- | @Selector@ for @runModal@
runModalSelector :: Selector '[] CLong
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @readPrintInfo@
readPrintInfoSelector :: Selector '[] ()
readPrintInfoSelector = mkSelector "readPrintInfo"

-- | @Selector@ for @writePrintInfo@
writePrintInfoSelector :: Selector '[] ()
writePrintInfoSelector = mkSelector "writePrintInfo"

-- | @Selector@ for @accessoryControllers@
accessoryControllersSelector :: Selector '[] (Id NSArray)
accessoryControllersSelector = mkSelector "accessoryControllers"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector '[] (Id NSPrintInfo)
printInfoSelector = mkSelector "printInfo"

