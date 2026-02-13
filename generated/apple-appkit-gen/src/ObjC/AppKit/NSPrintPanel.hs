{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPrintPanel@.
module ObjC.AppKit.NSPrintPanel
  ( NSPrintPanel
  , IsNSPrintPanel(..)
  , printPanel
  , addAccessoryController
  , removeAccessoryController
  , setDefaultButtonTitle
  , defaultButtonTitle
  , beginSheetUsingPrintInfo_onWindow_completionHandler
  , beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo
  , runModalWithPrintInfo
  , runModal
  , setAccessoryView
  , accessoryView
  , updateFromPrintInfo
  , finalWritePrintInfo
  , accessoryControllers
  , options
  , setOptions
  , helpAnchor
  , setHelpAnchor
  , jobStyleHint
  , setJobStyleHint
  , printInfo
  , accessoryControllersSelector
  , accessoryViewSelector
  , addAccessoryControllerSelector
  , beginSheetUsingPrintInfo_onWindow_completionHandlerSelector
  , beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector
  , defaultButtonTitleSelector
  , finalWritePrintInfoSelector
  , helpAnchorSelector
  , jobStyleHintSelector
  , optionsSelector
  , printInfoSelector
  , printPanelSelector
  , removeAccessoryControllerSelector
  , runModalSelector
  , runModalWithPrintInfoSelector
  , setAccessoryViewSelector
  , setDefaultButtonTitleSelector
  , setHelpAnchorSelector
  , setJobStyleHintSelector
  , setOptionsSelector
  , updateFromPrintInfoSelector

  -- * Enum types
  , NSPrintPanelOptions(NSPrintPanelOptions)
  , pattern NSPrintPanelShowsCopies
  , pattern NSPrintPanelShowsPageRange
  , pattern NSPrintPanelShowsPaperSize
  , pattern NSPrintPanelShowsOrientation
  , pattern NSPrintPanelShowsScaling
  , pattern NSPrintPanelShowsPrintSelection
  , pattern NSPrintPanelShowsPageSetupAccessory
  , pattern NSPrintPanelShowsPreview

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ printPanel@
printPanel :: IO (Id NSPrintPanel)
printPanel  =
  do
    cls' <- getRequiredClass "NSPrintPanel"
    sendClassMessage cls' printPanelSelector

-- | @- addAccessoryController:@
addAccessoryController :: (IsNSPrintPanel nsPrintPanel, IsNSViewController accessoryController) => nsPrintPanel -> accessoryController -> IO ()
addAccessoryController nsPrintPanel accessoryController =
  sendMessage nsPrintPanel addAccessoryControllerSelector (toNSViewController accessoryController)

-- | @- removeAccessoryController:@
removeAccessoryController :: (IsNSPrintPanel nsPrintPanel, IsNSViewController accessoryController) => nsPrintPanel -> accessoryController -> IO ()
removeAccessoryController nsPrintPanel accessoryController =
  sendMessage nsPrintPanel removeAccessoryControllerSelector (toNSViewController accessoryController)

-- | @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsNSPrintPanel nsPrintPanel, IsNSString defaultButtonTitle) => nsPrintPanel -> defaultButtonTitle -> IO ()
setDefaultButtonTitle nsPrintPanel defaultButtonTitle =
  sendMessage nsPrintPanel setDefaultButtonTitleSelector (toNSString defaultButtonTitle)

-- | @- defaultButtonTitle@
defaultButtonTitle :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSString)
defaultButtonTitle nsPrintPanel =
  sendMessage nsPrintPanel defaultButtonTitleSelector

-- | @- beginSheetUsingPrintInfo:onWindow:completionHandler:@
beginSheetUsingPrintInfo_onWindow_completionHandler :: (IsNSPrintPanel nsPrintPanel, IsNSPrintInfo printInfo, IsNSWindow parentWindow) => nsPrintPanel -> printInfo -> parentWindow -> Ptr () -> IO ()
beginSheetUsingPrintInfo_onWindow_completionHandler nsPrintPanel printInfo parentWindow handler =
  sendMessage nsPrintPanel beginSheetUsingPrintInfo_onWindow_completionHandlerSelector (toNSPrintInfo printInfo) (toNSWindow parentWindow) handler

-- | @- beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:@
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo :: (IsNSPrintPanel nsPrintPanel, IsNSPrintInfo printInfo, IsNSWindow docWindow) => nsPrintPanel -> printInfo -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo nsPrintPanel printInfo docWindow delegate didEndSelector contextInfo =
  sendMessage nsPrintPanel beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector (toNSPrintInfo printInfo) (toNSWindow docWindow) delegate didEndSelector contextInfo

-- | @- runModalWithPrintInfo:@
runModalWithPrintInfo :: (IsNSPrintPanel nsPrintPanel, IsNSPrintInfo printInfo) => nsPrintPanel -> printInfo -> IO CLong
runModalWithPrintInfo nsPrintPanel printInfo =
  sendMessage nsPrintPanel runModalWithPrintInfoSelector (toNSPrintInfo printInfo)

-- | @- runModal@
runModal :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO CLong
runModal nsPrintPanel =
  sendMessage nsPrintPanel runModalSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSPrintPanel nsPrintPanel, IsNSView accessoryView) => nsPrintPanel -> accessoryView -> IO ()
setAccessoryView nsPrintPanel accessoryView =
  sendMessage nsPrintPanel setAccessoryViewSelector (toNSView accessoryView)

-- | @- accessoryView@
accessoryView :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSView)
accessoryView nsPrintPanel =
  sendMessage nsPrintPanel accessoryViewSelector

-- | @- updateFromPrintInfo@
updateFromPrintInfo :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO ()
updateFromPrintInfo nsPrintPanel =
  sendMessage nsPrintPanel updateFromPrintInfoSelector

-- | @- finalWritePrintInfo@
finalWritePrintInfo :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO ()
finalWritePrintInfo nsPrintPanel =
  sendMessage nsPrintPanel finalWritePrintInfoSelector

-- | @- accessoryControllers@
accessoryControllers :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSArray)
accessoryControllers nsPrintPanel =
  sendMessage nsPrintPanel accessoryControllersSelector

-- | @- options@
options :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO NSPrintPanelOptions
options nsPrintPanel =
  sendMessage nsPrintPanel optionsSelector

-- | @- setOptions:@
setOptions :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> NSPrintPanelOptions -> IO ()
setOptions nsPrintPanel value =
  sendMessage nsPrintPanel setOptionsSelector value

-- | @- helpAnchor@
helpAnchor :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSString)
helpAnchor nsPrintPanel =
  sendMessage nsPrintPanel helpAnchorSelector

-- | @- setHelpAnchor:@
setHelpAnchor :: (IsNSPrintPanel nsPrintPanel, IsNSString value) => nsPrintPanel -> value -> IO ()
setHelpAnchor nsPrintPanel value =
  sendMessage nsPrintPanel setHelpAnchorSelector (toNSString value)

-- | @- jobStyleHint@
jobStyleHint :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSString)
jobStyleHint nsPrintPanel =
  sendMessage nsPrintPanel jobStyleHintSelector

-- | @- setJobStyleHint:@
setJobStyleHint :: (IsNSPrintPanel nsPrintPanel, IsNSString value) => nsPrintPanel -> value -> IO ()
setJobStyleHint nsPrintPanel value =
  sendMessage nsPrintPanel setJobStyleHintSelector (toNSString value)

-- | @- printInfo@
printInfo :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSPrintInfo)
printInfo nsPrintPanel =
  sendMessage nsPrintPanel printInfoSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printPanel@
printPanelSelector :: Selector '[] (Id NSPrintPanel)
printPanelSelector = mkSelector "printPanel"

-- | @Selector@ for @addAccessoryController:@
addAccessoryControllerSelector :: Selector '[Id NSViewController] ()
addAccessoryControllerSelector = mkSelector "addAccessoryController:"

-- | @Selector@ for @removeAccessoryController:@
removeAccessoryControllerSelector :: Selector '[Id NSViewController] ()
removeAccessoryControllerSelector = mkSelector "removeAccessoryController:"

-- | @Selector@ for @setDefaultButtonTitle:@
setDefaultButtonTitleSelector :: Selector '[Id NSString] ()
setDefaultButtonTitleSelector = mkSelector "setDefaultButtonTitle:"

-- | @Selector@ for @defaultButtonTitle@
defaultButtonTitleSelector :: Selector '[] (Id NSString)
defaultButtonTitleSelector = mkSelector "defaultButtonTitle"

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

-- | @Selector@ for @updateFromPrintInfo@
updateFromPrintInfoSelector :: Selector '[] ()
updateFromPrintInfoSelector = mkSelector "updateFromPrintInfo"

-- | @Selector@ for @finalWritePrintInfo@
finalWritePrintInfoSelector :: Selector '[] ()
finalWritePrintInfoSelector = mkSelector "finalWritePrintInfo"

-- | @Selector@ for @accessoryControllers@
accessoryControllersSelector :: Selector '[] (Id NSArray)
accessoryControllersSelector = mkSelector "accessoryControllers"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] NSPrintPanelOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[NSPrintPanelOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector '[] (Id NSString)
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @setHelpAnchor:@
setHelpAnchorSelector :: Selector '[Id NSString] ()
setHelpAnchorSelector = mkSelector "setHelpAnchor:"

-- | @Selector@ for @jobStyleHint@
jobStyleHintSelector :: Selector '[] (Id NSString)
jobStyleHintSelector = mkSelector "jobStyleHint"

-- | @Selector@ for @setJobStyleHint:@
setJobStyleHintSelector :: Selector '[Id NSString] ()
setJobStyleHintSelector = mkSelector "setJobStyleHint:"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector '[] (Id NSPrintInfo)
printInfoSelector = mkSelector "printInfo"

