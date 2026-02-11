{-# LANGUAGE PatternSynonyms #-}
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
  , options
  , setOptions
  , helpAnchor
  , setHelpAnchor
  , jobStyleHint
  , setJobStyleHint
  , printPanelSelector
  , addAccessoryControllerSelector
  , removeAccessoryControllerSelector
  , setDefaultButtonTitleSelector
  , defaultButtonTitleSelector
  , beginSheetUsingPrintInfo_onWindow_completionHandlerSelector
  , beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector
  , runModalWithPrintInfoSelector
  , runModalSelector
  , setAccessoryViewSelector
  , accessoryViewSelector
  , updateFromPrintInfoSelector
  , finalWritePrintInfoSelector
  , optionsSelector
  , setOptionsSelector
  , helpAnchorSelector
  , setHelpAnchorSelector
  , jobStyleHintSelector
  , setJobStyleHintSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    sendClassMsg cls' (mkSelector "printPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addAccessoryController:@
addAccessoryController :: (IsNSPrintPanel nsPrintPanel, IsNSViewController accessoryController) => nsPrintPanel -> accessoryController -> IO ()
addAccessoryController nsPrintPanel  accessoryController =
withObjCPtr accessoryController $ \raw_accessoryController ->
    sendMsg nsPrintPanel (mkSelector "addAccessoryController:") retVoid [argPtr (castPtr raw_accessoryController :: Ptr ())]

-- | @- removeAccessoryController:@
removeAccessoryController :: (IsNSPrintPanel nsPrintPanel, IsNSViewController accessoryController) => nsPrintPanel -> accessoryController -> IO ()
removeAccessoryController nsPrintPanel  accessoryController =
withObjCPtr accessoryController $ \raw_accessoryController ->
    sendMsg nsPrintPanel (mkSelector "removeAccessoryController:") retVoid [argPtr (castPtr raw_accessoryController :: Ptr ())]

-- | @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsNSPrintPanel nsPrintPanel, IsNSString defaultButtonTitle) => nsPrintPanel -> defaultButtonTitle -> IO ()
setDefaultButtonTitle nsPrintPanel  defaultButtonTitle =
withObjCPtr defaultButtonTitle $ \raw_defaultButtonTitle ->
    sendMsg nsPrintPanel (mkSelector "setDefaultButtonTitle:") retVoid [argPtr (castPtr raw_defaultButtonTitle :: Ptr ())]

-- | @- defaultButtonTitle@
defaultButtonTitle :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSString)
defaultButtonTitle nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "defaultButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beginSheetUsingPrintInfo:onWindow:completionHandler:@
beginSheetUsingPrintInfo_onWindow_completionHandler :: (IsNSPrintPanel nsPrintPanel, IsNSPrintInfo printInfo, IsNSWindow parentWindow) => nsPrintPanel -> printInfo -> parentWindow -> Ptr () -> IO ()
beginSheetUsingPrintInfo_onWindow_completionHandler nsPrintPanel  printInfo parentWindow handler =
withObjCPtr printInfo $ \raw_printInfo ->
  withObjCPtr parentWindow $ \raw_parentWindow ->
      sendMsg nsPrintPanel (mkSelector "beginSheetUsingPrintInfo:onWindow:completionHandler:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argPtr (castPtr raw_parentWindow :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:@
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo :: (IsNSPrintPanel nsPrintPanel, IsNSPrintInfo printInfo, IsNSWindow docWindow) => nsPrintPanel -> printInfo -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo nsPrintPanel  printInfo docWindow delegate didEndSelector contextInfo =
withObjCPtr printInfo $ \raw_printInfo ->
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg nsPrintPanel (mkSelector "beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | @- runModalWithPrintInfo:@
runModalWithPrintInfo :: (IsNSPrintPanel nsPrintPanel, IsNSPrintInfo printInfo) => nsPrintPanel -> printInfo -> IO CLong
runModalWithPrintInfo nsPrintPanel  printInfo =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg nsPrintPanel (mkSelector "runModalWithPrintInfo:") retCLong [argPtr (castPtr raw_printInfo :: Ptr ())]

-- | @- runModal@
runModal :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO CLong
runModal nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "runModal") retCLong []

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSPrintPanel nsPrintPanel, IsNSView accessoryView) => nsPrintPanel -> accessoryView -> IO ()
setAccessoryView nsPrintPanel  accessoryView =
withObjCPtr accessoryView $ \raw_accessoryView ->
    sendMsg nsPrintPanel (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_accessoryView :: Ptr ())]

-- | @- accessoryView@
accessoryView :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSView)
accessoryView nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updateFromPrintInfo@
updateFromPrintInfo :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO ()
updateFromPrintInfo nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "updateFromPrintInfo") retVoid []

-- | @- finalWritePrintInfo@
finalWritePrintInfo :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO ()
finalWritePrintInfo nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "finalWritePrintInfo") retVoid []

-- | @- options@
options :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO NSPrintPanelOptions
options nsPrintPanel  =
  fmap (coerce :: CULong -> NSPrintPanelOptions) $ sendMsg nsPrintPanel (mkSelector "options") retCULong []

-- | @- setOptions:@
setOptions :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> NSPrintPanelOptions -> IO ()
setOptions nsPrintPanel  value =
  sendMsg nsPrintPanel (mkSelector "setOptions:") retVoid [argCULong (coerce value)]

-- | @- helpAnchor@
helpAnchor :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSString)
helpAnchor nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "helpAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHelpAnchor:@
setHelpAnchor :: (IsNSPrintPanel nsPrintPanel, IsNSString value) => nsPrintPanel -> value -> IO ()
setHelpAnchor nsPrintPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPrintPanel (mkSelector "setHelpAnchor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- jobStyleHint@
jobStyleHint :: IsNSPrintPanel nsPrintPanel => nsPrintPanel -> IO (Id NSString)
jobStyleHint nsPrintPanel  =
  sendMsg nsPrintPanel (mkSelector "jobStyleHint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setJobStyleHint:@
setJobStyleHint :: (IsNSPrintPanel nsPrintPanel, IsNSString value) => nsPrintPanel -> value -> IO ()
setJobStyleHint nsPrintPanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPrintPanel (mkSelector "setJobStyleHint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @printPanel@
printPanelSelector :: Selector
printPanelSelector = mkSelector "printPanel"

-- | @Selector@ for @addAccessoryController:@
addAccessoryControllerSelector :: Selector
addAccessoryControllerSelector = mkSelector "addAccessoryController:"

-- | @Selector@ for @removeAccessoryController:@
removeAccessoryControllerSelector :: Selector
removeAccessoryControllerSelector = mkSelector "removeAccessoryController:"

-- | @Selector@ for @setDefaultButtonTitle:@
setDefaultButtonTitleSelector :: Selector
setDefaultButtonTitleSelector = mkSelector "setDefaultButtonTitle:"

-- | @Selector@ for @defaultButtonTitle@
defaultButtonTitleSelector :: Selector
defaultButtonTitleSelector = mkSelector "defaultButtonTitle"

-- | @Selector@ for @beginSheetUsingPrintInfo:onWindow:completionHandler:@
beginSheetUsingPrintInfo_onWindow_completionHandlerSelector :: Selector
beginSheetUsingPrintInfo_onWindow_completionHandlerSelector = mkSelector "beginSheetUsingPrintInfo:onWindow:completionHandler:"

-- | @Selector@ for @beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:@
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector :: Selector
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalWithPrintInfo:@
runModalWithPrintInfoSelector :: Selector
runModalWithPrintInfoSelector = mkSelector "runModalWithPrintInfo:"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @updateFromPrintInfo@
updateFromPrintInfoSelector :: Selector
updateFromPrintInfoSelector = mkSelector "updateFromPrintInfo"

-- | @Selector@ for @finalWritePrintInfo@
finalWritePrintInfoSelector :: Selector
finalWritePrintInfoSelector = mkSelector "finalWritePrintInfo"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @setHelpAnchor:@
setHelpAnchorSelector :: Selector
setHelpAnchorSelector = mkSelector "setHelpAnchor:"

-- | @Selector@ for @jobStyleHint@
jobStyleHintSelector :: Selector
jobStyleHintSelector = mkSelector "jobStyleHint"

-- | @Selector@ for @setJobStyleHint:@
setJobStyleHintSelector :: Selector
setJobStyleHintSelector = mkSelector "setJobStyleHint:"

