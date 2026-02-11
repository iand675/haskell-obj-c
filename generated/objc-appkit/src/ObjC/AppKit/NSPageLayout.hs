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
  , printInfo
  , pageLayoutSelector
  , addAccessoryControllerSelector
  , removeAccessoryControllerSelector
  , beginSheetUsingPrintInfo_onWindow_completionHandlerSelector
  , beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfoSelector
  , runModalWithPrintInfoSelector
  , runModalSelector
  , setAccessoryViewSelector
  , accessoryViewSelector
  , readPrintInfoSelector
  , writePrintInfoSelector
  , printInfoSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ pageLayout@
pageLayout :: IO (Id NSPageLayout)
pageLayout  =
  do
    cls' <- getRequiredClass "NSPageLayout"
    sendClassMsg cls' (mkSelector "pageLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addAccessoryController:@
addAccessoryController :: (IsNSPageLayout nsPageLayout, IsNSViewController accessoryController) => nsPageLayout -> accessoryController -> IO ()
addAccessoryController nsPageLayout  accessoryController =
withObjCPtr accessoryController $ \raw_accessoryController ->
    sendMsg nsPageLayout (mkSelector "addAccessoryController:") retVoid [argPtr (castPtr raw_accessoryController :: Ptr ())]

-- | @- removeAccessoryController:@
removeAccessoryController :: (IsNSPageLayout nsPageLayout, IsNSViewController accessoryController) => nsPageLayout -> accessoryController -> IO ()
removeAccessoryController nsPageLayout  accessoryController =
withObjCPtr accessoryController $ \raw_accessoryController ->
    sendMsg nsPageLayout (mkSelector "removeAccessoryController:") retVoid [argPtr (castPtr raw_accessoryController :: Ptr ())]

-- | @- beginSheetUsingPrintInfo:onWindow:completionHandler:@
beginSheetUsingPrintInfo_onWindow_completionHandler :: (IsNSPageLayout nsPageLayout, IsNSPrintInfo printInfo, IsNSWindow parentWindow) => nsPageLayout -> printInfo -> parentWindow -> Ptr () -> IO ()
beginSheetUsingPrintInfo_onWindow_completionHandler nsPageLayout  printInfo parentWindow handler =
withObjCPtr printInfo $ \raw_printInfo ->
  withObjCPtr parentWindow $ \raw_parentWindow ->
      sendMsg nsPageLayout (mkSelector "beginSheetUsingPrintInfo:onWindow:completionHandler:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argPtr (castPtr raw_parentWindow :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:@
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo :: (IsNSPageLayout nsPageLayout, IsNSPrintInfo printInfo, IsNSWindow docWindow) => nsPageLayout -> printInfo -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
beginSheetWithPrintInfo_modalForWindow_delegate_didEndSelector_contextInfo nsPageLayout  printInfo docWindow delegate didEndSelector contextInfo =
withObjCPtr printInfo $ \raw_printInfo ->
  withObjCPtr docWindow $ \raw_docWindow ->
      sendMsg nsPageLayout (mkSelector "beginSheetWithPrintInfo:modalForWindow:delegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_printInfo :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | @- runModalWithPrintInfo:@
runModalWithPrintInfo :: (IsNSPageLayout nsPageLayout, IsNSPrintInfo printInfo) => nsPageLayout -> printInfo -> IO CLong
runModalWithPrintInfo nsPageLayout  printInfo =
withObjCPtr printInfo $ \raw_printInfo ->
    sendMsg nsPageLayout (mkSelector "runModalWithPrintInfo:") retCLong [argPtr (castPtr raw_printInfo :: Ptr ())]

-- | @- runModal@
runModal :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO CLong
runModal nsPageLayout  =
  sendMsg nsPageLayout (mkSelector "runModal") retCLong []

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSPageLayout nsPageLayout, IsNSView accessoryView) => nsPageLayout -> accessoryView -> IO ()
setAccessoryView nsPageLayout  accessoryView =
withObjCPtr accessoryView $ \raw_accessoryView ->
    sendMsg nsPageLayout (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_accessoryView :: Ptr ())]

-- | @- accessoryView@
accessoryView :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO (Id NSView)
accessoryView nsPageLayout  =
  sendMsg nsPageLayout (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- readPrintInfo@
readPrintInfo :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO ()
readPrintInfo nsPageLayout  =
  sendMsg nsPageLayout (mkSelector "readPrintInfo") retVoid []

-- | @- writePrintInfo@
writePrintInfo :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO ()
writePrintInfo nsPageLayout  =
  sendMsg nsPageLayout (mkSelector "writePrintInfo") retVoid []

-- | @- printInfo@
printInfo :: IsNSPageLayout nsPageLayout => nsPageLayout -> IO (Id NSPrintInfo)
printInfo nsPageLayout  =
  sendMsg nsPageLayout (mkSelector "printInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pageLayout@
pageLayoutSelector :: Selector
pageLayoutSelector = mkSelector "pageLayout"

-- | @Selector@ for @addAccessoryController:@
addAccessoryControllerSelector :: Selector
addAccessoryControllerSelector = mkSelector "addAccessoryController:"

-- | @Selector@ for @removeAccessoryController:@
removeAccessoryControllerSelector :: Selector
removeAccessoryControllerSelector = mkSelector "removeAccessoryController:"

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

-- | @Selector@ for @readPrintInfo@
readPrintInfoSelector :: Selector
readPrintInfoSelector = mkSelector "readPrintInfo"

-- | @Selector@ for @writePrintInfo@
writePrintInfoSelector :: Selector
writePrintInfoSelector = mkSelector "writePrintInfo"

-- | @Selector@ for @printInfo@
printInfoSelector :: Selector
printInfoSelector = mkSelector "printInfo"

