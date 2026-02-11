{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSWindowController@.
module ObjC.AppKit.NSWindowController
  ( NSWindowController
  , IsNSWindowController(..)
  , initWithWindow
  , initWithCoder
  , initWithWindowNibName
  , initWithWindowNibName_owner
  , initWithWindowNibPath_owner
  , setDocumentEdited
  , synchronizeWindowTitleWithDocumentName
  , windowTitleForDocumentDisplayName
  , windowWillLoad
  , windowDidLoad
  , loadWindow
  , close
  , showWindow
  , dismissController
  , windowNibName
  , windowNibPath
  , owner
  , windowFrameAutosaveName
  , setWindowFrameAutosaveName
  , shouldCascadeWindows
  , setShouldCascadeWindows
  , previewRepresentableActivityItems
  , setPreviewRepresentableActivityItems
  , document
  , setDocument
  , shouldCloseDocument
  , setShouldCloseDocument
  , contentViewController
  , setContentViewController
  , window
  , setWindow
  , windowLoaded
  , storyboard
  , initWithWindowSelector
  , initWithCoderSelector
  , initWithWindowNibNameSelector
  , initWithWindowNibName_ownerSelector
  , initWithWindowNibPath_ownerSelector
  , setDocumentEditedSelector
  , synchronizeWindowTitleWithDocumentNameSelector
  , windowTitleForDocumentDisplayNameSelector
  , windowWillLoadSelector
  , windowDidLoadSelector
  , loadWindowSelector
  , closeSelector
  , showWindowSelector
  , dismissControllerSelector
  , windowNibNameSelector
  , windowNibPathSelector
  , ownerSelector
  , windowFrameAutosaveNameSelector
  , setWindowFrameAutosaveNameSelector
  , shouldCascadeWindowsSelector
  , setShouldCascadeWindowsSelector
  , previewRepresentableActivityItemsSelector
  , setPreviewRepresentableActivityItemsSelector
  , documentSelector
  , setDocumentSelector
  , shouldCloseDocumentSelector
  , setShouldCloseDocumentSelector
  , contentViewControllerSelector
  , setContentViewControllerSelector
  , windowSelector
  , setWindowSelector
  , windowLoadedSelector
  , storyboardSelector


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

-- | @- initWithWindow:@
initWithWindow :: (IsNSWindowController nsWindowController, IsNSWindow window) => nsWindowController -> window -> IO (Id NSWindowController)
initWithWindow nsWindowController  window =
  withObjCPtr window $ \raw_window ->
      sendMsg nsWindowController (mkSelector "initWithWindow:") (retPtr retVoid) [argPtr (castPtr raw_window :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSWindowController nsWindowController, IsNSCoder coder) => nsWindowController -> coder -> IO (Id NSWindowController)
initWithCoder nsWindowController  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsWindowController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithWindowNibName:@
initWithWindowNibName :: (IsNSWindowController nsWindowController, IsNSString windowNibName) => nsWindowController -> windowNibName -> IO (Id NSWindowController)
initWithWindowNibName nsWindowController  windowNibName =
  withObjCPtr windowNibName $ \raw_windowNibName ->
      sendMsg nsWindowController (mkSelector "initWithWindowNibName:") (retPtr retVoid) [argPtr (castPtr raw_windowNibName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithWindowNibName:owner:@
initWithWindowNibName_owner :: (IsNSWindowController nsWindowController, IsNSString windowNibName) => nsWindowController -> windowNibName -> RawId -> IO (Id NSWindowController)
initWithWindowNibName_owner nsWindowController  windowNibName owner =
  withObjCPtr windowNibName $ \raw_windowNibName ->
      sendMsg nsWindowController (mkSelector "initWithWindowNibName:owner:") (retPtr retVoid) [argPtr (castPtr raw_windowNibName :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithWindowNibPath:owner:@
initWithWindowNibPath_owner :: (IsNSWindowController nsWindowController, IsNSString windowNibPath) => nsWindowController -> windowNibPath -> RawId -> IO (Id NSWindowController)
initWithWindowNibPath_owner nsWindowController  windowNibPath owner =
  withObjCPtr windowNibPath $ \raw_windowNibPath ->
      sendMsg nsWindowController (mkSelector "initWithWindowNibPath:owner:") (retPtr retVoid) [argPtr (castPtr raw_windowNibPath :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())] >>= ownedObject . castPtr

-- | @- setDocumentEdited:@
setDocumentEdited :: IsNSWindowController nsWindowController => nsWindowController -> Bool -> IO ()
setDocumentEdited nsWindowController  dirtyFlag =
    sendMsg nsWindowController (mkSelector "setDocumentEdited:") retVoid [argCULong (if dirtyFlag then 1 else 0)]

-- | @- synchronizeWindowTitleWithDocumentName@
synchronizeWindowTitleWithDocumentName :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
synchronizeWindowTitleWithDocumentName nsWindowController  =
    sendMsg nsWindowController (mkSelector "synchronizeWindowTitleWithDocumentName") retVoid []

-- | @- windowTitleForDocumentDisplayName:@
windowTitleForDocumentDisplayName :: (IsNSWindowController nsWindowController, IsNSString displayName) => nsWindowController -> displayName -> IO (Id NSString)
windowTitleForDocumentDisplayName nsWindowController  displayName =
  withObjCPtr displayName $ \raw_displayName ->
      sendMsg nsWindowController (mkSelector "windowTitleForDocumentDisplayName:") (retPtr retVoid) [argPtr (castPtr raw_displayName :: Ptr ())] >>= retainedObject . castPtr

-- | @- windowWillLoad@
windowWillLoad :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
windowWillLoad nsWindowController  =
    sendMsg nsWindowController (mkSelector "windowWillLoad") retVoid []

-- | @- windowDidLoad@
windowDidLoad :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
windowDidLoad nsWindowController  =
    sendMsg nsWindowController (mkSelector "windowDidLoad") retVoid []

-- | @- loadWindow@
loadWindow :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
loadWindow nsWindowController  =
    sendMsg nsWindowController (mkSelector "loadWindow") retVoid []

-- | @- close@
close :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
close nsWindowController  =
    sendMsg nsWindowController (mkSelector "close") retVoid []

-- | @- showWindow:@
showWindow :: IsNSWindowController nsWindowController => nsWindowController -> RawId -> IO ()
showWindow nsWindowController  sender =
    sendMsg nsWindowController (mkSelector "showWindow:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- dismissController:@
dismissController :: IsNSWindowController nsWindowController => nsWindowController -> RawId -> IO ()
dismissController nsWindowController  sender =
    sendMsg nsWindowController (mkSelector "dismissController:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- windowNibName@
windowNibName :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSString)
windowNibName nsWindowController  =
    sendMsg nsWindowController (mkSelector "windowNibName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windowNibPath@
windowNibPath :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSString)
windowNibPath nsWindowController  =
    sendMsg nsWindowController (mkSelector "windowNibPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- owner@
owner :: IsNSWindowController nsWindowController => nsWindowController -> IO RawId
owner nsWindowController  =
    fmap (RawId . castPtr) $ sendMsg nsWindowController (mkSelector "owner") (retPtr retVoid) []

-- | @- windowFrameAutosaveName@
windowFrameAutosaveName :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSString)
windowFrameAutosaveName nsWindowController  =
    sendMsg nsWindowController (mkSelector "windowFrameAutosaveName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWindowFrameAutosaveName:@
setWindowFrameAutosaveName :: (IsNSWindowController nsWindowController, IsNSString value) => nsWindowController -> value -> IO ()
setWindowFrameAutosaveName nsWindowController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindowController (mkSelector "setWindowFrameAutosaveName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shouldCascadeWindows@
shouldCascadeWindows :: IsNSWindowController nsWindowController => nsWindowController -> IO Bool
shouldCascadeWindows nsWindowController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindowController (mkSelector "shouldCascadeWindows") retCULong []

-- | @- setShouldCascadeWindows:@
setShouldCascadeWindows :: IsNSWindowController nsWindowController => nsWindowController -> Bool -> IO ()
setShouldCascadeWindows nsWindowController  value =
    sendMsg nsWindowController (mkSelector "setShouldCascadeWindows:") retVoid [argCULong (if value then 1 else 0)]

-- | @- previewRepresentableActivityItems@
previewRepresentableActivityItems :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSArray)
previewRepresentableActivityItems nsWindowController  =
    sendMsg nsWindowController (mkSelector "previewRepresentableActivityItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviewRepresentableActivityItems:@
setPreviewRepresentableActivityItems :: (IsNSWindowController nsWindowController, IsNSArray value) => nsWindowController -> value -> IO ()
setPreviewRepresentableActivityItems nsWindowController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindowController (mkSelector "setPreviewRepresentableActivityItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- document@
document :: IsNSWindowController nsWindowController => nsWindowController -> IO RawId
document nsWindowController  =
    fmap (RawId . castPtr) $ sendMsg nsWindowController (mkSelector "document") (retPtr retVoid) []

-- | @- setDocument:@
setDocument :: IsNSWindowController nsWindowController => nsWindowController -> RawId -> IO ()
setDocument nsWindowController  value =
    sendMsg nsWindowController (mkSelector "setDocument:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- shouldCloseDocument@
shouldCloseDocument :: IsNSWindowController nsWindowController => nsWindowController -> IO Bool
shouldCloseDocument nsWindowController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindowController (mkSelector "shouldCloseDocument") retCULong []

-- | @- setShouldCloseDocument:@
setShouldCloseDocument :: IsNSWindowController nsWindowController => nsWindowController -> Bool -> IO ()
setShouldCloseDocument nsWindowController  value =
    sendMsg nsWindowController (mkSelector "setShouldCloseDocument:") retVoid [argCULong (if value then 1 else 0)]

-- | @- contentViewController@
contentViewController :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSViewController)
contentViewController nsWindowController  =
    sendMsg nsWindowController (mkSelector "contentViewController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentViewController:@
setContentViewController :: (IsNSWindowController nsWindowController, IsNSViewController value) => nsWindowController -> value -> IO ()
setContentViewController nsWindowController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindowController (mkSelector "setContentViewController:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- window@
window :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSWindow)
window nsWindowController  =
    sendMsg nsWindowController (mkSelector "window") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWindow:@
setWindow :: (IsNSWindowController nsWindowController, IsNSWindow value) => nsWindowController -> value -> IO ()
setWindow nsWindowController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsWindowController (mkSelector "setWindow:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- windowLoaded@
windowLoaded :: IsNSWindowController nsWindowController => nsWindowController -> IO Bool
windowLoaded nsWindowController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsWindowController (mkSelector "windowLoaded") retCULong []

-- | @- storyboard@
storyboard :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSStoryboard)
storyboard nsWindowController  =
    sendMsg nsWindowController (mkSelector "storyboard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWindow:@
initWithWindowSelector :: Selector
initWithWindowSelector = mkSelector "initWithWindow:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithWindowNibName:@
initWithWindowNibNameSelector :: Selector
initWithWindowNibNameSelector = mkSelector "initWithWindowNibName:"

-- | @Selector@ for @initWithWindowNibName:owner:@
initWithWindowNibName_ownerSelector :: Selector
initWithWindowNibName_ownerSelector = mkSelector "initWithWindowNibName:owner:"

-- | @Selector@ for @initWithWindowNibPath:owner:@
initWithWindowNibPath_ownerSelector :: Selector
initWithWindowNibPath_ownerSelector = mkSelector "initWithWindowNibPath:owner:"

-- | @Selector@ for @setDocumentEdited:@
setDocumentEditedSelector :: Selector
setDocumentEditedSelector = mkSelector "setDocumentEdited:"

-- | @Selector@ for @synchronizeWindowTitleWithDocumentName@
synchronizeWindowTitleWithDocumentNameSelector :: Selector
synchronizeWindowTitleWithDocumentNameSelector = mkSelector "synchronizeWindowTitleWithDocumentName"

-- | @Selector@ for @windowTitleForDocumentDisplayName:@
windowTitleForDocumentDisplayNameSelector :: Selector
windowTitleForDocumentDisplayNameSelector = mkSelector "windowTitleForDocumentDisplayName:"

-- | @Selector@ for @windowWillLoad@
windowWillLoadSelector :: Selector
windowWillLoadSelector = mkSelector "windowWillLoad"

-- | @Selector@ for @windowDidLoad@
windowDidLoadSelector :: Selector
windowDidLoadSelector = mkSelector "windowDidLoad"

-- | @Selector@ for @loadWindow@
loadWindowSelector :: Selector
loadWindowSelector = mkSelector "loadWindow"

-- | @Selector@ for @close@
closeSelector :: Selector
closeSelector = mkSelector "close"

-- | @Selector@ for @showWindow:@
showWindowSelector :: Selector
showWindowSelector = mkSelector "showWindow:"

-- | @Selector@ for @dismissController:@
dismissControllerSelector :: Selector
dismissControllerSelector = mkSelector "dismissController:"

-- | @Selector@ for @windowNibName@
windowNibNameSelector :: Selector
windowNibNameSelector = mkSelector "windowNibName"

-- | @Selector@ for @windowNibPath@
windowNibPathSelector :: Selector
windowNibPathSelector = mkSelector "windowNibPath"

-- | @Selector@ for @owner@
ownerSelector :: Selector
ownerSelector = mkSelector "owner"

-- | @Selector@ for @windowFrameAutosaveName@
windowFrameAutosaveNameSelector :: Selector
windowFrameAutosaveNameSelector = mkSelector "windowFrameAutosaveName"

-- | @Selector@ for @setWindowFrameAutosaveName:@
setWindowFrameAutosaveNameSelector :: Selector
setWindowFrameAutosaveNameSelector = mkSelector "setWindowFrameAutosaveName:"

-- | @Selector@ for @shouldCascadeWindows@
shouldCascadeWindowsSelector :: Selector
shouldCascadeWindowsSelector = mkSelector "shouldCascadeWindows"

-- | @Selector@ for @setShouldCascadeWindows:@
setShouldCascadeWindowsSelector :: Selector
setShouldCascadeWindowsSelector = mkSelector "setShouldCascadeWindows:"

-- | @Selector@ for @previewRepresentableActivityItems@
previewRepresentableActivityItemsSelector :: Selector
previewRepresentableActivityItemsSelector = mkSelector "previewRepresentableActivityItems"

-- | @Selector@ for @setPreviewRepresentableActivityItems:@
setPreviewRepresentableActivityItemsSelector :: Selector
setPreviewRepresentableActivityItemsSelector = mkSelector "setPreviewRepresentableActivityItems:"

-- | @Selector@ for @document@
documentSelector :: Selector
documentSelector = mkSelector "document"

-- | @Selector@ for @setDocument:@
setDocumentSelector :: Selector
setDocumentSelector = mkSelector "setDocument:"

-- | @Selector@ for @shouldCloseDocument@
shouldCloseDocumentSelector :: Selector
shouldCloseDocumentSelector = mkSelector "shouldCloseDocument"

-- | @Selector@ for @setShouldCloseDocument:@
setShouldCloseDocumentSelector :: Selector
setShouldCloseDocumentSelector = mkSelector "setShouldCloseDocument:"

-- | @Selector@ for @contentViewController@
contentViewControllerSelector :: Selector
contentViewControllerSelector = mkSelector "contentViewController"

-- | @Selector@ for @setContentViewController:@
setContentViewControllerSelector :: Selector
setContentViewControllerSelector = mkSelector "setContentViewController:"

-- | @Selector@ for @window@
windowSelector :: Selector
windowSelector = mkSelector "window"

-- | @Selector@ for @setWindow:@
setWindowSelector :: Selector
setWindowSelector = mkSelector "setWindow:"

-- | @Selector@ for @windowLoaded@
windowLoadedSelector :: Selector
windowLoadedSelector = mkSelector "windowLoaded"

-- | @Selector@ for @storyboard@
storyboardSelector :: Selector
storyboardSelector = mkSelector "storyboard"

