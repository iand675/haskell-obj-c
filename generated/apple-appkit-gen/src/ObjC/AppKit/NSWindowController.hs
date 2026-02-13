{-# LANGUAGE DataKinds #-}
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
  , closeSelector
  , contentViewControllerSelector
  , dismissControllerSelector
  , documentSelector
  , initWithCoderSelector
  , initWithWindowNibNameSelector
  , initWithWindowNibName_ownerSelector
  , initWithWindowNibPath_ownerSelector
  , initWithWindowSelector
  , loadWindowSelector
  , ownerSelector
  , previewRepresentableActivityItemsSelector
  , setContentViewControllerSelector
  , setDocumentEditedSelector
  , setDocumentSelector
  , setPreviewRepresentableActivityItemsSelector
  , setShouldCascadeWindowsSelector
  , setShouldCloseDocumentSelector
  , setWindowFrameAutosaveNameSelector
  , setWindowSelector
  , shouldCascadeWindowsSelector
  , shouldCloseDocumentSelector
  , showWindowSelector
  , storyboardSelector
  , synchronizeWindowTitleWithDocumentNameSelector
  , windowDidLoadSelector
  , windowFrameAutosaveNameSelector
  , windowLoadedSelector
  , windowNibNameSelector
  , windowNibPathSelector
  , windowSelector
  , windowTitleForDocumentDisplayNameSelector
  , windowWillLoadSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithWindow:@
initWithWindow :: (IsNSWindowController nsWindowController, IsNSWindow window) => nsWindowController -> window -> IO (Id NSWindowController)
initWithWindow nsWindowController window =
  sendOwnedMessage nsWindowController initWithWindowSelector (toNSWindow window)

-- | @- initWithCoder:@
initWithCoder :: (IsNSWindowController nsWindowController, IsNSCoder coder) => nsWindowController -> coder -> IO (Id NSWindowController)
initWithCoder nsWindowController coder =
  sendOwnedMessage nsWindowController initWithCoderSelector (toNSCoder coder)

-- | @- initWithWindowNibName:@
initWithWindowNibName :: (IsNSWindowController nsWindowController, IsNSString windowNibName) => nsWindowController -> windowNibName -> IO (Id NSWindowController)
initWithWindowNibName nsWindowController windowNibName =
  sendOwnedMessage nsWindowController initWithWindowNibNameSelector (toNSString windowNibName)

-- | @- initWithWindowNibName:owner:@
initWithWindowNibName_owner :: (IsNSWindowController nsWindowController, IsNSString windowNibName) => nsWindowController -> windowNibName -> RawId -> IO (Id NSWindowController)
initWithWindowNibName_owner nsWindowController windowNibName owner =
  sendOwnedMessage nsWindowController initWithWindowNibName_ownerSelector (toNSString windowNibName) owner

-- | @- initWithWindowNibPath:owner:@
initWithWindowNibPath_owner :: (IsNSWindowController nsWindowController, IsNSString windowNibPath) => nsWindowController -> windowNibPath -> RawId -> IO (Id NSWindowController)
initWithWindowNibPath_owner nsWindowController windowNibPath owner =
  sendOwnedMessage nsWindowController initWithWindowNibPath_ownerSelector (toNSString windowNibPath) owner

-- | @- setDocumentEdited:@
setDocumentEdited :: IsNSWindowController nsWindowController => nsWindowController -> Bool -> IO ()
setDocumentEdited nsWindowController dirtyFlag =
  sendMessage nsWindowController setDocumentEditedSelector dirtyFlag

-- | @- synchronizeWindowTitleWithDocumentName@
synchronizeWindowTitleWithDocumentName :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
synchronizeWindowTitleWithDocumentName nsWindowController =
  sendMessage nsWindowController synchronizeWindowTitleWithDocumentNameSelector

-- | @- windowTitleForDocumentDisplayName:@
windowTitleForDocumentDisplayName :: (IsNSWindowController nsWindowController, IsNSString displayName) => nsWindowController -> displayName -> IO (Id NSString)
windowTitleForDocumentDisplayName nsWindowController displayName =
  sendMessage nsWindowController windowTitleForDocumentDisplayNameSelector (toNSString displayName)

-- | @- windowWillLoad@
windowWillLoad :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
windowWillLoad nsWindowController =
  sendMessage nsWindowController windowWillLoadSelector

-- | @- windowDidLoad@
windowDidLoad :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
windowDidLoad nsWindowController =
  sendMessage nsWindowController windowDidLoadSelector

-- | @- loadWindow@
loadWindow :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
loadWindow nsWindowController =
  sendMessage nsWindowController loadWindowSelector

-- | @- close@
close :: IsNSWindowController nsWindowController => nsWindowController -> IO ()
close nsWindowController =
  sendMessage nsWindowController closeSelector

-- | @- showWindow:@
showWindow :: IsNSWindowController nsWindowController => nsWindowController -> RawId -> IO ()
showWindow nsWindowController sender =
  sendMessage nsWindowController showWindowSelector sender

-- | @- dismissController:@
dismissController :: IsNSWindowController nsWindowController => nsWindowController -> RawId -> IO ()
dismissController nsWindowController sender =
  sendMessage nsWindowController dismissControllerSelector sender

-- | @- windowNibName@
windowNibName :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSString)
windowNibName nsWindowController =
  sendMessage nsWindowController windowNibNameSelector

-- | @- windowNibPath@
windowNibPath :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSString)
windowNibPath nsWindowController =
  sendMessage nsWindowController windowNibPathSelector

-- | @- owner@
owner :: IsNSWindowController nsWindowController => nsWindowController -> IO RawId
owner nsWindowController =
  sendMessage nsWindowController ownerSelector

-- | @- windowFrameAutosaveName@
windowFrameAutosaveName :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSString)
windowFrameAutosaveName nsWindowController =
  sendMessage nsWindowController windowFrameAutosaveNameSelector

-- | @- setWindowFrameAutosaveName:@
setWindowFrameAutosaveName :: (IsNSWindowController nsWindowController, IsNSString value) => nsWindowController -> value -> IO ()
setWindowFrameAutosaveName nsWindowController value =
  sendMessage nsWindowController setWindowFrameAutosaveNameSelector (toNSString value)

-- | @- shouldCascadeWindows@
shouldCascadeWindows :: IsNSWindowController nsWindowController => nsWindowController -> IO Bool
shouldCascadeWindows nsWindowController =
  sendMessage nsWindowController shouldCascadeWindowsSelector

-- | @- setShouldCascadeWindows:@
setShouldCascadeWindows :: IsNSWindowController nsWindowController => nsWindowController -> Bool -> IO ()
setShouldCascadeWindows nsWindowController value =
  sendMessage nsWindowController setShouldCascadeWindowsSelector value

-- | @- previewRepresentableActivityItems@
previewRepresentableActivityItems :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSArray)
previewRepresentableActivityItems nsWindowController =
  sendMessage nsWindowController previewRepresentableActivityItemsSelector

-- | @- setPreviewRepresentableActivityItems:@
setPreviewRepresentableActivityItems :: (IsNSWindowController nsWindowController, IsNSArray value) => nsWindowController -> value -> IO ()
setPreviewRepresentableActivityItems nsWindowController value =
  sendMessage nsWindowController setPreviewRepresentableActivityItemsSelector (toNSArray value)

-- | @- document@
document :: IsNSWindowController nsWindowController => nsWindowController -> IO RawId
document nsWindowController =
  sendMessage nsWindowController documentSelector

-- | @- setDocument:@
setDocument :: IsNSWindowController nsWindowController => nsWindowController -> RawId -> IO ()
setDocument nsWindowController value =
  sendMessage nsWindowController setDocumentSelector value

-- | @- shouldCloseDocument@
shouldCloseDocument :: IsNSWindowController nsWindowController => nsWindowController -> IO Bool
shouldCloseDocument nsWindowController =
  sendMessage nsWindowController shouldCloseDocumentSelector

-- | @- setShouldCloseDocument:@
setShouldCloseDocument :: IsNSWindowController nsWindowController => nsWindowController -> Bool -> IO ()
setShouldCloseDocument nsWindowController value =
  sendMessage nsWindowController setShouldCloseDocumentSelector value

-- | @- contentViewController@
contentViewController :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSViewController)
contentViewController nsWindowController =
  sendMessage nsWindowController contentViewControllerSelector

-- | @- setContentViewController:@
setContentViewController :: (IsNSWindowController nsWindowController, IsNSViewController value) => nsWindowController -> value -> IO ()
setContentViewController nsWindowController value =
  sendMessage nsWindowController setContentViewControllerSelector (toNSViewController value)

-- | @- window@
window :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSWindow)
window nsWindowController =
  sendMessage nsWindowController windowSelector

-- | @- setWindow:@
setWindow :: (IsNSWindowController nsWindowController, IsNSWindow value) => nsWindowController -> value -> IO ()
setWindow nsWindowController value =
  sendMessage nsWindowController setWindowSelector (toNSWindow value)

-- | @- windowLoaded@
windowLoaded :: IsNSWindowController nsWindowController => nsWindowController -> IO Bool
windowLoaded nsWindowController =
  sendMessage nsWindowController windowLoadedSelector

-- | @- storyboard@
storyboard :: IsNSWindowController nsWindowController => nsWindowController -> IO (Id NSStoryboard)
storyboard nsWindowController =
  sendMessage nsWindowController storyboardSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWindow:@
initWithWindowSelector :: Selector '[Id NSWindow] (Id NSWindowController)
initWithWindowSelector = mkSelector "initWithWindow:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSWindowController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithWindowNibName:@
initWithWindowNibNameSelector :: Selector '[Id NSString] (Id NSWindowController)
initWithWindowNibNameSelector = mkSelector "initWithWindowNibName:"

-- | @Selector@ for @initWithWindowNibName:owner:@
initWithWindowNibName_ownerSelector :: Selector '[Id NSString, RawId] (Id NSWindowController)
initWithWindowNibName_ownerSelector = mkSelector "initWithWindowNibName:owner:"

-- | @Selector@ for @initWithWindowNibPath:owner:@
initWithWindowNibPath_ownerSelector :: Selector '[Id NSString, RawId] (Id NSWindowController)
initWithWindowNibPath_ownerSelector = mkSelector "initWithWindowNibPath:owner:"

-- | @Selector@ for @setDocumentEdited:@
setDocumentEditedSelector :: Selector '[Bool] ()
setDocumentEditedSelector = mkSelector "setDocumentEdited:"

-- | @Selector@ for @synchronizeWindowTitleWithDocumentName@
synchronizeWindowTitleWithDocumentNameSelector :: Selector '[] ()
synchronizeWindowTitleWithDocumentNameSelector = mkSelector "synchronizeWindowTitleWithDocumentName"

-- | @Selector@ for @windowTitleForDocumentDisplayName:@
windowTitleForDocumentDisplayNameSelector :: Selector '[Id NSString] (Id NSString)
windowTitleForDocumentDisplayNameSelector = mkSelector "windowTitleForDocumentDisplayName:"

-- | @Selector@ for @windowWillLoad@
windowWillLoadSelector :: Selector '[] ()
windowWillLoadSelector = mkSelector "windowWillLoad"

-- | @Selector@ for @windowDidLoad@
windowDidLoadSelector :: Selector '[] ()
windowDidLoadSelector = mkSelector "windowDidLoad"

-- | @Selector@ for @loadWindow@
loadWindowSelector :: Selector '[] ()
loadWindowSelector = mkSelector "loadWindow"

-- | @Selector@ for @close@
closeSelector :: Selector '[] ()
closeSelector = mkSelector "close"

-- | @Selector@ for @showWindow:@
showWindowSelector :: Selector '[RawId] ()
showWindowSelector = mkSelector "showWindow:"

-- | @Selector@ for @dismissController:@
dismissControllerSelector :: Selector '[RawId] ()
dismissControllerSelector = mkSelector "dismissController:"

-- | @Selector@ for @windowNibName@
windowNibNameSelector :: Selector '[] (Id NSString)
windowNibNameSelector = mkSelector "windowNibName"

-- | @Selector@ for @windowNibPath@
windowNibPathSelector :: Selector '[] (Id NSString)
windowNibPathSelector = mkSelector "windowNibPath"

-- | @Selector@ for @owner@
ownerSelector :: Selector '[] RawId
ownerSelector = mkSelector "owner"

-- | @Selector@ for @windowFrameAutosaveName@
windowFrameAutosaveNameSelector :: Selector '[] (Id NSString)
windowFrameAutosaveNameSelector = mkSelector "windowFrameAutosaveName"

-- | @Selector@ for @setWindowFrameAutosaveName:@
setWindowFrameAutosaveNameSelector :: Selector '[Id NSString] ()
setWindowFrameAutosaveNameSelector = mkSelector "setWindowFrameAutosaveName:"

-- | @Selector@ for @shouldCascadeWindows@
shouldCascadeWindowsSelector :: Selector '[] Bool
shouldCascadeWindowsSelector = mkSelector "shouldCascadeWindows"

-- | @Selector@ for @setShouldCascadeWindows:@
setShouldCascadeWindowsSelector :: Selector '[Bool] ()
setShouldCascadeWindowsSelector = mkSelector "setShouldCascadeWindows:"

-- | @Selector@ for @previewRepresentableActivityItems@
previewRepresentableActivityItemsSelector :: Selector '[] (Id NSArray)
previewRepresentableActivityItemsSelector = mkSelector "previewRepresentableActivityItems"

-- | @Selector@ for @setPreviewRepresentableActivityItems:@
setPreviewRepresentableActivityItemsSelector :: Selector '[Id NSArray] ()
setPreviewRepresentableActivityItemsSelector = mkSelector "setPreviewRepresentableActivityItems:"

-- | @Selector@ for @document@
documentSelector :: Selector '[] RawId
documentSelector = mkSelector "document"

-- | @Selector@ for @setDocument:@
setDocumentSelector :: Selector '[RawId] ()
setDocumentSelector = mkSelector "setDocument:"

-- | @Selector@ for @shouldCloseDocument@
shouldCloseDocumentSelector :: Selector '[] Bool
shouldCloseDocumentSelector = mkSelector "shouldCloseDocument"

-- | @Selector@ for @setShouldCloseDocument:@
setShouldCloseDocumentSelector :: Selector '[Bool] ()
setShouldCloseDocumentSelector = mkSelector "setShouldCloseDocument:"

-- | @Selector@ for @contentViewController@
contentViewControllerSelector :: Selector '[] (Id NSViewController)
contentViewControllerSelector = mkSelector "contentViewController"

-- | @Selector@ for @setContentViewController:@
setContentViewControllerSelector :: Selector '[Id NSViewController] ()
setContentViewControllerSelector = mkSelector "setContentViewController:"

-- | @Selector@ for @window@
windowSelector :: Selector '[] (Id NSWindow)
windowSelector = mkSelector "window"

-- | @Selector@ for @setWindow:@
setWindowSelector :: Selector '[Id NSWindow] ()
setWindowSelector = mkSelector "setWindow:"

-- | @Selector@ for @windowLoaded@
windowLoadedSelector :: Selector '[] Bool
windowLoadedSelector = mkSelector "windowLoaded"

-- | @Selector@ for @storyboard@
storyboardSelector :: Selector '[] (Id NSStoryboard)
storyboardSelector = mkSelector "storyboard"

