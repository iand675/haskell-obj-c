{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSavePanel@.
module ObjC.AppKit.NSSavePanel
  ( NSSavePanel
  , IsNSSavePanel(..)
  , savePanel
  , validateVisibleColumns
  , ok
  , cancel
  , beginSheetModalForWindow_completionHandler
  , beginWithCompletionHandler
  , runModal
  , filename
  , directory
  , setDirectory
  , requiredFileType
  , setRequiredFileType
  , beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo
  , runModalForDirectory_file
  , selectText
  , url
  , identifier
  , setIdentifier
  , allowsOtherFileTypes
  , setAllowsOtherFileTypes
  , accessoryView
  , setAccessoryView
  , expanded
  , canCreateDirectories
  , setCanCreateDirectories
  , canSelectHiddenExtension
  , setCanSelectHiddenExtension
  , extensionHidden
  , setExtensionHidden
  , treatsFilePackagesAsDirectories
  , setTreatsFilePackagesAsDirectories
  , prompt
  , setPrompt
  , title
  , setTitle
  , nameFieldLabel
  , setNameFieldLabel
  , message
  , setMessage
  , showsHiddenFiles
  , setShowsHiddenFiles
  , showsTagField
  , setShowsTagField
  , showsContentTypes
  , setShowsContentTypes
  , savePanelSelector
  , validateVisibleColumnsSelector
  , okSelector
  , cancelSelector
  , beginSheetModalForWindow_completionHandlerSelector
  , beginWithCompletionHandlerSelector
  , runModalSelector
  , filenameSelector
  , directorySelector
  , setDirectorySelector
  , requiredFileTypeSelector
  , setRequiredFileTypeSelector
  , beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , runModalForDirectory_fileSelector
  , selectTextSelector
  , urlSelector
  , identifierSelector
  , setIdentifierSelector
  , allowsOtherFileTypesSelector
  , setAllowsOtherFileTypesSelector
  , accessoryViewSelector
  , setAccessoryViewSelector
  , expandedSelector
  , canCreateDirectoriesSelector
  , setCanCreateDirectoriesSelector
  , canSelectHiddenExtensionSelector
  , setCanSelectHiddenExtensionSelector
  , extensionHiddenSelector
  , setExtensionHiddenSelector
  , treatsFilePackagesAsDirectoriesSelector
  , setTreatsFilePackagesAsDirectoriesSelector
  , promptSelector
  , setPromptSelector
  , titleSelector
  , setTitleSelector
  , nameFieldLabelSelector
  , setNameFieldLabelSelector
  , messageSelector
  , setMessageSelector
  , showsHiddenFilesSelector
  , setShowsHiddenFilesSelector
  , showsTagFieldSelector
  , setShowsTagFieldSelector
  , showsContentTypesSelector
  , setShowsContentTypesSelector


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

-- | Creates a new instance of the NSSavePanel. This class is not a singleton.
--
-- ObjC selector: @+ savePanel@
savePanel :: IO (Id NSSavePanel)
savePanel  =
  do
    cls' <- getRequiredClass "NSSavePanel"
    sendClassMsg cls' (mkSelector "savePanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Refreshes the open or save panel's contents.
--
-- ObjC selector: @- validateVisibleColumns@
validateVisibleColumns :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO ()
validateVisibleColumns nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "validateVisibleColumns") retVoid []

-- | @- ok:@
ok :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
ok nsSavePanel  sender =
  sendMsg nsSavePanel (mkSelector "ok:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- cancel:@
cancel :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
cancel nsSavePanel  sender =
  sendMsg nsSavePanel (mkSelector "cancel:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @NSSavePanel@/@NSOpenPanel@: Presents the panel as a sheet modal to @window@ and returns immediately. Configure the panel before calling this method. The completion handler block will be called after the user has closed the panel, however, the open/save panel sheet may still be on screen. If you require the sheet to be offscreen (for example, to show an alert), first call @[savePanel orderOut:nil]@ to close it. The @result@ will be @NSModalResponseOK@, @NSModalResponseCancel@, or if the panel fails to display, @NSModalResponseAbort@.
--
-- ObjC selector: @- beginSheetModalForWindow:completionHandler:@
beginSheetModalForWindow_completionHandler :: (IsNSSavePanel nsSavePanel, IsNSWindow window) => nsSavePanel -> window -> Ptr () -> IO ()
beginSheetModalForWindow_completionHandler nsSavePanel  window handler =
withObjCPtr window $ \raw_window ->
    sendMsg nsSavePanel (mkSelector "beginSheetModalForWindow:completionHandler:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @NSSavePanel@/@NSOpenPanel@: Presents the panel as a modeless window and returns immediately. Configure the panel before calling this method. The completion handler block will be called after the user has closed the panel. The @result@ will be @NSModalResponseOK@, @NSModalResponseCancel@, or if the panel fails to display, @NSModalResponseAbort@.
--
-- ObjC selector: @- beginWithCompletionHandler:@
beginWithCompletionHandler :: IsNSSavePanel nsSavePanel => nsSavePanel -> Ptr () -> IO ()
beginWithCompletionHandler nsSavePanel  handler =
  sendMsg nsSavePanel (mkSelector "beginWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @NSSavePanel@/@NSOpenPanel@: Presents the panel as an application modal window. Returns after the user has closed the panel.  - Returns: @NSModalResponseOK@, @NSModalResponseCancel@ or if the panel fails to display, @NSModalResponseAbort@.
--
-- ObjC selector: @- runModal@
runModal :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO CLong
runModal nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "runModal") retCLong []

-- | @- filename@
filename :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
filename nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "filename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- directory@
directory :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
directory nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "directory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDirectory:@
setDirectory :: (IsNSSavePanel nsSavePanel, IsNSString path) => nsSavePanel -> path -> IO ()
setDirectory nsSavePanel  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsSavePanel (mkSelector "setDirectory:") retVoid [argPtr (castPtr raw_path :: Ptr ())]

-- | @- requiredFileType@
requiredFileType :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
requiredFileType nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "requiredFileType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRequiredFileType:@
setRequiredFileType :: (IsNSSavePanel nsSavePanel, IsNSString type_) => nsSavePanel -> type_ -> IO ()
setRequiredFileType nsSavePanel  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsSavePanel (mkSelector "setRequiredFileType:") retVoid [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSSavePanel nsSavePanel, IsNSString path, IsNSString name, IsNSWindow docWindow) => nsSavePanel -> path -> name -> docWindow -> RawId -> Selector -> Ptr () -> IO ()
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo nsSavePanel  path name docWindow delegate didEndSelector contextInfo =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr docWindow $ \raw_docWindow ->
        sendMsg nsSavePanel (mkSelector "beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | @- runModalForDirectory:file:@
runModalForDirectory_file :: (IsNSSavePanel nsSavePanel, IsNSString path, IsNSString name) => nsSavePanel -> path -> name -> IO CLong
runModalForDirectory_file nsSavePanel  path name =
withObjCPtr path $ \raw_path ->
  withObjCPtr name $ \raw_name ->
      sendMsg nsSavePanel (mkSelector "runModalForDirectory:file:") retCLong [argPtr (castPtr raw_path :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- selectText:@
selectText :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
selectText nsSavePanel  sender =
  sendMsg nsSavePanel (mkSelector "selectText:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @NSSavePanel@: Returns the URL to save the file at. A file may already exist at @url@ if the user choose to overwrite it. @NSOpenPanel@: Returns the single filename selected by the user. Note: if -allowsMultipleSelection is set, you should use the -URLs on NSOpenPanel instead.
--
-- ObjC selector: @- URL@
url :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSURL)
url nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets and returns the identifier.
--
-- The panel's current state such as the root directory and the current directory are saved and restored relative to the identifier. - Note: When the identifier is changed, the properties that depend on the identifier are updated from user defaults. Properties that have a null value in user defaults are not changed (and keep their existing value). - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- identifier@
identifier :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
identifier nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets and returns the identifier.
--
-- The panel's current state such as the root directory and the current directory are saved and restored relative to the identifier. - Note: When the identifier is changed, the properties that depend on the identifier are updated from user defaults. Properties that have a null value in user defaults are not changed (and keep their existing value). - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setIdentifier nsSavePanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSavePanel (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@: Returns a BOOL value that indicates whether the panel allows the user to save files with an extension that is not in the list of @allowedFileTypes@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- allowsOtherFileTypes@
allowsOtherFileTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
allowsOtherFileTypes nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "allowsOtherFileTypes") retCULong []

-- | @NSSavePanel@: Returns a BOOL value that indicates whether the panel allows the user to save files with an extension that is not in the list of @allowedFileTypes@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setAllowsOtherFileTypes:@
setAllowsOtherFileTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setAllowsOtherFileTypes nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setAllowsOtherFileTypes:") retVoid [argCULong (if value then 1 else 0)]

-- | Sets and returns the accessory view shown in the panel.
--
-- For applications that link on 10.6 and later, the accessoryView's frame will be observed, and any changes the programmer makes to the frame will automatically be reflected in the panel (including animated changes to the frame height).
--
-- For applications that link on 26.0 and later and use the Liquid Glass design, the accessoryView's control metrics will be the larger Liquid Glass metrics.
--
-- ObjC selector: @- accessoryView@
accessoryView :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSView)
accessoryView nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets and returns the accessory view shown in the panel.
--
-- For applications that link on 10.6 and later, the accessoryView's frame will be observed, and any changes the programmer makes to the frame will automatically be reflected in the panel (including animated changes to the frame height).
--
-- For applications that link on 26.0 and later and use the Liquid Glass design, the accessoryView's control metrics will be the larger Liquid Glass metrics.
--
-- ObjC selector: @- setAccessoryView:@
setAccessoryView :: (IsNSSavePanel nsSavePanel, IsNSView value) => nsSavePanel -> value -> IO ()
setAccessoryView nsSavePanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSavePanel (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@: Returns @YES@ if the panel is expanded. Defaults to @NO@. Persists in the user defaults. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- expanded@
expanded :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
expanded nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "expanded") retCULong []

-- | @NSSavePanel@/@NSOpenPanel@: Set to @YES@ to show the "New Folder" button. Default is @YES@.
--
-- ObjC selector: @- canCreateDirectories@
canCreateDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
canCreateDirectories nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "canCreateDirectories") retCULong []

-- | @NSSavePanel@/@NSOpenPanel@: Set to @YES@ to show the "New Folder" button. Default is @YES@.
--
-- ObjC selector: @- setCanCreateDirectories:@
setCanCreateDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setCanCreateDirectories nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setCanCreateDirectories:") retVoid [argCULong (if value then 1 else 0)]

-- | @NSSavePanel@: Set to @YES@ to show the "Hide Extension" menu item. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- canSelectHiddenExtension@
canSelectHiddenExtension :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
canSelectHiddenExtension nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "canSelectHiddenExtension") retCULong []

-- | @NSSavePanel@: Set to @YES@ to show the "Hide Extension" menu item. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setCanSelectHiddenExtension:@
setCanSelectHiddenExtension :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setCanSelectHiddenExtension nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setCanSelectHiddenExtension:") retVoid [argCULong (if value then 1 else 0)]

-- | @NSSavePanel@: Set to @YES@ if the filename extension should be hidden. Otherwise, @NO@ if the filename extension should be shown. Default is @YES@. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- extensionHidden@
extensionHidden :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
extensionHidden nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "extensionHidden") retCULong []

-- | @NSSavePanel@: Set to @YES@ if the filename extension should be hidden. Otherwise, @NO@ if the filename extension should be shown. Default is @YES@. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setExtensionHidden:@
setExtensionHidden :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setExtensionHidden nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setExtensionHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @NSSavePanel@/@NSOpenPanel@: If set to @YES@, the user can navigate into file packages as if they were directories. Default is @NO@.
--
-- ObjC selector: @- treatsFilePackagesAsDirectories@
treatsFilePackagesAsDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
treatsFilePackagesAsDirectories nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "treatsFilePackagesAsDirectories") retCULong []

-- | @NSSavePanel@/@NSOpenPanel@: If set to @YES@, the user can navigate into file packages as if they were directories. Default is @NO@.
--
-- ObjC selector: @- setTreatsFilePackagesAsDirectories:@
setTreatsFilePackagesAsDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setTreatsFilePackagesAsDirectories nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setTreatsFilePackagesAsDirectories:") retVoid [argCULong (if value then 1 else 0)]

-- | @NSSavePanel@/@NSOpenPanel@: Sets the text shown on the Open or Save button. If set to an empty string, it will show a localized "Open" for the NSOpenPanel and "Save" for the NSSavePanel. The default value will be the correct localized prompt for the open or save panel, as appropriate.
--
-- ObjC selector: @- prompt@
prompt :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
prompt nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "prompt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@/@NSOpenPanel@: Sets the text shown on the Open or Save button. If set to an empty string, it will show a localized "Open" for the NSOpenPanel and "Save" for the NSSavePanel. The default value will be the correct localized prompt for the open or save panel, as appropriate.
--
-- ObjC selector: @- setPrompt:@
setPrompt :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setPrompt nsSavePanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSavePanel (mkSelector "setPrompt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the title for the panel shown at the top of the window. - Note: The open and save panel does not currently have a titlebar. So the title is not displayed.
--
-- ObjC selector: @- title@
title :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
title nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the title for the panel shown at the top of the window. - Note: The open and save panel does not currently have a titlebar. So the title is not displayed.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setTitle nsSavePanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSavePanel (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@: Sets and returns the text shown to the left of the "name field". Default value is a localized "Save As:" string. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- nameFieldLabel@
nameFieldLabel :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
nameFieldLabel nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "nameFieldLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@: Sets and returns the text shown to the left of the "name field". Default value is a localized "Save As:" string. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setNameFieldLabel:@
setNameFieldLabel :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setNameFieldLabel nsSavePanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSavePanel (mkSelector "setNameFieldLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the message shown under title of the panel.
--
-- ObjC selector: @- message@
message :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
message nsSavePanel  =
  sendMsg nsSavePanel (mkSelector "message") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the message shown under title of the panel.
--
-- ObjC selector: @- setMessage:@
setMessage :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setMessage nsSavePanel  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsSavePanel (mkSelector "setMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@/@NSOpenPanel@: If @showsHiddenFiles@ is set to @YES@, files that are normally hidden from the user are displayed. This method was published in Mac OS 10.6, but has existed since Mac OS 10.4. This property is KVO compliant. The user may invoke the keyboard shortcut (cmd-shift-.) to show or hide hidden files. Any user interface shown in an an accessory view should be updated by using key value observing (KVO) to watch for changes of this property. Alternatively, the user interface can be directly bound to this property. The default value is @NO@.
--
-- ObjC selector: @- showsHiddenFiles@
showsHiddenFiles :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
showsHiddenFiles nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "showsHiddenFiles") retCULong []

-- | @NSSavePanel@/@NSOpenPanel@: If @showsHiddenFiles@ is set to @YES@, files that are normally hidden from the user are displayed. This method was published in Mac OS 10.6, but has existed since Mac OS 10.4. This property is KVO compliant. The user may invoke the keyboard shortcut (cmd-shift-.) to show or hide hidden files. Any user interface shown in an an accessory view should be updated by using key value observing (KVO) to watch for changes of this property. Alternatively, the user interface can be directly bound to this property. The default value is @NO@.
--
-- ObjC selector: @- setShowsHiddenFiles:@
setShowsHiddenFiles :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setShowsHiddenFiles nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setShowsHiddenFiles:") retVoid [argCULong (if value then 1 else 0)]

-- | @NSSavePanel@: Shows or hides the "Tags" field in the receiver. By passing @YES@, you become responsible for setting Tag names on the resulting file after saving is complete. Default is @YES@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- showsTagField@
showsTagField :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
showsTagField nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "showsTagField") retCULong []

-- | @NSSavePanel@: Shows or hides the "Tags" field in the receiver. By passing @YES@, you become responsible for setting Tag names on the resulting file after saving is complete. Default is @YES@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setShowsTagField:@
setShowsTagField :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setShowsTagField nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setShowsTagField:") retVoid [argCULong (if value then 1 else 0)]

-- | @NSSavePanel@: Whether or not to show a control for selecting the type of the saved file. The control shows the types in @allowedContentTypes@. Default is @NO@. @NSOpenPanel@: Not used. - Note: If @allowedContentTypes@ is empty, the control is not displayed.
--
-- ObjC selector: @- showsContentTypes@
showsContentTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
showsContentTypes nsSavePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSavePanel (mkSelector "showsContentTypes") retCULong []

-- | @NSSavePanel@: Whether or not to show a control for selecting the type of the saved file. The control shows the types in @allowedContentTypes@. Default is @NO@. @NSOpenPanel@: Not used. - Note: If @allowedContentTypes@ is empty, the control is not displayed.
--
-- ObjC selector: @- setShowsContentTypes:@
setShowsContentTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setShowsContentTypes nsSavePanel  value =
  sendMsg nsSavePanel (mkSelector "setShowsContentTypes:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @savePanel@
savePanelSelector :: Selector
savePanelSelector = mkSelector "savePanel"

-- | @Selector@ for @validateVisibleColumns@
validateVisibleColumnsSelector :: Selector
validateVisibleColumnsSelector = mkSelector "validateVisibleColumns"

-- | @Selector@ for @ok:@
okSelector :: Selector
okSelector = mkSelector "ok:"

-- | @Selector@ for @cancel:@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel:"

-- | @Selector@ for @beginSheetModalForWindow:completionHandler:@
beginSheetModalForWindow_completionHandlerSelector :: Selector
beginSheetModalForWindow_completionHandlerSelector = mkSelector "beginSheetModalForWindow:completionHandler:"

-- | @Selector@ for @beginWithCompletionHandler:@
beginWithCompletionHandlerSelector :: Selector
beginWithCompletionHandlerSelector = mkSelector "beginWithCompletionHandler:"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @filename@
filenameSelector :: Selector
filenameSelector = mkSelector "filename"

-- | @Selector@ for @directory@
directorySelector :: Selector
directorySelector = mkSelector "directory"

-- | @Selector@ for @setDirectory:@
setDirectorySelector :: Selector
setDirectorySelector = mkSelector "setDirectory:"

-- | @Selector@ for @requiredFileType@
requiredFileTypeSelector :: Selector
requiredFileTypeSelector = mkSelector "requiredFileType"

-- | @Selector@ for @setRequiredFileType:@
setRequiredFileTypeSelector :: Selector
setRequiredFileTypeSelector = mkSelector "setRequiredFileType:"

-- | @Selector@ for @beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalForDirectory:file:@
runModalForDirectory_fileSelector :: Selector
runModalForDirectory_fileSelector = mkSelector "runModalForDirectory:file:"

-- | @Selector@ for @selectText:@
selectTextSelector :: Selector
selectTextSelector = mkSelector "selectText:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @allowsOtherFileTypes@
allowsOtherFileTypesSelector :: Selector
allowsOtherFileTypesSelector = mkSelector "allowsOtherFileTypes"

-- | @Selector@ for @setAllowsOtherFileTypes:@
setAllowsOtherFileTypesSelector :: Selector
setAllowsOtherFileTypesSelector = mkSelector "setAllowsOtherFileTypes:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @expanded@
expandedSelector :: Selector
expandedSelector = mkSelector "expanded"

-- | @Selector@ for @canCreateDirectories@
canCreateDirectoriesSelector :: Selector
canCreateDirectoriesSelector = mkSelector "canCreateDirectories"

-- | @Selector@ for @setCanCreateDirectories:@
setCanCreateDirectoriesSelector :: Selector
setCanCreateDirectoriesSelector = mkSelector "setCanCreateDirectories:"

-- | @Selector@ for @canSelectHiddenExtension@
canSelectHiddenExtensionSelector :: Selector
canSelectHiddenExtensionSelector = mkSelector "canSelectHiddenExtension"

-- | @Selector@ for @setCanSelectHiddenExtension:@
setCanSelectHiddenExtensionSelector :: Selector
setCanSelectHiddenExtensionSelector = mkSelector "setCanSelectHiddenExtension:"

-- | @Selector@ for @extensionHidden@
extensionHiddenSelector :: Selector
extensionHiddenSelector = mkSelector "extensionHidden"

-- | @Selector@ for @setExtensionHidden:@
setExtensionHiddenSelector :: Selector
setExtensionHiddenSelector = mkSelector "setExtensionHidden:"

-- | @Selector@ for @treatsFilePackagesAsDirectories@
treatsFilePackagesAsDirectoriesSelector :: Selector
treatsFilePackagesAsDirectoriesSelector = mkSelector "treatsFilePackagesAsDirectories"

-- | @Selector@ for @setTreatsFilePackagesAsDirectories:@
setTreatsFilePackagesAsDirectoriesSelector :: Selector
setTreatsFilePackagesAsDirectoriesSelector = mkSelector "setTreatsFilePackagesAsDirectories:"

-- | @Selector@ for @prompt@
promptSelector :: Selector
promptSelector = mkSelector "prompt"

-- | @Selector@ for @setPrompt:@
setPromptSelector :: Selector
setPromptSelector = mkSelector "setPrompt:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @nameFieldLabel@
nameFieldLabelSelector :: Selector
nameFieldLabelSelector = mkSelector "nameFieldLabel"

-- | @Selector@ for @setNameFieldLabel:@
setNameFieldLabelSelector :: Selector
setNameFieldLabelSelector = mkSelector "setNameFieldLabel:"

-- | @Selector@ for @message@
messageSelector :: Selector
messageSelector = mkSelector "message"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @showsHiddenFiles@
showsHiddenFilesSelector :: Selector
showsHiddenFilesSelector = mkSelector "showsHiddenFiles"

-- | @Selector@ for @setShowsHiddenFiles:@
setShowsHiddenFilesSelector :: Selector
setShowsHiddenFilesSelector = mkSelector "setShowsHiddenFiles:"

-- | @Selector@ for @showsTagField@
showsTagFieldSelector :: Selector
showsTagFieldSelector = mkSelector "showsTagField"

-- | @Selector@ for @setShowsTagField:@
setShowsTagFieldSelector :: Selector
setShowsTagFieldSelector = mkSelector "setShowsTagField:"

-- | @Selector@ for @showsContentTypes@
showsContentTypesSelector :: Selector
showsContentTypesSelector = mkSelector "showsContentTypes"

-- | @Selector@ for @setShowsContentTypes:@
setShowsContentTypesSelector :: Selector
setShowsContentTypesSelector = mkSelector "setShowsContentTypes:"

