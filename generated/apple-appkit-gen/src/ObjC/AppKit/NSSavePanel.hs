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
  , directoryURL
  , setDirectoryURL
  , allowedContentTypes
  , setAllowedContentTypes
  , allowsOtherFileTypes
  , setAllowsOtherFileTypes
  , currentContentType
  , setCurrentContentType
  , accessoryView
  , setAccessoryView
  , delegate
  , setDelegate
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
  , nameFieldStringValue
  , setNameFieldStringValue
  , message
  , setMessage
  , showsHiddenFiles
  , setShowsHiddenFiles
  , showsTagField
  , setShowsTagField
  , tagNames
  , setTagNames
  , showsContentTypes
  , setShowsContentTypes
  , allowedFileTypes
  , setAllowedFileTypes
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
  , directoryURLSelector
  , setDirectoryURLSelector
  , allowedContentTypesSelector
  , setAllowedContentTypesSelector
  , allowsOtherFileTypesSelector
  , setAllowsOtherFileTypesSelector
  , currentContentTypeSelector
  , setCurrentContentTypeSelector
  , accessoryViewSelector
  , setAccessoryViewSelector
  , delegateSelector
  , setDelegateSelector
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
  , nameFieldStringValueSelector
  , setNameFieldStringValueSelector
  , messageSelector
  , setMessageSelector
  , showsHiddenFilesSelector
  , setShowsHiddenFilesSelector
  , showsTagFieldSelector
  , setShowsTagFieldSelector
  , tagNamesSelector
  , setTagNamesSelector
  , showsContentTypesSelector
  , setShowsContentTypesSelector
  , allowedFileTypesSelector
  , setAllowedFileTypesSelector


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
import ObjC.UniformTypeIdentifiers.Internal.Classes

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

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the directory that is displayed. Set to @nil@ to display the default directory. This method will not block to resolve the URL, and the directory will asynchronously be set, if required. - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- directoryURL@
directoryURL :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSURL)
directoryURL nsSavePanel  =
    sendMsg nsSavePanel (mkSelector "directoryURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the directory that is displayed. Set to @nil@ to display the default directory. This method will not block to resolve the URL, and the directory will asynchronously be set, if required. - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- setDirectoryURL:@
setDirectoryURL :: (IsNSSavePanel nsSavePanel, IsNSURL value) => nsSavePanel -> value -> IO ()
setDirectoryURL nsSavePanel  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSavePanel (mkSelector "setDirectoryURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @NSSavePanel@: An array of UTTypes specifying the file types the user can save the file as. Set to `\@[]@ to specify that any file type can be used. If no extension is given by the user, the first preferred extension from the array will be used as the extension for the save panel. If the user specifies a type not in the array, and @allowsOtherFileTypes@ is @YES`, they will be presented with another dialog when prompted to save. The default value is the empty array. @NSOpenPanel@: This property determines which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. @allowedContentTypes@ can be changed while the panel is running (ie: from an accessory view). This is also known as the "enabled file types". Set to `\@[]` to specify that all files should be enabled.
--
-- ObjC selector: @- allowedContentTypes@
allowedContentTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSArray)
allowedContentTypes nsSavePanel  =
    sendMsg nsSavePanel (mkSelector "allowedContentTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@: An array of UTTypes specifying the file types the user can save the file as. Set to `\@[]@ to specify that any file type can be used. If no extension is given by the user, the first preferred extension from the array will be used as the extension for the save panel. If the user specifies a type not in the array, and @allowsOtherFileTypes@ is @YES`, they will be presented with another dialog when prompted to save. The default value is the empty array. @NSOpenPanel@: This property determines which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. @allowedContentTypes@ can be changed while the panel is running (ie: from an accessory view). This is also known as the "enabled file types". Set to `\@[]` to specify that all files should be enabled.
--
-- ObjC selector: @- setAllowedContentTypes:@
setAllowedContentTypes :: (IsNSSavePanel nsSavePanel, IsNSArray value) => nsSavePanel -> value -> IO ()
setAllowedContentTypes nsSavePanel  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSavePanel (mkSelector "setAllowedContentTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @NSSavePanel@:The current type. If set to @nil@, resets to the first allowed content type. Returns @nil@ if @allowedContentTypes@ is empty. @NSOpenPanel@: Not used. - Note: Asserts that @currentContentType@ conforms to @UTTypeData@ or @UTTypeDirectory@.
--
-- ObjC selector: @- currentContentType@
currentContentType :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id UTType)
currentContentType nsSavePanel  =
    sendMsg nsSavePanel (mkSelector "currentContentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@:The current type. If set to @nil@, resets to the first allowed content type. Returns @nil@ if @allowedContentTypes@ is empty. @NSOpenPanel@: Not used. - Note: Asserts that @currentContentType@ conforms to @UTTypeData@ or @UTTypeDirectory@.
--
-- ObjC selector: @- setCurrentContentType:@
setCurrentContentType :: (IsNSSavePanel nsSavePanel, IsUTType value) => nsSavePanel -> value -> IO ()
setCurrentContentType nsSavePanel  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSavePanel (mkSelector "setCurrentContentType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO RawId
delegate nsSavePanel  =
    fmap (RawId . castPtr) $ sendMsg nsSavePanel (mkSelector "delegate") (retPtr retVoid) []

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
setDelegate nsSavePanel  value =
    sendMsg nsSavePanel (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

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

-- | @NSSavePanel@: Sets and returns the user-editable file name shown in the name field. - Note: Calling the deprecated methods that take a "name:" parameter will overwrite any values set before the panel is shown. - Note: If @[panel isExtensionHidden]@ is set to @YES@, the extension will be hidden. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- nameFieldStringValue@
nameFieldStringValue :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
nameFieldStringValue nsSavePanel  =
    sendMsg nsSavePanel (mkSelector "nameFieldStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@: Sets and returns the user-editable file name shown in the name field. - Note: Calling the deprecated methods that take a "name:" parameter will overwrite any values set before the panel is shown. - Note: If @[panel isExtensionHidden]@ is set to @YES@, the extension will be hidden. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setNameFieldStringValue:@
setNameFieldStringValue :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setNameFieldStringValue nsSavePanel  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSavePanel (mkSelector "setNameFieldStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @NSSavePanel@: When -showsTagField returns YES, set any initial Tag names to be displayed, if necessary, prior to displaying the receiver. Also, if the user clicks "Save", take the result of -tagNames, and set them on the resulting file after saving is complete. Tag names are NSStrings, arrays of which can be used directly with the NSURLTagNamesKey API for getting and setting tags on files. Passing @nil@ or an empty array to -setTagNames: will result in no initial Tag names appearing in the receiver. When -showsTagField returns YES, -tagNames always returns a non-nil array, and when NO, -tagNames always returns @nil@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- tagNames@
tagNames :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSArray)
tagNames nsSavePanel  =
    sendMsg nsSavePanel (mkSelector "tagNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@: When -showsTagField returns YES, set any initial Tag names to be displayed, if necessary, prior to displaying the receiver. Also, if the user clicks "Save", take the result of -tagNames, and set them on the resulting file after saving is complete. Tag names are NSStrings, arrays of which can be used directly with the NSURLTagNamesKey API for getting and setting tags on files. Passing @nil@ or an empty array to -setTagNames: will result in no initial Tag names appearing in the receiver. When -showsTagField returns YES, -tagNames always returns a non-nil array, and when NO, -tagNames always returns @nil@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setTagNames:@
setTagNames :: (IsNSSavePanel nsSavePanel, IsNSArray value) => nsSavePanel -> value -> IO ()
setTagNames nsSavePanel  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSavePanel (mkSelector "setTagNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @NSSavePanel@: An array of NSStrings specifying the file types the user can save the file as. The file type can be a common file extension, or a UTI. A nil value indicates that any file type can be used. If the array is not nil and the array contains no items, an exception will be raised. If no extension is given by the user, the first item in the allowedFileTypes will be used as the extension for the save panel. If the user specifies a type not in the array, and 'allowsOtherFileTypes' is YES, they will be presented with another dialog when prompted to save. The default value is 'nil'. @NSOpenPanel@: On versions less than 10.6, this property is ignored. For applications that link against 10.6 and higher, this property will determine which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. The allowedFileTypes can be changed while the panel is running (ie: from an accessory view). The file type can be a common file extension, or a UTI. This is also known as the "enabled file types". A nil value indicates that all files should be enabled.
--
-- ObjC selector: @- allowedFileTypes@
allowedFileTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSArray)
allowedFileTypes nsSavePanel  =
    sendMsg nsSavePanel (mkSelector "allowedFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @NSSavePanel@: An array of NSStrings specifying the file types the user can save the file as. The file type can be a common file extension, or a UTI. A nil value indicates that any file type can be used. If the array is not nil and the array contains no items, an exception will be raised. If no extension is given by the user, the first item in the allowedFileTypes will be used as the extension for the save panel. If the user specifies a type not in the array, and 'allowsOtherFileTypes' is YES, they will be presented with another dialog when prompted to save. The default value is 'nil'. @NSOpenPanel@: On versions less than 10.6, this property is ignored. For applications that link against 10.6 and higher, this property will determine which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. The allowedFileTypes can be changed while the panel is running (ie: from an accessory view). The file type can be a common file extension, or a UTI. This is also known as the "enabled file types". A nil value indicates that all files should be enabled.
--
-- ObjC selector: @- setAllowedFileTypes:@
setAllowedFileTypes :: (IsNSSavePanel nsSavePanel, IsNSArray value) => nsSavePanel -> value -> IO ()
setAllowedFileTypes nsSavePanel  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSavePanel (mkSelector "setAllowedFileTypes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @directoryURL@
directoryURLSelector :: Selector
directoryURLSelector = mkSelector "directoryURL"

-- | @Selector@ for @setDirectoryURL:@
setDirectoryURLSelector :: Selector
setDirectoryURLSelector = mkSelector "setDirectoryURL:"

-- | @Selector@ for @allowedContentTypes@
allowedContentTypesSelector :: Selector
allowedContentTypesSelector = mkSelector "allowedContentTypes"

-- | @Selector@ for @setAllowedContentTypes:@
setAllowedContentTypesSelector :: Selector
setAllowedContentTypesSelector = mkSelector "setAllowedContentTypes:"

-- | @Selector@ for @allowsOtherFileTypes@
allowsOtherFileTypesSelector :: Selector
allowsOtherFileTypesSelector = mkSelector "allowsOtherFileTypes"

-- | @Selector@ for @setAllowsOtherFileTypes:@
setAllowsOtherFileTypesSelector :: Selector
setAllowsOtherFileTypesSelector = mkSelector "setAllowsOtherFileTypes:"

-- | @Selector@ for @currentContentType@
currentContentTypeSelector :: Selector
currentContentTypeSelector = mkSelector "currentContentType"

-- | @Selector@ for @setCurrentContentType:@
setCurrentContentTypeSelector :: Selector
setCurrentContentTypeSelector = mkSelector "setCurrentContentType:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

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

-- | @Selector@ for @nameFieldStringValue@
nameFieldStringValueSelector :: Selector
nameFieldStringValueSelector = mkSelector "nameFieldStringValue"

-- | @Selector@ for @setNameFieldStringValue:@
setNameFieldStringValueSelector :: Selector
setNameFieldStringValueSelector = mkSelector "setNameFieldStringValue:"

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

-- | @Selector@ for @tagNames@
tagNamesSelector :: Selector
tagNamesSelector = mkSelector "tagNames"

-- | @Selector@ for @setTagNames:@
setTagNamesSelector :: Selector
setTagNamesSelector = mkSelector "setTagNames:"

-- | @Selector@ for @showsContentTypes@
showsContentTypesSelector :: Selector
showsContentTypesSelector = mkSelector "showsContentTypes"

-- | @Selector@ for @setShowsContentTypes:@
setShowsContentTypesSelector :: Selector
setShowsContentTypesSelector = mkSelector "setShowsContentTypes:"

-- | @Selector@ for @allowedFileTypes@
allowedFileTypesSelector :: Selector
allowedFileTypesSelector = mkSelector "allowedFileTypes"

-- | @Selector@ for @setAllowedFileTypes:@
setAllowedFileTypesSelector :: Selector
setAllowedFileTypesSelector = mkSelector "setAllowedFileTypes:"

