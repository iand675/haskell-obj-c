{-# LANGUAGE DataKinds #-}
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
  , accessoryViewSelector
  , allowedContentTypesSelector
  , allowedFileTypesSelector
  , allowsOtherFileTypesSelector
  , beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , beginSheetModalForWindow_completionHandlerSelector
  , beginWithCompletionHandlerSelector
  , canCreateDirectoriesSelector
  , canSelectHiddenExtensionSelector
  , cancelSelector
  , currentContentTypeSelector
  , delegateSelector
  , directorySelector
  , directoryURLSelector
  , expandedSelector
  , extensionHiddenSelector
  , filenameSelector
  , identifierSelector
  , messageSelector
  , nameFieldLabelSelector
  , nameFieldStringValueSelector
  , okSelector
  , promptSelector
  , requiredFileTypeSelector
  , runModalForDirectory_fileSelector
  , runModalSelector
  , savePanelSelector
  , selectTextSelector
  , setAccessoryViewSelector
  , setAllowedContentTypesSelector
  , setAllowedFileTypesSelector
  , setAllowsOtherFileTypesSelector
  , setCanCreateDirectoriesSelector
  , setCanSelectHiddenExtensionSelector
  , setCurrentContentTypeSelector
  , setDelegateSelector
  , setDirectorySelector
  , setDirectoryURLSelector
  , setExtensionHiddenSelector
  , setIdentifierSelector
  , setMessageSelector
  , setNameFieldLabelSelector
  , setNameFieldStringValueSelector
  , setPromptSelector
  , setRequiredFileTypeSelector
  , setShowsContentTypesSelector
  , setShowsHiddenFilesSelector
  , setShowsTagFieldSelector
  , setTagNamesSelector
  , setTitleSelector
  , setTreatsFilePackagesAsDirectoriesSelector
  , showsContentTypesSelector
  , showsHiddenFilesSelector
  , showsTagFieldSelector
  , tagNamesSelector
  , titleSelector
  , treatsFilePackagesAsDirectoriesSelector
  , urlSelector
  , validateVisibleColumnsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' savePanelSelector

-- | Refreshes the open or save panel's contents.
--
-- ObjC selector: @- validateVisibleColumns@
validateVisibleColumns :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO ()
validateVisibleColumns nsSavePanel =
  sendMessage nsSavePanel validateVisibleColumnsSelector

-- | @- ok:@
ok :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
ok nsSavePanel sender =
  sendMessage nsSavePanel okSelector sender

-- | @- cancel:@
cancel :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
cancel nsSavePanel sender =
  sendMessage nsSavePanel cancelSelector sender

-- | @NSSavePanel@/@NSOpenPanel@: Presents the panel as a sheet modal to @window@ and returns immediately. Configure the panel before calling this method. The completion handler block will be called after the user has closed the panel, however, the open/save panel sheet may still be on screen. If you require the sheet to be offscreen (for example, to show an alert), first call @[savePanel orderOut:nil]@ to close it. The @result@ will be @NSModalResponseOK@, @NSModalResponseCancel@, or if the panel fails to display, @NSModalResponseAbort@.
--
-- ObjC selector: @- beginSheetModalForWindow:completionHandler:@
beginSheetModalForWindow_completionHandler :: (IsNSSavePanel nsSavePanel, IsNSWindow window) => nsSavePanel -> window -> Ptr () -> IO ()
beginSheetModalForWindow_completionHandler nsSavePanel window handler =
  sendMessage nsSavePanel beginSheetModalForWindow_completionHandlerSelector (toNSWindow window) handler

-- | @NSSavePanel@/@NSOpenPanel@: Presents the panel as a modeless window and returns immediately. Configure the panel before calling this method. The completion handler block will be called after the user has closed the panel. The @result@ will be @NSModalResponseOK@, @NSModalResponseCancel@, or if the panel fails to display, @NSModalResponseAbort@.
--
-- ObjC selector: @- beginWithCompletionHandler:@
beginWithCompletionHandler :: IsNSSavePanel nsSavePanel => nsSavePanel -> Ptr () -> IO ()
beginWithCompletionHandler nsSavePanel handler =
  sendMessage nsSavePanel beginWithCompletionHandlerSelector handler

-- | @NSSavePanel@/@NSOpenPanel@: Presents the panel as an application modal window. Returns after the user has closed the panel.  - Returns: @NSModalResponseOK@, @NSModalResponseCancel@ or if the panel fails to display, @NSModalResponseAbort@.
--
-- ObjC selector: @- runModal@
runModal :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO CLong
runModal nsSavePanel =
  sendMessage nsSavePanel runModalSelector

-- | @- filename@
filename :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
filename nsSavePanel =
  sendMessage nsSavePanel filenameSelector

-- | @- directory@
directory :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
directory nsSavePanel =
  sendMessage nsSavePanel directorySelector

-- | @- setDirectory:@
setDirectory :: (IsNSSavePanel nsSavePanel, IsNSString path) => nsSavePanel -> path -> IO ()
setDirectory nsSavePanel path =
  sendMessage nsSavePanel setDirectorySelector (toNSString path)

-- | @- requiredFileType@
requiredFileType :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
requiredFileType nsSavePanel =
  sendMessage nsSavePanel requiredFileTypeSelector

-- | @- setRequiredFileType:@
setRequiredFileType :: (IsNSSavePanel nsSavePanel, IsNSString type_) => nsSavePanel -> type_ -> IO ()
setRequiredFileType nsSavePanel type_ =
  sendMessage nsSavePanel setRequiredFileTypeSelector (toNSString type_)

-- | @- beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsNSSavePanel nsSavePanel, IsNSString path, IsNSString name, IsNSWindow docWindow) => nsSavePanel -> path -> name -> docWindow -> RawId -> Sel -> Ptr () -> IO ()
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo nsSavePanel path name docWindow delegate didEndSelector contextInfo =
  sendMessage nsSavePanel beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSString path) (toNSString name) (toNSWindow docWindow) delegate didEndSelector contextInfo

-- | @- runModalForDirectory:file:@
runModalForDirectory_file :: (IsNSSavePanel nsSavePanel, IsNSString path, IsNSString name) => nsSavePanel -> path -> name -> IO CLong
runModalForDirectory_file nsSavePanel path name =
  sendMessage nsSavePanel runModalForDirectory_fileSelector (toNSString path) (toNSString name)

-- | @- selectText:@
selectText :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
selectText nsSavePanel sender =
  sendMessage nsSavePanel selectTextSelector sender

-- | @NSSavePanel@: Returns the URL to save the file at. A file may already exist at @url@ if the user choose to overwrite it. @NSOpenPanel@: Returns the single filename selected by the user. Note: if -allowsMultipleSelection is set, you should use the -URLs on NSOpenPanel instead.
--
-- ObjC selector: @- URL@
url :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSURL)
url nsSavePanel =
  sendMessage nsSavePanel urlSelector

-- | Sets and returns the identifier.
--
-- The panel's current state such as the root directory and the current directory are saved and restored relative to the identifier. - Note: When the identifier is changed, the properties that depend on the identifier are updated from user defaults. Properties that have a null value in user defaults are not changed (and keep their existing value). - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- identifier@
identifier :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
identifier nsSavePanel =
  sendMessage nsSavePanel identifierSelector

-- | Sets and returns the identifier.
--
-- The panel's current state such as the root directory and the current directory are saved and restored relative to the identifier. - Note: When the identifier is changed, the properties that depend on the identifier are updated from user defaults. Properties that have a null value in user defaults are not changed (and keep their existing value). - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setIdentifier nsSavePanel value =
  sendMessage nsSavePanel setIdentifierSelector (toNSString value)

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the directory that is displayed. Set to @nil@ to display the default directory. This method will not block to resolve the URL, and the directory will asynchronously be set, if required. - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- directoryURL@
directoryURL :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSURL)
directoryURL nsSavePanel =
  sendMessage nsSavePanel directoryURLSelector

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the directory that is displayed. Set to @nil@ to display the default directory. This method will not block to resolve the URL, and the directory will asynchronously be set, if required. - Note: Can only be set during the configuration phase.
--
-- ObjC selector: @- setDirectoryURL:@
setDirectoryURL :: (IsNSSavePanel nsSavePanel, IsNSURL value) => nsSavePanel -> value -> IO ()
setDirectoryURL nsSavePanel value =
  sendMessage nsSavePanel setDirectoryURLSelector (toNSURL value)

-- | @NSSavePanel@: An array of UTTypes specifying the file types the user can save the file as. Set to `\@[]@ to specify that any file type can be used. If no extension is given by the user, the first preferred extension from the array will be used as the extension for the save panel. If the user specifies a type not in the array, and @allowsOtherFileTypes@ is @YES`, they will be presented with another dialog when prompted to save. The default value is the empty array. @NSOpenPanel@: This property determines which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. @allowedContentTypes@ can be changed while the panel is running (ie: from an accessory view). This is also known as the "enabled file types". Set to `\@[]` to specify that all files should be enabled.
--
-- ObjC selector: @- allowedContentTypes@
allowedContentTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSArray)
allowedContentTypes nsSavePanel =
  sendMessage nsSavePanel allowedContentTypesSelector

-- | @NSSavePanel@: An array of UTTypes specifying the file types the user can save the file as. Set to `\@[]@ to specify that any file type can be used. If no extension is given by the user, the first preferred extension from the array will be used as the extension for the save panel. If the user specifies a type not in the array, and @allowsOtherFileTypes@ is @YES`, they will be presented with another dialog when prompted to save. The default value is the empty array. @NSOpenPanel@: This property determines which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. @allowedContentTypes@ can be changed while the panel is running (ie: from an accessory view). This is also known as the "enabled file types". Set to `\@[]` to specify that all files should be enabled.
--
-- ObjC selector: @- setAllowedContentTypes:@
setAllowedContentTypes :: (IsNSSavePanel nsSavePanel, IsNSArray value) => nsSavePanel -> value -> IO ()
setAllowedContentTypes nsSavePanel value =
  sendMessage nsSavePanel setAllowedContentTypesSelector (toNSArray value)

-- | @NSSavePanel@: Returns a BOOL value that indicates whether the panel allows the user to save files with an extension that is not in the list of @allowedFileTypes@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- allowsOtherFileTypes@
allowsOtherFileTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
allowsOtherFileTypes nsSavePanel =
  sendMessage nsSavePanel allowsOtherFileTypesSelector

-- | @NSSavePanel@: Returns a BOOL value that indicates whether the panel allows the user to save files with an extension that is not in the list of @allowedFileTypes@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setAllowsOtherFileTypes:@
setAllowsOtherFileTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setAllowsOtherFileTypes nsSavePanel value =
  sendMessage nsSavePanel setAllowsOtherFileTypesSelector value

-- | @NSSavePanel@:The current type. If set to @nil@, resets to the first allowed content type. Returns @nil@ if @allowedContentTypes@ is empty. @NSOpenPanel@: Not used. - Note: Asserts that @currentContentType@ conforms to @UTTypeData@ or @UTTypeDirectory@.
--
-- ObjC selector: @- currentContentType@
currentContentType :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id UTType)
currentContentType nsSavePanel =
  sendMessage nsSavePanel currentContentTypeSelector

-- | @NSSavePanel@:The current type. If set to @nil@, resets to the first allowed content type. Returns @nil@ if @allowedContentTypes@ is empty. @NSOpenPanel@: Not used. - Note: Asserts that @currentContentType@ conforms to @UTTypeData@ or @UTTypeDirectory@.
--
-- ObjC selector: @- setCurrentContentType:@
setCurrentContentType :: (IsNSSavePanel nsSavePanel, IsUTType value) => nsSavePanel -> value -> IO ()
setCurrentContentType nsSavePanel value =
  sendMessage nsSavePanel setCurrentContentTypeSelector (toUTType value)

-- | Sets and returns the accessory view shown in the panel.
--
-- For applications that link on 10.6 and later, the accessoryView's frame will be observed, and any changes the programmer makes to the frame will automatically be reflected in the panel (including animated changes to the frame height).
--
-- For applications that link on 26.0 and later and use the Liquid Glass design, the accessoryView's control metrics will be the larger Liquid Glass metrics.
--
-- ObjC selector: @- accessoryView@
accessoryView :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSView)
accessoryView nsSavePanel =
  sendMessage nsSavePanel accessoryViewSelector

-- | Sets and returns the accessory view shown in the panel.
--
-- For applications that link on 10.6 and later, the accessoryView's frame will be observed, and any changes the programmer makes to the frame will automatically be reflected in the panel (including animated changes to the frame height).
--
-- For applications that link on 26.0 and later and use the Liquid Glass design, the accessoryView's control metrics will be the larger Liquid Glass metrics.
--
-- ObjC selector: @- setAccessoryView:@
setAccessoryView :: (IsNSSavePanel nsSavePanel, IsNSView value) => nsSavePanel -> value -> IO ()
setAccessoryView nsSavePanel value =
  sendMessage nsSavePanel setAccessoryViewSelector (toNSView value)

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO RawId
delegate nsSavePanel =
  sendMessage nsSavePanel delegateSelector

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSSavePanel nsSavePanel => nsSavePanel -> RawId -> IO ()
setDelegate nsSavePanel value =
  sendMessage nsSavePanel setDelegateSelector value

-- | @NSSavePanel@: Returns @YES@ if the panel is expanded. Defaults to @NO@. Persists in the user defaults. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- expanded@
expanded :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
expanded nsSavePanel =
  sendMessage nsSavePanel expandedSelector

-- | @NSSavePanel@/@NSOpenPanel@: Set to @YES@ to show the "New Folder" button. Default is @YES@.
--
-- ObjC selector: @- canCreateDirectories@
canCreateDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
canCreateDirectories nsSavePanel =
  sendMessage nsSavePanel canCreateDirectoriesSelector

-- | @NSSavePanel@/@NSOpenPanel@: Set to @YES@ to show the "New Folder" button. Default is @YES@.
--
-- ObjC selector: @- setCanCreateDirectories:@
setCanCreateDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setCanCreateDirectories nsSavePanel value =
  sendMessage nsSavePanel setCanCreateDirectoriesSelector value

-- | @NSSavePanel@: Set to @YES@ to show the "Hide Extension" menu item. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- canSelectHiddenExtension@
canSelectHiddenExtension :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
canSelectHiddenExtension nsSavePanel =
  sendMessage nsSavePanel canSelectHiddenExtensionSelector

-- | @NSSavePanel@: Set to @YES@ to show the "Hide Extension" menu item. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setCanSelectHiddenExtension:@
setCanSelectHiddenExtension :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setCanSelectHiddenExtension nsSavePanel value =
  sendMessage nsSavePanel setCanSelectHiddenExtensionSelector value

-- | @NSSavePanel@: Set to @YES@ if the filename extension should be hidden. Otherwise, @NO@ if the filename extension should be shown. Default is @YES@. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- extensionHidden@
extensionHidden :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
extensionHidden nsSavePanel =
  sendMessage nsSavePanel extensionHiddenSelector

-- | @NSSavePanel@: Set to @YES@ if the filename extension should be hidden. Otherwise, @NO@ if the filename extension should be shown. Default is @YES@. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setExtensionHidden:@
setExtensionHidden :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setExtensionHidden nsSavePanel value =
  sendMessage nsSavePanel setExtensionHiddenSelector value

-- | @NSSavePanel@/@NSOpenPanel@: If set to @YES@, the user can navigate into file packages as if they were directories. Default is @NO@.
--
-- ObjC selector: @- treatsFilePackagesAsDirectories@
treatsFilePackagesAsDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
treatsFilePackagesAsDirectories nsSavePanel =
  sendMessage nsSavePanel treatsFilePackagesAsDirectoriesSelector

-- | @NSSavePanel@/@NSOpenPanel@: If set to @YES@, the user can navigate into file packages as if they were directories. Default is @NO@.
--
-- ObjC selector: @- setTreatsFilePackagesAsDirectories:@
setTreatsFilePackagesAsDirectories :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setTreatsFilePackagesAsDirectories nsSavePanel value =
  sendMessage nsSavePanel setTreatsFilePackagesAsDirectoriesSelector value

-- | @NSSavePanel@/@NSOpenPanel@: Sets the text shown on the Open or Save button. If set to an empty string, it will show a localized "Open" for the NSOpenPanel and "Save" for the NSSavePanel. The default value will be the correct localized prompt for the open or save panel, as appropriate.
--
-- ObjC selector: @- prompt@
prompt :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
prompt nsSavePanel =
  sendMessage nsSavePanel promptSelector

-- | @NSSavePanel@/@NSOpenPanel@: Sets the text shown on the Open or Save button. If set to an empty string, it will show a localized "Open" for the NSOpenPanel and "Save" for the NSSavePanel. The default value will be the correct localized prompt for the open or save panel, as appropriate.
--
-- ObjC selector: @- setPrompt:@
setPrompt :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setPrompt nsSavePanel value =
  sendMessage nsSavePanel setPromptSelector (toNSString value)

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the title for the panel shown at the top of the window. - Note: The open and save panel does not currently have a titlebar. So the title is not displayed.
--
-- ObjC selector: @- title@
title :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
title nsSavePanel =
  sendMessage nsSavePanel titleSelector

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the title for the panel shown at the top of the window. - Note: The open and save panel does not currently have a titlebar. So the title is not displayed.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setTitle nsSavePanel value =
  sendMessage nsSavePanel setTitleSelector (toNSString value)

-- | @NSSavePanel@: Sets and returns the text shown to the left of the "name field". Default value is a localized "Save As:" string. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- nameFieldLabel@
nameFieldLabel :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
nameFieldLabel nsSavePanel =
  sendMessage nsSavePanel nameFieldLabelSelector

-- | @NSSavePanel@: Sets and returns the text shown to the left of the "name field". Default value is a localized "Save As:" string. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setNameFieldLabel:@
setNameFieldLabel :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setNameFieldLabel nsSavePanel value =
  sendMessage nsSavePanel setNameFieldLabelSelector (toNSString value)

-- | @NSSavePanel@: Sets and returns the user-editable file name shown in the name field. - Note: Calling the deprecated methods that take a "name:" parameter will overwrite any values set before the panel is shown. - Note: If @[panel isExtensionHidden]@ is set to @YES@, the extension will be hidden. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- nameFieldStringValue@
nameFieldStringValue :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
nameFieldStringValue nsSavePanel =
  sendMessage nsSavePanel nameFieldStringValueSelector

-- | @NSSavePanel@: Sets and returns the user-editable file name shown in the name field. - Note: Calling the deprecated methods that take a "name:" parameter will overwrite any values set before the panel is shown. - Note: If @[panel isExtensionHidden]@ is set to @YES@, the extension will be hidden. - Note: Can only be set during the configuration phase. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setNameFieldStringValue:@
setNameFieldStringValue :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setNameFieldStringValue nsSavePanel value =
  sendMessage nsSavePanel setNameFieldStringValueSelector (toNSString value)

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the message shown under title of the panel.
--
-- ObjC selector: @- message@
message :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSString)
message nsSavePanel =
  sendMessage nsSavePanel messageSelector

-- | @NSSavePanel@/@NSOpenPanel@: Sets and returns the message shown under title of the panel.
--
-- ObjC selector: @- setMessage:@
setMessage :: (IsNSSavePanel nsSavePanel, IsNSString value) => nsSavePanel -> value -> IO ()
setMessage nsSavePanel value =
  sendMessage nsSavePanel setMessageSelector (toNSString value)

-- | @NSSavePanel@/@NSOpenPanel@: If @showsHiddenFiles@ is set to @YES@, files that are normally hidden from the user are displayed. This method was published in Mac OS 10.6, but has existed since Mac OS 10.4. This property is KVO compliant. The user may invoke the keyboard shortcut (cmd-shift-.) to show or hide hidden files. Any user interface shown in an an accessory view should be updated by using key value observing (KVO) to watch for changes of this property. Alternatively, the user interface can be directly bound to this property. The default value is @NO@.
--
-- ObjC selector: @- showsHiddenFiles@
showsHiddenFiles :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
showsHiddenFiles nsSavePanel =
  sendMessage nsSavePanel showsHiddenFilesSelector

-- | @NSSavePanel@/@NSOpenPanel@: If @showsHiddenFiles@ is set to @YES@, files that are normally hidden from the user are displayed. This method was published in Mac OS 10.6, but has existed since Mac OS 10.4. This property is KVO compliant. The user may invoke the keyboard shortcut (cmd-shift-.) to show or hide hidden files. Any user interface shown in an an accessory view should be updated by using key value observing (KVO) to watch for changes of this property. Alternatively, the user interface can be directly bound to this property. The default value is @NO@.
--
-- ObjC selector: @- setShowsHiddenFiles:@
setShowsHiddenFiles :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setShowsHiddenFiles nsSavePanel value =
  sendMessage nsSavePanel setShowsHiddenFilesSelector value

-- | @NSSavePanel@: Shows or hides the "Tags" field in the receiver. By passing @YES@, you become responsible for setting Tag names on the resulting file after saving is complete. Default is @YES@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- showsTagField@
showsTagField :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
showsTagField nsSavePanel =
  sendMessage nsSavePanel showsTagFieldSelector

-- | @NSSavePanel@: Shows or hides the "Tags" field in the receiver. By passing @YES@, you become responsible for setting Tag names on the resulting file after saving is complete. Default is @YES@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setShowsTagField:@
setShowsTagField :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setShowsTagField nsSavePanel value =
  sendMessage nsSavePanel setShowsTagFieldSelector value

-- | @NSSavePanel@: When -showsTagField returns YES, set any initial Tag names to be displayed, if necessary, prior to displaying the receiver. Also, if the user clicks "Save", take the result of -tagNames, and set them on the resulting file after saving is complete. Tag names are NSStrings, arrays of which can be used directly with the NSURLTagNamesKey API for getting and setting tags on files. Passing @nil@ or an empty array to -setTagNames: will result in no initial Tag names appearing in the receiver. When -showsTagField returns YES, -tagNames always returns a non-nil array, and when NO, -tagNames always returns @nil@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- tagNames@
tagNames :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSArray)
tagNames nsSavePanel =
  sendMessage nsSavePanel tagNamesSelector

-- | @NSSavePanel@: When -showsTagField returns YES, set any initial Tag names to be displayed, if necessary, prior to displaying the receiver. Also, if the user clicks "Save", take the result of -tagNames, and set them on the resulting file after saving is complete. Tag names are NSStrings, arrays of which can be used directly with the NSURLTagNamesKey API for getting and setting tags on files. Passing @nil@ or an empty array to -setTagNames: will result in no initial Tag names appearing in the receiver. When -showsTagField returns YES, -tagNames always returns a non-nil array, and when NO, -tagNames always returns @nil@. @NSOpenPanel@: Not used.
--
-- ObjC selector: @- setTagNames:@
setTagNames :: (IsNSSavePanel nsSavePanel, IsNSArray value) => nsSavePanel -> value -> IO ()
setTagNames nsSavePanel value =
  sendMessage nsSavePanel setTagNamesSelector (toNSArray value)

-- | @NSSavePanel@: Whether or not to show a control for selecting the type of the saved file. The control shows the types in @allowedContentTypes@. Default is @NO@. @NSOpenPanel@: Not used. - Note: If @allowedContentTypes@ is empty, the control is not displayed.
--
-- ObjC selector: @- showsContentTypes@
showsContentTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO Bool
showsContentTypes nsSavePanel =
  sendMessage nsSavePanel showsContentTypesSelector

-- | @NSSavePanel@: Whether or not to show a control for selecting the type of the saved file. The control shows the types in @allowedContentTypes@. Default is @NO@. @NSOpenPanel@: Not used. - Note: If @allowedContentTypes@ is empty, the control is not displayed.
--
-- ObjC selector: @- setShowsContentTypes:@
setShowsContentTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> Bool -> IO ()
setShowsContentTypes nsSavePanel value =
  sendMessage nsSavePanel setShowsContentTypesSelector value

-- | @NSSavePanel@: An array of NSStrings specifying the file types the user can save the file as. The file type can be a common file extension, or a UTI. A nil value indicates that any file type can be used. If the array is not nil and the array contains no items, an exception will be raised. If no extension is given by the user, the first item in the allowedFileTypes will be used as the extension for the save panel. If the user specifies a type not in the array, and 'allowsOtherFileTypes' is YES, they will be presented with another dialog when prompted to save. The default value is 'nil'. @NSOpenPanel@: On versions less than 10.6, this property is ignored. For applications that link against 10.6 and higher, this property will determine which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. The allowedFileTypes can be changed while the panel is running (ie: from an accessory view). The file type can be a common file extension, or a UTI. This is also known as the "enabled file types". A nil value indicates that all files should be enabled.
--
-- ObjC selector: @- allowedFileTypes@
allowedFileTypes :: IsNSSavePanel nsSavePanel => nsSavePanel -> IO (Id NSArray)
allowedFileTypes nsSavePanel =
  sendMessage nsSavePanel allowedFileTypesSelector

-- | @NSSavePanel@: An array of NSStrings specifying the file types the user can save the file as. The file type can be a common file extension, or a UTI. A nil value indicates that any file type can be used. If the array is not nil and the array contains no items, an exception will be raised. If no extension is given by the user, the first item in the allowedFileTypes will be used as the extension for the save panel. If the user specifies a type not in the array, and 'allowsOtherFileTypes' is YES, they will be presented with another dialog when prompted to save. The default value is 'nil'. @NSOpenPanel@: On versions less than 10.6, this property is ignored. For applications that link against 10.6 and higher, this property will determine which files should be enabled in the open panel. Using the deprecated methods to show the open panel (the ones that take a "types:" parameter) will overwrite this value, and should not be used. The allowedFileTypes can be changed while the panel is running (ie: from an accessory view). The file type can be a common file extension, or a UTI. This is also known as the "enabled file types". A nil value indicates that all files should be enabled.
--
-- ObjC selector: @- setAllowedFileTypes:@
setAllowedFileTypes :: (IsNSSavePanel nsSavePanel, IsNSArray value) => nsSavePanel -> value -> IO ()
setAllowedFileTypes nsSavePanel value =
  sendMessage nsSavePanel setAllowedFileTypesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @savePanel@
savePanelSelector :: Selector '[] (Id NSSavePanel)
savePanelSelector = mkSelector "savePanel"

-- | @Selector@ for @validateVisibleColumns@
validateVisibleColumnsSelector :: Selector '[] ()
validateVisibleColumnsSelector = mkSelector "validateVisibleColumns"

-- | @Selector@ for @ok:@
okSelector :: Selector '[RawId] ()
okSelector = mkSelector "ok:"

-- | @Selector@ for @cancel:@
cancelSelector :: Selector '[RawId] ()
cancelSelector = mkSelector "cancel:"

-- | @Selector@ for @beginSheetModalForWindow:completionHandler:@
beginSheetModalForWindow_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
beginSheetModalForWindow_completionHandlerSelector = mkSelector "beginSheetModalForWindow:completionHandler:"

-- | @Selector@ for @beginWithCompletionHandler:@
beginWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
beginWithCompletionHandlerSelector = mkSelector "beginWithCompletionHandler:"

-- | @Selector@ for @runModal@
runModalSelector :: Selector '[] CLong
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @filename@
filenameSelector :: Selector '[] (Id NSString)
filenameSelector = mkSelector "filename"

-- | @Selector@ for @directory@
directorySelector :: Selector '[] (Id NSString)
directorySelector = mkSelector "directory"

-- | @Selector@ for @setDirectory:@
setDirectorySelector :: Selector '[Id NSString] ()
setDirectorySelector = mkSelector "setDirectory:"

-- | @Selector@ for @requiredFileType@
requiredFileTypeSelector :: Selector '[] (Id NSString)
requiredFileTypeSelector = mkSelector "requiredFileType"

-- | @Selector@ for @setRequiredFileType:@
setRequiredFileTypeSelector :: Selector '[Id NSString] ()
setRequiredFileTypeSelector = mkSelector "setRequiredFileType:"

-- | @Selector@ for @beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSString, Id NSString, Id NSWindow, RawId, Sel, Ptr ()] ()
beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalForDirectory:file:@
runModalForDirectory_fileSelector :: Selector '[Id NSString, Id NSString] CLong
runModalForDirectory_fileSelector = mkSelector "runModalForDirectory:file:"

-- | @Selector@ for @selectText:@
selectTextSelector :: Selector '[RawId] ()
selectTextSelector = mkSelector "selectText:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @directoryURL@
directoryURLSelector :: Selector '[] (Id NSURL)
directoryURLSelector = mkSelector "directoryURL"

-- | @Selector@ for @setDirectoryURL:@
setDirectoryURLSelector :: Selector '[Id NSURL] ()
setDirectoryURLSelector = mkSelector "setDirectoryURL:"

-- | @Selector@ for @allowedContentTypes@
allowedContentTypesSelector :: Selector '[] (Id NSArray)
allowedContentTypesSelector = mkSelector "allowedContentTypes"

-- | @Selector@ for @setAllowedContentTypes:@
setAllowedContentTypesSelector :: Selector '[Id NSArray] ()
setAllowedContentTypesSelector = mkSelector "setAllowedContentTypes:"

-- | @Selector@ for @allowsOtherFileTypes@
allowsOtherFileTypesSelector :: Selector '[] Bool
allowsOtherFileTypesSelector = mkSelector "allowsOtherFileTypes"

-- | @Selector@ for @setAllowsOtherFileTypes:@
setAllowsOtherFileTypesSelector :: Selector '[Bool] ()
setAllowsOtherFileTypesSelector = mkSelector "setAllowsOtherFileTypes:"

-- | @Selector@ for @currentContentType@
currentContentTypeSelector :: Selector '[] (Id UTType)
currentContentTypeSelector = mkSelector "currentContentType"

-- | @Selector@ for @setCurrentContentType:@
setCurrentContentTypeSelector :: Selector '[Id UTType] ()
setCurrentContentTypeSelector = mkSelector "setCurrentContentType:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @expanded@
expandedSelector :: Selector '[] Bool
expandedSelector = mkSelector "expanded"

-- | @Selector@ for @canCreateDirectories@
canCreateDirectoriesSelector :: Selector '[] Bool
canCreateDirectoriesSelector = mkSelector "canCreateDirectories"

-- | @Selector@ for @setCanCreateDirectories:@
setCanCreateDirectoriesSelector :: Selector '[Bool] ()
setCanCreateDirectoriesSelector = mkSelector "setCanCreateDirectories:"

-- | @Selector@ for @canSelectHiddenExtension@
canSelectHiddenExtensionSelector :: Selector '[] Bool
canSelectHiddenExtensionSelector = mkSelector "canSelectHiddenExtension"

-- | @Selector@ for @setCanSelectHiddenExtension:@
setCanSelectHiddenExtensionSelector :: Selector '[Bool] ()
setCanSelectHiddenExtensionSelector = mkSelector "setCanSelectHiddenExtension:"

-- | @Selector@ for @extensionHidden@
extensionHiddenSelector :: Selector '[] Bool
extensionHiddenSelector = mkSelector "extensionHidden"

-- | @Selector@ for @setExtensionHidden:@
setExtensionHiddenSelector :: Selector '[Bool] ()
setExtensionHiddenSelector = mkSelector "setExtensionHidden:"

-- | @Selector@ for @treatsFilePackagesAsDirectories@
treatsFilePackagesAsDirectoriesSelector :: Selector '[] Bool
treatsFilePackagesAsDirectoriesSelector = mkSelector "treatsFilePackagesAsDirectories"

-- | @Selector@ for @setTreatsFilePackagesAsDirectories:@
setTreatsFilePackagesAsDirectoriesSelector :: Selector '[Bool] ()
setTreatsFilePackagesAsDirectoriesSelector = mkSelector "setTreatsFilePackagesAsDirectories:"

-- | @Selector@ for @prompt@
promptSelector :: Selector '[] (Id NSString)
promptSelector = mkSelector "prompt"

-- | @Selector@ for @setPrompt:@
setPromptSelector :: Selector '[Id NSString] ()
setPromptSelector = mkSelector "setPrompt:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @nameFieldLabel@
nameFieldLabelSelector :: Selector '[] (Id NSString)
nameFieldLabelSelector = mkSelector "nameFieldLabel"

-- | @Selector@ for @setNameFieldLabel:@
setNameFieldLabelSelector :: Selector '[Id NSString] ()
setNameFieldLabelSelector = mkSelector "setNameFieldLabel:"

-- | @Selector@ for @nameFieldStringValue@
nameFieldStringValueSelector :: Selector '[] (Id NSString)
nameFieldStringValueSelector = mkSelector "nameFieldStringValue"

-- | @Selector@ for @setNameFieldStringValue:@
setNameFieldStringValueSelector :: Selector '[Id NSString] ()
setNameFieldStringValueSelector = mkSelector "setNameFieldStringValue:"

-- | @Selector@ for @message@
messageSelector :: Selector '[] (Id NSString)
messageSelector = mkSelector "message"

-- | @Selector@ for @setMessage:@
setMessageSelector :: Selector '[Id NSString] ()
setMessageSelector = mkSelector "setMessage:"

-- | @Selector@ for @showsHiddenFiles@
showsHiddenFilesSelector :: Selector '[] Bool
showsHiddenFilesSelector = mkSelector "showsHiddenFiles"

-- | @Selector@ for @setShowsHiddenFiles:@
setShowsHiddenFilesSelector :: Selector '[Bool] ()
setShowsHiddenFilesSelector = mkSelector "setShowsHiddenFiles:"

-- | @Selector@ for @showsTagField@
showsTagFieldSelector :: Selector '[] Bool
showsTagFieldSelector = mkSelector "showsTagField"

-- | @Selector@ for @setShowsTagField:@
setShowsTagFieldSelector :: Selector '[Bool] ()
setShowsTagFieldSelector = mkSelector "setShowsTagField:"

-- | @Selector@ for @tagNames@
tagNamesSelector :: Selector '[] (Id NSArray)
tagNamesSelector = mkSelector "tagNames"

-- | @Selector@ for @setTagNames:@
setTagNamesSelector :: Selector '[Id NSArray] ()
setTagNamesSelector = mkSelector "setTagNames:"

-- | @Selector@ for @showsContentTypes@
showsContentTypesSelector :: Selector '[] Bool
showsContentTypesSelector = mkSelector "showsContentTypes"

-- | @Selector@ for @setShowsContentTypes:@
setShowsContentTypesSelector :: Selector '[Bool] ()
setShowsContentTypesSelector = mkSelector "setShowsContentTypes:"

-- | @Selector@ for @allowedFileTypes@
allowedFileTypesSelector :: Selector '[] (Id NSArray)
allowedFileTypesSelector = mkSelector "allowedFileTypes"

-- | @Selector@ for @setAllowedFileTypes:@
setAllowedFileTypesSelector :: Selector '[Id NSArray] ()
setAllowedFileTypesSelector = mkSelector "setAllowedFileTypes:"

