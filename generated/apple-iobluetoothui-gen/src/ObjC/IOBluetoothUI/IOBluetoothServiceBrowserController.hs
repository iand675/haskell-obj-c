{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothServiceBrowserController
--
-- A NSWindowController subclass to display a window to search for and perform SDP queries on bluetooth                    devices within range.
--
-- This NSWindowController subclass will bring up a generic Bluetooth search and SDP browsing window                    allowing the user to find devices within range, perform SDP queries on a particular device, and                    select a SDP service to connect to.  The client application can provide NSArrays of valid service                    UUIDs to allow, and an NSArray of valid device types to allow.  The device type filter is not                    yet implemented.
--
-- Generated bindings for @IOBluetoothServiceBrowserController@.
module ObjC.IOBluetoothUI.IOBluetoothServiceBrowserController
  ( IOBluetoothServiceBrowserController
  , IsIOBluetoothServiceBrowserController(..)
  , serviceBrowserController
  , browseDevices_options
  , browseDevicesAsSheetForWindow_options_window
  , withServiceBrowserControllerRef
  , getServiceBrowserControllerRef
  , discover
  , discoverAsSheetForWindow_withRecord
  , discoverWithDeviceAttributes_serviceList_serviceRecord
  , setOptions
  , runModal
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo
  , getResults
  , getOptions
  , setSearchAttributes
  , getSearchAttributes
  , addAllowedUUID
  , addAllowedUUIDArray
  , clearAllowedUUIDs
  , setTitle
  , getTitle
  , setDescriptionText
  , getDescriptionText
  , setPrompt
  , getPrompt
  , serviceBrowserControllerSelector
  , browseDevices_optionsSelector
  , browseDevicesAsSheetForWindow_options_windowSelector
  , withServiceBrowserControllerRefSelector
  , getServiceBrowserControllerRefSelector
  , discoverSelector
  , discoverAsSheetForWindow_withRecordSelector
  , discoverWithDeviceAttributes_serviceList_serviceRecordSelector
  , setOptionsSelector
  , runModalSelector
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , getResultsSelector
  , getOptionsSelector
  , setSearchAttributesSelector
  , getSearchAttributesSelector
  , addAllowedUUIDSelector
  , addAllowedUUIDArraySelector
  , clearAllowedUUIDsSelector
  , setTitleSelector
  , getTitleSelector
  , setDescriptionTextSelector
  , getDescriptionTextSelector
  , setPromptSelector
  , getPromptSelector


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

import ObjC.IOBluetoothUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.IOBluetooth.Internal.Classes

-- | serviceBrowserController:
--
-- Allocator work Bluetooth Service Browser window controller.
--
-- @inOptions@ — Bit field for options to set in the newly allocated controller.  Currently no options are available.
--
-- Returns: a new instance of the IOBluetoothServiceBrowserController Controller, nil if unsuccessful.
--
-- ObjC selector: @+ serviceBrowserController:@
serviceBrowserController :: CUInt -> IO (Id IOBluetoothServiceBrowserController)
serviceBrowserController inOptions =
  do
    cls' <- getRequiredClass "IOBluetoothServiceBrowserController"
    sendClassMsg cls' (mkSelector "serviceBrowserController:") (retPtr retVoid) [argCUInt inOptions] >>= retainedObject . castPtr

-- | browseDevices:options:
--
-- ***WARNING*** This method has been deprecated in favor of -setOptions:, -runModal and -getResults.
--
-- @outRecord@ — Pointer to a (IOBluetoothSDPServiceRecord *) object.  This will get allocated and returned to the client if the user selects a service.
--
-- @inOptions@ — For future expansion.  Currently no options defined.
--
-- Returns: IOReturn -
--
-- kIOReturnSuccess  - on successful completion.
--
-- kCanceledErr - User canceled.
--
-- This method allocates and runs the browser window as a modal window waiting for the user to either select a                        service, or cancel the browser window.
--
-- ObjC selector: @+ browseDevices:options:@
browseDevices_options :: IsIOBluetoothSDPServiceRecord outRecord => outRecord -> CUInt -> IO CInt
browseDevices_options outRecord inOptions =
  do
    cls' <- getRequiredClass "IOBluetoothServiceBrowserController"
    withObjCPtr outRecord $ \raw_outRecord ->
      sendClassMsg cls' (mkSelector "browseDevices:options:") retCInt [argPtr (castPtr raw_outRecord :: Ptr ()), argCUInt inOptions]

-- | browseDevicesAsSheetForWindow:options:window:
--
-- ***WARNING*** This method has been deprecated in favor of - beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:.
--
-- @outRecord@ — Pointer to a (IOBluetoothSDPServiceRecord *) object.  This will get allocated and returned to the client if the user selects a service.
--
-- @inOptions@ — For future expansion.  Currently no options defined.
--
-- @inWindow@ — The window to be used as the anchor of the sheet.
--
-- Returns: IOReturn -  kIOReturnSuccess  - on successful completion. kCanceledErr - User canceled.
--
-- This method will allocate and run the IOBluetoothServiceBrowserController browser window as a sheet for a window.
--
-- ObjC selector: @+ browseDevicesAsSheetForWindow:options:window:@
browseDevicesAsSheetForWindow_options_window :: (IsIOBluetoothSDPServiceRecord outRecord, IsNSWindow inWindow) => outRecord -> CUInt -> inWindow -> IO CInt
browseDevicesAsSheetForWindow_options_window outRecord inOptions inWindow =
  do
    cls' <- getRequiredClass "IOBluetoothServiceBrowserController"
    withObjCPtr outRecord $ \raw_outRecord ->
      withObjCPtr inWindow $ \raw_inWindow ->
        sendClassMsg cls' (mkSelector "browseDevicesAsSheetForWindow:options:window:") retCInt [argPtr (castPtr raw_outRecord :: Ptr ()), argCUInt inOptions, argPtr (castPtr raw_inWindow :: Ptr ())]

-- | withServiceBrowserControllerRef:
--
-- Method call to convert an IOBluetoothServiceBrowserControllerRef into an IOBluetoothServiceBrowserController *.
--
-- @serviceBrowserControllerRef@ — IOBluetoothServiceBrowserControllerRef for which an IOBluetoothServiceBrowserController * is desired.
--
-- Returns: Returns the IOBluetoothServiceBrowserController * for the given IOBluetoothServiceBrowserControllerRef.
--
-- ObjC selector: @+ withServiceBrowserControllerRef:@
withServiceBrowserControllerRef :: Ptr () -> IO (Id IOBluetoothServiceBrowserController)
withServiceBrowserControllerRef serviceBrowserControllerRef =
  do
    cls' <- getRequiredClass "IOBluetoothServiceBrowserController"
    sendClassMsg cls' (mkSelector "withServiceBrowserControllerRef:") (retPtr retVoid) [argPtr serviceBrowserControllerRef] >>= retainedObject . castPtr

-- | getServiceBrowserControllerRef
--
-- Returns an IOBluetoothServiceBrowserControllerRef representation of the target IOBluetoothServiceBrowserController object.
--
-- Returns: Returns an IOBluetoothServiceBrowserControllerRef representation of the target IOBluetoothServiceBrowserController object.
--
-- ObjC selector: @- getServiceBrowserControllerRef@
getServiceBrowserControllerRef :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO (Ptr ())
getServiceBrowserControllerRef ioBluetoothServiceBrowserController  =
    fmap castPtr $ sendMsg ioBluetoothServiceBrowserController (mkSelector "getServiceBrowserControllerRef") (retPtr retVoid) []

-- | discover:
--
-- Invoke an already created window controller to display, and run the modal dialog.
--
-- ***WARNING*** This method has been deprecated in favor of -runModal and -getResults.
--
-- @outRecord@ — Pointer to a (IOBluetoothSDPServiceRecord *) object.  This will get allocated and returned to the client if the user selects a service.
--
-- Returns: IOReturn -  kIOReturnSuccess  - on successful completion. kCanceledErr - User canceled.
--
-- This method will run the IOBluetoothServiceBrowserController browser window modally.
--
-- ObjC selector: @- discover:@
discover :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsIOBluetoothSDPServiceRecord outRecord) => ioBluetoothServiceBrowserController -> outRecord -> IO CInt
discover ioBluetoothServiceBrowserController  outRecord =
  withObjCPtr outRecord $ \raw_outRecord ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "discover:") retCInt [argPtr (castPtr raw_outRecord :: Ptr ())]

-- | discoverAsSheetForWindow:withRecord:
--
-- Invoke an already created window controller to display, and run the modal dialog.
--
-- ***WARNING*** This method has been deprecated in favor of -beginSheetModalForWindow:... and -getResults.
--
-- @sheetWindow@ — The window to use for the anchor of the sheet..
--
-- @outRecord@ — Pointer to a (IOBluetoothSDPServiceRecord *) object.  This will get allocated and returned to the client if the user selects a service.
--
-- Returns: IOReturn -  kIOReturnSuccess  - on successful completion. kCanceledErr - User canceled.
--
-- This method will run the IOBluetoothServiceBrowserController browser window as a sheet for the window passed to it in sheetWindow.
--
-- ObjC selector: @- discoverAsSheetForWindow:withRecord:@
discoverAsSheetForWindow_withRecord :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSWindow sheetWindow, IsIOBluetoothSDPServiceRecord outRecord) => ioBluetoothServiceBrowserController -> sheetWindow -> outRecord -> IO CInt
discoverAsSheetForWindow_withRecord ioBluetoothServiceBrowserController  sheetWindow outRecord =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
    withObjCPtr outRecord $ \raw_outRecord ->
        sendMsg ioBluetoothServiceBrowserController (mkSelector "discoverAsSheetForWindow:withRecord:") retCInt [argPtr (castPtr raw_sheetWindow :: Ptr ()), argPtr (castPtr raw_outRecord :: Ptr ())]

-- | discoverWithDeviceAttributes:serviceList:serviceRecord:
--
-- Invoke an already created window controller to display, and run the modal dialog.
--
-- ***WARNING*** This method has been deprecated in favor of -setSearchAttributes:, -addAllowedUUID:, -runModal and -getResults.
--
-- @deviceArray@ — A NSArray of valid device type objects to allow.  Not implemented yet.
--
-- @serviceArray@ — A NSArray of valid UUIDs to allow. The array should contain NSData objects                                specifying the UUID to allow.  We currently only support 16-bit short UUID forms, but                                will allow for any of the 16, 32 or full 128-bit UUID forms.
--
-- @outRecord@ — Pointer to a (IOBluetoothSDPServiceRecord *) object.  This will get allocated                                and returned to the client if the user selects a service.
--
-- Returns: IOReturn -  kIOReturnSuccess  - on successful completion. kCanceledErr - User canceled.
--
-- This method will run the IOBluetoothServiceBrowserController browser window as a sheet for the window passed to it in sheetWindow.
--
-- ObjC selector: @- discoverWithDeviceAttributes:serviceList:serviceRecord:@
discoverWithDeviceAttributes_serviceList_serviceRecord :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSArray serviceArray, IsIOBluetoothSDPServiceRecord outRecord) => ioBluetoothServiceBrowserController -> RawId -> serviceArray -> outRecord -> IO CInt
discoverWithDeviceAttributes_serviceList_serviceRecord ioBluetoothServiceBrowserController  deviceAttributes serviceArray outRecord =
  withObjCPtr serviceArray $ \raw_serviceArray ->
    withObjCPtr outRecord $ \raw_outRecord ->
        sendMsg ioBluetoothServiceBrowserController (mkSelector "discoverWithDeviceAttributes:serviceList:serviceRecord:") retCInt [argPtr (castPtr (unRawId deviceAttributes) :: Ptr ()), argPtr (castPtr raw_serviceArray :: Ptr ()), argPtr (castPtr raw_outRecord :: Ptr ())]

-- | setOptions:
--
-- Modify the options for the window controller.
--
-- @inOptions@ — Bit field to set the options to.
--
-- Returns: None.
--
-- This method will set the options for the browser to new values.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> CUInt -> IO ()
setOptions ioBluetoothServiceBrowserController  inOptions =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "setOptions:") retVoid [argCUInt inOptions]

-- | runModal
--
-- Runs the service browser panel in a modal session to allow the user to select a service on a Bluetooth device.
--
-- The controller will use the panel attributes to filter what devices the user sees.  The allowed UUIDs				will be used to validate the selection the user makes.  The user will only be able to select services				that match the allowed UUIDs.  Only when a selection has been validated (or				the panel cancelled), will this method return.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns kIOBluetoothUISuccess if a successful, validated service selection was made by the user.				Returns kIOBluetoothUIUserCanceledErr if the user cancelled the panel.  These return values are the				same as NSRunStoppedResponse and NSRunAbortedResponse respectively.  They are the standard values				used in a modal session.
--
-- ObjC selector: @- runModal@
runModal :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO CInt
runModal ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "runModal") retCInt []

-- | beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- Runs the service browser panel as a sheet on the target window.
--
-- This function works the same way as -[NSApplication beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:].				The didEndSelector has a similar prototype as in NSApplication except that the first argument is the 				IOBluetoothServiceBrowserController object instead of the window: 				-(void)sheetDidEnd:(IOBluetoothServiceBrowserController *)controller returnCode:(int)returnCode contextInfo:(void *)contextInfo.				The returnCode parameter will either be kIOBluetoothUISuccess or kIOBluetoothUIUserCancelledErr as described in				-runModal.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @sheetWindow@ — NSWindow to attach the service browser panel to as a sheet.
--
-- @modalDelegate@ — Delegate object that gets sent the didEndSelector when the sheet modal session is finished.
--
-- @didEndSelector@ — Selector sent to the modalDelegate when the sheet modal session is finished.
--
-- @contextInfo@ — User-definied value passed to the modalDelegate in the didEndSelector.
--
-- Returns: Returns kIOReturnSuccess if the sheet modal session was started.
--
-- ObjC selector: @- beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSWindow sheetWindow) => ioBluetoothServiceBrowserController -> sheetWindow -> RawId -> Selector -> Ptr () -> IO CInt
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo ioBluetoothServiceBrowserController  sheetWindow modalDelegate didEndSelector contextInfo =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:") retCInt [argPtr (castPtr raw_sheetWindow :: Ptr ()), argPtr (castPtr (unRawId modalDelegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | getResults
--
-- Returns the result of the user's selection.
--
-- There will only be results if the panel has been run, the user has successfully made a selection and that 				selection has been validated.  If kIOBluetoothUISuccess was returned for the session, there should be valid 				results.  Currently only a single device is allowed to be selected, so the results array will only contain 				one object.  However in the future multiple selection will be supported.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns an NSArray of IOBluetoothSDPServiceRecord objects corresponding to the user's selection.  If the user cancelled				the panel, nil will be returned.
--
-- ObjC selector: @- getResults@
getResults :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO (Id NSArray)
getResults ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "getResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | getOptions
--
-- Returns the option bits that control the panel's behavior.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the option bits set by setOptions:
--
-- ObjC selector: @- getOptions@
getOptions :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO CUInt
getOptions ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "getOptions") retCUInt []

-- | setSearchAttributes:
--
-- Sets the search attributes that control the panel's search/inquiry behavior.
--
-- The device search attributes control the inquiry behavior of the panel.  They allow only devices				that match the specified attributes (i.e. class of device) to be displayed to the user.  Note that				this only covers attributes returned in an inquiry result and not actual SDP services on the device.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @searchAttributes@ — Attributes to control the panel's inquiry behavior.
--
-- ObjC selector: @- setSearchAttributes:@
setSearchAttributes :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> Const RawId -> IO ()
setSearchAttributes ioBluetoothServiceBrowserController  searchAttributes =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "setSearchAttributes:") retVoid [argPtr (castPtr (unRawId (unConst searchAttributes)) :: Ptr ())]

-- | getSearchAttributes
--
-- Returns the search attributes that control the panel's search/inquiry behavior.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the search attributes set by setSearchAttributes:
--
-- ObjC selector: @- getSearchAttributes@
getSearchAttributes :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO (Const RawId)
getSearchAttributes ioBluetoothServiceBrowserController  =
    fmap Const $ fmap (RawId . castPtr) $ sendMsg ioBluetoothServiceBrowserController (mkSelector "getSearchAttributes") (retPtr retVoid) []

-- | addAllowedUUID:
--
-- Adds a UUID to the list of UUIDs that are used to validate the user's selection.
--
-- The user's device selection gets validated against the UUIDs passed to -addAllowedUUID:				addAllowedUUIDArray:.  Each call to those methods essentially adds a filter that the				selected device gets validated with.  If any of the filters match, the device is considered				valid.  If they all fail, the device is not valid and the user is presented with an				error code that the device does not support the required services.  The UUID passed to				-addAllowedUUID: is the only UUID that must be present in the device's SDP service records.				Alternatively, all of the UUIDs in the UUID array passed to -addAllowedUUIDArray must be				present.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @allowedUUID@ — UUID that a device may contain to be selected
--
-- ObjC selector: @- addAllowedUUID:@
addAllowedUUID :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsIOBluetoothSDPUUID allowedUUID) => ioBluetoothServiceBrowserController -> allowedUUID -> IO ()
addAllowedUUID ioBluetoothServiceBrowserController  allowedUUID =
  withObjCPtr allowedUUID $ \raw_allowedUUID ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "addAllowedUUID:") retVoid [argPtr (castPtr raw_allowedUUID :: Ptr ())]

-- | addAllowedUUIDArray:
--
-- Adds an array of UUIDs to the list of UUIDs that are used to validate the user's selection.
--
-- The user's device selection gets validated against the UUIDs passed to -addAllowedUUID:				addAllowedUUIDArray:.  Each call to those methods essentially adds a filter that the				selected device gets validated with.  If any of the filters match, the device is considered				valid.  If they all fail, the device is not valid and the user is presented with an				error code that the device does not support the required services.  The UUID passed to				-addAllowedUUID: is the only UUID that must be present in the device's SDP service records.				Alternatively, all of the UUIDs in the UUID array passed to -addAllowedUUIDArray must be				present.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @allowedUUIDArray@ — An NSArray of UUIDs that all must be present in a device for it to be selectable.
--
-- ObjC selector: @- addAllowedUUIDArray:@
addAllowedUUIDArray :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSArray allowedUUIDArray) => ioBluetoothServiceBrowserController -> allowedUUIDArray -> IO ()
addAllowedUUIDArray ioBluetoothServiceBrowserController  allowedUUIDArray =
  withObjCPtr allowedUUIDArray $ \raw_allowedUUIDArray ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "addAllowedUUIDArray:") retVoid [argPtr (castPtr raw_allowedUUIDArray :: Ptr ())]

-- | clearAllowedUUIDs
--
-- Resets the controller back to the default state where it will accept any device the user selects.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- ObjC selector: @- clearAllowedUUIDs@
clearAllowedUUIDs :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO ()
clearAllowedUUIDs ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "clearAllowedUUIDs") retVoid []

-- | setTitle:
--
-- Sets the title of the panel when not run as a sheet.
--
-- The panel title should be localized for best user experience.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @windowTitle@ — Title of the device selector panel.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSString windowTitle) => ioBluetoothServiceBrowserController -> windowTitle -> IO ()
setTitle ioBluetoothServiceBrowserController  windowTitle =
  withObjCPtr windowTitle $ \raw_windowTitle ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_windowTitle :: Ptr ())]

-- | getTitle
--
-- Returns the title of the device selector panel (i.e. what was set in -setTitle:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the title of the device selector panel.
--
-- ObjC selector: @- getTitle@
getTitle :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO (Id NSString)
getTitle ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "getTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setDescriptionText:
--
-- Sets the description text that appears in the device selector panel.
--
-- The description text should be localized for best user experience.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @descriptionText@ — String that appears in the description section of the device selector panel.
--
-- ObjC selector: @- setDescriptionText:@
setDescriptionText :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSString descriptionText) => ioBluetoothServiceBrowserController -> descriptionText -> IO ()
setDescriptionText ioBluetoothServiceBrowserController  descriptionText =
  withObjCPtr descriptionText $ \raw_descriptionText ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "setDescriptionText:") retVoid [argPtr (castPtr raw_descriptionText :: Ptr ())]

-- | getDescriptionText
--
-- Returns the description text that appears in the device selector panel (i.e. what was set in -setDescriptionText:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the description text of the device selector panel.
--
-- ObjC selector: @- getDescriptionText@
getDescriptionText :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO (Id NSString)
getDescriptionText ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "getDescriptionText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setPrompt:
--
-- Sets the title of the default/select button in the device selector panel.
--
-- The prompt text should be localized for best user experience.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @prompt@ — String that appears in the default/select button in the device selector panel.
--
-- ObjC selector: @- setPrompt:@
setPrompt :: (IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController, IsNSString prompt) => ioBluetoothServiceBrowserController -> prompt -> IO ()
setPrompt ioBluetoothServiceBrowserController  prompt =
  withObjCPtr prompt $ \raw_prompt ->
      sendMsg ioBluetoothServiceBrowserController (mkSelector "setPrompt:") retVoid [argPtr (castPtr raw_prompt :: Ptr ())]

-- | getPrompt
--
-- Returns the title of the default/select button in the device selector panel (i.e. what was set in -setPrompt:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the default button title of the device selector panel.
--
-- ObjC selector: @- getPrompt@
getPrompt :: IsIOBluetoothServiceBrowserController ioBluetoothServiceBrowserController => ioBluetoothServiceBrowserController -> IO (Id NSString)
getPrompt ioBluetoothServiceBrowserController  =
    sendMsg ioBluetoothServiceBrowserController (mkSelector "getPrompt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serviceBrowserController:@
serviceBrowserControllerSelector :: Selector
serviceBrowserControllerSelector = mkSelector "serviceBrowserController:"

-- | @Selector@ for @browseDevices:options:@
browseDevices_optionsSelector :: Selector
browseDevices_optionsSelector = mkSelector "browseDevices:options:"

-- | @Selector@ for @browseDevicesAsSheetForWindow:options:window:@
browseDevicesAsSheetForWindow_options_windowSelector :: Selector
browseDevicesAsSheetForWindow_options_windowSelector = mkSelector "browseDevicesAsSheetForWindow:options:window:"

-- | @Selector@ for @withServiceBrowserControllerRef:@
withServiceBrowserControllerRefSelector :: Selector
withServiceBrowserControllerRefSelector = mkSelector "withServiceBrowserControllerRef:"

-- | @Selector@ for @getServiceBrowserControllerRef@
getServiceBrowserControllerRefSelector :: Selector
getServiceBrowserControllerRefSelector = mkSelector "getServiceBrowserControllerRef"

-- | @Selector@ for @discover:@
discoverSelector :: Selector
discoverSelector = mkSelector "discover:"

-- | @Selector@ for @discoverAsSheetForWindow:withRecord:@
discoverAsSheetForWindow_withRecordSelector :: Selector
discoverAsSheetForWindow_withRecordSelector = mkSelector "discoverAsSheetForWindow:withRecord:"

-- | @Selector@ for @discoverWithDeviceAttributes:serviceList:serviceRecord:@
discoverWithDeviceAttributes_serviceList_serviceRecordSelector :: Selector
discoverWithDeviceAttributes_serviceList_serviceRecordSelector = mkSelector "discoverWithDeviceAttributes:serviceList:serviceRecord:"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @getResults@
getResultsSelector :: Selector
getResultsSelector = mkSelector "getResults"

-- | @Selector@ for @getOptions@
getOptionsSelector :: Selector
getOptionsSelector = mkSelector "getOptions"

-- | @Selector@ for @setSearchAttributes:@
setSearchAttributesSelector :: Selector
setSearchAttributesSelector = mkSelector "setSearchAttributes:"

-- | @Selector@ for @getSearchAttributes@
getSearchAttributesSelector :: Selector
getSearchAttributesSelector = mkSelector "getSearchAttributes"

-- | @Selector@ for @addAllowedUUID:@
addAllowedUUIDSelector :: Selector
addAllowedUUIDSelector = mkSelector "addAllowedUUID:"

-- | @Selector@ for @addAllowedUUIDArray:@
addAllowedUUIDArraySelector :: Selector
addAllowedUUIDArraySelector = mkSelector "addAllowedUUIDArray:"

-- | @Selector@ for @clearAllowedUUIDs@
clearAllowedUUIDsSelector :: Selector
clearAllowedUUIDsSelector = mkSelector "clearAllowedUUIDs"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @getTitle@
getTitleSelector :: Selector
getTitleSelector = mkSelector "getTitle"

-- | @Selector@ for @setDescriptionText:@
setDescriptionTextSelector :: Selector
setDescriptionTextSelector = mkSelector "setDescriptionText:"

-- | @Selector@ for @getDescriptionText@
getDescriptionTextSelector :: Selector
getDescriptionTextSelector = mkSelector "getDescriptionText"

-- | @Selector@ for @setPrompt:@
setPromptSelector :: Selector
setPromptSelector = mkSelector "setPrompt:"

-- | @Selector@ for @getPrompt@
getPromptSelector :: Selector
getPromptSelector = mkSelector "getPrompt"

