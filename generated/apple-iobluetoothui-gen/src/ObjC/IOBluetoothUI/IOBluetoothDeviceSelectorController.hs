{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothDeviceSelectorController
--
-- A NSWindowController subclass to display a window to initiate pairing to other bluetooth devices.
--
-- Implementation of a window controller to return a NSArray of selected bluetooth devices.  This                    class will handle connecting to the Bluetooth Daemon for the purposes of searches, and displaying                    the results.  This controller will return a NSArray of IOBluetoothDevice objects to the user.
--
-- Generated bindings for @IOBluetoothDeviceSelectorController@.
module ObjC.IOBluetoothUI.IOBluetoothDeviceSelectorController
  ( IOBluetoothDeviceSelectorController
  , IsIOBluetoothDeviceSelectorController(..)
  , deviceSelector
  , runModal
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo
  , getResults
  , setOptions
  , getOptions
  , setSearchAttributes
  , getSearchAttributes
  , addAllowedUUID
  , addAllowedUUIDArray
  , clearAllowedUUIDs
  , setTitle
  , getTitle
  , setHeader
  , getHeader
  , setDescriptionText
  , getDescriptionText
  , setPrompt
  , getPrompt
  , setCancel
  , getCancel
  , deviceSelectorSelector
  , runModalSelector
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , getResultsSelector
  , setOptionsSelector
  , getOptionsSelector
  , setSearchAttributesSelector
  , getSearchAttributesSelector
  , addAllowedUUIDSelector
  , addAllowedUUIDArraySelector
  , clearAllowedUUIDsSelector
  , setTitleSelector
  , getTitleSelector
  , setHeaderSelector
  , getHeaderSelector
  , setDescriptionTextSelector
  , getDescriptionTextSelector
  , setPromptSelector
  , getPromptSelector
  , setCancelSelector
  , getCancelSelector


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

-- | deviceSelector
--
-- Method call to instantiate a new IOBluetoothDeviceSelectorController object.
--
-- Returns: Success - a new instance of the device selector Controller                        Failure	- nil
--
-- ObjC selector: @+ deviceSelector@
deviceSelector :: IO (Id IOBluetoothDeviceSelectorController)
deviceSelector  =
  do
    cls' <- getRequiredClass "IOBluetoothDeviceSelectorController"
    sendClassMsg cls' (mkSelector "deviceSelector") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runModal
--
-- Runs the device selector panel in a modal session to allow the user to select a Bluetooth device.
--
-- The controller will use the panel attributes to filter what devices the user sees.  The allowed UUIDs				will be used to validate the selection the user makes.  Only when a selection has been validated (or				the panel cancelled), will this method return.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns kIOBluetoothUISuccess if a successful, validated device selection was made by the user.				Returns kIOBluetoothUIUserCanceledErr if the user cancelled the panel.  These return values are the				same as NSRunStoppedResponse and NSRunAbortedResponse respectively.  They are the standard values				used in a modal session.
--
-- ObjC selector: @- runModal@
runModal :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO CInt
runModal ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "runModal") retCInt []

-- | beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- Runs the device selector panel as a sheet on the target window.
--
-- This function works the same way as -[NSApplication beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:].				The didEndSelector has a similar prototype as in NSApplication except that the first argument is the 				IOBluetoothDeviceSelectorController object instead of the window: 				-(void)sheetDidEnd:(IOBluetoothDeviceSelectorController *)controller returnCode:(int)returnCode contextInfo:(void *)contextInfo.				The returnCode parameter will either be kIOBluetoothUISuccess or kIOBluetoothUIUserCancelledErr as described in				-runModal.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @sheetWindow@ — NSWindow to attach the device selector panel to as a sheet.
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
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSWindow sheetWindow) => ioBluetoothDeviceSelectorController -> sheetWindow -> RawId -> Selector -> Ptr () -> IO CInt
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo ioBluetoothDeviceSelectorController  sheetWindow modalDelegate didEndSelector contextInfo =
  withObjCPtr sheetWindow $ \raw_sheetWindow ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:") retCInt [argPtr (castPtr raw_sheetWindow :: Ptr ()), argPtr (castPtr (unRawId modalDelegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | getResults
--
-- Returns the result of the user's selection.
--
-- There will only be results if the panel has been run, the user has successfully made a selection and that 				selection has been validated.  If kIOBluetoothUISuccess was returned for the session, there should be valid 				results.  Currently only a single device is allowed to be selected, so the results array will only contain 				one object.  However in the future multiple selection will be supported.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns an NSArray of IOBluetoothDevice objects corresponding to the user's selection.  If the user cancelled				the panel, nil will be returned.
--
-- ObjC selector: @- getResults@
getResults :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Id NSArray)
getResults ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setOptions:
--
-- Sets the option bits that control the panel's behavior.
--
-- The service browser controller options control the behavior of the panel.  Currently				kIOBluetoothServiceBrowserControllerOptionsAutoStartInquiry is the only supported option.				In the future more options will be added to control things like whether the connection to				the device is closed when the controller is finished or if multiple selection is allowed.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @options@ — Options to control the panel's behavior.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> CUInt -> IO ()
setOptions ioBluetoothDeviceSelectorController  options =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "setOptions:") retVoid [argCUInt options]

-- | getOptions
--
-- Returns the option bits that control the panel's behavior.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the option bits set by setOptions:
--
-- ObjC selector: @- getOptions@
getOptions :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO CUInt
getOptions ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getOptions") retCUInt []

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
setSearchAttributes :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> Const RawId -> IO ()
setSearchAttributes ioBluetoothDeviceSelectorController  searchAttributes =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "setSearchAttributes:") retVoid [argPtr (castPtr (unRawId (unConst searchAttributes)) :: Ptr ())]

-- | getSearchAttributes
--
-- Returns the search attributes that control the panel's search/inquiry behavior.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the search attributes set by setSearchAttributes:
--
-- ObjC selector: @- getSearchAttributes@
getSearchAttributes :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Const RawId)
getSearchAttributes ioBluetoothDeviceSelectorController  =
    fmap Const $ fmap (RawId . castPtr) $ sendMsg ioBluetoothDeviceSelectorController (mkSelector "getSearchAttributes") (retPtr retVoid) []

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
addAllowedUUID :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsIOBluetoothSDPUUID allowedUUID) => ioBluetoothDeviceSelectorController -> allowedUUID -> IO ()
addAllowedUUID ioBluetoothDeviceSelectorController  allowedUUID =
  withObjCPtr allowedUUID $ \raw_allowedUUID ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "addAllowedUUID:") retVoid [argPtr (castPtr raw_allowedUUID :: Ptr ())]

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
addAllowedUUIDArray :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSArray allowedUUIDArray) => ioBluetoothDeviceSelectorController -> allowedUUIDArray -> IO ()
addAllowedUUIDArray ioBluetoothDeviceSelectorController  allowedUUIDArray =
  withObjCPtr allowedUUIDArray $ \raw_allowedUUIDArray ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "addAllowedUUIDArray:") retVoid [argPtr (castPtr raw_allowedUUIDArray :: Ptr ())]

-- | clearAllowedUUIDs
--
-- Resets the controller back to the default state where it will accept any device the user selects.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- ObjC selector: @- clearAllowedUUIDs@
clearAllowedUUIDs :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO ()
clearAllowedUUIDs ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "clearAllowedUUIDs") retVoid []

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
setTitle :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSString windowTitle) => ioBluetoothDeviceSelectorController -> windowTitle -> IO ()
setTitle ioBluetoothDeviceSelectorController  windowTitle =
  withObjCPtr windowTitle $ \raw_windowTitle ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_windowTitle :: Ptr ())]

-- | getTitle
--
-- Returns the title of the device selector panel (i.e. what was set in -setTitle:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the title of the device selector panel.
--
-- ObjC selector: @- getTitle@
getTitle :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Id NSString)
getTitle ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setHeader:
--
-- Sets the header text that appears in the device selector panel.
--
-- The description text should be localized for best user experience.
--
-- NOTE: This method is only available in Mac OS X 10.9 or later.
--
-- @headerText@ — String that appears in the description section of the device selector panel.
--
-- ObjC selector: @- setHeader:@
setHeader :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSString headerText) => ioBluetoothDeviceSelectorController -> headerText -> IO ()
setHeader ioBluetoothDeviceSelectorController  headerText =
  withObjCPtr headerText $ \raw_headerText ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "setHeader:") retVoid [argPtr (castPtr raw_headerText :: Ptr ())]

-- | getHeader
--
-- Returns the header text that appears in the device selector panel (i.e. what was set in -setHeader:).
--
-- NOTE: This method is only available in Mac OS X 10.9 or later.
--
-- Returns: Returns the header text of the device selector panel.
--
-- ObjC selector: @- getHeader@
getHeader :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Id NSString)
getHeader ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getHeader") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setDescriptionText :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSString descriptionText) => ioBluetoothDeviceSelectorController -> descriptionText -> IO ()
setDescriptionText ioBluetoothDeviceSelectorController  descriptionText =
  withObjCPtr descriptionText $ \raw_descriptionText ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "setDescriptionText:") retVoid [argPtr (castPtr raw_descriptionText :: Ptr ())]

-- | getDescriptionText
--
-- Returns the description text that appears in the device selector panel (i.e. what was set in -setDescriptionText:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the description text of the device selector panel.
--
-- ObjC selector: @- getDescriptionText@
getDescriptionText :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Id NSString)
getDescriptionText ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getDescriptionText") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setPrompt :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSString prompt) => ioBluetoothDeviceSelectorController -> prompt -> IO ()
setPrompt ioBluetoothDeviceSelectorController  prompt =
  withObjCPtr prompt $ \raw_prompt ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "setPrompt:") retVoid [argPtr (castPtr raw_prompt :: Ptr ())]

-- | getPrompt
--
-- Returns the title of the default/select button in the device selector panel (i.e. what was set in -setPrompt:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the default button title of the device selector panel.
--
-- ObjC selector: @- getPrompt@
getPrompt :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Id NSString)
getPrompt ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getPrompt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setCancel:
--
-- Sets the title of the default/cancel button in the device selector panel.
--
-- The prompt text should be localized for best user experience.
--
-- NOTE: This method is only available in Mac OS X 10.9 or later.
--
-- @prompt@ — String that appears in the default/cancel button in the device selector panel.
--
-- ObjC selector: @- setCancel:@
setCancel :: (IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController, IsNSString prompt) => ioBluetoothDeviceSelectorController -> prompt -> IO ()
setCancel ioBluetoothDeviceSelectorController  prompt =
  withObjCPtr prompt $ \raw_prompt ->
      sendMsg ioBluetoothDeviceSelectorController (mkSelector "setCancel:") retVoid [argPtr (castPtr raw_prompt :: Ptr ())]

-- | getCancel
--
-- Returns the title of the default/cancel button in the device selector panel (i.e. what was set in -setPrompt:).
--
-- NOTE: This method is only available in Mac OS X 10.9 or later.
--
-- Returns: Returns the default cancel button title of the device selector panel.
--
-- ObjC selector: @- getCancel@
getCancel :: IsIOBluetoothDeviceSelectorController ioBluetoothDeviceSelectorController => ioBluetoothDeviceSelectorController -> IO (Id NSString)
getCancel ioBluetoothDeviceSelectorController  =
    sendMsg ioBluetoothDeviceSelectorController (mkSelector "getCancel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceSelector@
deviceSelectorSelector :: Selector
deviceSelectorSelector = mkSelector "deviceSelector"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @getResults@
getResultsSelector :: Selector
getResultsSelector = mkSelector "getResults"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

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

-- | @Selector@ for @setHeader:@
setHeaderSelector :: Selector
setHeaderSelector = mkSelector "setHeader:"

-- | @Selector@ for @getHeader@
getHeaderSelector :: Selector
getHeaderSelector = mkSelector "getHeader"

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

-- | @Selector@ for @setCancel:@
setCancelSelector :: Selector
setCancelSelector = mkSelector "setCancel:"

-- | @Selector@ for @getCancel@
getCancelSelector :: Selector
getCancelSelector = mkSelector "getCancel"

