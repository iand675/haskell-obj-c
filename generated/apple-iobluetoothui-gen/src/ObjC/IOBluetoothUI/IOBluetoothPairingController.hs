{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothPairingController
--
-- A NSWindowController subclass to display a window to initiate pairing to other bluetooth devices.
--
-- Implementation of a window controller to handle pairing with a bluetooth device.  This					class will handle connecting to the Bluetooth Daemon for the purposes of searches, and displaying the results.					When necessary this class will display a sheet asking the user for a PIN code.  This window will not return					anything to the caller if it is canceled or if pairing occurs.
--
-- Generated bindings for @IOBluetoothPairingController@.
module ObjC.IOBluetoothUI.IOBluetoothPairingController
  ( IOBluetoothPairingController
  , IsIOBluetoothPairingController(..)
  , pairingController
  , runModal
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
  , setDescriptionText
  , getDescriptionText
  , setPrompt
  , getPrompt
  , pairingControllerSelector
  , runModalSelector
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

-- | pairingController
--
-- Method call to instantiate a new IOBluetoothPairingController object.
--
-- Returns: An IOBluetoothPairingController instance.  Call runPanelWithAttributes
--
-- Success - a new instance of the Pairing Controller                        Failure	- nil
--
-- ObjC selector: @+ pairingController@
pairingController :: IO (Id IOBluetoothPairingController)
pairingController  =
  do
    cls' <- getRequiredClass "IOBluetoothPairingController"
    sendClassMsg cls' (mkSelector "pairingController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runModal
--
-- Runs the pairing panel in a modal session to allow the user to select a Bluetooth device.
--
-- The controller will use the panel attributes to filter what devices the user sees.  The allowed UUIDs				will be used to validate the selection the user makes.  Only when a selection has been validated (or				the panel cancelled) and the device paired, will this method return.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns kIOBluetoothUISuccess if a successful, validated device selection was made by the user and				that device successfully paired.				Returns kIOBluetoothUIUserCanceledErr if the user cancelled the panel.  These return values are the				same as NSRunStoppedResponse and NSRunAbortedResponse respectively.  They are the standard values				used in a modal session.
--
-- ObjC selector: @- runModal@
runModal :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO CInt
runModal ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "runModal") retCInt []

-- | getResults
--
-- Returns an NSArray of the devices that were paired.
--
-- There will only be results if the panel has been run, the user has successfully made a selection, that 				selection has been validated and the selected device paired.  If kIOBluetoothUISuccess was returned for 				the session, there should be valid results.  Currently only a single device is allowed to be selected, 				so the results array will only contain one object.  However in the future multiple selection may be supported.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns an NSArray of IOBluetoothDevice objects of devices that were paired.  If the user cancelled				the panel, nil will be returned.
--
-- ObjC selector: @- getResults@
getResults :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO (Id NSArray)
getResults ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "getResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setOptions:
--
-- Sets the option bits that control the panel's behavior.
--
-- The pairing controller options control the behavior of the panel.  Currently				kIOBluetoothServiceBrowserControllerOptionsAutoStartInquiry is the only supported option.				In the future more options will be added to control things like whether the connection to				the device is closed when the controller is finished or if multiple selection is allowed.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- @options@ — Options to control the panel's behavior.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> CUInt -> IO ()
setOptions ioBluetoothPairingController  options =
    sendMsg ioBluetoothPairingController (mkSelector "setOptions:") retVoid [argCUInt options]

-- | getOptions
--
-- Returns the option bits that control the panel's behavior.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the option bits set by setOptions:
--
-- ObjC selector: @- getOptions@
getOptions :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO CUInt
getOptions ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "getOptions") retCUInt []

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
setSearchAttributes :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> Const RawId -> IO ()
setSearchAttributes ioBluetoothPairingController  searchAttributes =
    sendMsg ioBluetoothPairingController (mkSelector "setSearchAttributes:") retVoid [argPtr (castPtr (unRawId (unConst searchAttributes)) :: Ptr ())]

-- | getSearchAttributes
--
-- Returns the search attributes that control the panel's search/inquiry behavior.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the search attributes set by setSearchAttributes:
--
-- ObjC selector: @- getSearchAttributes@
getSearchAttributes :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO (Const RawId)
getSearchAttributes ioBluetoothPairingController  =
    fmap Const $ fmap (RawId . castPtr) $ sendMsg ioBluetoothPairingController (mkSelector "getSearchAttributes") (retPtr retVoid) []

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
addAllowedUUID :: (IsIOBluetoothPairingController ioBluetoothPairingController, IsIOBluetoothSDPUUID allowedUUID) => ioBluetoothPairingController -> allowedUUID -> IO ()
addAllowedUUID ioBluetoothPairingController  allowedUUID =
  withObjCPtr allowedUUID $ \raw_allowedUUID ->
      sendMsg ioBluetoothPairingController (mkSelector "addAllowedUUID:") retVoid [argPtr (castPtr raw_allowedUUID :: Ptr ())]

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
addAllowedUUIDArray :: (IsIOBluetoothPairingController ioBluetoothPairingController, IsNSArray allowedUUIDArray) => ioBluetoothPairingController -> allowedUUIDArray -> IO ()
addAllowedUUIDArray ioBluetoothPairingController  allowedUUIDArray =
  withObjCPtr allowedUUIDArray $ \raw_allowedUUIDArray ->
      sendMsg ioBluetoothPairingController (mkSelector "addAllowedUUIDArray:") retVoid [argPtr (castPtr raw_allowedUUIDArray :: Ptr ())]

-- | clearAllowedUUIDs
--
-- Resets the controller back to the default state where it will accept any device the user selects.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- ObjC selector: @- clearAllowedUUIDs@
clearAllowedUUIDs :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO ()
clearAllowedUUIDs ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "clearAllowedUUIDs") retVoid []

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
setTitle :: (IsIOBluetoothPairingController ioBluetoothPairingController, IsNSString windowTitle) => ioBluetoothPairingController -> windowTitle -> IO ()
setTitle ioBluetoothPairingController  windowTitle =
  withObjCPtr windowTitle $ \raw_windowTitle ->
      sendMsg ioBluetoothPairingController (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_windowTitle :: Ptr ())]

-- | getTitle
--
-- Returns the title of the device selector panel (i.e. what was set in -setTitle:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the title of the device selector panel.
--
-- ObjC selector: @- getTitle@
getTitle :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO (Id NSString)
getTitle ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "getTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setDescriptionText :: (IsIOBluetoothPairingController ioBluetoothPairingController, IsNSString descriptionText) => ioBluetoothPairingController -> descriptionText -> IO ()
setDescriptionText ioBluetoothPairingController  descriptionText =
  withObjCPtr descriptionText $ \raw_descriptionText ->
      sendMsg ioBluetoothPairingController (mkSelector "setDescriptionText:") retVoid [argPtr (castPtr raw_descriptionText :: Ptr ())]

-- | getDescriptionText
--
-- Returns the description text that appears in the device selector panel (i.e. what was set in -setDescriptionText:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the description text of the device selector panel.
--
-- ObjC selector: @- getDescriptionText@
getDescriptionText :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO (Id NSString)
getDescriptionText ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "getDescriptionText") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setPrompt :: (IsIOBluetoothPairingController ioBluetoothPairingController, IsNSString prompt) => ioBluetoothPairingController -> prompt -> IO ()
setPrompt ioBluetoothPairingController  prompt =
  withObjCPtr prompt $ \raw_prompt ->
      sendMsg ioBluetoothPairingController (mkSelector "setPrompt:") retVoid [argPtr (castPtr raw_prompt :: Ptr ())]

-- | getPrompt
--
-- Returns the title of the default/select button in the device selector panel (i.e. what was set in -setPrompt:).
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- Returns: Returns the default button title of the device selector panel.
--
-- ObjC selector: @- getPrompt@
getPrompt :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO (Id NSString)
getPrompt ioBluetoothPairingController  =
    sendMsg ioBluetoothPairingController (mkSelector "getPrompt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pairingController@
pairingControllerSelector :: Selector
pairingControllerSelector = mkSelector "pairingController"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

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

