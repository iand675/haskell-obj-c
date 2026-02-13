{-# LANGUAGE DataKinds #-}
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
  , addAllowedUUIDArraySelector
  , addAllowedUUIDSelector
  , clearAllowedUUIDsSelector
  , getDescriptionTextSelector
  , getOptionsSelector
  , getPromptSelector
  , getResultsSelector
  , getSearchAttributesSelector
  , getTitleSelector
  , pairingControllerSelector
  , runModalSelector
  , setDescriptionTextSelector
  , setOptionsSelector
  , setPromptSelector
  , setSearchAttributesSelector
  , setTitleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' pairingControllerSelector

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
runModal ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController runModalSelector

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
getResults ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController getResultsSelector

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
setOptions ioBluetoothPairingController options =
  sendMessage ioBluetoothPairingController setOptionsSelector options

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
getOptions ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController getOptionsSelector

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
setSearchAttributes ioBluetoothPairingController searchAttributes =
  sendMessage ioBluetoothPairingController setSearchAttributesSelector searchAttributes

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
getSearchAttributes ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController getSearchAttributesSelector

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
addAllowedUUID ioBluetoothPairingController allowedUUID =
  sendMessage ioBluetoothPairingController addAllowedUUIDSelector (toIOBluetoothSDPUUID allowedUUID)

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
addAllowedUUIDArray ioBluetoothPairingController allowedUUIDArray =
  sendMessage ioBluetoothPairingController addAllowedUUIDArraySelector (toNSArray allowedUUIDArray)

-- | clearAllowedUUIDs
--
-- Resets the controller back to the default state where it will accept any device the user selects.
--
-- NOTE: This method is only available in Mac OS X 10.2.4 (Bluetooth v1.1) or later.
--
-- ObjC selector: @- clearAllowedUUIDs@
clearAllowedUUIDs :: IsIOBluetoothPairingController ioBluetoothPairingController => ioBluetoothPairingController -> IO ()
clearAllowedUUIDs ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController clearAllowedUUIDsSelector

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
setTitle ioBluetoothPairingController windowTitle =
  sendMessage ioBluetoothPairingController setTitleSelector (toNSString windowTitle)

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
getTitle ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController getTitleSelector

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
setDescriptionText ioBluetoothPairingController descriptionText =
  sendMessage ioBluetoothPairingController setDescriptionTextSelector (toNSString descriptionText)

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
getDescriptionText ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController getDescriptionTextSelector

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
setPrompt ioBluetoothPairingController prompt =
  sendMessage ioBluetoothPairingController setPromptSelector (toNSString prompt)

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
getPrompt ioBluetoothPairingController =
  sendMessage ioBluetoothPairingController getPromptSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pairingController@
pairingControllerSelector :: Selector '[] (Id IOBluetoothPairingController)
pairingControllerSelector = mkSelector "pairingController"

-- | @Selector@ for @runModal@
runModalSelector :: Selector '[] CInt
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @getResults@
getResultsSelector :: Selector '[] (Id NSArray)
getResultsSelector = mkSelector "getResults"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[CUInt] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @getOptions@
getOptionsSelector :: Selector '[] CUInt
getOptionsSelector = mkSelector "getOptions"

-- | @Selector@ for @setSearchAttributes:@
setSearchAttributesSelector :: Selector '[Const RawId] ()
setSearchAttributesSelector = mkSelector "setSearchAttributes:"

-- | @Selector@ for @getSearchAttributes@
getSearchAttributesSelector :: Selector '[] (Const RawId)
getSearchAttributesSelector = mkSelector "getSearchAttributes"

-- | @Selector@ for @addAllowedUUID:@
addAllowedUUIDSelector :: Selector '[Id IOBluetoothSDPUUID] ()
addAllowedUUIDSelector = mkSelector "addAllowedUUID:"

-- | @Selector@ for @addAllowedUUIDArray:@
addAllowedUUIDArraySelector :: Selector '[Id NSArray] ()
addAllowedUUIDArraySelector = mkSelector "addAllowedUUIDArray:"

-- | @Selector@ for @clearAllowedUUIDs@
clearAllowedUUIDsSelector :: Selector '[] ()
clearAllowedUUIDsSelector = mkSelector "clearAllowedUUIDs"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @getTitle@
getTitleSelector :: Selector '[] (Id NSString)
getTitleSelector = mkSelector "getTitle"

-- | @Selector@ for @setDescriptionText:@
setDescriptionTextSelector :: Selector '[Id NSString] ()
setDescriptionTextSelector = mkSelector "setDescriptionText:"

-- | @Selector@ for @getDescriptionText@
getDescriptionTextSelector :: Selector '[] (Id NSString)
getDescriptionTextSelector = mkSelector "getDescriptionText"

-- | @Selector@ for @setPrompt:@
setPromptSelector :: Selector '[Id NSString] ()
setPromptSelector = mkSelector "setPrompt:"

-- | @Selector@ for @getPrompt@
getPromptSelector :: Selector '[] (Id NSString)
getPromptSelector = mkSelector "getPrompt"

