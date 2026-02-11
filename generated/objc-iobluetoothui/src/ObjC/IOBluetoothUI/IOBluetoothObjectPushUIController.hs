{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An NSWindowController subclass that supports the creation of an IOBluetoothObjectPushUIController object.
--
-- Generated bindings for @IOBluetoothObjectPushUIController@.
module ObjC.IOBluetoothUI.IOBluetoothObjectPushUIController
  ( IOBluetoothObjectPushUIController
  , IsIOBluetoothObjectPushUIController(..)
  , initObjectPushWithBluetoothDevice_withFiles_delegate
  , runModal
  , runPanel
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo
  , stop
  , setTitle
  , getTitle
  , setIconImage
  , getDevice
  , isTransferInProgress
  , initObjectPushWithBluetoothDevice_withFiles_delegateSelector
  , runModalSelector
  , runPanelSelector
  , beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , stopSelector
  , setTitleSelector
  , getTitleSelector
  , setIconImageSelector
  , getDeviceSelector
  , isTransferInProgressSelector


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

-- | initObjectPushWithBluetoothDevice: withFiles: delegate:
--
-- Creates and returns a new IOBluetoothObjectPush object
--
-- The event delegate should implement a single delegate method:
--
-- - (void) objectPushComplete: (IOBluetoothObjectPushUIController*) inPusher
--
-- The method will be called when the transaction is complete and				should be used to release the push object by the delegate. If no delegate is set				the object will release itself when the transfer is finished.
--
-- @inDevice@ — The remote device to send the files to
--
-- @inFiles@ — An array of file paths to send
--
-- @inDelegate@ — A delegate object that implements the single method above.  If no delegate							is specified this object will release itself when the transaction is complete.
--
-- Returns: An IOBluetoothObjectPushUIController object on success, nil on fail.
--
-- ObjC selector: @- initObjectPushWithBluetoothDevice:withFiles:delegate:@
initObjectPushWithBluetoothDevice_withFiles_delegate :: (IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController, IsIOBluetoothDevice inDevice, IsNSArray inFiles) => ioBluetoothObjectPushUIController -> inDevice -> inFiles -> RawId -> IO (Id IOBluetoothObjectPushUIController)
initObjectPushWithBluetoothDevice_withFiles_delegate ioBluetoothObjectPushUIController  inDevice inFiles inDelegate =
withObjCPtr inDevice $ \raw_inDevice ->
  withObjCPtr inFiles $ \raw_inFiles ->
      sendMsg ioBluetoothObjectPushUIController (mkSelector "initObjectPushWithBluetoothDevice:withFiles:delegate:") (retPtr retVoid) [argPtr (castPtr raw_inDevice :: Ptr ()), argPtr (castPtr raw_inFiles :: Ptr ()), argPtr (castPtr (unRawId inDelegate) :: Ptr ())] >>= ownedObject . castPtr

-- | runModal
--
-- Runs the transfer UI panel in a modal session
--
-- Returns when the modal session has ended. This object will call back over the 				delegate method (above) when the transfer is complete.  Users should release 				the object then. If no delegate is set the object will release itself.
--
-- Returns: The call will stall in this method until the modal session is complete.
--
-- ObjC selector: @- runModal@
runModal :: IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController => ioBluetoothObjectPushUIController -> IO ()
runModal ioBluetoothObjectPushUIController  =
  sendMsg ioBluetoothObjectPushUIController (mkSelector "runModal") retVoid []

-- | runPanel
--
-- Runs the transfer UI as a panel with no modal session
--
-- Returns immediately.  The object will callback over the delegate method (above)				when the transfer is completed.  If no delegate is set the object will release itself.
--
-- Returns: The method will return immediately.
--
-- ObjC selector: @- runPanel@
runPanel :: IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController => ioBluetoothObjectPushUIController -> IO ()
runPanel ioBluetoothObjectPushUIController  =
  sendMsg ioBluetoothObjectPushUIController (mkSelector "runPanel") retVoid []

-- | beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:
--
-- Runs the  transfer UI as a sheet on the target window.
--
-- This function works the same way as -[NSApplication beginSheet:modalForWindow:modalDelegate:didEndSelector:contextInfo:].				The didEndSelector has a similar prototype as in NSApplication except that the first argument is the 				IOBluetoothDeviceSelectorController object instead of the window:
--
-- -(void)sheetDidEnd:(IOBluetoothDeviceSelectorController *)controller returnCode:(int)returnCode contextInfo:(void *)contextInfo.				The returnCode parameter will either be kIOBluetoothUISuccess or kIOBluetoothUIUserCancelledErr as described in				-runModal.
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
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController, IsNSWindow sheetWindow) => ioBluetoothObjectPushUIController -> sheetWindow -> RawId -> Selector -> Ptr () -> IO CInt
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo ioBluetoothObjectPushUIController  sheetWindow modalDelegate didEndSelector contextInfo =
withObjCPtr sheetWindow $ \raw_sheetWindow ->
    sendMsg ioBluetoothObjectPushUIController (mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:") retCInt [argPtr (castPtr raw_sheetWindow :: Ptr ()), argPtr (castPtr (unRawId modalDelegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | stop
--
-- Stops the transfer UI
--
-- Returns immediately. The object will callback over the delegate method (above)				when the transfer is completed, or will release itself if no delegate is set.
--
-- Returns: The method will return immediately.
--
-- ObjC selector: @- stop@
stop :: IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController => ioBluetoothObjectPushUIController -> IO ()
stop ioBluetoothObjectPushUIController  =
  sendMsg ioBluetoothObjectPushUIController (mkSelector "stop") retVoid []

-- | setTitle:
--
-- Sets the title of the panel when not run as a sheet.
--
-- The panel title should be localized for best user experience.
--
-- @windowTitle@ — Title of the device selector panel.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController, IsNSString windowTitle) => ioBluetoothObjectPushUIController -> windowTitle -> IO ()
setTitle ioBluetoothObjectPushUIController  windowTitle =
withObjCPtr windowTitle $ \raw_windowTitle ->
    sendMsg ioBluetoothObjectPushUIController (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_windowTitle :: Ptr ())]

-- | getTitle
--
-- Returns the title of the transfer panel (i.e. what was set in -setTitle:).
--
-- Returns: Returns the title of the transfer panel.
--
-- ObjC selector: @- getTitle@
getTitle :: IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController => ioBluetoothObjectPushUIController -> IO (Id NSString)
getTitle ioBluetoothObjectPushUIController  =
  sendMsg ioBluetoothObjectPushUIController (mkSelector "getTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setIconImage:
--
-- Manually sets the icon used in the panel.
--
-- The panel icon should be set to the icon of the calling application.  If not set, the panel				will try to load up the correct icon for the target device, and will default to the icon of				the running application on fail.
--
-- @image@ — Image to use as the icon.
--
-- ObjC selector: @- setIconImage:@
setIconImage :: (IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController, IsNSImage image) => ioBluetoothObjectPushUIController -> image -> IO ()
setIconImage ioBluetoothObjectPushUIController  image =
withObjCPtr image $ \raw_image ->
    sendMsg ioBluetoothObjectPushUIController (mkSelector "setIconImage:") retVoid [argPtr (castPtr raw_image :: Ptr ())]

-- | getDevice:
--
-- Gets the object representing the remote target device in the transfer.
--
-- Returns: The remote device of the transfer.
--
-- ObjC selector: @- getDevice@
getDevice :: IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController => ioBluetoothObjectPushUIController -> IO (Id IOBluetoothDevice)
getDevice ioBluetoothObjectPushUIController  =
  sendMsg ioBluetoothObjectPushUIController (mkSelector "getDevice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isTransferInProgress:
--
-- Gets state of the transfer
--
-- Returns: The state of the transfer
--
-- ObjC selector: @- isTransferInProgress@
isTransferInProgress :: IsIOBluetoothObjectPushUIController ioBluetoothObjectPushUIController => ioBluetoothObjectPushUIController -> IO Bool
isTransferInProgress ioBluetoothObjectPushUIController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothObjectPushUIController (mkSelector "isTransferInProgress") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initObjectPushWithBluetoothDevice:withFiles:delegate:@
initObjectPushWithBluetoothDevice_withFiles_delegateSelector :: Selector
initObjectPushWithBluetoothDevice_withFiles_delegateSelector = mkSelector "initObjectPushWithBluetoothDevice:withFiles:delegate:"

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @runPanel@
runPanelSelector :: Selector
runPanelSelector = mkSelector "runPanel"

-- | @Selector@ for @beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:@
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "beginSheetModalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @getTitle@
getTitleSelector :: Selector
getTitleSelector = mkSelector "getTitle"

-- | @Selector@ for @setIconImage:@
setIconImageSelector :: Selector
setIconImageSelector = mkSelector "setIconImage:"

-- | @Selector@ for @getDevice@
getDeviceSelector :: Selector
getDeviceSelector = mkSelector "getDevice"

-- | @Selector@ for @isTransferInProgress@
isTransferInProgressSelector :: Selector
isTransferInProgressSelector = mkSelector "isTransferInProgress"

