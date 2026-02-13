{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICDeviceBrowser
--
-- The ICDeviceBrowser object is used to find devices such as digital cameras and scanners that are supported by Image Capture. These device may be directly attached to the USB or FireWire bus on the host computer, or available over a TCP/IP network. This object communicates with an Image Capture agent process asynchronously to accomplish this.
--
-- Generated bindings for @ICDeviceBrowser@.
module ObjC.ImageCaptureCore.ICDeviceBrowser
  ( ICDeviceBrowser
  , IsICDeviceBrowser(..)
  , init_
  , start
  , stop
  , requestContentsAuthorizationWithCompletion
  , requestControlAuthorizationWithCompletion
  , resetContentsAuthorizationWithCompletion
  , resetControlAuthorizationWithCompletion
  , delegate
  , setDelegate
  , browsing
  , suspended
  , browsedDeviceTypeMask
  , setBrowsedDeviceTypeMask
  , devices
  , preferredDevice
  , contentsAuthorizationStatus
  , controlAuthorizationStatus
  , browsedDeviceTypeMaskSelector
  , browsingSelector
  , contentsAuthorizationStatusSelector
  , controlAuthorizationStatusSelector
  , delegateSelector
  , devicesSelector
  , initSelector
  , preferredDeviceSelector
  , requestContentsAuthorizationWithCompletionSelector
  , requestControlAuthorizationWithCompletionSelector
  , resetContentsAuthorizationWithCompletionSelector
  , resetControlAuthorizationWithCompletionSelector
  , setBrowsedDeviceTypeMaskSelector
  , setDelegateSelector
  , startSelector
  , stopSelector
  , suspendedSelector

  -- * Enum types
  , ICDeviceTypeMask(ICDeviceTypeMask)
  , pattern ICDeviceTypeMaskCamera
  , pattern ICDeviceTypeMaskScanner

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | init
--
-- This is the designated initializer.
--
-- ObjC selector: @- init@
init_ :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id ICDeviceBrowser)
init_ icDeviceBrowser =
  sendOwnedMessage icDeviceBrowser initSelector

-- | start:
--
-- This message tells the receiver to start looking for devices.
--
-- Make sure that the receiver's delegate is set prior to sending this message; otherwise this message will be ignored. The messages the delegate can expect to receive are described by ICDeviceBrowserDelegate protocol.
--
-- ObjC selector: @- start@
start :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO ()
start icDeviceBrowser =
  sendMessage icDeviceBrowser startSelector

-- | stop:
--
-- This method tells the receiver to stop looking for devices.
--
-- This will free all device instances that are not in use.
--
-- ObjC selector: @- stop@
stop :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO ()
stop icDeviceBrowser =
  sendMessage icDeviceBrowser stopSelector

-- | requestContentsAuthorizationWithCompletion:
--
-- This method requests the user’s permission, if needed, for accessing the conents of an external media device.
--
-- ObjC selector: @- requestContentsAuthorizationWithCompletion:@
requestContentsAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
requestContentsAuthorizationWithCompletion icDeviceBrowser completion =
  sendMessage icDeviceBrowser requestContentsAuthorizationWithCompletionSelector completion

-- | requestControlAuthorizationWithCompletion:
--
-- This method requests the user’s permission, if needed, for controlling the attached camera device.
--
-- ObjC selector: @- requestControlAuthorizationWithCompletion:@
requestControlAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
requestControlAuthorizationWithCompletion icDeviceBrowser completion =
  sendMessage icDeviceBrowser requestControlAuthorizationWithCompletionSelector completion

-- | resetContentsAuthorizationWithCompletion:
--
-- This method resets the authorization status for the application accessing the conents of an external media device.
--
-- ObjC selector: @- resetContentsAuthorizationWithCompletion:@
resetContentsAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
resetContentsAuthorizationWithCompletion icDeviceBrowser completion =
  sendMessage icDeviceBrowser resetContentsAuthorizationWithCompletionSelector completion

-- | resetControlAuthorizationWithCompletion:
--
-- This method resets the authorization status for the application controlling the attached camera device.
--
-- If the application already has been granted camera access, this will reset only the presentation dialog letting the user know the app has permission, not the camera access itself.
--
-- ObjC selector: @- resetControlAuthorizationWithCompletion:@
resetControlAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
resetControlAuthorizationWithCompletion icDeviceBrowser completion =
  sendMessage icDeviceBrowser resetControlAuthorizationWithCompletionSelector completion

-- | delegate
--
-- The delegate. It must conform to ICDeviceBrowserDelegate protocol. The messages this delegate can expect to receive are described by ICDeviceBrowserDelegate protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO RawId
delegate icDeviceBrowser =
  sendMessage icDeviceBrowser delegateSelector

-- | delegate
--
-- The delegate. It must conform to ICDeviceBrowserDelegate protocol. The messages this delegate can expect to receive are described by ICDeviceBrowserDelegate protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> RawId -> IO ()
setDelegate icDeviceBrowser value =
  sendMessage icDeviceBrowser setDelegateSelector value

-- | browsing
--
-- Indicates whether the device browser is browsing for devices.
--
-- ObjC selector: @- browsing@
browsing :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO Bool
browsing icDeviceBrowser =
  sendMessage icDeviceBrowser browsingSelector

-- | suspended
--
-- Indicates whether the  devices in the browser have suspended communication.
--
-- ObjC selector: @- suspended@
suspended :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO Bool
suspended icDeviceBrowser =
  sendMessage icDeviceBrowser suspendedSelector

-- | browsedDeviceTypeMask
--
-- A mask whose set bits indicate the type of device(s) being browsed after the receiver receives the start message. This property can be changed while the browser is browsing for devices. This property can be constructed by OR'd values of ICDeviceTypeMask with values of ICDeviceLocationTypeMask.
--
-- ObjC selector: @- browsedDeviceTypeMask@
browsedDeviceTypeMask :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO ICDeviceTypeMask
browsedDeviceTypeMask icDeviceBrowser =
  sendMessage icDeviceBrowser browsedDeviceTypeMaskSelector

-- | browsedDeviceTypeMask
--
-- A mask whose set bits indicate the type of device(s) being browsed after the receiver receives the start message. This property can be changed while the browser is browsing for devices. This property can be constructed by OR'd values of ICDeviceTypeMask with values of ICDeviceLocationTypeMask.
--
-- ObjC selector: @- setBrowsedDeviceTypeMask:@
setBrowsedDeviceTypeMask :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> ICDeviceTypeMask -> IO ()
setBrowsedDeviceTypeMask icDeviceBrowser value =
  sendMessage icDeviceBrowser setBrowsedDeviceTypeMaskSelector value

-- | devices
--
-- All devices found by the browser. This property will change as devices appear and disappear. This array is empty before the first invocation of the delegate method 'deviceBrowser:didAddDevice:moreComing:'.
--
-- ObjC selector: @- devices@
devices :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id NSArray)
devices icDeviceBrowser =
  sendMessage icDeviceBrowser devicesSelector

-- | preferredDevice
--
-- This property returns a device object that should be selected by the client application when it is launched.
--
-- If the client application that calls this method is the auto-launch application associated with a device and that device is the last device attached (through USB, FireWire or network), then that device will be the preferred device. The best place to call this method is in the implmentation of the ICDeviceBrowser delegate method "deviceBrowser:didAddDevice:moreComing:", if the "moreComing" parameter passed to the delegate is "NO"; or in the delegate method "deviceBrowserDidEnumerateLocalDevices:".
--
-- ObjC selector: @- preferredDevice@
preferredDevice :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO RawId
preferredDevice icDeviceBrowser =
  sendMessage icDeviceBrowser preferredDeviceSelector

-- | contentsAuthorizationStatus
--
-- This property returns a constant indicating whether the app has permission to acces the contents of an attached media device.
--
-- A constant indicating authorization status.
--
-- ObjC selector: @- contentsAuthorizationStatus@
contentsAuthorizationStatus :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id NSString)
contentsAuthorizationStatus icDeviceBrowser =
  sendMessage icDeviceBrowser contentsAuthorizationStatusSelector

-- | controlAuthorizationStatus
--
-- This property returns a constant indicating whether the app has permission to control the attached camera device.
--
-- ObjC selector: @- controlAuthorizationStatus@
controlAuthorizationStatus :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id NSString)
controlAuthorizationStatus icDeviceBrowser =
  sendMessage icDeviceBrowser controlAuthorizationStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ICDeviceBrowser)
initSelector = mkSelector "init"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @requestContentsAuthorizationWithCompletion:@
requestContentsAuthorizationWithCompletionSelector :: Selector '[Ptr ()] ()
requestContentsAuthorizationWithCompletionSelector = mkSelector "requestContentsAuthorizationWithCompletion:"

-- | @Selector@ for @requestControlAuthorizationWithCompletion:@
requestControlAuthorizationWithCompletionSelector :: Selector '[Ptr ()] ()
requestControlAuthorizationWithCompletionSelector = mkSelector "requestControlAuthorizationWithCompletion:"

-- | @Selector@ for @resetContentsAuthorizationWithCompletion:@
resetContentsAuthorizationWithCompletionSelector :: Selector '[Ptr ()] ()
resetContentsAuthorizationWithCompletionSelector = mkSelector "resetContentsAuthorizationWithCompletion:"

-- | @Selector@ for @resetControlAuthorizationWithCompletion:@
resetControlAuthorizationWithCompletionSelector :: Selector '[Ptr ()] ()
resetControlAuthorizationWithCompletionSelector = mkSelector "resetControlAuthorizationWithCompletion:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @browsing@
browsingSelector :: Selector '[] Bool
browsingSelector = mkSelector "browsing"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] Bool
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @browsedDeviceTypeMask@
browsedDeviceTypeMaskSelector :: Selector '[] ICDeviceTypeMask
browsedDeviceTypeMaskSelector = mkSelector "browsedDeviceTypeMask"

-- | @Selector@ for @setBrowsedDeviceTypeMask:@
setBrowsedDeviceTypeMaskSelector :: Selector '[ICDeviceTypeMask] ()
setBrowsedDeviceTypeMaskSelector = mkSelector "setBrowsedDeviceTypeMask:"

-- | @Selector@ for @devices@
devicesSelector :: Selector '[] (Id NSArray)
devicesSelector = mkSelector "devices"

-- | @Selector@ for @preferredDevice@
preferredDeviceSelector :: Selector '[] RawId
preferredDeviceSelector = mkSelector "preferredDevice"

-- | @Selector@ for @contentsAuthorizationStatus@
contentsAuthorizationStatusSelector :: Selector '[] (Id NSString)
contentsAuthorizationStatusSelector = mkSelector "contentsAuthorizationStatus"

-- | @Selector@ for @controlAuthorizationStatus@
controlAuthorizationStatusSelector :: Selector '[] (Id NSString)
controlAuthorizationStatusSelector = mkSelector "controlAuthorizationStatus"

