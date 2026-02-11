{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , startSelector
  , stopSelector
  , requestContentsAuthorizationWithCompletionSelector
  , requestControlAuthorizationWithCompletionSelector
  , resetContentsAuthorizationWithCompletionSelector
  , resetControlAuthorizationWithCompletionSelector
  , delegateSelector
  , setDelegateSelector
  , browsingSelector
  , suspendedSelector
  , browsedDeviceTypeMaskSelector
  , setBrowsedDeviceTypeMaskSelector
  , devicesSelector
  , preferredDeviceSelector
  , contentsAuthorizationStatusSelector
  , controlAuthorizationStatusSelector

  -- * Enum types
  , ICDeviceTypeMask(ICDeviceTypeMask)
  , pattern ICDeviceTypeMaskCamera
  , pattern ICDeviceTypeMaskScanner

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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.ImageCaptureCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | init
--
-- This is the designated initializer.
--
-- ObjC selector: @- init@
init_ :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id ICDeviceBrowser)
init_ icDeviceBrowser  =
    sendMsg icDeviceBrowser (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | start:
--
-- This message tells the receiver to start looking for devices.
--
-- Make sure that the receiver's delegate is set prior to sending this message; otherwise this message will be ignored. The messages the delegate can expect to receive are described by ICDeviceBrowserDelegate protocol.
--
-- ObjC selector: @- start@
start :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO ()
start icDeviceBrowser  =
    sendMsg icDeviceBrowser (mkSelector "start") retVoid []

-- | stop:
--
-- This method tells the receiver to stop looking for devices.
--
-- This will free all device instances that are not in use.
--
-- ObjC selector: @- stop@
stop :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO ()
stop icDeviceBrowser  =
    sendMsg icDeviceBrowser (mkSelector "stop") retVoid []

-- | requestContentsAuthorizationWithCompletion:
--
-- This method requests the user’s permission, if needed, for accessing the conents of an external media device.
--
-- ObjC selector: @- requestContentsAuthorizationWithCompletion:@
requestContentsAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
requestContentsAuthorizationWithCompletion icDeviceBrowser  completion =
    sendMsg icDeviceBrowser (mkSelector "requestContentsAuthorizationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | requestControlAuthorizationWithCompletion:
--
-- This method requests the user’s permission, if needed, for controlling the attached camera device.
--
-- ObjC selector: @- requestControlAuthorizationWithCompletion:@
requestControlAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
requestControlAuthorizationWithCompletion icDeviceBrowser  completion =
    sendMsg icDeviceBrowser (mkSelector "requestControlAuthorizationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | resetContentsAuthorizationWithCompletion:
--
-- This method resets the authorization status for the application accessing the conents of an external media device.
--
-- ObjC selector: @- resetContentsAuthorizationWithCompletion:@
resetContentsAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
resetContentsAuthorizationWithCompletion icDeviceBrowser  completion =
    sendMsg icDeviceBrowser (mkSelector "resetContentsAuthorizationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | resetControlAuthorizationWithCompletion:
--
-- This method resets the authorization status for the application controlling the attached camera device.
--
-- If the application already has been granted camera access, this will reset only the presentation dialog letting the user know the app has permission, not the camera access itself.
--
-- ObjC selector: @- resetControlAuthorizationWithCompletion:@
resetControlAuthorizationWithCompletion :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> Ptr () -> IO ()
resetControlAuthorizationWithCompletion icDeviceBrowser  completion =
    sendMsg icDeviceBrowser (mkSelector "resetControlAuthorizationWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | delegate
--
-- The delegate. It must conform to ICDeviceBrowserDelegate protocol. The messages this delegate can expect to receive are described by ICDeviceBrowserDelegate protocol.
--
-- ObjC selector: @- delegate@
delegate :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO RawId
delegate icDeviceBrowser  =
    fmap (RawId . castPtr) $ sendMsg icDeviceBrowser (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- The delegate. It must conform to ICDeviceBrowserDelegate protocol. The messages this delegate can expect to receive are described by ICDeviceBrowserDelegate protocol.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> RawId -> IO ()
setDelegate icDeviceBrowser  value =
    sendMsg icDeviceBrowser (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | browsing
--
-- Indicates whether the device browser is browsing for devices.
--
-- ObjC selector: @- browsing@
browsing :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO Bool
browsing icDeviceBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icDeviceBrowser (mkSelector "browsing") retCULong []

-- | suspended
--
-- Indicates whether the  devices in the browser have suspended communication.
--
-- ObjC selector: @- suspended@
suspended :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO Bool
suspended icDeviceBrowser  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg icDeviceBrowser (mkSelector "suspended") retCULong []

-- | browsedDeviceTypeMask
--
-- A mask whose set bits indicate the type of device(s) being browsed after the receiver receives the start message. This property can be changed while the browser is browsing for devices. This property can be constructed by OR'd values of ICDeviceTypeMask with values of ICDeviceLocationTypeMask.
--
-- ObjC selector: @- browsedDeviceTypeMask@
browsedDeviceTypeMask :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO ICDeviceTypeMask
browsedDeviceTypeMask icDeviceBrowser  =
    fmap (coerce :: CULong -> ICDeviceTypeMask) $ sendMsg icDeviceBrowser (mkSelector "browsedDeviceTypeMask") retCULong []

-- | browsedDeviceTypeMask
--
-- A mask whose set bits indicate the type of device(s) being browsed after the receiver receives the start message. This property can be changed while the browser is browsing for devices. This property can be constructed by OR'd values of ICDeviceTypeMask with values of ICDeviceLocationTypeMask.
--
-- ObjC selector: @- setBrowsedDeviceTypeMask:@
setBrowsedDeviceTypeMask :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> ICDeviceTypeMask -> IO ()
setBrowsedDeviceTypeMask icDeviceBrowser  value =
    sendMsg icDeviceBrowser (mkSelector "setBrowsedDeviceTypeMask:") retVoid [argCULong (coerce value)]

-- | devices
--
-- All devices found by the browser. This property will change as devices appear and disappear. This array is empty before the first invocation of the delegate method 'deviceBrowser:didAddDevice:moreComing:'.
--
-- ObjC selector: @- devices@
devices :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id NSArray)
devices icDeviceBrowser  =
    sendMsg icDeviceBrowser (mkSelector "devices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferredDevice
--
-- This property returns a device object that should be selected by the client application when it is launched.
--
-- If the client application that calls this method is the auto-launch application associated with a device and that device is the last device attached (through USB, FireWire or network), then that device will be the preferred device. The best place to call this method is in the implmentation of the ICDeviceBrowser delegate method "deviceBrowser:didAddDevice:moreComing:", if the "moreComing" parameter passed to the delegate is "NO"; or in the delegate method "deviceBrowserDidEnumerateLocalDevices:".
--
-- ObjC selector: @- preferredDevice@
preferredDevice :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO RawId
preferredDevice icDeviceBrowser  =
    fmap (RawId . castPtr) $ sendMsg icDeviceBrowser (mkSelector "preferredDevice") (retPtr retVoid) []

-- | contentsAuthorizationStatus
--
-- This property returns a constant indicating whether the app has permission to acces the contents of an attached media device.
--
-- A constant indicating authorization status.
--
-- ObjC selector: @- contentsAuthorizationStatus@
contentsAuthorizationStatus :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id NSString)
contentsAuthorizationStatus icDeviceBrowser  =
    sendMsg icDeviceBrowser (mkSelector "contentsAuthorizationStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | controlAuthorizationStatus
--
-- This property returns a constant indicating whether the app has permission to control the attached camera device.
--
-- ObjC selector: @- controlAuthorizationStatus@
controlAuthorizationStatus :: IsICDeviceBrowser icDeviceBrowser => icDeviceBrowser -> IO (Id NSString)
controlAuthorizationStatus icDeviceBrowser  =
    sendMsg icDeviceBrowser (mkSelector "controlAuthorizationStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @requestContentsAuthorizationWithCompletion:@
requestContentsAuthorizationWithCompletionSelector :: Selector
requestContentsAuthorizationWithCompletionSelector = mkSelector "requestContentsAuthorizationWithCompletion:"

-- | @Selector@ for @requestControlAuthorizationWithCompletion:@
requestControlAuthorizationWithCompletionSelector :: Selector
requestControlAuthorizationWithCompletionSelector = mkSelector "requestControlAuthorizationWithCompletion:"

-- | @Selector@ for @resetContentsAuthorizationWithCompletion:@
resetContentsAuthorizationWithCompletionSelector :: Selector
resetContentsAuthorizationWithCompletionSelector = mkSelector "resetContentsAuthorizationWithCompletion:"

-- | @Selector@ for @resetControlAuthorizationWithCompletion:@
resetControlAuthorizationWithCompletionSelector :: Selector
resetControlAuthorizationWithCompletionSelector = mkSelector "resetControlAuthorizationWithCompletion:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @browsing@
browsingSelector :: Selector
browsingSelector = mkSelector "browsing"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @browsedDeviceTypeMask@
browsedDeviceTypeMaskSelector :: Selector
browsedDeviceTypeMaskSelector = mkSelector "browsedDeviceTypeMask"

-- | @Selector@ for @setBrowsedDeviceTypeMask:@
setBrowsedDeviceTypeMaskSelector :: Selector
setBrowsedDeviceTypeMaskSelector = mkSelector "setBrowsedDeviceTypeMask:"

-- | @Selector@ for @devices@
devicesSelector :: Selector
devicesSelector = mkSelector "devices"

-- | @Selector@ for @preferredDevice@
preferredDeviceSelector :: Selector
preferredDeviceSelector = mkSelector "preferredDevice"

-- | @Selector@ for @contentsAuthorizationStatus@
contentsAuthorizationStatusSelector :: Selector
contentsAuthorizationStatusSelector = mkSelector "contentsAuthorizationStatus"

-- | @Selector@ for @controlAuthorizationStatus@
controlAuthorizationStatusSelector :: Selector
controlAuthorizationStatusSelector = mkSelector "controlAuthorizationStatus"

