{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothDevicePair
--
-- An instance of IOBluetoothDevicePair represents a pairing attempt to a remote Bluetooth device.
--
-- Use the IOBluetoothDevicePair object to attempt to pair with any Bluetooth device. Once -start is invoked				on it, progress is returned to the delegate via the messages defined below. This object enables you to				pair with devices within your application without having to use the standard panels provided by the				IOBluetoothUI framework, allowing you to write custom UI to select devices, and still handle the ability				to perform device pairings.
--
-- Of note is that this object MAY attempt to perform two low-level pairings, depending on the type of device				you are attempting to pair. This is inconsequential to your code, however, as it occurs automatically and				does not change the messaging.
--
-- Once started, the pairing can be stopped. This will set the delegate to nil and then attempt to disconnect 				from the device if already connected.
--
-- Generated bindings for @IOBluetoothDevicePair@.
module ObjC.IOBluetooth.IOBluetoothDevicePair
  ( IOBluetoothDevicePair
  , IsIOBluetoothDevicePair(..)
  , pairWithDevice
  , start
  , stop
  , device
  , setDevice
  , replyPINCode_PINCode
  , replyUserConfirmation
  , delegate
  , setDelegate
  , delegateSelector
  , deviceSelector
  , pairWithDeviceSelector
  , replyPINCode_PINCodeSelector
  , replyUserConfirmationSelector
  , setDelegateSelector
  , setDeviceSelector
  , startSelector
  , stopSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | pairWithDevice:
--
-- Creates an autorelease IOBluetoothDevicePair object with a device as the pairing target.
--
-- @device@ — An IOBluetoothDevice to attept to pair with. The device is retained.
--
-- Returns: Returns an IOReturn or Bluetooth error code, if the pairing could not be started.
--
-- ObjC selector: @+ pairWithDevice:@
pairWithDevice :: IsIOBluetoothDevice device => device -> IO (Id IOBluetoothDevicePair)
pairWithDevice device =
  do
    cls' <- getRequiredClass "IOBluetoothDevicePair"
    sendClassMessage cls' pairWithDeviceSelector (toIOBluetoothDevice device)

-- | start
--
-- Kicks off the pairing with the device.
--
-- Returns: Returns an IOReturn or Bluetooth error code, if the pairing could not be started.
--
-- ObjC selector: @- start@
start :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> IO CInt
start ioBluetoothDevicePair =
  sendMessage ioBluetoothDevicePair startSelector

-- | stop
--
-- Stops the current pairing. Removes the delegate and disconnects if device was connected.
--
-- ObjC selector: @- stop@
stop :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> IO ()
stop ioBluetoothDevicePair =
  sendMessage ioBluetoothDevicePair stopSelector

-- | device
--
-- Get the IOBluetoothDevice being used by the object.
--
-- Returns: device		The IOBluetoothDevice object that the IOBluetoothDevicePair object is pairing with, as							specified in -setDevice: or pairWithDevice:
--
-- ObjC selector: @- device@
device :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> IO (Id IOBluetoothDevice)
device ioBluetoothDevicePair =
  sendMessage ioBluetoothDevicePair deviceSelector

-- | setDevice:
--
-- Set the device object to pair with. It is retained by the object.
--
-- @device@ — The IOBluetoothDevice object that the IOBluetoothDevicePair object with which to perform a pairing.
--
-- ObjC selector: @- setDevice:@
setDevice :: (IsIOBluetoothDevicePair ioBluetoothDevicePair, IsIOBluetoothDevice inDevice) => ioBluetoothDevicePair -> inDevice -> IO ()
setDevice ioBluetoothDevicePair inDevice =
  sendMessage ioBluetoothDevicePair setDeviceSelector (toIOBluetoothDevice inDevice)

-- | replyPINCode:
--
-- This is the required reply to the devicePairingPINCodeRequest delegate message.				Set the PIN code to use during pairing if required.
--
-- @PINCodeSize@ — The PIN code length in octets (8 bits).
--
-- @PINcode@ — PIN code for the device.  Can be up to a maximum of 128 bits.
--
-- ObjC selector: @- replyPINCode:PINCode:@
replyPINCode_PINCode :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> CULong -> RawId -> IO ()
replyPINCode_PINCode ioBluetoothDevicePair pinCodeSize pinCode =
  sendMessage ioBluetoothDevicePair replyPINCode_PINCodeSelector pinCodeSize pinCode

-- | replyUserConfirmation:
--
-- This is the required reply to the devicePairingUserConfirmationRequest delegate message.
--
-- @reply@ — A yes/no answer provide by the user to the numeric comparison presented.
--
-- ObjC selector: @- replyUserConfirmation:@
replyUserConfirmation :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> Bool -> IO ()
replyUserConfirmation ioBluetoothDevicePair reply =
  sendMessage ioBluetoothDevicePair replyUserConfirmationSelector reply

-- | @- delegate@
delegate :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> IO RawId
delegate ioBluetoothDevicePair =
  sendMessage ioBluetoothDevicePair delegateSelector

-- | @- setDelegate:@
setDelegate :: IsIOBluetoothDevicePair ioBluetoothDevicePair => ioBluetoothDevicePair -> RawId -> IO ()
setDelegate ioBluetoothDevicePair value =
  sendMessage ioBluetoothDevicePair setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pairWithDevice:@
pairWithDeviceSelector :: Selector '[Id IOBluetoothDevice] (Id IOBluetoothDevicePair)
pairWithDeviceSelector = mkSelector "pairWithDevice:"

-- | @Selector@ for @start@
startSelector :: Selector '[] CInt
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id IOBluetoothDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector '[Id IOBluetoothDevice] ()
setDeviceSelector = mkSelector "setDevice:"

-- | @Selector@ for @replyPINCode:PINCode:@
replyPINCode_PINCodeSelector :: Selector '[CULong, RawId] ()
replyPINCode_PINCodeSelector = mkSelector "replyPINCode:PINCode:"

-- | @Selector@ for @replyUserConfirmation:@
replyUserConfirmationSelector :: Selector '[Bool] ()
replyUserConfirmationSelector = mkSelector "replyUserConfirmation:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

