{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothHostController
--
-- This class is a representation of a Bluetooth Host Controller Interface that is present on the					local computer (either plugged in externally or available internally).
--
-- This object can be used to ask a Bluetooth HCI for certain pieces of information, and be used to make					it perform certain functions.
--
-- Generated bindings for @IOBluetoothHostController@.
module ObjC.IOBluetooth.IOBluetoothHostController
  ( IOBluetoothHostController
  , IsIOBluetoothHostController(..)
  , defaultController
  , classOfDevice
  , setClassOfDevice_forTimeInterval
  , addressAsString
  , nameAsString
  , delegate
  , setDelegate
  , addressAsStringSelector
  , classOfDeviceSelector
  , defaultControllerSelector
  , delegateSelector
  , nameAsStringSelector
  , setClassOfDevice_forTimeIntervalSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | defaultController
--
-- Gets the default HCI controller object.
--
-- Returns: A (autoreleased) pointer to the created IOBluetoothHostController object.
--
-- ObjC selector: @+ defaultController@
defaultController :: IO (Id IOBluetoothHostController)
defaultController  =
  do
    cls' <- getRequiredClass "IOBluetoothHostController"
    sendClassMessage cls' defaultControllerSelector

-- | classOfDevice
--
-- Gets the current class of device value.
--
-- Returns: Returns the current class of device value.
--
-- ObjC selector: @- classOfDevice@
classOfDevice :: IsIOBluetoothHostController ioBluetoothHostController => ioBluetoothHostController -> IO CUInt
classOfDevice ioBluetoothHostController =
  sendMessage ioBluetoothHostController classOfDeviceSelector

-- | setClassOfDevice:forTimeInterval:
--
-- Sets the current class of device value, for the specified amount of time. Note that the time interval *must*				be set and valid. The range of acceptable values is 30-120 seconds. Anything above or below will be rounded				up, or down, as appropriate.
--
-- Returns: Returns the whether setting the class of device value was successful. 0 if success, error code otherwise.
--
-- ObjC selector: @- setClassOfDevice:forTimeInterval:@
setClassOfDevice_forTimeInterval :: IsIOBluetoothHostController ioBluetoothHostController => ioBluetoothHostController -> CUInt -> CDouble -> IO CInt
setClassOfDevice_forTimeInterval ioBluetoothHostController classOfDevice seconds =
  sendMessage ioBluetoothHostController setClassOfDevice_forTimeIntervalSelector classOfDevice seconds

-- | addressAsString
--
-- Convience routine to get the HCI controller's Bluetooth address as an NSString object.
--
-- Returns: Returns NSString *. nil if the address could not be retrieved.
--
-- ObjC selector: @- addressAsString@
addressAsString :: IsIOBluetoothHostController ioBluetoothHostController => ioBluetoothHostController -> IO (Id NSString)
addressAsString ioBluetoothHostController =
  sendMessage ioBluetoothHostController addressAsStringSelector

-- | nameAsString
--
-- Gets the "friendly" name of HCI controller.
--
-- Returns: Returns NSString with the device name, nil if there is not one or it cannot be read.
--
-- ObjC selector: @- nameAsString@
nameAsString :: IsIOBluetoothHostController ioBluetoothHostController => ioBluetoothHostController -> IO (Id NSString)
nameAsString ioBluetoothHostController =
  sendMessage ioBluetoothHostController nameAsStringSelector

-- | @- delegate@
delegate :: IsIOBluetoothHostController ioBluetoothHostController => ioBluetoothHostController -> IO RawId
delegate ioBluetoothHostController =
  sendMessage ioBluetoothHostController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsIOBluetoothHostController ioBluetoothHostController => ioBluetoothHostController -> RawId -> IO ()
setDelegate ioBluetoothHostController value =
  sendMessage ioBluetoothHostController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultController@
defaultControllerSelector :: Selector '[] (Id IOBluetoothHostController)
defaultControllerSelector = mkSelector "defaultController"

-- | @Selector@ for @classOfDevice@
classOfDeviceSelector :: Selector '[] CUInt
classOfDeviceSelector = mkSelector "classOfDevice"

-- | @Selector@ for @setClassOfDevice:forTimeInterval:@
setClassOfDevice_forTimeIntervalSelector :: Selector '[CUInt, CDouble] CInt
setClassOfDevice_forTimeIntervalSelector = mkSelector "setClassOfDevice:forTimeInterval:"

-- | @Selector@ for @addressAsString@
addressAsStringSelector :: Selector '[] (Id NSString)
addressAsStringSelector = mkSelector "addressAsString"

-- | @Selector@ for @nameAsString@
nameAsStringSelector :: Selector '[] (Id NSString)
nameAsStringSelector = mkSelector "nameAsString"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

