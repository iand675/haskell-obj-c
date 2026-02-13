{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothSDPUUID
--
-- An NSData subclass that represents a UUID as defined in the Bluetooth SDP spec.
--
-- The IOBluetoothSDPUUID class can represent a UUID of any valid size (16, 32 or 128 bits).            It provides the ability to compare two UUIDs no matter what their size as well as the ability            to promote the size of a UUID to a larger one.
--
-- Generated bindings for @IOBluetoothSDPUUID@.
module ObjC.IOBluetooth.IOBluetoothSDPUUID
  ( IOBluetoothSDPUUID
  , IsIOBluetoothSDPUUID(..)
  , uuidWithBytes_length
  , uuidWithData
  , uuid16
  , uuid32
  , withSDPUUIDRef
  , initWithUUID16
  , initWithUUID32
  , getSDPUUIDRef
  , getUUIDWithLength
  , isEqualToUUID
  , classForCoder
  , classForArchiver
  , classForPortCoder
  , classForArchiverSelector
  , classForCoderSelector
  , classForPortCoderSelector
  , getSDPUUIDRefSelector
  , getUUIDWithLengthSelector
  , initWithUUID16Selector
  , initWithUUID32Selector
  , isEqualToUUIDSelector
  , uuid16Selector
  , uuid32Selector
  , uuidWithBytes_lengthSelector
  , uuidWithDataSelector
  , withSDPUUIDRefSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | uuidWithBytes:length:
--
-- Creates a new IOBluetoothSDPUUID object with the given bytes of the given length.
--
-- If the length is invalid for a UUID, nil is returned.
--
-- @bytes@ — An array of bytes representing the UUID.
--
-- @length@ — The length of the array of bytes.
--
-- Returns: Returns the new IOBluetoothSDPUUID object or nil on failure.
--
-- ObjC selector: @+ uuidWithBytes:length:@
uuidWithBytes_length :: Const (Ptr ()) -> CUInt -> IO (Id IOBluetoothSDPUUID)
uuidWithBytes_length bytes length_ =
  do
    cls' <- getRequiredClass "IOBluetoothSDPUUID"
    sendClassMessage cls' uuidWithBytes_lengthSelector bytes length_

-- | uuidWithData:
--
-- Creates a new IOBluetoothSDPUUID object from the given NSData.
--
-- If the length of the NSData is invalid for a UUID, nil is returned.
--
-- @data@ — The NSData containing the UUID bytes.
--
-- Returns: Returns the new IOBluetoothSDPUUID object or nil on failure.
--
-- ObjC selector: @+ uuidWithData:@
uuidWithData :: IsNSData data_ => data_ -> IO (Id IOBluetoothSDPUUID)
uuidWithData data_ =
  do
    cls' <- getRequiredClass "IOBluetoothSDPUUID"
    sendClassMessage cls' uuidWithDataSelector (toNSData data_)

-- | uuid16:
--
-- Creates a new 16-bit IOBluetoothSDPUUID with the given UUID16
--
-- @uuid16@ — A scalar representing a 16-bit UUID
--
-- Returns: Returns the new IOBluetoothSDPUUID object.
--
-- ObjC selector: @+ uuid16:@
uuid16 :: CUShort -> IO (Id IOBluetoothSDPUUID)
uuid16 uuid16 =
  do
    cls' <- getRequiredClass "IOBluetoothSDPUUID"
    sendClassMessage cls' uuid16Selector uuid16

-- | uuid32:
--
-- Creates a new 32-bit IOBluetoothSDPUUID with the given UUID32
--
-- @uuid32@ — A scalar representing a 32-bit UUID
--
-- Returns: Returns the new IOBluetoothSDPUUID object.
--
-- ObjC selector: @+ uuid32:@
uuid32 :: CUInt -> IO (Id IOBluetoothSDPUUID)
uuid32 uuid32 =
  do
    cls' <- getRequiredClass "IOBluetoothSDPUUID"
    sendClassMessage cls' uuid32Selector uuid32

-- | withSDPUUIDRef:
--
-- Method call to convert an IOBluetoothSDPUUIDRef into an IOBluetoothSDPUUID *.
--
-- @sdpUUIDRef@ — IOBluetoothSDPUUIDRef for which an IOBluetoothSDPUUID * is desired.
--
-- Returns: Returns the IOBluetoothSDPUUID * for the given IOBluetoothSDPUUIDRef.
--
-- ObjC selector: @+ withSDPUUIDRef:@
withSDPUUIDRef :: Ptr () -> IO (Id IOBluetoothSDPUUID)
withSDPUUIDRef sdpUUIDRef =
  do
    cls' <- getRequiredClass "IOBluetoothSDPUUID"
    sendClassMessage cls' withSDPUUIDRefSelector sdpUUIDRef

-- | initWithUUID16:
--
-- Initializes a new 16-bit IOBluetoothSDPUUID with the given UUID16
--
-- @uuid16@ — A scalar representing a 16-bit UUID
--
-- Returns: Returns self.
--
-- ObjC selector: @- initWithUUID16:@
initWithUUID16 :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> CUShort -> IO (Id IOBluetoothSDPUUID)
initWithUUID16 ioBluetoothSDPUUID uuid16 =
  sendOwnedMessage ioBluetoothSDPUUID initWithUUID16Selector uuid16

-- | initWithUUID32:
--
-- Creates a new 32-bit IOBluetoothSDPUUID with the given UUID32
--
-- @uuid32@ — A scalar representing a 32-bit UUID
--
-- Returns: Returns self.
--
-- ObjC selector: @- initWithUUID32:@
initWithUUID32 :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> CUInt -> IO (Id IOBluetoothSDPUUID)
initWithUUID32 ioBluetoothSDPUUID uuid32 =
  sendOwnedMessage ioBluetoothSDPUUID initWithUUID32Selector uuid32

-- | getSDPUUIDRef
--
-- Returns an IOBluetoothSDPUUIDRef representation of the target IOBluetoothSDPUUID object.
--
-- Returns: Returns an IOBluetoothSDPUUIDRef representation of the target IOBluetoothSDPUUID object.
--
-- ObjC selector: @- getSDPUUIDRef@
getSDPUUIDRef :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO (Ptr ())
getSDPUUIDRef ioBluetoothSDPUUID =
  sendMessage ioBluetoothSDPUUID getSDPUUIDRefSelector

-- | getUUIDWithLength:
--
-- Returns an IOBluetoothSDPUUID object matching the target UUID, but with the given number of bytes.
--
-- If the target object is the same length as newLength, it returns self.  If newLength is greater                it creates a new IOBluetoothSDPUUID object with the correct value for the given length.  If                newLength is smaller, it will attempt to create a new IOBluetoothSDPUUID that is smaller if                the data matches the Bluetooth UUID base.  This downconversion is currently unimplemented.
--
-- @newLength@ — The desired length for the UUID.
--
-- Returns: Returns an IOBluetoothSDPUUID object with the same data as the target but with the given length if it            is possible to do so.  Otherwise, nil is returned.
--
-- ObjC selector: @- getUUIDWithLength:@
getUUIDWithLength :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> CUInt -> IO (Id IOBluetoothSDPUUID)
getUUIDWithLength ioBluetoothSDPUUID newLength =
  sendMessage ioBluetoothSDPUUID getUUIDWithLengthSelector newLength

-- | isEqualToUUID:
--
-- Compares the target IOBluetoothSDPUUID object with the given otherUUID object.
--
-- This method will compare the two UUID values independent of their length.
--
-- @otherUUID@ — The UUID object to be compared with the target.
--
-- Returns: Returns true if the UUID values of each object are equal.  This includes the case where the sizes are different            but the data itself is the same when the Bluetooth UUID base is applied.
--
-- ObjC selector: @- isEqualToUUID:@
isEqualToUUID :: (IsIOBluetoothSDPUUID ioBluetoothSDPUUID, IsIOBluetoothSDPUUID otherUUID) => ioBluetoothSDPUUID -> otherUUID -> IO Bool
isEqualToUUID ioBluetoothSDPUUID otherUUID =
  sendMessage ioBluetoothSDPUUID isEqualToUUIDSelector (toIOBluetoothSDPUUID otherUUID)

-- | @- classForCoder@
classForCoder :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO Class
classForCoder ioBluetoothSDPUUID =
  sendMessage ioBluetoothSDPUUID classForCoderSelector

-- | @- classForArchiver@
classForArchiver :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO Class
classForArchiver ioBluetoothSDPUUID =
  sendMessage ioBluetoothSDPUUID classForArchiverSelector

-- | @- classForPortCoder@
classForPortCoder :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO Class
classForPortCoder ioBluetoothSDPUUID =
  sendMessage ioBluetoothSDPUUID classForPortCoderSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uuidWithBytes:length:@
uuidWithBytes_lengthSelector :: Selector '[Const (Ptr ()), CUInt] (Id IOBluetoothSDPUUID)
uuidWithBytes_lengthSelector = mkSelector "uuidWithBytes:length:"

-- | @Selector@ for @uuidWithData:@
uuidWithDataSelector :: Selector '[Id NSData] (Id IOBluetoothSDPUUID)
uuidWithDataSelector = mkSelector "uuidWithData:"

-- | @Selector@ for @uuid16:@
uuid16Selector :: Selector '[CUShort] (Id IOBluetoothSDPUUID)
uuid16Selector = mkSelector "uuid16:"

-- | @Selector@ for @uuid32:@
uuid32Selector :: Selector '[CUInt] (Id IOBluetoothSDPUUID)
uuid32Selector = mkSelector "uuid32:"

-- | @Selector@ for @withSDPUUIDRef:@
withSDPUUIDRefSelector :: Selector '[Ptr ()] (Id IOBluetoothSDPUUID)
withSDPUUIDRefSelector = mkSelector "withSDPUUIDRef:"

-- | @Selector@ for @initWithUUID16:@
initWithUUID16Selector :: Selector '[CUShort] (Id IOBluetoothSDPUUID)
initWithUUID16Selector = mkSelector "initWithUUID16:"

-- | @Selector@ for @initWithUUID32:@
initWithUUID32Selector :: Selector '[CUInt] (Id IOBluetoothSDPUUID)
initWithUUID32Selector = mkSelector "initWithUUID32:"

-- | @Selector@ for @getSDPUUIDRef@
getSDPUUIDRefSelector :: Selector '[] (Ptr ())
getSDPUUIDRefSelector = mkSelector "getSDPUUIDRef"

-- | @Selector@ for @getUUIDWithLength:@
getUUIDWithLengthSelector :: Selector '[CUInt] (Id IOBluetoothSDPUUID)
getUUIDWithLengthSelector = mkSelector "getUUIDWithLength:"

-- | @Selector@ for @isEqualToUUID:@
isEqualToUUIDSelector :: Selector '[Id IOBluetoothSDPUUID] Bool
isEqualToUUIDSelector = mkSelector "isEqualToUUID:"

-- | @Selector@ for @classForCoder@
classForCoderSelector :: Selector '[] Class
classForCoderSelector = mkSelector "classForCoder"

-- | @Selector@ for @classForArchiver@
classForArchiverSelector :: Selector '[] Class
classForArchiverSelector = mkSelector "classForArchiver"

-- | @Selector@ for @classForPortCoder@
classForPortCoderSelector :: Selector '[] Class
classForPortCoderSelector = mkSelector "classForPortCoder"

