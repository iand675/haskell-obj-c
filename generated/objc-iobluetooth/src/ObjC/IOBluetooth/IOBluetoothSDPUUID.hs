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
  , uuidWithBytes_lengthSelector
  , uuidWithDataSelector
  , uuid16Selector
  , uuid32Selector
  , withSDPUUIDRefSelector
  , initWithUUID16Selector
  , initWithUUID32Selector
  , getSDPUUIDRefSelector
  , getUUIDWithLengthSelector
  , isEqualToUUIDSelector
  , classForCoderSelector
  , classForArchiverSelector
  , classForPortCoderSelector


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
    sendClassMsg cls' (mkSelector "uuidWithBytes:length:") (retPtr retVoid) [argPtr (unConst bytes), argCUInt (fromIntegral length_)] >>= retainedObject . castPtr

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
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "uuidWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "uuid16:") (retPtr retVoid) [argCUInt (fromIntegral uuid16)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "uuid32:") (retPtr retVoid) [argCUInt (fromIntegral uuid32)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "withSDPUUIDRef:") (retPtr retVoid) [argPtr sdpUUIDRef] >>= retainedObject . castPtr

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
initWithUUID16 ioBluetoothSDPUUID  uuid16 =
  sendMsg ioBluetoothSDPUUID (mkSelector "initWithUUID16:") (retPtr retVoid) [argCUInt (fromIntegral uuid16)] >>= ownedObject . castPtr

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
initWithUUID32 ioBluetoothSDPUUID  uuid32 =
  sendMsg ioBluetoothSDPUUID (mkSelector "initWithUUID32:") (retPtr retVoid) [argCUInt (fromIntegral uuid32)] >>= ownedObject . castPtr

-- | getSDPUUIDRef
--
-- Returns an IOBluetoothSDPUUIDRef representation of the target IOBluetoothSDPUUID object.
--
-- Returns: Returns an IOBluetoothSDPUUIDRef representation of the target IOBluetoothSDPUUID object.
--
-- ObjC selector: @- getSDPUUIDRef@
getSDPUUIDRef :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO (Ptr ())
getSDPUUIDRef ioBluetoothSDPUUID  =
  fmap castPtr $ sendMsg ioBluetoothSDPUUID (mkSelector "getSDPUUIDRef") (retPtr retVoid) []

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
getUUIDWithLength ioBluetoothSDPUUID  newLength =
  sendMsg ioBluetoothSDPUUID (mkSelector "getUUIDWithLength:") (retPtr retVoid) [argCUInt (fromIntegral newLength)] >>= retainedObject . castPtr

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
isEqualToUUID ioBluetoothSDPUUID  otherUUID =
withObjCPtr otherUUID $ \raw_otherUUID ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ioBluetoothSDPUUID (mkSelector "isEqualToUUID:") retCULong [argPtr (castPtr raw_otherUUID :: Ptr ())]

-- | @- classForCoder@
classForCoder :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO Class
classForCoder ioBluetoothSDPUUID  =
  fmap (Class . castPtr) $ sendMsg ioBluetoothSDPUUID (mkSelector "classForCoder") (retPtr retVoid) []

-- | @- classForArchiver@
classForArchiver :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO Class
classForArchiver ioBluetoothSDPUUID  =
  fmap (Class . castPtr) $ sendMsg ioBluetoothSDPUUID (mkSelector "classForArchiver") (retPtr retVoid) []

-- | @- classForPortCoder@
classForPortCoder :: IsIOBluetoothSDPUUID ioBluetoothSDPUUID => ioBluetoothSDPUUID -> IO Class
classForPortCoder ioBluetoothSDPUUID  =
  fmap (Class . castPtr) $ sendMsg ioBluetoothSDPUUID (mkSelector "classForPortCoder") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uuidWithBytes:length:@
uuidWithBytes_lengthSelector :: Selector
uuidWithBytes_lengthSelector = mkSelector "uuidWithBytes:length:"

-- | @Selector@ for @uuidWithData:@
uuidWithDataSelector :: Selector
uuidWithDataSelector = mkSelector "uuidWithData:"

-- | @Selector@ for @uuid16:@
uuid16Selector :: Selector
uuid16Selector = mkSelector "uuid16:"

-- | @Selector@ for @uuid32:@
uuid32Selector :: Selector
uuid32Selector = mkSelector "uuid32:"

-- | @Selector@ for @withSDPUUIDRef:@
withSDPUUIDRefSelector :: Selector
withSDPUUIDRefSelector = mkSelector "withSDPUUIDRef:"

-- | @Selector@ for @initWithUUID16:@
initWithUUID16Selector :: Selector
initWithUUID16Selector = mkSelector "initWithUUID16:"

-- | @Selector@ for @initWithUUID32:@
initWithUUID32Selector :: Selector
initWithUUID32Selector = mkSelector "initWithUUID32:"

-- | @Selector@ for @getSDPUUIDRef@
getSDPUUIDRefSelector :: Selector
getSDPUUIDRefSelector = mkSelector "getSDPUUIDRef"

-- | @Selector@ for @getUUIDWithLength:@
getUUIDWithLengthSelector :: Selector
getUUIDWithLengthSelector = mkSelector "getUUIDWithLength:"

-- | @Selector@ for @isEqualToUUID:@
isEqualToUUIDSelector :: Selector
isEqualToUUIDSelector = mkSelector "isEqualToUUID:"

-- | @Selector@ for @classForCoder@
classForCoderSelector :: Selector
classForCoderSelector = mkSelector "classForCoder"

-- | @Selector@ for @classForArchiver@
classForArchiverSelector :: Selector
classForArchiverSelector = mkSelector "classForArchiver"

-- | @Selector@ for @classForPortCoder@
classForPortCoderSelector :: Selector
classForPortCoderSelector = mkSelector "classForPortCoder"

