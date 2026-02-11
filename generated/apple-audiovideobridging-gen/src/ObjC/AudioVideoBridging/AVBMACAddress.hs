{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVBMACAddress
--
-- AVBMACAddress is a class for holding and representing an Ethernet MAC Address.
--
-- Generated bindings for @AVBMACAddress@.
module ObjC.AudioVideoBridging.AVBMACAddress
  ( AVBMACAddress
  , IsAVBMACAddress(..)
  , initWithBytes
  , bytes
  , dataRepresentation
  , setDataRepresentation
  , stringRepresentation
  , setStringRepresentation
  , multicast
  , setMulticast
  , initWithBytesSelector
  , bytesSelector
  , dataRepresentationSelector
  , setDataRepresentationSelector
  , stringRepresentationSelector
  , setStringRepresentationSelector
  , multicastSelector
  , setMulticastSelector


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

import ObjC.AudioVideoBridging.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithBytes:
--
-- This method initializes the receiver to contain the MAC address specified.
--
-- @bytes@ — A pointer to 6 octets of memory containing the MAC address.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithBytes:@
initWithBytes :: IsAVBMACAddress avbmacAddress => avbmacAddress -> Const (Ptr CUChar) -> IO (Id AVBMACAddress)
initWithBytes avbmacAddress  bytes =
    sendMsg avbmacAddress (mkSelector "initWithBytes:") (retPtr retVoid) [argPtr (unConst bytes)] >>= ownedObject . castPtr

-- | bytes
--
-- The .
--
-- ObjC selector: @- bytes@
bytes :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO RawId
bytes avbmacAddress  =
    fmap (RawId . castPtr) $ sendMsg avbmacAddress (mkSelector "bytes") (retPtr retVoid) []

-- | dataRepresentation
--
-- An NSData object containing the bytes of the MAC address.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO (Id NSData)
dataRepresentation avbmacAddress  =
    sendMsg avbmacAddress (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dataRepresentation
--
-- An NSData object containing the bytes of the MAC address.
--
-- ObjC selector: @- setDataRepresentation:@
setDataRepresentation :: (IsAVBMACAddress avbmacAddress, IsNSData value) => avbmacAddress -> value -> IO ()
setDataRepresentation avbmacAddress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avbmacAddress (mkSelector "setDataRepresentation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | stringRepresentation
--
-- The colon separated cannonical string representation of the MAC address e.g. 12:34:56:78:ab:cd
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO (Id NSString)
stringRepresentation avbmacAddress  =
    sendMsg avbmacAddress (mkSelector "stringRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | stringRepresentation
--
-- The colon separated cannonical string representation of the MAC address e.g. 12:34:56:78:ab:cd
--
-- ObjC selector: @- setStringRepresentation:@
setStringRepresentation :: (IsAVBMACAddress avbmacAddress, IsNSString value) => avbmacAddress -> value -> IO ()
setStringRepresentation avbmacAddress  value =
  withObjCPtr value $ \raw_value ->
      sendMsg avbmacAddress (mkSelector "setStringRepresentation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | multicast
--
-- Returns if the multicast bit is set in the MAC address.
--
-- ObjC selector: @- multicast@
multicast :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO Bool
multicast avbmacAddress  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avbmacAddress (mkSelector "multicast") retCULong []

-- | multicast
--
-- Returns if the multicast bit is set in the MAC address.
--
-- ObjC selector: @- setMulticast:@
setMulticast :: IsAVBMACAddress avbmacAddress => avbmacAddress -> Bool -> IO ()
setMulticast avbmacAddress  value =
    sendMsg avbmacAddress (mkSelector "setMulticast:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBytes:@
initWithBytesSelector :: Selector
initWithBytesSelector = mkSelector "initWithBytes:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @setDataRepresentation:@
setDataRepresentationSelector :: Selector
setDataRepresentationSelector = mkSelector "setDataRepresentation:"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector
stringRepresentationSelector = mkSelector "stringRepresentation"

-- | @Selector@ for @setStringRepresentation:@
setStringRepresentationSelector :: Selector
setStringRepresentationSelector = mkSelector "setStringRepresentation:"

-- | @Selector@ for @multicast@
multicastSelector :: Selector
multicastSelector = mkSelector "multicast"

-- | @Selector@ for @setMulticast:@
setMulticastSelector :: Selector
setMulticastSelector = mkSelector "setMulticast:"

