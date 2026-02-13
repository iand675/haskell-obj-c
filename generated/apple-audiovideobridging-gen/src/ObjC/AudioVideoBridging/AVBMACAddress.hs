{-# LANGUAGE DataKinds #-}
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
  , bytesSelector
  , dataRepresentationSelector
  , initWithBytesSelector
  , multicastSelector
  , setDataRepresentationSelector
  , setMulticastSelector
  , setStringRepresentationSelector
  , stringRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithBytes avbmacAddress bytes =
  sendOwnedMessage avbmacAddress initWithBytesSelector bytes

-- | bytes
--
-- The .
--
-- ObjC selector: @- bytes@
bytes :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO RawId
bytes avbmacAddress =
  sendMessage avbmacAddress bytesSelector

-- | dataRepresentation
--
-- An NSData object containing the bytes of the MAC address.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO (Id NSData)
dataRepresentation avbmacAddress =
  sendMessage avbmacAddress dataRepresentationSelector

-- | dataRepresentation
--
-- An NSData object containing the bytes of the MAC address.
--
-- ObjC selector: @- setDataRepresentation:@
setDataRepresentation :: (IsAVBMACAddress avbmacAddress, IsNSData value) => avbmacAddress -> value -> IO ()
setDataRepresentation avbmacAddress value =
  sendMessage avbmacAddress setDataRepresentationSelector (toNSData value)

-- | stringRepresentation
--
-- The colon separated cannonical string representation of the MAC address e.g. 12:34:56:78:ab:cd
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO (Id NSString)
stringRepresentation avbmacAddress =
  sendMessage avbmacAddress stringRepresentationSelector

-- | stringRepresentation
--
-- The colon separated cannonical string representation of the MAC address e.g. 12:34:56:78:ab:cd
--
-- ObjC selector: @- setStringRepresentation:@
setStringRepresentation :: (IsAVBMACAddress avbmacAddress, IsNSString value) => avbmacAddress -> value -> IO ()
setStringRepresentation avbmacAddress value =
  sendMessage avbmacAddress setStringRepresentationSelector (toNSString value)

-- | multicast
--
-- Returns if the multicast bit is set in the MAC address.
--
-- ObjC selector: @- multicast@
multicast :: IsAVBMACAddress avbmacAddress => avbmacAddress -> IO Bool
multicast avbmacAddress =
  sendMessage avbmacAddress multicastSelector

-- | multicast
--
-- Returns if the multicast bit is set in the MAC address.
--
-- ObjC selector: @- setMulticast:@
setMulticast :: IsAVBMACAddress avbmacAddress => avbmacAddress -> Bool -> IO ()
setMulticast avbmacAddress value =
  sendMessage avbmacAddress setMulticastSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBytes:@
initWithBytesSelector :: Selector '[Const (Ptr CUChar)] (Id AVBMACAddress)
initWithBytesSelector = mkSelector "initWithBytes:"

-- | @Selector@ for @bytes@
bytesSelector :: Selector '[] RawId
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @setDataRepresentation:@
setDataRepresentationSelector :: Selector '[Id NSData] ()
setDataRepresentationSelector = mkSelector "setDataRepresentation:"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector '[] (Id NSString)
stringRepresentationSelector = mkSelector "stringRepresentation"

-- | @Selector@ for @setStringRepresentation:@
setStringRepresentationSelector :: Selector '[Id NSString] ()
setStringRepresentationSelector = mkSelector "setStringRepresentation:"

-- | @Selector@ for @multicast@
multicastSelector :: Selector '[] Bool
multicastSelector = mkSelector "multicast"

-- | @Selector@ for @setMulticast:@
setMulticastSelector :: Selector '[Bool] ()
setMulticastSelector = mkSelector "setMulticast:"

