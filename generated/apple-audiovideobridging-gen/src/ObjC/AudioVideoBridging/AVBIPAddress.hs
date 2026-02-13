{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVBIPAddress
--
-- AVBIPAddress is a class for holding and representing an IP Address, either IPv4 or IPv6.
--
-- Generated bindings for @AVBIPAddress@.
module ObjC.AudioVideoBridging.AVBIPAddress
  ( AVBIPAddress
  , IsAVBIPAddress(..)
  , initWithIPv6Address
  , initWithIPv6AddressData
  , initWithIPv4Address
  , representsIPv4Address
  , ipv6Address
  , setIpv6Address
  , ipv4Address
  , setIpv4Address
  , stringRepresentation
  , setStringRepresentation
  , initWithIPv4AddressSelector
  , initWithIPv6AddressDataSelector
  , initWithIPv6AddressSelector
  , ipv4AddressSelector
  , ipv6AddressSelector
  , representsIPv4AddressSelector
  , setIpv4AddressSelector
  , setIpv6AddressSelector
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

-- | initWithIPv6Address:
--
-- This method initializes the receiver to contain the IPv6 address specified.
--
-- @ipv6Address@ — A pointer to 16 octets of memory containing the IPv6 address.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithIPv6Address:@
initWithIPv6Address :: IsAVBIPAddress avbipAddress => avbipAddress -> Const (Ptr CUChar) -> IO (Id AVBIPAddress)
initWithIPv6Address avbipAddress ipv6Address =
  sendOwnedMessage avbipAddress initWithIPv6AddressSelector ipv6Address

-- | initWithIPv6AddressData:
--
-- This method initializes the receiver to contain the IPv6 address specified.
--
-- @ipv6Address@ — An NSData containing 16 octets with the IPv6 address.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithIPv6AddressData:@
initWithIPv6AddressData :: (IsAVBIPAddress avbipAddress, IsNSData ipv6Address) => avbipAddress -> ipv6Address -> IO (Id AVBIPAddress)
initWithIPv6AddressData avbipAddress ipv6Address =
  sendOwnedMessage avbipAddress initWithIPv6AddressDataSelector (toNSData ipv6Address)

-- | initWithIPv4Address:
--
-- This method initializes the receiver to contain the IPv4 address specified.
--
-- @ipv4Address@ — A uint32_t containing the IPv4 address in host byte order.
--
-- Returns: The initialized receiver.
--
-- ObjC selector: @- initWithIPv4Address:@
initWithIPv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> CUInt -> IO (Id AVBIPAddress)
initWithIPv4Address avbipAddress ipv4Address =
  sendOwnedMessage avbipAddress initWithIPv4AddressSelector ipv4Address

-- | representsIPv4Address
--
-- A boolean indicating if the address is an IPv4 address.
--
-- ObjC selector: @- representsIPv4Address@
representsIPv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> IO Bool
representsIPv4Address avbipAddress =
  sendMessage avbipAddress representsIPv4AddressSelector

-- | ipv6Address
--
-- An NSData object containing the bytes of the IPv6 representaion of the address. This value is always valid and uses the IPv4 to IPv6 address translation of 2.5.5.2 of RFC 4291 to encode the address
--
-- ObjC selector: @- ipv6Address@
ipv6Address :: IsAVBIPAddress avbipAddress => avbipAddress -> IO (Id NSData)
ipv6Address avbipAddress =
  sendMessage avbipAddress ipv6AddressSelector

-- | ipv6Address
--
-- An NSData object containing the bytes of the IPv6 representaion of the address. This value is always valid and uses the IPv4 to IPv6 address translation of 2.5.5.2 of RFC 4291 to encode the address
--
-- ObjC selector: @- setIpv6Address:@
setIpv6Address :: (IsAVBIPAddress avbipAddress, IsNSData value) => avbipAddress -> value -> IO ()
setIpv6Address avbipAddress value =
  sendMessage avbipAddress setIpv6AddressSelector (toNSData value)

-- | ipv4Address
--
-- An unsigned32 bit integer containg the IPv4 representaion in host byte order. This value is only valid when representsIPv4Address returns YES.
--
-- ObjC selector: @- ipv4Address@
ipv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> IO CUInt
ipv4Address avbipAddress =
  sendMessage avbipAddress ipv4AddressSelector

-- | ipv4Address
--
-- An unsigned32 bit integer containg the IPv4 representaion in host byte order. This value is only valid when representsIPv4Address returns YES.
--
-- ObjC selector: @- setIpv4Address:@
setIpv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> CUInt -> IO ()
setIpv4Address avbipAddress value =
  sendMessage avbipAddress setIpv4AddressSelector value

-- | stringRepresentation
--
-- A strign representation of the IP address in the appropriate representation for IPv4 or IPv6.
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsAVBIPAddress avbipAddress => avbipAddress -> IO (Id NSString)
stringRepresentation avbipAddress =
  sendMessage avbipAddress stringRepresentationSelector

-- | stringRepresentation
--
-- A strign representation of the IP address in the appropriate representation for IPv4 or IPv6.
--
-- ObjC selector: @- setStringRepresentation:@
setStringRepresentation :: (IsAVBIPAddress avbipAddress, IsNSString value) => avbipAddress -> value -> IO ()
setStringRepresentation avbipAddress value =
  sendMessage avbipAddress setStringRepresentationSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIPv6Address:@
initWithIPv6AddressSelector :: Selector '[Const (Ptr CUChar)] (Id AVBIPAddress)
initWithIPv6AddressSelector = mkSelector "initWithIPv6Address:"

-- | @Selector@ for @initWithIPv6AddressData:@
initWithIPv6AddressDataSelector :: Selector '[Id NSData] (Id AVBIPAddress)
initWithIPv6AddressDataSelector = mkSelector "initWithIPv6AddressData:"

-- | @Selector@ for @initWithIPv4Address:@
initWithIPv4AddressSelector :: Selector '[CUInt] (Id AVBIPAddress)
initWithIPv4AddressSelector = mkSelector "initWithIPv4Address:"

-- | @Selector@ for @representsIPv4Address@
representsIPv4AddressSelector :: Selector '[] Bool
representsIPv4AddressSelector = mkSelector "representsIPv4Address"

-- | @Selector@ for @ipv6Address@
ipv6AddressSelector :: Selector '[] (Id NSData)
ipv6AddressSelector = mkSelector "ipv6Address"

-- | @Selector@ for @setIpv6Address:@
setIpv6AddressSelector :: Selector '[Id NSData] ()
setIpv6AddressSelector = mkSelector "setIpv6Address:"

-- | @Selector@ for @ipv4Address@
ipv4AddressSelector :: Selector '[] CUInt
ipv4AddressSelector = mkSelector "ipv4Address"

-- | @Selector@ for @setIpv4Address:@
setIpv4AddressSelector :: Selector '[CUInt] ()
setIpv4AddressSelector = mkSelector "setIpv4Address:"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector '[] (Id NSString)
stringRepresentationSelector = mkSelector "stringRepresentation"

-- | @Selector@ for @setStringRepresentation:@
setStringRepresentationSelector :: Selector '[Id NSString] ()
setStringRepresentationSelector = mkSelector "setStringRepresentation:"

