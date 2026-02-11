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
  , initWithIPv6AddressSelector
  , initWithIPv6AddressDataSelector
  , initWithIPv4AddressSelector
  , representsIPv4AddressSelector
  , ipv6AddressSelector
  , setIpv6AddressSelector
  , ipv4AddressSelector
  , setIpv4AddressSelector


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
initWithIPv6Address avbipAddress  ipv6Address =
  sendMsg avbipAddress (mkSelector "initWithIPv6Address:") (retPtr retVoid) [argPtr (unConst ipv6Address)] >>= ownedObject . castPtr

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
initWithIPv6AddressData avbipAddress  ipv6Address =
withObjCPtr ipv6Address $ \raw_ipv6Address ->
    sendMsg avbipAddress (mkSelector "initWithIPv6AddressData:") (retPtr retVoid) [argPtr (castPtr raw_ipv6Address :: Ptr ())] >>= ownedObject . castPtr

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
initWithIPv4Address avbipAddress  ipv4Address =
  sendMsg avbipAddress (mkSelector "initWithIPv4Address:") (retPtr retVoid) [argCUInt (fromIntegral ipv4Address)] >>= ownedObject . castPtr

-- | representsIPv4Address
--
-- A boolean indicating if the address is an IPv4 address.
--
-- ObjC selector: @- representsIPv4Address@
representsIPv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> IO Bool
representsIPv4Address avbipAddress  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avbipAddress (mkSelector "representsIPv4Address") retCULong []

-- | ipv6Address
--
-- An NSData object containing the bytes of the IPv6 representaion of the address. This value is always valid and uses the IPv4 to IPv6 address translation of 2.5.5.2 of RFC 4291 to encode the address
--
-- ObjC selector: @- ipv6Address@
ipv6Address :: IsAVBIPAddress avbipAddress => avbipAddress -> IO (Id NSData)
ipv6Address avbipAddress  =
  sendMsg avbipAddress (mkSelector "ipv6Address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ipv6Address
--
-- An NSData object containing the bytes of the IPv6 representaion of the address. This value is always valid and uses the IPv4 to IPv6 address translation of 2.5.5.2 of RFC 4291 to encode the address
--
-- ObjC selector: @- setIpv6Address:@
setIpv6Address :: (IsAVBIPAddress avbipAddress, IsNSData value) => avbipAddress -> value -> IO ()
setIpv6Address avbipAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg avbipAddress (mkSelector "setIpv6Address:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | ipv4Address
--
-- An unsigned32 bit integer containg the IPv4 representaion in host byte order. This value is only valid when representsIPv4Address returns YES.
--
-- ObjC selector: @- ipv4Address@
ipv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> IO CUInt
ipv4Address avbipAddress  =
  sendMsg avbipAddress (mkSelector "ipv4Address") retCUInt []

-- | ipv4Address
--
-- An unsigned32 bit integer containg the IPv4 representaion in host byte order. This value is only valid when representsIPv4Address returns YES.
--
-- ObjC selector: @- setIpv4Address:@
setIpv4Address :: IsAVBIPAddress avbipAddress => avbipAddress -> CUInt -> IO ()
setIpv4Address avbipAddress  value =
  sendMsg avbipAddress (mkSelector "setIpv4Address:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIPv6Address:@
initWithIPv6AddressSelector :: Selector
initWithIPv6AddressSelector = mkSelector "initWithIPv6Address:"

-- | @Selector@ for @initWithIPv6AddressData:@
initWithIPv6AddressDataSelector :: Selector
initWithIPv6AddressDataSelector = mkSelector "initWithIPv6AddressData:"

-- | @Selector@ for @initWithIPv4Address:@
initWithIPv4AddressSelector :: Selector
initWithIPv4AddressSelector = mkSelector "initWithIPv4Address:"

-- | @Selector@ for @representsIPv4Address@
representsIPv4AddressSelector :: Selector
representsIPv4AddressSelector = mkSelector "representsIPv4Address"

-- | @Selector@ for @ipv6Address@
ipv6AddressSelector :: Selector
ipv6AddressSelector = mkSelector "ipv6Address"

-- | @Selector@ for @setIpv6Address:@
setIpv6AddressSelector :: Selector
setIpv6AddressSelector = mkSelector "setIpv6Address:"

-- | @Selector@ for @ipv4Address@
ipv4AddressSelector :: Selector
ipv4AddressSelector = mkSelector "ipv4Address"

-- | @Selector@ for @setIpv4Address:@
setIpv4AddressSelector :: Selector
setIpv4AddressSelector = mkSelector "setIpv4Address:"

