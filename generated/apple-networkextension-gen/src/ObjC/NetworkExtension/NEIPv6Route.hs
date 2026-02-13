{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEIPv6Route
--
-- The NEIPv6Route class declares the programmatic interface for an object that contains settings for an IPv6 route.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEIPv6Route@.
module ObjC.NetworkExtension.NEIPv6Route
  ( NEIPv6Route
  , IsNEIPv6Route(..)
  , initWithDestinationAddress_networkPrefixLength
  , destinationAddress
  , destinationNetworkPrefixLength
  , gatewayAddress
  , setGatewayAddress
  , destinationAddressSelector
  , destinationNetworkPrefixLengthSelector
  , gatewayAddressSelector
  , initWithDestinationAddress_networkPrefixLengthSelector
  , setGatewayAddressSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithDestinationAddress:networkPrefixLength:
--
-- Initialize a newly-allocated NEIPv6Route.
--
-- @address@ — The IPv6 address of the destination network.
--
-- @networkPrefixLength@ — A number containing the length in bits of the network prefix of the destination network.
--
-- Returns: The initialized NEIPv6Route.
--
-- ObjC selector: @- initWithDestinationAddress:networkPrefixLength:@
initWithDestinationAddress_networkPrefixLength :: (IsNEIPv6Route neiPv6Route, IsNSString address, IsNSNumber networkPrefixLength) => neiPv6Route -> address -> networkPrefixLength -> IO (Id NEIPv6Route)
initWithDestinationAddress_networkPrefixLength neiPv6Route address networkPrefixLength =
  sendOwnedMessage neiPv6Route initWithDestinationAddress_networkPrefixLengthSelector (toNSString address) (toNSNumber networkPrefixLength)

-- | destinationAddress
--
-- An IPv6 address represented as a string.
--
-- ObjC selector: @- destinationAddress@
destinationAddress :: IsNEIPv6Route neiPv6Route => neiPv6Route -> IO (Id NSString)
destinationAddress neiPv6Route =
  sendMessage neiPv6Route destinationAddressSelector

-- | destinationNetworkPrefixLength
--
-- A number containing the length in bits of the network prefix of the destination network. This prefix in combination with the destinationAddress property is used to determine the destination network of the route.
--
-- ObjC selector: @- destinationNetworkPrefixLength@
destinationNetworkPrefixLength :: IsNEIPv6Route neiPv6Route => neiPv6Route -> IO (Id NSNumber)
destinationNetworkPrefixLength neiPv6Route =
  sendMessage neiPv6Route destinationNetworkPrefixLengthSelector

-- | gatewayAddress
--
-- The IPv6 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- gatewayAddress@
gatewayAddress :: IsNEIPv6Route neiPv6Route => neiPv6Route -> IO (Id NSString)
gatewayAddress neiPv6Route =
  sendMessage neiPv6Route gatewayAddressSelector

-- | gatewayAddress
--
-- The IPv6 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- setGatewayAddress:@
setGatewayAddress :: (IsNEIPv6Route neiPv6Route, IsNSString value) => neiPv6Route -> value -> IO ()
setGatewayAddress neiPv6Route value =
  sendMessage neiPv6Route setGatewayAddressSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationAddress:networkPrefixLength:@
initWithDestinationAddress_networkPrefixLengthSelector :: Selector '[Id NSString, Id NSNumber] (Id NEIPv6Route)
initWithDestinationAddress_networkPrefixLengthSelector = mkSelector "initWithDestinationAddress:networkPrefixLength:"

-- | @Selector@ for @destinationAddress@
destinationAddressSelector :: Selector '[] (Id NSString)
destinationAddressSelector = mkSelector "destinationAddress"

-- | @Selector@ for @destinationNetworkPrefixLength@
destinationNetworkPrefixLengthSelector :: Selector '[] (Id NSNumber)
destinationNetworkPrefixLengthSelector = mkSelector "destinationNetworkPrefixLength"

-- | @Selector@ for @gatewayAddress@
gatewayAddressSelector :: Selector '[] (Id NSString)
gatewayAddressSelector = mkSelector "gatewayAddress"

-- | @Selector@ for @setGatewayAddress:@
setGatewayAddressSelector :: Selector '[Id NSString] ()
setGatewayAddressSelector = mkSelector "setGatewayAddress:"

