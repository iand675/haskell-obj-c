{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEIPv4Route
--
-- The NEIPv4Route class declares the programmatic interface for an object that contains settings for an IPv4 route.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEIPv4Route@.
module ObjC.NetworkExtension.NEIPv4Route
  ( NEIPv4Route
  , IsNEIPv4Route(..)
  , initWithDestinationAddress_subnetMask
  , destinationAddress
  , destinationSubnetMask
  , gatewayAddress
  , setGatewayAddress
  , destinationAddressSelector
  , destinationSubnetMaskSelector
  , gatewayAddressSelector
  , initWithDestinationAddress_subnetMaskSelector
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

-- | initWithDestinationAddress:subnetMask:
--
-- Initialize a newly-allocated NEIPv4Route.
--
-- @address@ — The IPv4 address of the destination network.
--
-- @subnetMask@ — The subnet mask of the destination network.
--
-- Returns: The initialized NEIPv4Route.
--
-- ObjC selector: @- initWithDestinationAddress:subnetMask:@
initWithDestinationAddress_subnetMask :: (IsNEIPv4Route neiPv4Route, IsNSString address, IsNSString subnetMask) => neiPv4Route -> address -> subnetMask -> IO (Id NEIPv4Route)
initWithDestinationAddress_subnetMask neiPv4Route address subnetMask =
  sendOwnedMessage neiPv4Route initWithDestinationAddress_subnetMaskSelector (toNSString address) (toNSString subnetMask)

-- | destinationAddress
--
-- An IPv4 address represented as a dotted decimal string.
--
-- ObjC selector: @- destinationAddress@
destinationAddress :: IsNEIPv4Route neiPv4Route => neiPv4Route -> IO (Id NSString)
destinationAddress neiPv4Route =
  sendMessage neiPv4Route destinationAddressSelector

-- | destinationSubnetMask
--
-- An IPv4 subnet mask represented as a dotted decimal string. This mask in combination with the destinationAddress property is used to determine the destination network of the route.
--
-- ObjC selector: @- destinationSubnetMask@
destinationSubnetMask :: IsNEIPv4Route neiPv4Route => neiPv4Route -> IO (Id NSString)
destinationSubnetMask neiPv4Route =
  sendMessage neiPv4Route destinationSubnetMaskSelector

-- | gatewayAddress
--
-- The IPv4 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- gatewayAddress@
gatewayAddress :: IsNEIPv4Route neiPv4Route => neiPv4Route -> IO (Id NSString)
gatewayAddress neiPv4Route =
  sendMessage neiPv4Route gatewayAddressSelector

-- | gatewayAddress
--
-- The IPv4 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- setGatewayAddress:@
setGatewayAddress :: (IsNEIPv4Route neiPv4Route, IsNSString value) => neiPv4Route -> value -> IO ()
setGatewayAddress neiPv4Route value =
  sendMessage neiPv4Route setGatewayAddressSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationAddress:subnetMask:@
initWithDestinationAddress_subnetMaskSelector :: Selector '[Id NSString, Id NSString] (Id NEIPv4Route)
initWithDestinationAddress_subnetMaskSelector = mkSelector "initWithDestinationAddress:subnetMask:"

-- | @Selector@ for @destinationAddress@
destinationAddressSelector :: Selector '[] (Id NSString)
destinationAddressSelector = mkSelector "destinationAddress"

-- | @Selector@ for @destinationSubnetMask@
destinationSubnetMaskSelector :: Selector '[] (Id NSString)
destinationSubnetMaskSelector = mkSelector "destinationSubnetMask"

-- | @Selector@ for @gatewayAddress@
gatewayAddressSelector :: Selector '[] (Id NSString)
gatewayAddressSelector = mkSelector "gatewayAddress"

-- | @Selector@ for @setGatewayAddress:@
setGatewayAddressSelector :: Selector '[Id NSString] ()
setGatewayAddressSelector = mkSelector "setGatewayAddress:"

