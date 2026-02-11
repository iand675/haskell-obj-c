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
  , initWithDestinationAddress_networkPrefixLengthSelector
  , destinationAddressSelector
  , destinationNetworkPrefixLengthSelector
  , gatewayAddressSelector
  , setGatewayAddressSelector


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
initWithDestinationAddress_networkPrefixLength neiPv6Route  address networkPrefixLength =
  withObjCPtr address $ \raw_address ->
    withObjCPtr networkPrefixLength $ \raw_networkPrefixLength ->
        sendMsg neiPv6Route (mkSelector "initWithDestinationAddress:networkPrefixLength:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ()), argPtr (castPtr raw_networkPrefixLength :: Ptr ())] >>= ownedObject . castPtr

-- | destinationAddress
--
-- An IPv6 address represented as a string.
--
-- ObjC selector: @- destinationAddress@
destinationAddress :: IsNEIPv6Route neiPv6Route => neiPv6Route -> IO (Id NSString)
destinationAddress neiPv6Route  =
    sendMsg neiPv6Route (mkSelector "destinationAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | destinationNetworkPrefixLength
--
-- A number containing the length in bits of the network prefix of the destination network. This prefix in combination with the destinationAddress property is used to determine the destination network of the route.
--
-- ObjC selector: @- destinationNetworkPrefixLength@
destinationNetworkPrefixLength :: IsNEIPv6Route neiPv6Route => neiPv6Route -> IO (Id NSNumber)
destinationNetworkPrefixLength neiPv6Route  =
    sendMsg neiPv6Route (mkSelector "destinationNetworkPrefixLength") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gatewayAddress
--
-- The IPv6 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- gatewayAddress@
gatewayAddress :: IsNEIPv6Route neiPv6Route => neiPv6Route -> IO (Id NSString)
gatewayAddress neiPv6Route  =
    sendMsg neiPv6Route (mkSelector "gatewayAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gatewayAddress
--
-- The IPv6 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- setGatewayAddress:@
setGatewayAddress :: (IsNEIPv6Route neiPv6Route, IsNSString value) => neiPv6Route -> value -> IO ()
setGatewayAddress neiPv6Route  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv6Route (mkSelector "setGatewayAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationAddress:networkPrefixLength:@
initWithDestinationAddress_networkPrefixLengthSelector :: Selector
initWithDestinationAddress_networkPrefixLengthSelector = mkSelector "initWithDestinationAddress:networkPrefixLength:"

-- | @Selector@ for @destinationAddress@
destinationAddressSelector :: Selector
destinationAddressSelector = mkSelector "destinationAddress"

-- | @Selector@ for @destinationNetworkPrefixLength@
destinationNetworkPrefixLengthSelector :: Selector
destinationNetworkPrefixLengthSelector = mkSelector "destinationNetworkPrefixLength"

-- | @Selector@ for @gatewayAddress@
gatewayAddressSelector :: Selector
gatewayAddressSelector = mkSelector "gatewayAddress"

-- | @Selector@ for @setGatewayAddress:@
setGatewayAddressSelector :: Selector
setGatewayAddressSelector = mkSelector "setGatewayAddress:"

