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
  , initWithDestinationAddress_subnetMaskSelector
  , destinationAddressSelector
  , destinationSubnetMaskSelector
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
initWithDestinationAddress_subnetMask neiPv4Route  address subnetMask =
  withObjCPtr address $ \raw_address ->
    withObjCPtr subnetMask $ \raw_subnetMask ->
        sendMsg neiPv4Route (mkSelector "initWithDestinationAddress:subnetMask:") (retPtr retVoid) [argPtr (castPtr raw_address :: Ptr ()), argPtr (castPtr raw_subnetMask :: Ptr ())] >>= ownedObject . castPtr

-- | destinationAddress
--
-- An IPv4 address represented as a dotted decimal string.
--
-- ObjC selector: @- destinationAddress@
destinationAddress :: IsNEIPv4Route neiPv4Route => neiPv4Route -> IO (Id NSString)
destinationAddress neiPv4Route  =
    sendMsg neiPv4Route (mkSelector "destinationAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | destinationSubnetMask
--
-- An IPv4 subnet mask represented as a dotted decimal string. This mask in combination with the destinationAddress property is used to determine the destination network of the route.
--
-- ObjC selector: @- destinationSubnetMask@
destinationSubnetMask :: IsNEIPv4Route neiPv4Route => neiPv4Route -> IO (Id NSString)
destinationSubnetMask neiPv4Route  =
    sendMsg neiPv4Route (mkSelector "destinationSubnetMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gatewayAddress
--
-- The IPv4 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- gatewayAddress@
gatewayAddress :: IsNEIPv4Route neiPv4Route => neiPv4Route -> IO (Id NSString)
gatewayAddress neiPv4Route  =
    sendMsg neiPv4Route (mkSelector "gatewayAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gatewayAddress
--
-- The IPv4 address of the route's gateway. If this property is nil then the route's gateway will be set to the tunnel's virtual interface.
--
-- ObjC selector: @- setGatewayAddress:@
setGatewayAddress :: (IsNEIPv4Route neiPv4Route, IsNSString value) => neiPv4Route -> value -> IO ()
setGatewayAddress neiPv4Route  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv4Route (mkSelector "setGatewayAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationAddress:subnetMask:@
initWithDestinationAddress_subnetMaskSelector :: Selector
initWithDestinationAddress_subnetMaskSelector = mkSelector "initWithDestinationAddress:subnetMask:"

-- | @Selector@ for @destinationAddress@
destinationAddressSelector :: Selector
destinationAddressSelector = mkSelector "destinationAddress"

-- | @Selector@ for @destinationSubnetMask@
destinationSubnetMaskSelector :: Selector
destinationSubnetMaskSelector = mkSelector "destinationSubnetMask"

-- | @Selector@ for @gatewayAddress@
gatewayAddressSelector :: Selector
gatewayAddressSelector = mkSelector "gatewayAddress"

-- | @Selector@ for @setGatewayAddress:@
setGatewayAddressSelector :: Selector
setGatewayAddressSelector = mkSelector "setGatewayAddress:"

