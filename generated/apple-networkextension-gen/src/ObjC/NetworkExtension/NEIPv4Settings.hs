{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEIPv4Settings
--
-- The NEIPv4Settings class declares the programmatic interface for an object that contains IPv4 settings.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEIPv4Settings@.
module ObjC.NetworkExtension.NEIPv4Settings
  ( NEIPv4Settings
  , IsNEIPv4Settings(..)
  , initWithAddresses_subnetMasks
  , settingsWithAutomaticAddressing
  , addresses
  , subnetMasks
  , router
  , setRouter
  , includedRoutes
  , setIncludedRoutes
  , excludedRoutes
  , setExcludedRoutes
  , initWithAddresses_subnetMasksSelector
  , settingsWithAutomaticAddressingSelector
  , addressesSelector
  , subnetMasksSelector
  , routerSelector
  , setRouterSelector
  , includedRoutesSelector
  , setIncludedRoutesSelector
  , excludedRoutesSelector
  , setExcludedRoutesSelector


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

-- | initWithAddresses:subnetMasks:
--
-- Initialize a newly-allocated NEIPv4Settings object.
--
-- @addresses@ — An array of IPv4 addresses represented as dotted decimal strings.
--
-- @subnetMasks@ — An array of IPv4 subnet masks represented as dotted decimal strings.
--
-- Returns: The initialized object.
--
-- ObjC selector: @- initWithAddresses:subnetMasks:@
initWithAddresses_subnetMasks :: (IsNEIPv4Settings neiPv4Settings, IsNSArray addresses, IsNSArray subnetMasks) => neiPv4Settings -> addresses -> subnetMasks -> IO (Id NEIPv4Settings)
initWithAddresses_subnetMasks neiPv4Settings  addresses subnetMasks =
  withObjCPtr addresses $ \raw_addresses ->
    withObjCPtr subnetMasks $ \raw_subnetMasks ->
        sendMsg neiPv4Settings (mkSelector "initWithAddresses:subnetMasks:") (retPtr retVoid) [argPtr (castPtr raw_addresses :: Ptr ()), argPtr (castPtr raw_subnetMasks :: Ptr ())] >>= ownedObject . castPtr

-- | settingsWithAutomaticAddressing
--
-- Create a NEIPv4Settings object that will obtain IP addresses and netmasks using DHCP.
--
-- Returns: The initialized object.
--
-- ObjC selector: @+ settingsWithAutomaticAddressing@
settingsWithAutomaticAddressing :: IO (Id NEIPv4Settings)
settingsWithAutomaticAddressing  =
  do
    cls' <- getRequiredClass "NEIPv4Settings"
    sendClassMsg cls' (mkSelector "settingsWithAutomaticAddressing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | addresses
--
-- An array of IPv4 addresses represented as dotted decimal strings. These addresses will be set on the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- addresses@
addresses :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
addresses neiPv4Settings  =
    sendMsg neiPv4Settings (mkSelector "addresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | subnetMasks
--
-- An array of IPv4 subnet masks represented as dotted decimal strings. These subnet masks will be set along with their corresponding addresses from the addresses array on the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- subnetMasks@
subnetMasks :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
subnetMasks neiPv4Settings  =
    sendMsg neiPv4Settings (mkSelector "subnetMasks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | router
--
-- The address of the next-hop gateway router represented as a dotted decimal string. This property is ignored for TUN interfaces.
--
-- ObjC selector: @- router@
router :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSString)
router neiPv4Settings  =
    sendMsg neiPv4Settings (mkSelector "router") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | router
--
-- The address of the next-hop gateway router represented as a dotted decimal string. This property is ignored for TUN interfaces.
--
-- ObjC selector: @- setRouter:@
setRouter :: (IsNEIPv4Settings neiPv4Settings, IsNSString value) => neiPv4Settings -> value -> IO ()
setRouter neiPv4Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv4Settings (mkSelector "setRouter:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | includedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- includedRoutes@
includedRoutes :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
includedRoutes neiPv4Settings  =
    sendMsg neiPv4Settings (mkSelector "includedRoutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | includedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- setIncludedRoutes:@
setIncludedRoutes :: (IsNEIPv4Settings neiPv4Settings, IsNSArray value) => neiPv4Settings -> value -> IO ()
setIncludedRoutes neiPv4Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv4Settings (mkSelector "setIncludedRoutes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- excludedRoutes@
excludedRoutes :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
excludedRoutes neiPv4Settings  =
    sendMsg neiPv4Settings (mkSelector "excludedRoutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- setExcludedRoutes:@
setExcludedRoutes :: (IsNEIPv4Settings neiPv4Settings, IsNSArray value) => neiPv4Settings -> value -> IO ()
setExcludedRoutes neiPv4Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv4Settings (mkSelector "setExcludedRoutes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddresses:subnetMasks:@
initWithAddresses_subnetMasksSelector :: Selector
initWithAddresses_subnetMasksSelector = mkSelector "initWithAddresses:subnetMasks:"

-- | @Selector@ for @settingsWithAutomaticAddressing@
settingsWithAutomaticAddressingSelector :: Selector
settingsWithAutomaticAddressingSelector = mkSelector "settingsWithAutomaticAddressing"

-- | @Selector@ for @addresses@
addressesSelector :: Selector
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @subnetMasks@
subnetMasksSelector :: Selector
subnetMasksSelector = mkSelector "subnetMasks"

-- | @Selector@ for @router@
routerSelector :: Selector
routerSelector = mkSelector "router"

-- | @Selector@ for @setRouter:@
setRouterSelector :: Selector
setRouterSelector = mkSelector "setRouter:"

-- | @Selector@ for @includedRoutes@
includedRoutesSelector :: Selector
includedRoutesSelector = mkSelector "includedRoutes"

-- | @Selector@ for @setIncludedRoutes:@
setIncludedRoutesSelector :: Selector
setIncludedRoutesSelector = mkSelector "setIncludedRoutes:"

-- | @Selector@ for @excludedRoutes@
excludedRoutesSelector :: Selector
excludedRoutesSelector = mkSelector "excludedRoutes"

-- | @Selector@ for @setExcludedRoutes:@
setExcludedRoutesSelector :: Selector
setExcludedRoutesSelector = mkSelector "setExcludedRoutes:"

