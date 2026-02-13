{-# LANGUAGE DataKinds #-}
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
  , addressesSelector
  , excludedRoutesSelector
  , includedRoutesSelector
  , initWithAddresses_subnetMasksSelector
  , routerSelector
  , setExcludedRoutesSelector
  , setIncludedRoutesSelector
  , setRouterSelector
  , settingsWithAutomaticAddressingSelector
  , subnetMasksSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithAddresses_subnetMasks neiPv4Settings addresses subnetMasks =
  sendOwnedMessage neiPv4Settings initWithAddresses_subnetMasksSelector (toNSArray addresses) (toNSArray subnetMasks)

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
    sendClassMessage cls' settingsWithAutomaticAddressingSelector

-- | addresses
--
-- An array of IPv4 addresses represented as dotted decimal strings. These addresses will be set on the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- addresses@
addresses :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
addresses neiPv4Settings =
  sendMessage neiPv4Settings addressesSelector

-- | subnetMasks
--
-- An array of IPv4 subnet masks represented as dotted decimal strings. These subnet masks will be set along with their corresponding addresses from the addresses array on the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- subnetMasks@
subnetMasks :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
subnetMasks neiPv4Settings =
  sendMessage neiPv4Settings subnetMasksSelector

-- | router
--
-- The address of the next-hop gateway router represented as a dotted decimal string. This property is ignored for TUN interfaces.
--
-- ObjC selector: @- router@
router :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSString)
router neiPv4Settings =
  sendMessage neiPv4Settings routerSelector

-- | router
--
-- The address of the next-hop gateway router represented as a dotted decimal string. This property is ignored for TUN interfaces.
--
-- ObjC selector: @- setRouter:@
setRouter :: (IsNEIPv4Settings neiPv4Settings, IsNSString value) => neiPv4Settings -> value -> IO ()
setRouter neiPv4Settings value =
  sendMessage neiPv4Settings setRouterSelector (toNSString value)

-- | includedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- includedRoutes@
includedRoutes :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
includedRoutes neiPv4Settings =
  sendMessage neiPv4Settings includedRoutesSelector

-- | includedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- setIncludedRoutes:@
setIncludedRoutes :: (IsNEIPv4Settings neiPv4Settings, IsNSArray value) => neiPv4Settings -> value -> IO ()
setIncludedRoutes neiPv4Settings value =
  sendMessage neiPv4Settings setIncludedRoutesSelector (toNSArray value)

-- | excludedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- excludedRoutes@
excludedRoutes :: IsNEIPv4Settings neiPv4Settings => neiPv4Settings -> IO (Id NSArray)
excludedRoutes neiPv4Settings =
  sendMessage neiPv4Settings excludedRoutesSelector

-- | excludedRoutes
--
-- An array of NEIPv4Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- setExcludedRoutes:@
setExcludedRoutes :: (IsNEIPv4Settings neiPv4Settings, IsNSArray value) => neiPv4Settings -> value -> IO ()
setExcludedRoutes neiPv4Settings value =
  sendMessage neiPv4Settings setExcludedRoutesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddresses:subnetMasks:@
initWithAddresses_subnetMasksSelector :: Selector '[Id NSArray, Id NSArray] (Id NEIPv4Settings)
initWithAddresses_subnetMasksSelector = mkSelector "initWithAddresses:subnetMasks:"

-- | @Selector@ for @settingsWithAutomaticAddressing@
settingsWithAutomaticAddressingSelector :: Selector '[] (Id NEIPv4Settings)
settingsWithAutomaticAddressingSelector = mkSelector "settingsWithAutomaticAddressing"

-- | @Selector@ for @addresses@
addressesSelector :: Selector '[] (Id NSArray)
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @subnetMasks@
subnetMasksSelector :: Selector '[] (Id NSArray)
subnetMasksSelector = mkSelector "subnetMasks"

-- | @Selector@ for @router@
routerSelector :: Selector '[] (Id NSString)
routerSelector = mkSelector "router"

-- | @Selector@ for @setRouter:@
setRouterSelector :: Selector '[Id NSString] ()
setRouterSelector = mkSelector "setRouter:"

-- | @Selector@ for @includedRoutes@
includedRoutesSelector :: Selector '[] (Id NSArray)
includedRoutesSelector = mkSelector "includedRoutes"

-- | @Selector@ for @setIncludedRoutes:@
setIncludedRoutesSelector :: Selector '[Id NSArray] ()
setIncludedRoutesSelector = mkSelector "setIncludedRoutes:"

-- | @Selector@ for @excludedRoutes@
excludedRoutesSelector :: Selector '[] (Id NSArray)
excludedRoutesSelector = mkSelector "excludedRoutes"

-- | @Selector@ for @setExcludedRoutes:@
setExcludedRoutesSelector :: Selector '[Id NSArray] ()
setExcludedRoutesSelector = mkSelector "setExcludedRoutes:"

