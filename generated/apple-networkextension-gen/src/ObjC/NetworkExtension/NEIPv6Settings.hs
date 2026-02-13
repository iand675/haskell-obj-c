{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEIPv6Settings
--
-- The NEIPv6Settings class declares the programmatic interface for an object that contains IPv6 settings.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEIPv6Settings@.
module ObjC.NetworkExtension.NEIPv6Settings
  ( NEIPv6Settings
  , IsNEIPv6Settings(..)
  , initWithAddresses_networkPrefixLengths
  , settingsWithAutomaticAddressing
  , settingsWithLinkLocalAddressing
  , addresses
  , networkPrefixLengths
  , includedRoutes
  , setIncludedRoutes
  , excludedRoutes
  , setExcludedRoutes
  , addressesSelector
  , excludedRoutesSelector
  , includedRoutesSelector
  , initWithAddresses_networkPrefixLengthsSelector
  , networkPrefixLengthsSelector
  , setExcludedRoutesSelector
  , setIncludedRoutesSelector
  , settingsWithAutomaticAddressingSelector
  , settingsWithLinkLocalAddressingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithAddresses:networkPrefixLengths:
--
-- Initialize a newly-allocated NEIPv6Settings object.
--
-- @addresses@ — An array of IPv6 addresses represented as dotted decimal strings.
--
-- @networkPrefixLengths@ — An array of NSNumber objects each containing the length in bits of the network prefix of the corresponding address in the addresses parameter.
--
-- Returns: The initialized object.
--
-- ObjC selector: @- initWithAddresses:networkPrefixLengths:@
initWithAddresses_networkPrefixLengths :: (IsNEIPv6Settings neiPv6Settings, IsNSArray addresses, IsNSArray networkPrefixLengths) => neiPv6Settings -> addresses -> networkPrefixLengths -> IO (Id NEIPv6Settings)
initWithAddresses_networkPrefixLengths neiPv6Settings addresses networkPrefixLengths =
  sendOwnedMessage neiPv6Settings initWithAddresses_networkPrefixLengthsSelector (toNSArray addresses) (toNSArray networkPrefixLengths)

-- | settingsWithAutomaticAddressing
--
-- Create a NEIPv6Settings object that will obtain IP addresses and netmasks automatically.
--
-- ObjC selector: @+ settingsWithAutomaticAddressing@
settingsWithAutomaticAddressing :: IO (Id NEIPv6Settings)
settingsWithAutomaticAddressing  =
  do
    cls' <- getRequiredClass "NEIPv6Settings"
    sendClassMessage cls' settingsWithAutomaticAddressingSelector

-- | settingsWithLinkLocalAddressing
--
-- Create a NEIPv6Settings object that will only use link-local IPv6 addresses.
--
-- ObjC selector: @+ settingsWithLinkLocalAddressing@
settingsWithLinkLocalAddressing :: IO (Id NEIPv6Settings)
settingsWithLinkLocalAddressing  =
  do
    cls' <- getRequiredClass "NEIPv6Settings"
    sendClassMessage cls' settingsWithLinkLocalAddressingSelector

-- | addresses
--
-- An array of IPv6 addresses represented strings. These addresses will be set on the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- addresses@
addresses :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
addresses neiPv6Settings =
  sendMessage neiPv6Settings addressesSelector

-- | networkPrefixLengths
--
-- An array of NSNumber objects each representing the length in bits of the network prefix of the corresponding address in the addresses property.
--
-- ObjC selector: @- networkPrefixLengths@
networkPrefixLengths :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
networkPrefixLengths neiPv6Settings =
  sendMessage neiPv6Settings networkPrefixLengthsSelector

-- | includedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- includedRoutes@
includedRoutes :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
includedRoutes neiPv6Settings =
  sendMessage neiPv6Settings includedRoutesSelector

-- | includedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- setIncludedRoutes:@
setIncludedRoutes :: (IsNEIPv6Settings neiPv6Settings, IsNSArray value) => neiPv6Settings -> value -> IO ()
setIncludedRoutes neiPv6Settings value =
  sendMessage neiPv6Settings setIncludedRoutesSelector (toNSArray value)

-- | excludedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- excludedRoutes@
excludedRoutes :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
excludedRoutes neiPv6Settings =
  sendMessage neiPv6Settings excludedRoutesSelector

-- | excludedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- setExcludedRoutes:@
setExcludedRoutes :: (IsNEIPv6Settings neiPv6Settings, IsNSArray value) => neiPv6Settings -> value -> IO ()
setExcludedRoutes neiPv6Settings value =
  sendMessage neiPv6Settings setExcludedRoutesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddresses:networkPrefixLengths:@
initWithAddresses_networkPrefixLengthsSelector :: Selector '[Id NSArray, Id NSArray] (Id NEIPv6Settings)
initWithAddresses_networkPrefixLengthsSelector = mkSelector "initWithAddresses:networkPrefixLengths:"

-- | @Selector@ for @settingsWithAutomaticAddressing@
settingsWithAutomaticAddressingSelector :: Selector '[] (Id NEIPv6Settings)
settingsWithAutomaticAddressingSelector = mkSelector "settingsWithAutomaticAddressing"

-- | @Selector@ for @settingsWithLinkLocalAddressing@
settingsWithLinkLocalAddressingSelector :: Selector '[] (Id NEIPv6Settings)
settingsWithLinkLocalAddressingSelector = mkSelector "settingsWithLinkLocalAddressing"

-- | @Selector@ for @addresses@
addressesSelector :: Selector '[] (Id NSArray)
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @networkPrefixLengths@
networkPrefixLengthsSelector :: Selector '[] (Id NSArray)
networkPrefixLengthsSelector = mkSelector "networkPrefixLengths"

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

