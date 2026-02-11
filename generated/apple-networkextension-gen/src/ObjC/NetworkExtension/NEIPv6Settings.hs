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
  , initWithAddresses_networkPrefixLengthsSelector
  , settingsWithAutomaticAddressingSelector
  , settingsWithLinkLocalAddressingSelector
  , addressesSelector
  , networkPrefixLengthsSelector
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
initWithAddresses_networkPrefixLengths neiPv6Settings  addresses networkPrefixLengths =
  withObjCPtr addresses $ \raw_addresses ->
    withObjCPtr networkPrefixLengths $ \raw_networkPrefixLengths ->
        sendMsg neiPv6Settings (mkSelector "initWithAddresses:networkPrefixLengths:") (retPtr retVoid) [argPtr (castPtr raw_addresses :: Ptr ()), argPtr (castPtr raw_networkPrefixLengths :: Ptr ())] >>= ownedObject . castPtr

-- | settingsWithAutomaticAddressing
--
-- Create a NEIPv6Settings object that will obtain IP addresses and netmasks automatically.
--
-- ObjC selector: @+ settingsWithAutomaticAddressing@
settingsWithAutomaticAddressing :: IO (Id NEIPv6Settings)
settingsWithAutomaticAddressing  =
  do
    cls' <- getRequiredClass "NEIPv6Settings"
    sendClassMsg cls' (mkSelector "settingsWithAutomaticAddressing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | settingsWithLinkLocalAddressing
--
-- Create a NEIPv6Settings object that will only use link-local IPv6 addresses.
--
-- ObjC selector: @+ settingsWithLinkLocalAddressing@
settingsWithLinkLocalAddressing :: IO (Id NEIPv6Settings)
settingsWithLinkLocalAddressing  =
  do
    cls' <- getRequiredClass "NEIPv6Settings"
    sendClassMsg cls' (mkSelector "settingsWithLinkLocalAddressing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | addresses
--
-- An array of IPv6 addresses represented strings. These addresses will be set on the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- addresses@
addresses :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
addresses neiPv6Settings  =
    sendMsg neiPv6Settings (mkSelector "addresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | networkPrefixLengths
--
-- An array of NSNumber objects each representing the length in bits of the network prefix of the corresponding address in the addresses property.
--
-- ObjC selector: @- networkPrefixLengths@
networkPrefixLengths :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
networkPrefixLengths neiPv6Settings  =
    sendMsg neiPv6Settings (mkSelector "networkPrefixLengths") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | includedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- includedRoutes@
includedRoutes :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
includedRoutes neiPv6Settings  =
    sendMsg neiPv6Settings (mkSelector "includedRoutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | includedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the virtual interface used by the VPN tunnel.
--
-- ObjC selector: @- setIncludedRoutes:@
setIncludedRoutes :: (IsNEIPv6Settings neiPv6Settings, IsNSArray value) => neiPv6Settings -> value -> IO ()
setIncludedRoutes neiPv6Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv6Settings (mkSelector "setIncludedRoutes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- excludedRoutes@
excludedRoutes :: IsNEIPv6Settings neiPv6Settings => neiPv6Settings -> IO (Id NSArray)
excludedRoutes neiPv6Settings  =
    sendMsg neiPv6Settings (mkSelector "excludedRoutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedRoutes
--
-- An array of NEIPv6Route objects. Traffic matching these routes will be routed through the current primary physical interface of the device.
--
-- ObjC selector: @- setExcludedRoutes:@
setExcludedRoutes :: (IsNEIPv6Settings neiPv6Settings, IsNSArray value) => neiPv6Settings -> value -> IO ()
setExcludedRoutes neiPv6Settings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neiPv6Settings (mkSelector "setExcludedRoutes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddresses:networkPrefixLengths:@
initWithAddresses_networkPrefixLengthsSelector :: Selector
initWithAddresses_networkPrefixLengthsSelector = mkSelector "initWithAddresses:networkPrefixLengths:"

-- | @Selector@ for @settingsWithAutomaticAddressing@
settingsWithAutomaticAddressingSelector :: Selector
settingsWithAutomaticAddressingSelector = mkSelector "settingsWithAutomaticAddressing"

-- | @Selector@ for @settingsWithLinkLocalAddressing@
settingsWithLinkLocalAddressingSelector :: Selector
settingsWithLinkLocalAddressingSelector = mkSelector "settingsWithLinkLocalAddressing"

-- | @Selector@ for @addresses@
addressesSelector :: Selector
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @networkPrefixLengths@
networkPrefixLengthsSelector :: Selector
networkPrefixLengthsSelector = mkSelector "networkPrefixLengths"

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

