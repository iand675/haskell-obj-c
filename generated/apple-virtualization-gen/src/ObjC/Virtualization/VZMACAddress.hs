{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VZMACAddress represents a media access control address (MAC address), the 48-bit ethernet address.
--
-- The easiest way to obtain a MAC address is with +[VZMACAddress randomLocallyAdministeredAddress]. The method    returns a valid local MAC address typically used with network interfaces.
--
-- See: VZNetworkDeviceConfiguration
--
-- Generated bindings for @VZMACAddress@.
module ObjC.Virtualization.VZMACAddress
  ( VZMACAddress
  , IsVZMACAddress(..)
  , new
  , init_
  , initWithString
  , randomLocallyAdministeredAddress
  , string
  , isBroadcastAddress
  , isMulticastAddress
  , isUnicastAddress
  , isLocallyAdministeredAddress
  , isUniversallyAdministeredAddress
  , initSelector
  , initWithStringSelector
  , isBroadcastAddressSelector
  , isLocallyAdministeredAddressSelector
  , isMulticastAddressSelector
  , isUnicastAddressSelector
  , isUniversallyAdministeredAddressSelector
  , newSelector
  , randomLocallyAdministeredAddressSelector
  , stringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZMACAddress)
new  =
  do
    cls' <- getRequiredClass "VZMACAddress"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO (Id VZMACAddress)
init_ vzmacAddress =
  sendOwnedMessage vzmacAddress initSelector

-- | Initialize the VZMACAddress from a string representation of a MAC address.
--
-- @string@ — The string should be formatted representing the 6 bytes in hexadecimal separated by a colon character.    e.g. "01:23:45:ab:cd:ef"
--
-- The alphabetical characters can appear lowercase or uppercase.
--
-- Returns: A VZMACAddress or nil if the string is not formatted correctly.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsVZMACAddress vzmacAddress, IsNSString string) => vzmacAddress -> string -> IO (Id VZMACAddress)
initWithString vzmacAddress string =
  sendOwnedMessage vzmacAddress initWithStringSelector (toNSString string)

-- | Create a valid, random, unicast, locally administered address.
--
-- The generated address is not guaranteed to be unique.
--
-- ObjC selector: @+ randomLocallyAdministeredAddress@
randomLocallyAdministeredAddress :: IO (Id VZMACAddress)
randomLocallyAdministeredAddress  =
  do
    cls' <- getRequiredClass "VZMACAddress"
    sendClassMessage cls' randomLocallyAdministeredAddressSelector

-- | The address represented as a string.
--
-- The 6 bytes are represented in hexadecimal form, separated by a colon character.    Alphabetical characters are lowercase.
--
-- The address is compatible with the parameter of -[VZMACAddress initWithString:].
--
-- ObjC selector: @- string@
string :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO (Id NSString)
string vzmacAddress =
  sendMessage vzmacAddress stringSelector

-- | True if the address is the broadcast address, false otherwise.
--
-- ObjC selector: @- isBroadcastAddress@
isBroadcastAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isBroadcastAddress vzmacAddress =
  sendMessage vzmacAddress isBroadcastAddressSelector

-- | True if the address is a multicast address, false otherwise.
--
-- ObjC selector: @- isMulticastAddress@
isMulticastAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isMulticastAddress vzmacAddress =
  sendMessage vzmacAddress isMulticastAddressSelector

-- | True if the address is a unicast address, false otherwise.
--
-- ObjC selector: @- isUnicastAddress@
isUnicastAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isUnicastAddress vzmacAddress =
  sendMessage vzmacAddress isUnicastAddressSelector

-- | True if the address is a locally administered addresses (LAA), false otherwise.
--
-- ObjC selector: @- isLocallyAdministeredAddress@
isLocallyAdministeredAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isLocallyAdministeredAddress vzmacAddress =
  sendMessage vzmacAddress isLocallyAdministeredAddressSelector

-- | True if the address is a universally administered addresses (UAA), false otherwise.
--
-- ObjC selector: @- isUniversallyAdministeredAddress@
isUniversallyAdministeredAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isUniversallyAdministeredAddress vzmacAddress =
  sendMessage vzmacAddress isUniversallyAdministeredAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZMACAddress)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMACAddress)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id VZMACAddress)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @randomLocallyAdministeredAddress@
randomLocallyAdministeredAddressSelector :: Selector '[] (Id VZMACAddress)
randomLocallyAdministeredAddressSelector = mkSelector "randomLocallyAdministeredAddress"

-- | @Selector@ for @string@
stringSelector :: Selector '[] (Id NSString)
stringSelector = mkSelector "string"

-- | @Selector@ for @isBroadcastAddress@
isBroadcastAddressSelector :: Selector '[] Bool
isBroadcastAddressSelector = mkSelector "isBroadcastAddress"

-- | @Selector@ for @isMulticastAddress@
isMulticastAddressSelector :: Selector '[] Bool
isMulticastAddressSelector = mkSelector "isMulticastAddress"

-- | @Selector@ for @isUnicastAddress@
isUnicastAddressSelector :: Selector '[] Bool
isUnicastAddressSelector = mkSelector "isUnicastAddress"

-- | @Selector@ for @isLocallyAdministeredAddress@
isLocallyAdministeredAddressSelector :: Selector '[] Bool
isLocallyAdministeredAddressSelector = mkSelector "isLocallyAdministeredAddress"

-- | @Selector@ for @isUniversallyAdministeredAddress@
isUniversallyAdministeredAddressSelector :: Selector '[] Bool
isUniversallyAdministeredAddressSelector = mkSelector "isUniversallyAdministeredAddress"

