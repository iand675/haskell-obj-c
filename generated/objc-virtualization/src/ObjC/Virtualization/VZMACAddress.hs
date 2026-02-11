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
  , newSelector
  , initSelector
  , initWithStringSelector
  , randomLocallyAdministeredAddressSelector
  , stringSelector
  , isBroadcastAddressSelector
  , isMulticastAddressSelector
  , isUnicastAddressSelector
  , isLocallyAdministeredAddressSelector
  , isUniversallyAdministeredAddressSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZMACAddress)
new  =
  do
    cls' <- getRequiredClass "VZMACAddress"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO (Id VZMACAddress)
init_ vzmacAddress  =
  sendMsg vzmacAddress (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithString vzmacAddress  string =
withObjCPtr string $ \raw_string ->
    sendMsg vzmacAddress (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | Create a valid, random, unicast, locally administered address.
--
-- The generated address is not guaranteed to be unique.
--
-- ObjC selector: @+ randomLocallyAdministeredAddress@
randomLocallyAdministeredAddress :: IO (Id VZMACAddress)
randomLocallyAdministeredAddress  =
  do
    cls' <- getRequiredClass "VZMACAddress"
    sendClassMsg cls' (mkSelector "randomLocallyAdministeredAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The address represented as a string.
--
-- The 6 bytes are represented in hexadecimal form, separated by a colon character.    Alphabetical characters are lowercase.
--
-- The address is compatible with the parameter of -[VZMACAddress initWithString:].
--
-- ObjC selector: @- string@
string :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO (Id NSString)
string vzmacAddress  =
  sendMsg vzmacAddress (mkSelector "string") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | True if the address is the broadcast address, false otherwise.
--
-- ObjC selector: @- isBroadcastAddress@
isBroadcastAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isBroadcastAddress vzmacAddress  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzmacAddress (mkSelector "isBroadcastAddress") retCULong []

-- | True if the address is a multicast address, false otherwise.
--
-- ObjC selector: @- isMulticastAddress@
isMulticastAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isMulticastAddress vzmacAddress  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzmacAddress (mkSelector "isMulticastAddress") retCULong []

-- | True if the address is a unicast address, false otherwise.
--
-- ObjC selector: @- isUnicastAddress@
isUnicastAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isUnicastAddress vzmacAddress  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzmacAddress (mkSelector "isUnicastAddress") retCULong []

-- | True if the address is a locally administered addresses (LAA), false otherwise.
--
-- ObjC selector: @- isLocallyAdministeredAddress@
isLocallyAdministeredAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isLocallyAdministeredAddress vzmacAddress  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzmacAddress (mkSelector "isLocallyAdministeredAddress") retCULong []

-- | True if the address is a universally administered addresses (UAA), false otherwise.
--
-- ObjC selector: @- isUniversallyAdministeredAddress@
isUniversallyAdministeredAddress :: IsVZMACAddress vzmacAddress => vzmacAddress -> IO Bool
isUniversallyAdministeredAddress vzmacAddress  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzmacAddress (mkSelector "isUniversallyAdministeredAddress") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @randomLocallyAdministeredAddress@
randomLocallyAdministeredAddressSelector :: Selector
randomLocallyAdministeredAddressSelector = mkSelector "randomLocallyAdministeredAddress"

-- | @Selector@ for @string@
stringSelector :: Selector
stringSelector = mkSelector "string"

-- | @Selector@ for @isBroadcastAddress@
isBroadcastAddressSelector :: Selector
isBroadcastAddressSelector = mkSelector "isBroadcastAddress"

-- | @Selector@ for @isMulticastAddress@
isMulticastAddressSelector :: Selector
isMulticastAddressSelector = mkSelector "isMulticastAddress"

-- | @Selector@ for @isUnicastAddress@
isUnicastAddressSelector :: Selector
isUnicastAddressSelector = mkSelector "isUnicastAddress"

-- | @Selector@ for @isLocallyAdministeredAddress@
isLocallyAdministeredAddressSelector :: Selector
isLocallyAdministeredAddressSelector = mkSelector "isLocallyAdministeredAddress"

-- | @Selector@ for @isUniversallyAdministeredAddress@
isUniversallyAdministeredAddressSelector :: Selector
isUniversallyAdministeredAddressSelector = mkSelector "isUniversallyAdministeredAddress"

