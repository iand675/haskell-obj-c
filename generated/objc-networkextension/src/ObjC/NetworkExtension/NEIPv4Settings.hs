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
  , initWithAddresses_subnetMasksSelector
  , settingsWithAutomaticAddressingSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAddresses:subnetMasks:@
initWithAddresses_subnetMasksSelector :: Selector
initWithAddresses_subnetMasksSelector = mkSelector "initWithAddresses:subnetMasks:"

-- | @Selector@ for @settingsWithAutomaticAddressing@
settingsWithAutomaticAddressingSelector :: Selector
settingsWithAutomaticAddressingSelector = mkSelector "settingsWithAutomaticAddressing"

