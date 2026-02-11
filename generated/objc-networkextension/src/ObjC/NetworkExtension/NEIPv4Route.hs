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
  , initWithDestinationAddress_subnetMaskSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationAddress:subnetMask:@
initWithDestinationAddress_subnetMaskSelector :: Selector
initWithDestinationAddress_subnetMaskSelector = mkSelector "initWithDestinationAddress:subnetMask:"

