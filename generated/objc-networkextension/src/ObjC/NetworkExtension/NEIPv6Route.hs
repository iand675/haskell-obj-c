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
  , initWithDestinationAddress_networkPrefixLengthSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDestinationAddress:networkPrefixLength:@
initWithDestinationAddress_networkPrefixLengthSelector :: Selector
initWithDestinationAddress_networkPrefixLengthSelector = mkSelector "initWithDestinationAddress:networkPrefixLength:"

