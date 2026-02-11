{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelProviderManager
--
-- The NETunnelProviderManager class declares the programmatic interface for an object that is used to configure and control network tunnels provided by NETunnelProviders.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETunnelProviderManager@.
module ObjC.NetworkExtension.NETunnelProviderManager
  ( NETunnelProviderManager
  , IsNETunnelProviderManager(..)
  , forPerAppVPN
  , copyAppRules
  , routingMethod
  , forPerAppVPNSelector
  , copyAppRulesSelector
  , routingMethodSelector

  -- * Enum types
  , NETunnelProviderRoutingMethod(NETunnelProviderRoutingMethod)
  , pattern NETunnelProviderRoutingMethodDestinationIP
  , pattern NETunnelProviderRoutingMethodSourceApplication
  , pattern NETunnelProviderRoutingMethodNetworkRule

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
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | forPerAppVPN
--
-- Create a NETunnelProviderManager instance that is used to manage a per-app VPN configuration.
--
-- ObjC selector: @+ forPerAppVPN@
forPerAppVPN :: IO (Id NETunnelProviderManager)
forPerAppVPN  =
  do
    cls' <- getRequiredClass "NETunnelProviderManager"
    sendClassMsg cls' (mkSelector "forPerAppVPN") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | copyAppRules
--
-- This function returns an array of NEAppRule objects.
--
-- ObjC selector: @- copyAppRules@
copyAppRules :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
copyAppRules neTunnelProviderManager  =
  sendMsg neTunnelProviderManager (mkSelector "copyAppRules") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | routingMethod
--
-- The method by which network traffic is routed to the tunnel. The default is NETunnelProviderRoutingMethodDestinationIP.
--
-- ObjC selector: @- routingMethod@
routingMethod :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO NETunnelProviderRoutingMethod
routingMethod neTunnelProviderManager  =
  fmap (coerce :: CLong -> NETunnelProviderRoutingMethod) $ sendMsg neTunnelProviderManager (mkSelector "routingMethod") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forPerAppVPN@
forPerAppVPNSelector :: Selector
forPerAppVPNSelector = mkSelector "forPerAppVPN"

-- | @Selector@ for @copyAppRules@
copyAppRulesSelector :: Selector
copyAppRulesSelector = mkSelector "copyAppRules"

-- | @Selector@ for @routingMethod@
routingMethodSelector :: Selector
routingMethodSelector = mkSelector "routingMethod"

