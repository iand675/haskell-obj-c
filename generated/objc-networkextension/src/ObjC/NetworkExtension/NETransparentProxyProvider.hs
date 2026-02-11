{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETransparentProxyProvider
--
-- The NETransparentProxyProvider class declares the programmatic interface for an object that implements the client side of a custom transparent network proxy solution.     The NETransparentProxyProvider class has the following behavior differences from its super class NEAppProxyProvider:         - Returning NO from handleNewFlow: and handleNewUDPFlow:initialRemoteEndpoint: causes the flow to proceed to communicate directly with the flow's ultimate destination, instead of closing the flow with a "Connection Refused" error.         - NEDNSSettings and NEProxySettings specified within NETransparentProxyNetworkSettings are ignored. Flows that match the includedNetworkRules within NETransparentProxyNetworkSettings will use the same DNS and proxy settings that other flows on the system are currently using.         - Flows that are created using a "connect by name" API (such as Network.framework or NSURLSession) that match the includedNetworkRules will not bypass DNS resolution.
--
-- NETransparentProxyProvider is part of NetworkExtension.framework
--
-- Generated bindings for @NETransparentProxyProvider@.
module ObjC.NetworkExtension.NETransparentProxyProvider
  ( NETransparentProxyProvider
  , IsNETransparentProxyProvider(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

