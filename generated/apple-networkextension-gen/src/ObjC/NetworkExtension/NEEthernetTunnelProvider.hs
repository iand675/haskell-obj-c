{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEEthernetTunnelProvider
--
-- The NEEthernetTunnelProvider class declares the programmatic interface of an object that implements the client side of a custom link-layer packet tunneling protocol.
--
-- NEEthernetTunnelProvider is part of NetworkExtension.framework.
--
-- Generated bindings for @NEEthernetTunnelProvider@.
module ObjC.NetworkExtension.NEEthernetTunnelProvider
  ( NEEthernetTunnelProvider
  , IsNEEthernetTunnelProvider(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

