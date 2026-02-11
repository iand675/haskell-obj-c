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

