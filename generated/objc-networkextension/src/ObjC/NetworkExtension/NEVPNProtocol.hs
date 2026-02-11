{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelProvider.h
--
-- This file declares the NETunnelProvider API. The NETunnelProvider API is used to implement Network Extension providers that provide network tunneling services.
--
-- This API is part of NetworkExtension.framework
--
-- Generated bindings for @NEVPNProtocol@.
module ObjC.NetworkExtension.NEVPNProtocol
  ( NEVPNProtocol
  , IsNEVPNProtocol(..)


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

