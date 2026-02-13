{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppProxyTCPFlow
--
-- This file declares the NEAppProxyTCPFlow API. The NEAppProxyTCPFlow API is used by NEAppProxyProvider implementations to proxy the payload of TCP connections.
--
-- Generated bindings for @NWEndpoint@.
module ObjC.NetworkExtension.NWEndpoint
  ( NWEndpoint
  , IsNWEndpoint(..)


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

