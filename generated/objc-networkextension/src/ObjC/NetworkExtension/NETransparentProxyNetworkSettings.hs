{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETransparentProxyNetworkSettings
--
-- The NETransparentProxyNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NETransparentProxyNetworkSettings is used by NEAppProxyProviders to communicate the desired network settings for the proxy to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETransparentProxyNetworkSettings@.
module ObjC.NetworkExtension.NETransparentProxyNetworkSettings
  ( NETransparentProxyNetworkSettings
  , IsNETransparentProxyNetworkSettings(..)


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

