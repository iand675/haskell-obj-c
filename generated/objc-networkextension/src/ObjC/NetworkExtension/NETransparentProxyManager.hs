{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETransparentProxyManager
--
-- The NETransparentProxyManager class declares the programmatic interface for an object that is used to configure and control transparent proxies provided by NEAppProxyProviders.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETransparentProxyManager@.
module ObjC.NetworkExtension.NETransparentProxyManager
  ( NETransparentProxyManager
  , IsNETransparentProxyManager(..)


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

