{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKSecurityOrigin object contains information about a security origin.
--
-- An instance of this class is a transient, data-only object; it does not uniquely identify a security origin across multiple delegate method calls.
--
-- Generated bindings for @WKSecurityOrigin@.
module ObjC.WebKit.WKSecurityOrigin
  ( WKSecurityOrigin
  , IsWKSecurityOrigin(..)
  , init_
  , protocol
  , host
  , port
  , hostSelector
  , initSelector
  , portSelector
  , protocolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO (Id WKSecurityOrigin)
init_ wkSecurityOrigin =
  sendOwnedMessage wkSecurityOrigin initSelector

-- | The security origin's protocol.
--
-- ObjC selector: @- protocol@
protocol :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO (Id NSString)
protocol wkSecurityOrigin =
  sendMessage wkSecurityOrigin protocolSelector

-- | The security origin's host.
--
-- ObjC selector: @- host@
host :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO (Id NSString)
host wkSecurityOrigin =
  sendMessage wkSecurityOrigin hostSelector

-- | The security origin's port.
--
-- ObjC selector: @- port@
port :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO CLong
port wkSecurityOrigin =
  sendMessage wkSecurityOrigin portSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKSecurityOrigin)
initSelector = mkSelector "init"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] (Id NSString)
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @host@
hostSelector :: Selector '[] (Id NSString)
hostSelector = mkSelector "host"

-- | @Selector@ for @port@
portSelector :: Selector '[] CLong
portSelector = mkSelector "port"

