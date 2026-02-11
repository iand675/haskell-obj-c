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
  , initSelector
  , protocolSelector
  , hostSelector
  , portSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO (Id WKSecurityOrigin)
init_ wkSecurityOrigin  =
  sendMsg wkSecurityOrigin (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The security origin's protocol.
--
-- ObjC selector: @- protocol@
protocol :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO (Id NSString)
protocol wkSecurityOrigin  =
  sendMsg wkSecurityOrigin (mkSelector "protocol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The security origin's host.
--
-- ObjC selector: @- host@
host :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO (Id NSString)
host wkSecurityOrigin  =
  sendMsg wkSecurityOrigin (mkSelector "host") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The security origin's port.
--
-- ObjC selector: @- port@
port :: IsWKSecurityOrigin wkSecurityOrigin => wkSecurityOrigin -> IO CLong
port wkSecurityOrigin  =
  sendMsg wkSecurityOrigin (mkSelector "port") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @host@
hostSelector :: Selector
hostSelector = mkSelector "host"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

