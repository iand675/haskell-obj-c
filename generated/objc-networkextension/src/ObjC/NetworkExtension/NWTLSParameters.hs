{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DEPRECATION NOTICE
--
-- NW object wrappers are hidden in Swift 6. To continue accessing them, you can prepend double underscores to the symbol name.
--
-- Generated bindings for @NWTLSParameters@.
module ObjC.NetworkExtension.NWTLSParameters
  ( NWTLSParameters
  , IsNWTLSParameters(..)
  , minimumSSLProtocolVersion
  , setMinimumSSLProtocolVersion
  , maximumSSLProtocolVersion
  , setMaximumSSLProtocolVersion
  , minimumSSLProtocolVersionSelector
  , setMinimumSSLProtocolVersionSelector
  , maximumSSLProtocolVersionSelector
  , setMaximumSSLProtocolVersionSelector


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

-- | minimumSSLProtocolVersion
--
-- The minimum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version older than the minimum.
--
-- ObjC selector: @- minimumSSLProtocolVersion@
minimumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO CULong
minimumSSLProtocolVersion nwtlsParameters  =
  sendMsg nwtlsParameters (mkSelector "minimumSSLProtocolVersion") retCULong []

-- | minimumSSLProtocolVersion
--
-- The minimum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version older than the minimum.
--
-- ObjC selector: @- setMinimumSSLProtocolVersion:@
setMinimumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> CULong -> IO ()
setMinimumSSLProtocolVersion nwtlsParameters  value =
  sendMsg nwtlsParameters (mkSelector "setMinimumSSLProtocolVersion:") retVoid [argCULong (fromIntegral value)]

-- | maximumSSLProtocolVersion
--
-- The maximum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version newer than the maximum.		This property should be used with caution, since it may limit the use of preferred		SSL protocols.
--
-- ObjC selector: @- maximumSSLProtocolVersion@
maximumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO CULong
maximumSSLProtocolVersion nwtlsParameters  =
  sendMsg nwtlsParameters (mkSelector "maximumSSLProtocolVersion") retCULong []

-- | maximumSSLProtocolVersion
--
-- The maximum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version newer than the maximum.		This property should be used with caution, since it may limit the use of preferred		SSL protocols.
--
-- ObjC selector: @- setMaximumSSLProtocolVersion:@
setMaximumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> CULong -> IO ()
setMaximumSSLProtocolVersion nwtlsParameters  value =
  sendMsg nwtlsParameters (mkSelector "setMaximumSSLProtocolVersion:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumSSLProtocolVersion@
minimumSSLProtocolVersionSelector :: Selector
minimumSSLProtocolVersionSelector = mkSelector "minimumSSLProtocolVersion"

-- | @Selector@ for @setMinimumSSLProtocolVersion:@
setMinimumSSLProtocolVersionSelector :: Selector
setMinimumSSLProtocolVersionSelector = mkSelector "setMinimumSSLProtocolVersion:"

-- | @Selector@ for @maximumSSLProtocolVersion@
maximumSSLProtocolVersionSelector :: Selector
maximumSSLProtocolVersionSelector = mkSelector "maximumSSLProtocolVersion"

-- | @Selector@ for @setMaximumSSLProtocolVersion:@
setMaximumSSLProtocolVersionSelector :: Selector
setMaximumSSLProtocolVersionSelector = mkSelector "setMaximumSSLProtocolVersion:"

