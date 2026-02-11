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
  , tlsSessionID
  , setTLSSessionID
  , sslCipherSuites
  , setSSLCipherSuites
  , minimumSSLProtocolVersion
  , setMinimumSSLProtocolVersion
  , maximumSSLProtocolVersion
  , setMaximumSSLProtocolVersion
  , tlsSessionIDSelector
  , setTLSSessionIDSelector
  , sslCipherSuitesSelector
  , setSSLCipherSuitesSelector
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

-- | TLSSessionID
--
-- The session ID for the associated connection, used for TLS session resumption.		This property is optional when using TLS.
--
-- ObjC selector: @- TLSSessionID@
tlsSessionID :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO (Id NSData)
tlsSessionID nwtlsParameters  =
    sendMsg nwtlsParameters (mkSelector "TLSSessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | TLSSessionID
--
-- The session ID for the associated connection, used for TLS session resumption.		This property is optional when using TLS.
--
-- ObjC selector: @- setTLSSessionID:@
setTLSSessionID :: (IsNWTLSParameters nwtlsParameters, IsNSData value) => nwtlsParameters -> value -> IO ()
setTLSSessionID nwtlsParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nwtlsParameters (mkSelector "setTLSSessionID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | SSLCipherSuites
--
-- The set of allowed cipher suites, as defined in <Security/CipherSuite.h>.		If set to nil, the default cipher suites will be used.
--
-- ObjC selector: @- SSLCipherSuites@
sslCipherSuites :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO (Id NSSet)
sslCipherSuites nwtlsParameters  =
    sendMsg nwtlsParameters (mkSelector "SSLCipherSuites") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SSLCipherSuites
--
-- The set of allowed cipher suites, as defined in <Security/CipherSuite.h>.		If set to nil, the default cipher suites will be used.
--
-- ObjC selector: @- setSSLCipherSuites:@
setSSLCipherSuites :: (IsNWTLSParameters nwtlsParameters, IsNSSet value) => nwtlsParameters -> value -> IO ()
setSSLCipherSuites nwtlsParameters  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nwtlsParameters (mkSelector "setSSLCipherSuites:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
    sendMsg nwtlsParameters (mkSelector "setMinimumSSLProtocolVersion:") retVoid [argCULong value]

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
    sendMsg nwtlsParameters (mkSelector "setMaximumSSLProtocolVersion:") retVoid [argCULong value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @TLSSessionID@
tlsSessionIDSelector :: Selector
tlsSessionIDSelector = mkSelector "TLSSessionID"

-- | @Selector@ for @setTLSSessionID:@
setTLSSessionIDSelector :: Selector
setTLSSessionIDSelector = mkSelector "setTLSSessionID:"

-- | @Selector@ for @SSLCipherSuites@
sslCipherSuitesSelector :: Selector
sslCipherSuitesSelector = mkSelector "SSLCipherSuites"

-- | @Selector@ for @setSSLCipherSuites:@
setSSLCipherSuitesSelector :: Selector
setSSLCipherSuitesSelector = mkSelector "setSSLCipherSuites:"

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

