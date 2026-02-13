{-# LANGUAGE DataKinds #-}
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
  , maximumSSLProtocolVersionSelector
  , minimumSSLProtocolVersionSelector
  , setMaximumSSLProtocolVersionSelector
  , setMinimumSSLProtocolVersionSelector
  , setSSLCipherSuitesSelector
  , setTLSSessionIDSelector
  , sslCipherSuitesSelector
  , tlsSessionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
tlsSessionID nwtlsParameters =
  sendMessage nwtlsParameters tlsSessionIDSelector

-- | TLSSessionID
--
-- The session ID for the associated connection, used for TLS session resumption.		This property is optional when using TLS.
--
-- ObjC selector: @- setTLSSessionID:@
setTLSSessionID :: (IsNWTLSParameters nwtlsParameters, IsNSData value) => nwtlsParameters -> value -> IO ()
setTLSSessionID nwtlsParameters value =
  sendMessage nwtlsParameters setTLSSessionIDSelector (toNSData value)

-- | SSLCipherSuites
--
-- The set of allowed cipher suites, as defined in <Security/CipherSuite.h>.		If set to nil, the default cipher suites will be used.
--
-- ObjC selector: @- SSLCipherSuites@
sslCipherSuites :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO (Id NSSet)
sslCipherSuites nwtlsParameters =
  sendMessage nwtlsParameters sslCipherSuitesSelector

-- | SSLCipherSuites
--
-- The set of allowed cipher suites, as defined in <Security/CipherSuite.h>.		If set to nil, the default cipher suites will be used.
--
-- ObjC selector: @- setSSLCipherSuites:@
setSSLCipherSuites :: (IsNWTLSParameters nwtlsParameters, IsNSSet value) => nwtlsParameters -> value -> IO ()
setSSLCipherSuites nwtlsParameters value =
  sendMessage nwtlsParameters setSSLCipherSuitesSelector (toNSSet value)

-- | minimumSSLProtocolVersion
--
-- The minimum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version older than the minimum.
--
-- ObjC selector: @- minimumSSLProtocolVersion@
minimumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO CULong
minimumSSLProtocolVersion nwtlsParameters =
  sendMessage nwtlsParameters minimumSSLProtocolVersionSelector

-- | minimumSSLProtocolVersion
--
-- The minimum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version older than the minimum.
--
-- ObjC selector: @- setMinimumSSLProtocolVersion:@
setMinimumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> CULong -> IO ()
setMinimumSSLProtocolVersion nwtlsParameters value =
  sendMessage nwtlsParameters setMinimumSSLProtocolVersionSelector value

-- | maximumSSLProtocolVersion
--
-- The maximum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version newer than the maximum.		This property should be used with caution, since it may limit the use of preferred		SSL protocols.
--
-- ObjC selector: @- maximumSSLProtocolVersion@
maximumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> IO CULong
maximumSSLProtocolVersion nwtlsParameters =
  sendMessage nwtlsParameters maximumSSLProtocolVersionSelector

-- | maximumSSLProtocolVersion
--
-- The maximum allowed SSLProtocol value. as defined in <Security/SecureTransport.h>.		If set, the SSL handshake will not accept any protocol version newer than the maximum.		This property should be used with caution, since it may limit the use of preferred		SSL protocols.
--
-- ObjC selector: @- setMaximumSSLProtocolVersion:@
setMaximumSSLProtocolVersion :: IsNWTLSParameters nwtlsParameters => nwtlsParameters -> CULong -> IO ()
setMaximumSSLProtocolVersion nwtlsParameters value =
  sendMessage nwtlsParameters setMaximumSSLProtocolVersionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @TLSSessionID@
tlsSessionIDSelector :: Selector '[] (Id NSData)
tlsSessionIDSelector = mkSelector "TLSSessionID"

-- | @Selector@ for @setTLSSessionID:@
setTLSSessionIDSelector :: Selector '[Id NSData] ()
setTLSSessionIDSelector = mkSelector "setTLSSessionID:"

-- | @Selector@ for @SSLCipherSuites@
sslCipherSuitesSelector :: Selector '[] (Id NSSet)
sslCipherSuitesSelector = mkSelector "SSLCipherSuites"

-- | @Selector@ for @setSSLCipherSuites:@
setSSLCipherSuitesSelector :: Selector '[Id NSSet] ()
setSSLCipherSuitesSelector = mkSelector "setSSLCipherSuites:"

-- | @Selector@ for @minimumSSLProtocolVersion@
minimumSSLProtocolVersionSelector :: Selector '[] CULong
minimumSSLProtocolVersionSelector = mkSelector "minimumSSLProtocolVersion"

-- | @Selector@ for @setMinimumSSLProtocolVersion:@
setMinimumSSLProtocolVersionSelector :: Selector '[CULong] ()
setMinimumSSLProtocolVersionSelector = mkSelector "setMinimumSSLProtocolVersion:"

-- | @Selector@ for @maximumSSLProtocolVersion@
maximumSSLProtocolVersionSelector :: Selector '[] CULong
maximumSSLProtocolVersionSelector = mkSelector "maximumSSLProtocolVersion"

-- | @Selector@ for @setMaximumSSLProtocolVersion:@
setMaximumSSLProtocolVersionSelector :: Selector '[CULong] ()
setMaximumSSLProtocolVersionSelector = mkSelector "setMaximumSSLProtocolVersion:"

