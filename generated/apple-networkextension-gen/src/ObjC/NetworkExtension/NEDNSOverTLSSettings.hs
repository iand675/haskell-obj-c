{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NEDNSOverTLSSettings@.
module ObjC.NetworkExtension.NEDNSOverTLSSettings
  ( NEDNSOverTLSSettings
  , IsNEDNSOverTLSSettings(..)
  , serverName
  , setServerName
  , identityReference
  , setIdentityReference
  , identityReferenceSelector
  , serverNameSelector
  , setIdentityReferenceSelector
  , setServerNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | serverName
--
-- The name of the server to use for TLS certificate validation.
--
-- ObjC selector: @- serverName@
serverName :: IsNEDNSOverTLSSettings nednsOverTLSSettings => nednsOverTLSSettings -> IO (Id NSString)
serverName nednsOverTLSSettings =
  sendMessage nednsOverTLSSettings serverNameSelector

-- | serverName
--
-- The name of the server to use for TLS certificate validation.
--
-- ObjC selector: @- setServerName:@
setServerName :: (IsNEDNSOverTLSSettings nednsOverTLSSettings, IsNSString value) => nednsOverTLSSettings -> value -> IO ()
setServerName nednsOverTLSSettings value =
  sendMessage nednsOverTLSSettings setServerNameSelector (toNSString value)

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- identityReference@
identityReference :: IsNEDNSOverTLSSettings nednsOverTLSSettings => nednsOverTLSSettings -> IO (Id NSData)
identityReference nednsOverTLSSettings =
  sendMessage nednsOverTLSSettings identityReferenceSelector

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- setIdentityReference:@
setIdentityReference :: (IsNEDNSOverTLSSettings nednsOverTLSSettings, IsNSData value) => nednsOverTLSSettings -> value -> IO ()
setIdentityReference nednsOverTLSSettings value =
  sendMessage nednsOverTLSSettings setIdentityReferenceSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serverName@
serverNameSelector :: Selector '[] (Id NSString)
serverNameSelector = mkSelector "serverName"

-- | @Selector@ for @setServerName:@
setServerNameSelector :: Selector '[Id NSString] ()
setServerNameSelector = mkSelector "setServerName:"

-- | @Selector@ for @identityReference@
identityReferenceSelector :: Selector '[] (Id NSData)
identityReferenceSelector = mkSelector "identityReference"

-- | @Selector@ for @setIdentityReference:@
setIdentityReferenceSelector :: Selector '[Id NSData] ()
setIdentityReferenceSelector = mkSelector "setIdentityReference:"

