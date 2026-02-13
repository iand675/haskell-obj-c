{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NEDNSOverHTTPSSettings@.
module ObjC.NetworkExtension.NEDNSOverHTTPSSettings
  ( NEDNSOverHTTPSSettings
  , IsNEDNSOverHTTPSSettings(..)
  , serverURL
  , setServerURL
  , identityReference
  , setIdentityReference
  , identityReferenceSelector
  , serverURLSelector
  , setIdentityReferenceSelector
  , setServerURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | serverURL
--
-- The URL to which to make DNS-over-HTTPS requests. The format should be an HTTPS URL with the path indicating the location of the DNS-over-HTTPS server, such as: "https://dnsserver.example.net/dns-query".
--
-- ObjC selector: @- serverURL@
serverURL :: IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings => nednsOverHTTPSSettings -> IO (Id NSURL)
serverURL nednsOverHTTPSSettings =
  sendMessage nednsOverHTTPSSettings serverURLSelector

-- | serverURL
--
-- The URL to which to make DNS-over-HTTPS requests. The format should be an HTTPS URL with the path indicating the location of the DNS-over-HTTPS server, such as: "https://dnsserver.example.net/dns-query".
--
-- ObjC selector: @- setServerURL:@
setServerURL :: (IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings, IsNSURL value) => nednsOverHTTPSSettings -> value -> IO ()
setServerURL nednsOverHTTPSSettings value =
  sendMessage nednsOverHTTPSSettings setServerURLSelector (toNSURL value)

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- identityReference@
identityReference :: IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings => nednsOverHTTPSSettings -> IO (Id NSData)
identityReference nednsOverHTTPSSettings =
  sendMessage nednsOverHTTPSSettings identityReferenceSelector

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- setIdentityReference:@
setIdentityReference :: (IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings, IsNSData value) => nednsOverHTTPSSettings -> value -> IO ()
setIdentityReference nednsOverHTTPSSettings value =
  sendMessage nednsOverHTTPSSettings setIdentityReferenceSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serverURL@
serverURLSelector :: Selector '[] (Id NSURL)
serverURLSelector = mkSelector "serverURL"

-- | @Selector@ for @setServerURL:@
setServerURLSelector :: Selector '[Id NSURL] ()
setServerURLSelector = mkSelector "setServerURL:"

-- | @Selector@ for @identityReference@
identityReferenceSelector :: Selector '[] (Id NSData)
identityReferenceSelector = mkSelector "identityReference"

-- | @Selector@ for @setIdentityReference:@
setIdentityReferenceSelector :: Selector '[Id NSData] ()
setIdentityReferenceSelector = mkSelector "setIdentityReference:"

