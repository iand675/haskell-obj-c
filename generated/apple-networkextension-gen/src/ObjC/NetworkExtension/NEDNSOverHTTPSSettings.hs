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
  , serverURLSelector
  , setServerURLSelector
  , identityReferenceSelector
  , setIdentityReferenceSelector


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

-- | serverURL
--
-- The URL to which to make DNS-over-HTTPS requests. The format should be an HTTPS URL with the path indicating the location of the DNS-over-HTTPS server, such as: "https://dnsserver.example.net/dns-query".
--
-- ObjC selector: @- serverURL@
serverURL :: IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings => nednsOverHTTPSSettings -> IO (Id NSURL)
serverURL nednsOverHTTPSSettings  =
    sendMsg nednsOverHTTPSSettings (mkSelector "serverURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverURL
--
-- The URL to which to make DNS-over-HTTPS requests. The format should be an HTTPS URL with the path indicating the location of the DNS-over-HTTPS server, such as: "https://dnsserver.example.net/dns-query".
--
-- ObjC selector: @- setServerURL:@
setServerURL :: (IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings, IsNSURL value) => nednsOverHTTPSSettings -> value -> IO ()
setServerURL nednsOverHTTPSSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsOverHTTPSSettings (mkSelector "setServerURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- identityReference@
identityReference :: IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings => nednsOverHTTPSSettings -> IO (Id NSData)
identityReference nednsOverHTTPSSettings  =
    sendMsg nednsOverHTTPSSettings (mkSelector "identityReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identityReference
--
-- The optional certificate identity keychain reference to use as a TLS client certificate.
--
-- ObjC selector: @- setIdentityReference:@
setIdentityReference :: (IsNEDNSOverHTTPSSettings nednsOverHTTPSSettings, IsNSData value) => nednsOverHTTPSSettings -> value -> IO ()
setIdentityReference nednsOverHTTPSSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsOverHTTPSSettings (mkSelector "setIdentityReference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @serverURL@
serverURLSelector :: Selector
serverURLSelector = mkSelector "serverURL"

-- | @Selector@ for @setServerURL:@
setServerURLSelector :: Selector
setServerURLSelector = mkSelector "setServerURL:"

-- | @Selector@ for @identityReference@
identityReferenceSelector :: Selector
identityReferenceSelector = mkSelector "identityReference"

-- | @Selector@ for @setIdentityReference:@
setIdentityReferenceSelector :: Selector
setIdentityReferenceSelector = mkSelector "setIdentityReference:"

