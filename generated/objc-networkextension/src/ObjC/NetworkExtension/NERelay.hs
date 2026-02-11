{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NERelay
--
-- The NERelay class declares the programmatic interface of an object that 			manages the details of a relay's configuration, such as authentication and URL details.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NERelay@.
module ObjC.NetworkExtension.NERelay
  ( NERelay
  , IsNERelay(..)
  , httP3RelayURL
  , setHTTP3RelayURL
  , httP2RelayURL
  , setHTTP2RelayURL
  , dnsOverHTTPSURL
  , setDnsOverHTTPSURL
  , syntheticDNSAnswerIPv4Prefix
  , setSyntheticDNSAnswerIPv4Prefix
  , syntheticDNSAnswerIPv6Prefix
  , setSyntheticDNSAnswerIPv6Prefix
  , additionalHTTPHeaderFields
  , setAdditionalHTTPHeaderFields
  , rawPublicKeys
  , setRawPublicKeys
  , identityData
  , setIdentityData
  , identityDataPassword
  , setIdentityDataPassword
  , httP3RelayURLSelector
  , setHTTP3RelayURLSelector
  , httP2RelayURLSelector
  , setHTTP2RelayURLSelector
  , dnsOverHTTPSURLSelector
  , setDnsOverHTTPSURLSelector
  , syntheticDNSAnswerIPv4PrefixSelector
  , setSyntheticDNSAnswerIPv4PrefixSelector
  , syntheticDNSAnswerIPv6PrefixSelector
  , setSyntheticDNSAnswerIPv6PrefixSelector
  , additionalHTTPHeaderFieldsSelector
  , setAdditionalHTTPHeaderFieldsSelector
  , rawPublicKeysSelector
  , setRawPublicKeysSelector
  , identityDataSelector
  , setIdentityDataSelector
  , identityDataPasswordSelector
  , setIdentityDataPasswordSelector


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

-- | HTTP3RelayURL
--
-- The URL of the relay accessible over HTTP/3.
--
-- ObjC selector: @- HTTP3RelayURL@
httP3RelayURL :: IsNERelay neRelay => neRelay -> IO (Id NSURL)
httP3RelayURL neRelay  =
  sendMsg neRelay (mkSelector "HTTP3RelayURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTP3RelayURL
--
-- The URL of the relay accessible over HTTP/3.
--
-- ObjC selector: @- setHTTP3RelayURL:@
setHTTP3RelayURL :: (IsNERelay neRelay, IsNSURL value) => neRelay -> value -> IO ()
setHTTP3RelayURL neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setHTTP3RelayURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | HTTP2RelayURL
--
-- The URL of the relay accessible over HTTP/2.
--
-- ObjC selector: @- HTTP2RelayURL@
httP2RelayURL :: IsNERelay neRelay => neRelay -> IO (Id NSURL)
httP2RelayURL neRelay  =
  sendMsg neRelay (mkSelector "HTTP2RelayURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTP2RelayURL
--
-- The URL of the relay accessible over HTTP/2.
--
-- ObjC selector: @- setHTTP2RelayURL:@
setHTTP2RelayURL :: (IsNERelay neRelay, IsNSURL value) => neRelay -> value -> IO ()
setHTTP2RelayURL neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setHTTP2RelayURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | dnsOverHTTPSURL
--
-- The URL of a DNS-over-HTTPS (DoH) resolver accessible via the relay.
--
-- ObjC selector: @- dnsOverHTTPSURL@
dnsOverHTTPSURL :: IsNERelay neRelay => neRelay -> IO (Id NSURL)
dnsOverHTTPSURL neRelay  =
  sendMsg neRelay (mkSelector "dnsOverHTTPSURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dnsOverHTTPSURL
--
-- The URL of a DNS-over-HTTPS (DoH) resolver accessible via the relay.
--
-- ObjC selector: @- setDnsOverHTTPSURL:@
setDnsOverHTTPSURL :: (IsNERelay neRelay, IsNSURL value) => neRelay -> value -> IO ()
setDnsOverHTTPSURL neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setDnsOverHTTPSURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | syntheticDNSAnswerIPv4Prefix
--
-- An IPv4 address prefix (such as "192.0.2.0/24") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- syntheticDNSAnswerIPv4Prefix@
syntheticDNSAnswerIPv4Prefix :: IsNERelay neRelay => neRelay -> IO (Id NSString)
syntheticDNSAnswerIPv4Prefix neRelay  =
  sendMsg neRelay (mkSelector "syntheticDNSAnswerIPv4Prefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | syntheticDNSAnswerIPv4Prefix
--
-- An IPv4 address prefix (such as "192.0.2.0/24") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- setSyntheticDNSAnswerIPv4Prefix:@
setSyntheticDNSAnswerIPv4Prefix :: (IsNERelay neRelay, IsNSString value) => neRelay -> value -> IO ()
setSyntheticDNSAnswerIPv4Prefix neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setSyntheticDNSAnswerIPv4Prefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | syntheticDNSAnswerIPv6Prefix
--
-- An IPv6 address prefix (such as "2001:DB8::/32") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- syntheticDNSAnswerIPv6Prefix@
syntheticDNSAnswerIPv6Prefix :: IsNERelay neRelay => neRelay -> IO (Id NSString)
syntheticDNSAnswerIPv6Prefix neRelay  =
  sendMsg neRelay (mkSelector "syntheticDNSAnswerIPv6Prefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | syntheticDNSAnswerIPv6Prefix
--
-- An IPv6 address prefix (such as "2001:DB8::/32") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- setSyntheticDNSAnswerIPv6Prefix:@
setSyntheticDNSAnswerIPv6Prefix :: (IsNERelay neRelay, IsNSString value) => neRelay -> value -> IO ()
setSyntheticDNSAnswerIPv6Prefix neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setSyntheticDNSAnswerIPv6Prefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | additionalHTTPHeaderFields
--
-- Additional HTTP header field names and values to be added to all relay requests.
--
-- ObjC selector: @- additionalHTTPHeaderFields@
additionalHTTPHeaderFields :: IsNERelay neRelay => neRelay -> IO (Id NSDictionary)
additionalHTTPHeaderFields neRelay  =
  sendMsg neRelay (mkSelector "additionalHTTPHeaderFields") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | additionalHTTPHeaderFields
--
-- Additional HTTP header field names and values to be added to all relay requests.
--
-- ObjC selector: @- setAdditionalHTTPHeaderFields:@
setAdditionalHTTPHeaderFields :: (IsNERelay neRelay, IsNSDictionary value) => neRelay -> value -> IO ()
setAdditionalHTTPHeaderFields neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setAdditionalHTTPHeaderFields:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | rawPublicKeys
--
-- TLS 1.3 raw public keys to use to authenticate the relay servers.
--
-- ObjC selector: @- rawPublicKeys@
rawPublicKeys :: IsNERelay neRelay => neRelay -> IO (Id NSArray)
rawPublicKeys neRelay  =
  sendMsg neRelay (mkSelector "rawPublicKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rawPublicKeys
--
-- TLS 1.3 raw public keys to use to authenticate the relay servers.
--
-- ObjC selector: @- setRawPublicKeys:@
setRawPublicKeys :: (IsNERelay neRelay, IsNSArray value) => neRelay -> value -> IO ()
setRawPublicKeys neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setRawPublicKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | identityData
--
-- The PKCS12 data for the relay client authentication. The value is a NSData in PKCS12 format.
--
-- ObjC selector: @- identityData@
identityData :: IsNERelay neRelay => neRelay -> IO (Id NSData)
identityData neRelay  =
  sendMsg neRelay (mkSelector "identityData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identityData
--
-- The PKCS12 data for the relay client authentication. The value is a NSData in PKCS12 format.
--
-- ObjC selector: @- setIdentityData:@
setIdentityData :: (IsNERelay neRelay, IsNSData value) => neRelay -> value -> IO ()
setIdentityData neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setIdentityData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | identityDataPassword
--
-- The password to be used to decrypt the PKCS12 identity data.
--
-- ObjC selector: @- identityDataPassword@
identityDataPassword :: IsNERelay neRelay => neRelay -> IO (Id NSString)
identityDataPassword neRelay  =
  sendMsg neRelay (mkSelector "identityDataPassword") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identityDataPassword
--
-- The password to be used to decrypt the PKCS12 identity data.
--
-- ObjC selector: @- setIdentityDataPassword:@
setIdentityDataPassword :: (IsNERelay neRelay, IsNSString value) => neRelay -> value -> IO ()
setIdentityDataPassword neRelay  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelay (mkSelector "setIdentityDataPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @HTTP3RelayURL@
httP3RelayURLSelector :: Selector
httP3RelayURLSelector = mkSelector "HTTP3RelayURL"

-- | @Selector@ for @setHTTP3RelayURL:@
setHTTP3RelayURLSelector :: Selector
setHTTP3RelayURLSelector = mkSelector "setHTTP3RelayURL:"

-- | @Selector@ for @HTTP2RelayURL@
httP2RelayURLSelector :: Selector
httP2RelayURLSelector = mkSelector "HTTP2RelayURL"

-- | @Selector@ for @setHTTP2RelayURL:@
setHTTP2RelayURLSelector :: Selector
setHTTP2RelayURLSelector = mkSelector "setHTTP2RelayURL:"

-- | @Selector@ for @dnsOverHTTPSURL@
dnsOverHTTPSURLSelector :: Selector
dnsOverHTTPSURLSelector = mkSelector "dnsOverHTTPSURL"

-- | @Selector@ for @setDnsOverHTTPSURL:@
setDnsOverHTTPSURLSelector :: Selector
setDnsOverHTTPSURLSelector = mkSelector "setDnsOverHTTPSURL:"

-- | @Selector@ for @syntheticDNSAnswerIPv4Prefix@
syntheticDNSAnswerIPv4PrefixSelector :: Selector
syntheticDNSAnswerIPv4PrefixSelector = mkSelector "syntheticDNSAnswerIPv4Prefix"

-- | @Selector@ for @setSyntheticDNSAnswerIPv4Prefix:@
setSyntheticDNSAnswerIPv4PrefixSelector :: Selector
setSyntheticDNSAnswerIPv4PrefixSelector = mkSelector "setSyntheticDNSAnswerIPv4Prefix:"

-- | @Selector@ for @syntheticDNSAnswerIPv6Prefix@
syntheticDNSAnswerIPv6PrefixSelector :: Selector
syntheticDNSAnswerIPv6PrefixSelector = mkSelector "syntheticDNSAnswerIPv6Prefix"

-- | @Selector@ for @setSyntheticDNSAnswerIPv6Prefix:@
setSyntheticDNSAnswerIPv6PrefixSelector :: Selector
setSyntheticDNSAnswerIPv6PrefixSelector = mkSelector "setSyntheticDNSAnswerIPv6Prefix:"

-- | @Selector@ for @additionalHTTPHeaderFields@
additionalHTTPHeaderFieldsSelector :: Selector
additionalHTTPHeaderFieldsSelector = mkSelector "additionalHTTPHeaderFields"

-- | @Selector@ for @setAdditionalHTTPHeaderFields:@
setAdditionalHTTPHeaderFieldsSelector :: Selector
setAdditionalHTTPHeaderFieldsSelector = mkSelector "setAdditionalHTTPHeaderFields:"

-- | @Selector@ for @rawPublicKeys@
rawPublicKeysSelector :: Selector
rawPublicKeysSelector = mkSelector "rawPublicKeys"

-- | @Selector@ for @setRawPublicKeys:@
setRawPublicKeysSelector :: Selector
setRawPublicKeysSelector = mkSelector "setRawPublicKeys:"

-- | @Selector@ for @identityData@
identityDataSelector :: Selector
identityDataSelector = mkSelector "identityData"

-- | @Selector@ for @setIdentityData:@
setIdentityDataSelector :: Selector
setIdentityDataSelector = mkSelector "setIdentityData:"

-- | @Selector@ for @identityDataPassword@
identityDataPasswordSelector :: Selector
identityDataPasswordSelector = mkSelector "identityDataPassword"

-- | @Selector@ for @setIdentityDataPassword:@
setIdentityDataPasswordSelector :: Selector
setIdentityDataPasswordSelector = mkSelector "setIdentityDataPassword:"

