{-# LANGUAGE DataKinds #-}
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
  , additionalHTTPHeaderFieldsSelector
  , dnsOverHTTPSURLSelector
  , httP2RelayURLSelector
  , httP3RelayURLSelector
  , identityDataPasswordSelector
  , identityDataSelector
  , rawPublicKeysSelector
  , setAdditionalHTTPHeaderFieldsSelector
  , setDnsOverHTTPSURLSelector
  , setHTTP2RelayURLSelector
  , setHTTP3RelayURLSelector
  , setIdentityDataPasswordSelector
  , setIdentityDataSelector
  , setRawPublicKeysSelector
  , setSyntheticDNSAnswerIPv4PrefixSelector
  , setSyntheticDNSAnswerIPv6PrefixSelector
  , syntheticDNSAnswerIPv4PrefixSelector
  , syntheticDNSAnswerIPv6PrefixSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
httP3RelayURL neRelay =
  sendMessage neRelay httP3RelayURLSelector

-- | HTTP3RelayURL
--
-- The URL of the relay accessible over HTTP/3.
--
-- ObjC selector: @- setHTTP3RelayURL:@
setHTTP3RelayURL :: (IsNERelay neRelay, IsNSURL value) => neRelay -> value -> IO ()
setHTTP3RelayURL neRelay value =
  sendMessage neRelay setHTTP3RelayURLSelector (toNSURL value)

-- | HTTP2RelayURL
--
-- The URL of the relay accessible over HTTP/2.
--
-- ObjC selector: @- HTTP2RelayURL@
httP2RelayURL :: IsNERelay neRelay => neRelay -> IO (Id NSURL)
httP2RelayURL neRelay =
  sendMessage neRelay httP2RelayURLSelector

-- | HTTP2RelayURL
--
-- The URL of the relay accessible over HTTP/2.
--
-- ObjC selector: @- setHTTP2RelayURL:@
setHTTP2RelayURL :: (IsNERelay neRelay, IsNSURL value) => neRelay -> value -> IO ()
setHTTP2RelayURL neRelay value =
  sendMessage neRelay setHTTP2RelayURLSelector (toNSURL value)

-- | dnsOverHTTPSURL
--
-- The URL of a DNS-over-HTTPS (DoH) resolver accessible via the relay.
--
-- ObjC selector: @- dnsOverHTTPSURL@
dnsOverHTTPSURL :: IsNERelay neRelay => neRelay -> IO (Id NSURL)
dnsOverHTTPSURL neRelay =
  sendMessage neRelay dnsOverHTTPSURLSelector

-- | dnsOverHTTPSURL
--
-- The URL of a DNS-over-HTTPS (DoH) resolver accessible via the relay.
--
-- ObjC selector: @- setDnsOverHTTPSURL:@
setDnsOverHTTPSURL :: (IsNERelay neRelay, IsNSURL value) => neRelay -> value -> IO ()
setDnsOverHTTPSURL neRelay value =
  sendMessage neRelay setDnsOverHTTPSURLSelector (toNSURL value)

-- | syntheticDNSAnswerIPv4Prefix
--
-- An IPv4 address prefix (such as "192.0.2.0/24") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- syntheticDNSAnswerIPv4Prefix@
syntheticDNSAnswerIPv4Prefix :: IsNERelay neRelay => neRelay -> IO (Id NSString)
syntheticDNSAnswerIPv4Prefix neRelay =
  sendMessage neRelay syntheticDNSAnswerIPv4PrefixSelector

-- | syntheticDNSAnswerIPv4Prefix
--
-- An IPv4 address prefix (such as "192.0.2.0/24") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- setSyntheticDNSAnswerIPv4Prefix:@
setSyntheticDNSAnswerIPv4Prefix :: (IsNERelay neRelay, IsNSString value) => neRelay -> value -> IO ()
setSyntheticDNSAnswerIPv4Prefix neRelay value =
  sendMessage neRelay setSyntheticDNSAnswerIPv4PrefixSelector (toNSString value)

-- | syntheticDNSAnswerIPv6Prefix
--
-- An IPv6 address prefix (such as "2001:DB8::/32") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- syntheticDNSAnswerIPv6Prefix@
syntheticDNSAnswerIPv6Prefix :: IsNERelay neRelay => neRelay -> IO (Id NSString)
syntheticDNSAnswerIPv6Prefix neRelay =
  sendMessage neRelay syntheticDNSAnswerIPv6PrefixSelector

-- | syntheticDNSAnswerIPv6Prefix
--
-- An IPv6 address prefix (such as "2001:DB8::/32") that will be used to synthesize      DNS answers for apps that use @getaddrinfo()@ to resolve domains included in @matchDomains@
--
-- ObjC selector: @- setSyntheticDNSAnswerIPv6Prefix:@
setSyntheticDNSAnswerIPv6Prefix :: (IsNERelay neRelay, IsNSString value) => neRelay -> value -> IO ()
setSyntheticDNSAnswerIPv6Prefix neRelay value =
  sendMessage neRelay setSyntheticDNSAnswerIPv6PrefixSelector (toNSString value)

-- | additionalHTTPHeaderFields
--
-- Additional HTTP header field names and values to be added to all relay requests.
--
-- ObjC selector: @- additionalHTTPHeaderFields@
additionalHTTPHeaderFields :: IsNERelay neRelay => neRelay -> IO (Id NSDictionary)
additionalHTTPHeaderFields neRelay =
  sendMessage neRelay additionalHTTPHeaderFieldsSelector

-- | additionalHTTPHeaderFields
--
-- Additional HTTP header field names and values to be added to all relay requests.
--
-- ObjC selector: @- setAdditionalHTTPHeaderFields:@
setAdditionalHTTPHeaderFields :: (IsNERelay neRelay, IsNSDictionary value) => neRelay -> value -> IO ()
setAdditionalHTTPHeaderFields neRelay value =
  sendMessage neRelay setAdditionalHTTPHeaderFieldsSelector (toNSDictionary value)

-- | rawPublicKeys
--
-- TLS 1.3 raw public keys to use to authenticate the relay servers.
--
-- ObjC selector: @- rawPublicKeys@
rawPublicKeys :: IsNERelay neRelay => neRelay -> IO (Id NSArray)
rawPublicKeys neRelay =
  sendMessage neRelay rawPublicKeysSelector

-- | rawPublicKeys
--
-- TLS 1.3 raw public keys to use to authenticate the relay servers.
--
-- ObjC selector: @- setRawPublicKeys:@
setRawPublicKeys :: (IsNERelay neRelay, IsNSArray value) => neRelay -> value -> IO ()
setRawPublicKeys neRelay value =
  sendMessage neRelay setRawPublicKeysSelector (toNSArray value)

-- | identityData
--
-- The PKCS12 data for the relay client authentication. The value is a NSData in PKCS12 format.
--
-- ObjC selector: @- identityData@
identityData :: IsNERelay neRelay => neRelay -> IO (Id NSData)
identityData neRelay =
  sendMessage neRelay identityDataSelector

-- | identityData
--
-- The PKCS12 data for the relay client authentication. The value is a NSData in PKCS12 format.
--
-- ObjC selector: @- setIdentityData:@
setIdentityData :: (IsNERelay neRelay, IsNSData value) => neRelay -> value -> IO ()
setIdentityData neRelay value =
  sendMessage neRelay setIdentityDataSelector (toNSData value)

-- | identityDataPassword
--
-- The password to be used to decrypt the PKCS12 identity data.
--
-- ObjC selector: @- identityDataPassword@
identityDataPassword :: IsNERelay neRelay => neRelay -> IO (Id NSString)
identityDataPassword neRelay =
  sendMessage neRelay identityDataPasswordSelector

-- | identityDataPassword
--
-- The password to be used to decrypt the PKCS12 identity data.
--
-- ObjC selector: @- setIdentityDataPassword:@
setIdentityDataPassword :: (IsNERelay neRelay, IsNSString value) => neRelay -> value -> IO ()
setIdentityDataPassword neRelay value =
  sendMessage neRelay setIdentityDataPasswordSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @HTTP3RelayURL@
httP3RelayURLSelector :: Selector '[] (Id NSURL)
httP3RelayURLSelector = mkSelector "HTTP3RelayURL"

-- | @Selector@ for @setHTTP3RelayURL:@
setHTTP3RelayURLSelector :: Selector '[Id NSURL] ()
setHTTP3RelayURLSelector = mkSelector "setHTTP3RelayURL:"

-- | @Selector@ for @HTTP2RelayURL@
httP2RelayURLSelector :: Selector '[] (Id NSURL)
httP2RelayURLSelector = mkSelector "HTTP2RelayURL"

-- | @Selector@ for @setHTTP2RelayURL:@
setHTTP2RelayURLSelector :: Selector '[Id NSURL] ()
setHTTP2RelayURLSelector = mkSelector "setHTTP2RelayURL:"

-- | @Selector@ for @dnsOverHTTPSURL@
dnsOverHTTPSURLSelector :: Selector '[] (Id NSURL)
dnsOverHTTPSURLSelector = mkSelector "dnsOverHTTPSURL"

-- | @Selector@ for @setDnsOverHTTPSURL:@
setDnsOverHTTPSURLSelector :: Selector '[Id NSURL] ()
setDnsOverHTTPSURLSelector = mkSelector "setDnsOverHTTPSURL:"

-- | @Selector@ for @syntheticDNSAnswerIPv4Prefix@
syntheticDNSAnswerIPv4PrefixSelector :: Selector '[] (Id NSString)
syntheticDNSAnswerIPv4PrefixSelector = mkSelector "syntheticDNSAnswerIPv4Prefix"

-- | @Selector@ for @setSyntheticDNSAnswerIPv4Prefix:@
setSyntheticDNSAnswerIPv4PrefixSelector :: Selector '[Id NSString] ()
setSyntheticDNSAnswerIPv4PrefixSelector = mkSelector "setSyntheticDNSAnswerIPv4Prefix:"

-- | @Selector@ for @syntheticDNSAnswerIPv6Prefix@
syntheticDNSAnswerIPv6PrefixSelector :: Selector '[] (Id NSString)
syntheticDNSAnswerIPv6PrefixSelector = mkSelector "syntheticDNSAnswerIPv6Prefix"

-- | @Selector@ for @setSyntheticDNSAnswerIPv6Prefix:@
setSyntheticDNSAnswerIPv6PrefixSelector :: Selector '[Id NSString] ()
setSyntheticDNSAnswerIPv6PrefixSelector = mkSelector "setSyntheticDNSAnswerIPv6Prefix:"

-- | @Selector@ for @additionalHTTPHeaderFields@
additionalHTTPHeaderFieldsSelector :: Selector '[] (Id NSDictionary)
additionalHTTPHeaderFieldsSelector = mkSelector "additionalHTTPHeaderFields"

-- | @Selector@ for @setAdditionalHTTPHeaderFields:@
setAdditionalHTTPHeaderFieldsSelector :: Selector '[Id NSDictionary] ()
setAdditionalHTTPHeaderFieldsSelector = mkSelector "setAdditionalHTTPHeaderFields:"

-- | @Selector@ for @rawPublicKeys@
rawPublicKeysSelector :: Selector '[] (Id NSArray)
rawPublicKeysSelector = mkSelector "rawPublicKeys"

-- | @Selector@ for @setRawPublicKeys:@
setRawPublicKeysSelector :: Selector '[Id NSArray] ()
setRawPublicKeysSelector = mkSelector "setRawPublicKeys:"

-- | @Selector@ for @identityData@
identityDataSelector :: Selector '[] (Id NSData)
identityDataSelector = mkSelector "identityData"

-- | @Selector@ for @setIdentityData:@
setIdentityDataSelector :: Selector '[Id NSData] ()
setIdentityDataSelector = mkSelector "setIdentityData:"

-- | @Selector@ for @identityDataPassword@
identityDataPasswordSelector :: Selector '[] (Id NSString)
identityDataPasswordSelector = mkSelector "identityDataPassword"

-- | @Selector@ for @setIdentityDataPassword:@
setIdentityDataPasswordSelector :: Selector '[Id NSString] ()
setIdentityDataPasswordSelector = mkSelector "setIdentityDataPassword:"

