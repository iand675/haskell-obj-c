{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationProviderExtensionAuthorizationResult@.
module ObjC.AuthenticationServices.ASAuthorizationProviderExtensionAuthorizationResult
  ( ASAuthorizationProviderExtensionAuthorizationResult
  , IsASAuthorizationProviderExtensionAuthorizationResult(..)
  , initWithHTTPAuthorizationHeaders
  , initWithHTTPResponse_httpBody
  , httpAuthorizationHeaders
  , setHttpAuthorizationHeaders
  , httpResponse
  , setHttpResponse
  , httpBody
  , setHttpBody
  , privateKeys
  , setPrivateKeys
  , httpAuthorizationHeadersSelector
  , httpBodySelector
  , httpResponseSelector
  , initWithHTTPAuthorizationHeadersSelector
  , initWithHTTPResponse_httpBodySelector
  , privateKeysSelector
  , setHttpAuthorizationHeadersSelector
  , setHttpBodySelector
  , setHttpResponseSelector
  , setPrivateKeysSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Authorization succeeded with an authorization tokens stored in HTTP headers.
--
-- ObjC selector: @- initWithHTTPAuthorizationHeaders:@
initWithHTTPAuthorizationHeaders :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSDictionary httpAuthorizationHeaders) => asAuthorizationProviderExtensionAuthorizationResult -> httpAuthorizationHeaders -> IO (Id ASAuthorizationProviderExtensionAuthorizationResult)
initWithHTTPAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationResult httpAuthorizationHeaders =
  sendOwnedMessage asAuthorizationProviderExtensionAuthorizationResult initWithHTTPAuthorizationHeadersSelector (toNSDictionary httpAuthorizationHeaders)

-- | Authorization succeeded with a HTTP response.
--
-- ObjC selector: @- initWithHTTPResponse:httpBody:@
initWithHTTPResponse_httpBody :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSHTTPURLResponse httpResponse, IsNSData httpBody) => asAuthorizationProviderExtensionAuthorizationResult -> httpResponse -> httpBody -> IO (Id ASAuthorizationProviderExtensionAuthorizationResult)
initWithHTTPResponse_httpBody asAuthorizationProviderExtensionAuthorizationResult httpResponse httpBody =
  sendOwnedMessage asAuthorizationProviderExtensionAuthorizationResult initWithHTTPResponse_httpBodySelector (toNSHTTPURLResponse httpResponse) (toNSData httpBody)

-- | HTTP extra headers for addition with credentials.
--
-- ObjC selector: @- httpAuthorizationHeaders@
httpAuthorizationHeaders :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSDictionary)
httpAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationResult =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult httpAuthorizationHeadersSelector

-- | HTTP extra headers for addition with credentials.
--
-- ObjC selector: @- setHttpAuthorizationHeaders:@
setHttpAuthorizationHeaders :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSDictionary value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setHttpAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationResult value =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult setHttpAuthorizationHeadersSelector (toNSDictionary value)

-- | HTTP response for OAUth and SAML based authentications.
--
-- ObjC selector: @- httpResponse@
httpResponse :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSHTTPURLResponse)
httpResponse asAuthorizationProviderExtensionAuthorizationResult =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult httpResponseSelector

-- | HTTP response for OAUth and SAML based authentications.
--
-- ObjC selector: @- setHttpResponse:@
setHttpResponse :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSHTTPURLResponse value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setHttpResponse asAuthorizationProviderExtensionAuthorizationResult value =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult setHttpResponseSelector (toNSHTTPURLResponse value)

-- | HTTP response body for OAUth and SAML based authentications.
--
-- ObjC selector: @- httpBody@
httpBody :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSData)
httpBody asAuthorizationProviderExtensionAuthorizationResult =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult httpBodySelector

-- | HTTP response body for OAUth and SAML based authentications.
--
-- ObjC selector: @- setHttpBody:@
setHttpBody :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSData value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setHttpBody asAuthorizationProviderExtensionAuthorizationResult value =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult setHttpBodySelector (toNSData value)

-- | Private SecKeys.
--
-- ObjC selector: @- privateKeys@
privateKeys :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSArray)
privateKeys asAuthorizationProviderExtensionAuthorizationResult =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult privateKeysSelector

-- | Private SecKeys.
--
-- ObjC selector: @- setPrivateKeys:@
setPrivateKeys :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSArray value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setPrivateKeys asAuthorizationProviderExtensionAuthorizationResult value =
  sendMessage asAuthorizationProviderExtensionAuthorizationResult setPrivateKeysSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHTTPAuthorizationHeaders:@
initWithHTTPAuthorizationHeadersSelector :: Selector '[Id NSDictionary] (Id ASAuthorizationProviderExtensionAuthorizationResult)
initWithHTTPAuthorizationHeadersSelector = mkSelector "initWithHTTPAuthorizationHeaders:"

-- | @Selector@ for @initWithHTTPResponse:httpBody:@
initWithHTTPResponse_httpBodySelector :: Selector '[Id NSHTTPURLResponse, Id NSData] (Id ASAuthorizationProviderExtensionAuthorizationResult)
initWithHTTPResponse_httpBodySelector = mkSelector "initWithHTTPResponse:httpBody:"

-- | @Selector@ for @httpAuthorizationHeaders@
httpAuthorizationHeadersSelector :: Selector '[] (Id NSDictionary)
httpAuthorizationHeadersSelector = mkSelector "httpAuthorizationHeaders"

-- | @Selector@ for @setHttpAuthorizationHeaders:@
setHttpAuthorizationHeadersSelector :: Selector '[Id NSDictionary] ()
setHttpAuthorizationHeadersSelector = mkSelector "setHttpAuthorizationHeaders:"

-- | @Selector@ for @httpResponse@
httpResponseSelector :: Selector '[] (Id NSHTTPURLResponse)
httpResponseSelector = mkSelector "httpResponse"

-- | @Selector@ for @setHttpResponse:@
setHttpResponseSelector :: Selector '[Id NSHTTPURLResponse] ()
setHttpResponseSelector = mkSelector "setHttpResponse:"

-- | @Selector@ for @httpBody@
httpBodySelector :: Selector '[] (Id NSData)
httpBodySelector = mkSelector "httpBody"

-- | @Selector@ for @setHttpBody:@
setHttpBodySelector :: Selector '[Id NSData] ()
setHttpBodySelector = mkSelector "setHttpBody:"

-- | @Selector@ for @privateKeys@
privateKeysSelector :: Selector '[] (Id NSArray)
privateKeysSelector = mkSelector "privateKeys"

-- | @Selector@ for @setPrivateKeys:@
setPrivateKeysSelector :: Selector '[Id NSArray] ()
setPrivateKeysSelector = mkSelector "setPrivateKeys:"

