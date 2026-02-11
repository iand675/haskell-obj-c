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
  , initWithHTTPAuthorizationHeadersSelector
  , initWithHTTPResponse_httpBodySelector
  , httpAuthorizationHeadersSelector
  , setHttpAuthorizationHeadersSelector
  , httpResponseSelector
  , setHttpResponseSelector
  , httpBodySelector
  , setHttpBodySelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Authorization succeeded with an authorization tokens stored in HTTP headers.
--
-- ObjC selector: @- initWithHTTPAuthorizationHeaders:@
initWithHTTPAuthorizationHeaders :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSDictionary httpAuthorizationHeaders) => asAuthorizationProviderExtensionAuthorizationResult -> httpAuthorizationHeaders -> IO (Id ASAuthorizationProviderExtensionAuthorizationResult)
initWithHTTPAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationResult  httpAuthorizationHeaders =
withObjCPtr httpAuthorizationHeaders $ \raw_httpAuthorizationHeaders ->
    sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "initWithHTTPAuthorizationHeaders:") (retPtr retVoid) [argPtr (castPtr raw_httpAuthorizationHeaders :: Ptr ())] >>= ownedObject . castPtr

-- | Authorization succeeded with a HTTP response.
--
-- ObjC selector: @- initWithHTTPResponse:httpBody:@
initWithHTTPResponse_httpBody :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSHTTPURLResponse httpResponse, IsNSData httpBody) => asAuthorizationProviderExtensionAuthorizationResult -> httpResponse -> httpBody -> IO (Id ASAuthorizationProviderExtensionAuthorizationResult)
initWithHTTPResponse_httpBody asAuthorizationProviderExtensionAuthorizationResult  httpResponse httpBody =
withObjCPtr httpResponse $ \raw_httpResponse ->
  withObjCPtr httpBody $ \raw_httpBody ->
      sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "initWithHTTPResponse:httpBody:") (retPtr retVoid) [argPtr (castPtr raw_httpResponse :: Ptr ()), argPtr (castPtr raw_httpBody :: Ptr ())] >>= ownedObject . castPtr

-- | HTTP extra headers for addition with credentials.
--
-- ObjC selector: @- httpAuthorizationHeaders@
httpAuthorizationHeaders :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSDictionary)
httpAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationResult  =
  sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "httpAuthorizationHeaders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTP extra headers for addition with credentials.
--
-- ObjC selector: @- setHttpAuthorizationHeaders:@
setHttpAuthorizationHeaders :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSDictionary value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setHttpAuthorizationHeaders asAuthorizationProviderExtensionAuthorizationResult  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "setHttpAuthorizationHeaders:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | HTTP response for OAUth and SAML based authentications.
--
-- ObjC selector: @- httpResponse@
httpResponse :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSHTTPURLResponse)
httpResponse asAuthorizationProviderExtensionAuthorizationResult  =
  sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "httpResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTP response for OAUth and SAML based authentications.
--
-- ObjC selector: @- setHttpResponse:@
setHttpResponse :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSHTTPURLResponse value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setHttpResponse asAuthorizationProviderExtensionAuthorizationResult  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "setHttpResponse:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | HTTP response body for OAUth and SAML based authentications.
--
-- ObjC selector: @- httpBody@
httpBody :: IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult => asAuthorizationProviderExtensionAuthorizationResult -> IO (Id NSData)
httpBody asAuthorizationProviderExtensionAuthorizationResult  =
  sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "httpBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | HTTP response body for OAUth and SAML based authentications.
--
-- ObjC selector: @- setHttpBody:@
setHttpBody :: (IsASAuthorizationProviderExtensionAuthorizationResult asAuthorizationProviderExtensionAuthorizationResult, IsNSData value) => asAuthorizationProviderExtensionAuthorizationResult -> value -> IO ()
setHttpBody asAuthorizationProviderExtensionAuthorizationResult  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionAuthorizationResult (mkSelector "setHttpBody:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHTTPAuthorizationHeaders:@
initWithHTTPAuthorizationHeadersSelector :: Selector
initWithHTTPAuthorizationHeadersSelector = mkSelector "initWithHTTPAuthorizationHeaders:"

-- | @Selector@ for @initWithHTTPResponse:httpBody:@
initWithHTTPResponse_httpBodySelector :: Selector
initWithHTTPResponse_httpBodySelector = mkSelector "initWithHTTPResponse:httpBody:"

-- | @Selector@ for @httpAuthorizationHeaders@
httpAuthorizationHeadersSelector :: Selector
httpAuthorizationHeadersSelector = mkSelector "httpAuthorizationHeaders"

-- | @Selector@ for @setHttpAuthorizationHeaders:@
setHttpAuthorizationHeadersSelector :: Selector
setHttpAuthorizationHeadersSelector = mkSelector "setHttpAuthorizationHeaders:"

-- | @Selector@ for @httpResponse@
httpResponseSelector :: Selector
httpResponseSelector = mkSelector "httpResponse"

-- | @Selector@ for @setHttpResponse:@
setHttpResponseSelector :: Selector
setHttpResponseSelector = mkSelector "setHttpResponse:"

-- | @Selector@ for @httpBody@
httpBodySelector :: Selector
httpBodySelector = mkSelector "httpBody"

-- | @Selector@ for @setHttpBody:@
setHttpBodySelector :: Selector
setHttpBodySelector = mkSelector "setHttpBody:"

