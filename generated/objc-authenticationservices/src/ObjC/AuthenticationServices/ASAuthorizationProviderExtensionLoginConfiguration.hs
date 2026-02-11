{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationProviderExtensionLoginConfiguration@.
module ObjC.AuthenticationServices.ASAuthorizationProviderExtensionLoginConfiguration
  ( ASAuthorizationProviderExtensionLoginConfiguration
  , IsASAuthorizationProviderExtensionLoginConfiguration(..)
  , new
  , init_
  , initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audience
  , configurationWithOpenIDConfigurationURL_clientID_issuer_completion
  , setCustomAssertionRequestHeaderClaims_returningError
  , setCustomAssertionRequestBodyClaims_returningError
  , setCustomLoginRequestHeaderClaims_returningError
  , setCustomLoginRequestBodyClaims_returningError
  , setCustomRefreshRequestHeaderClaims_returningError
  , setCustomRefreshRequestBodyClaims_returningError
  , setCustomKeyExchangeRequestHeaderClaims_returningError
  , setCustomKeyExchangeRequestBodyClaims_returningError
  , setCustomKeyRequestHeaderClaims_returningError
  , setCustomKeyRequestBodyClaims_returningError
  , invalidCredentialPredicate
  , setInvalidCredentialPredicate
  , accountDisplayName
  , setAccountDisplayName
  , clientID
  , issuer
  , audience
  , setAudience
  , tokenEndpointURL
  , setTokenEndpointURL
  , jwksEndpointURL
  , setJwksEndpointURL
  , userSecureEnclaveKeyBiometricPolicy
  , setUserSecureEnclaveKeyBiometricPolicy
  , nonceEndpointURL
  , setNonceEndpointURL
  , nonceResponseKeypath
  , setNonceResponseKeypath
  , serverNonceClaimName
  , setServerNonceClaimName
  , customNonceRequestValues
  , setCustomNonceRequestValues
  , additionalScopes
  , setAdditionalScopes
  , includePreviousRefreshTokenInLoginRequest
  , setIncludePreviousRefreshTokenInLoginRequest
  , previousRefreshTokenClaimName
  , setPreviousRefreshTokenClaimName
  , customLoginRequestValues
  , setCustomLoginRequestValues
  , kerberosTicketMappings
  , setKerberosTicketMappings
  , federationType
  , setFederationType
  , loginRequestEncryptionPublicKey
  , setLoginRequestEncryptionPublicKey
  , loginRequestEncryptionAlgorithm
  , setLoginRequestEncryptionAlgorithm
  , hpkeAuthPublicKey
  , setHpkeAuthPublicKey
  , newSelector
  , initSelector
  , initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector
  , configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector
  , setCustomAssertionRequestHeaderClaims_returningErrorSelector
  , setCustomAssertionRequestBodyClaims_returningErrorSelector
  , setCustomLoginRequestHeaderClaims_returningErrorSelector
  , setCustomLoginRequestBodyClaims_returningErrorSelector
  , setCustomRefreshRequestHeaderClaims_returningErrorSelector
  , setCustomRefreshRequestBodyClaims_returningErrorSelector
  , setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector
  , setCustomKeyExchangeRequestBodyClaims_returningErrorSelector
  , setCustomKeyRequestHeaderClaims_returningErrorSelector
  , setCustomKeyRequestBodyClaims_returningErrorSelector
  , invalidCredentialPredicateSelector
  , setInvalidCredentialPredicateSelector
  , accountDisplayNameSelector
  , setAccountDisplayNameSelector
  , clientIDSelector
  , issuerSelector
  , audienceSelector
  , setAudienceSelector
  , tokenEndpointURLSelector
  , setTokenEndpointURLSelector
  , jwksEndpointURLSelector
  , setJwksEndpointURLSelector
  , userSecureEnclaveKeyBiometricPolicySelector
  , setUserSecureEnclaveKeyBiometricPolicySelector
  , nonceEndpointURLSelector
  , setNonceEndpointURLSelector
  , nonceResponseKeypathSelector
  , setNonceResponseKeypathSelector
  , serverNonceClaimNameSelector
  , setServerNonceClaimNameSelector
  , customNonceRequestValuesSelector
  , setCustomNonceRequestValuesSelector
  , additionalScopesSelector
  , setAdditionalScopesSelector
  , includePreviousRefreshTokenInLoginRequestSelector
  , setIncludePreviousRefreshTokenInLoginRequestSelector
  , previousRefreshTokenClaimNameSelector
  , setPreviousRefreshTokenClaimNameSelector
  , customLoginRequestValuesSelector
  , setCustomLoginRequestValuesSelector
  , kerberosTicketMappingsSelector
  , setKerberosTicketMappingsSelector
  , federationTypeSelector
  , setFederationTypeSelector
  , loginRequestEncryptionPublicKeySelector
  , setLoginRequestEncryptionPublicKeySelector
  , loginRequestEncryptionAlgorithmSelector
  , setLoginRequestEncryptionAlgorithmSelector
  , hpkeAuthPublicKeySelector
  , setHpkeAuthPublicKeySelector

  -- * Enum types
  , ASAuthorizationProviderExtensionFederationType(ASAuthorizationProviderExtensionFederationType)
  , pattern ASAuthorizationProviderExtensionFederationTypeNone
  , pattern ASAuthorizationProviderExtensionFederationTypeWSTrust
  , pattern ASAuthorizationProviderExtensionFederationTypeDynamicWSTrust
  , ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy(ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy)
  , pattern ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyNone
  , pattern ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyTouchIDOrWatchCurrentSet
  , pattern ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyTouchIDOrWatchAny
  , pattern ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyReuseDuringUnlock
  , pattern ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyPasswordFallback

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
import ObjC.AuthenticationServices.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationProviderExtensionLoginConfiguration)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationProviderExtensionLoginConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id ASAuthorizationProviderExtensionLoginConfiguration)
init_ asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an ASAuthorizationProviderExtensionLoginConfiguration class with the required values.
--
-- @clientID@ — The client_id for the Apple platform SSO login at the identity provider.
--
-- @issuer@ — The issuer for the requests, used to validate responses.
--
-- @tokenEndpointURL@ — The token endpoint at the idP for login.
--
-- @jwksEndpointURL@ — The JWKS URL at the idP for validating tokens.
--
-- @audience@ — The audience used for signed assertions.  This should be the tenent at the idP.
--
-- Returns: An instance of a ASAuthorizationProviderExtensionLoginConfiguration.
--
-- ObjC selector: @- initWithClientID:issuer:tokenEndpointURL:jwksEndpointURL:audience:@
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audience :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString clientID, IsNSString issuer, IsNSURL tokenEndpointURL, IsNSURL jwksEndpointURL, IsNSString audience) => asAuthorizationProviderExtensionLoginConfiguration -> clientID -> issuer -> tokenEndpointURL -> jwksEndpointURL -> audience -> IO (Id ASAuthorizationProviderExtensionLoginConfiguration)
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audience asAuthorizationProviderExtensionLoginConfiguration  clientID issuer tokenEndpointURL jwksEndpointURL audience =
withObjCPtr clientID $ \raw_clientID ->
  withObjCPtr issuer $ \raw_issuer ->
    withObjCPtr tokenEndpointURL $ \raw_tokenEndpointURL ->
      withObjCPtr jwksEndpointURL $ \raw_jwksEndpointURL ->
        withObjCPtr audience $ \raw_audience ->
            sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "initWithClientID:issuer:tokenEndpointURL:jwksEndpointURL:audience:") (retPtr retVoid) [argPtr (castPtr raw_clientID :: Ptr ()), argPtr (castPtr raw_issuer :: Ptr ()), argPtr (castPtr raw_tokenEndpointURL :: Ptr ()), argPtr (castPtr raw_jwksEndpointURL :: Ptr ()), argPtr (castPtr raw_audience :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a login configuration using the OpenID configuration.
--
-- @openIDConfigurationURL@ — The base URL to load the .well-known/openid-configuration.
--
-- @clientID@ — The client_id for the Apple platform SSO login at the identity provider.
--
-- @issuer@ — The issuer for the requests, used to validate responses.
--
-- @completion@ — The completion called when it is complete or the error.
--
-- ObjC selector: @+ configurationWithOpenIDConfigurationURL:clientID:issuer:completion:@
configurationWithOpenIDConfigurationURL_clientID_issuer_completion :: (IsNSURL openIDConfigurationURL, IsNSString clientID, IsNSString issuer) => openIDConfigurationURL -> clientID -> issuer -> Ptr () -> IO ()
configurationWithOpenIDConfigurationURL_clientID_issuer_completion openIDConfigurationURL clientID issuer completion =
  do
    cls' <- getRequiredClass "ASAuthorizationProviderExtensionLoginConfiguration"
    withObjCPtr openIDConfigurationURL $ \raw_openIDConfigurationURL ->
      withObjCPtr clientID $ \raw_clientID ->
        withObjCPtr issuer $ \raw_issuer ->
          sendClassMsg cls' (mkSelector "configurationWithOpenIDConfigurationURL:clientID:issuer:completion:") retVoid [argPtr (castPtr raw_openIDConfigurationURL :: Ptr ()), argPtr (castPtr raw_clientID :: Ptr ()), argPtr (castPtr raw_issuer :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Sets custom claims to be added to the embedded assertion request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomAssertionRequestHeaderClaims:returningError:@
setCustomAssertionRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomAssertionRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomAssertionRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the embedded assertion request body.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomAssertionRequestBodyClaims:returningError:@
setCustomAssertionRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomAssertionRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomAssertionRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the login request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomLoginRequestHeaderClaims:returningError:@
setCustomLoginRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomLoginRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomLoginRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the login request body.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomLoginRequestBodyClaims:returningError:@
setCustomLoginRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomLoginRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomLoginRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the refresh request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomRefreshRequestHeaderClaims:returningError:@
setCustomRefreshRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomRefreshRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomRefreshRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the refresh request bode.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomRefreshRequestBodyClaims:returningError:@
setCustomRefreshRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomRefreshRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomRefreshRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the key exchange request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomKeyExchangeRequestHeaderClaims:returningError:@
setCustomKeyExchangeRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomKeyExchangeRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomKeyExchangeRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the key exchange request body.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomKeyExchangeRequestBodyClaims:returningError:@
setCustomKeyExchangeRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomKeyExchangeRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomKeyExchangeRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the key request header.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomKeyRequestHeaderClaims:returningError:@
setCustomKeyRequestHeaderClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomKeyRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomKeyRequestHeaderClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets custom claims to be added to the key request body.
--
-- @claims@ — The claims to be added. It must serialize as valid JSON to be accepted.
--
-- @error@ — Nil or an NSError indicating why the claims were rejected.
--
-- YES when successful and NO when claims are rejected.
--
-- ObjC selector: @- setCustomKeyRequestBodyClaims:returningError:@
setCustomKeyRequestBodyClaims_returningError :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSDictionary claims, IsNSError error_) => asAuthorizationProviderExtensionLoginConfiguration -> claims -> error_ -> IO Bool
setCustomKeyRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration  claims error_ =
withObjCPtr claims $ \raw_claims ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomKeyRequestBodyClaims:returningError:") retCULong [argPtr (castPtr raw_claims :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Predicate string used to identify invalid credential errors.
--
-- If there is an HTTP 400 or HTTP 401 error when authenticating, this predicate will be used on the response body JSON to determine if the error is due to an invalid password or something else.  If nil, then only an HTTP 401 will be used for an invalid credential.
--
-- ObjC selector: @- invalidCredentialPredicate@
invalidCredentialPredicate :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
invalidCredentialPredicate asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "invalidCredentialPredicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Predicate string used to identify invalid credential errors.
--
-- If there is an HTTP 400 or HTTP 401 error when authenticating, this predicate will be used on the response body JSON to determine if the error is due to an invalid password or something else.  If nil, then only an HTTP 401 will be used for an invalid credential.
--
-- ObjC selector: @- setInvalidCredentialPredicate:@
setInvalidCredentialPredicate :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setInvalidCredentialPredicate asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setInvalidCredentialPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The display name for the account.  Used for notifications and login prompts.
--
-- ObjC selector: @- accountDisplayName@
accountDisplayName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
accountDisplayName asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "accountDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The display name for the account.  Used for notifications and login prompts.
--
-- ObjC selector: @- setAccountDisplayName:@
setAccountDisplayName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAccountDisplayName asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setAccountDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The login client_id.
--
-- ObjC selector: @- clientID@
clientID :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
clientID asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "clientID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The issuer for validation.
--
-- ObjC selector: @- issuer@
issuer :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
issuer asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "issuer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The audience for validation and requests.
--
-- ObjC selector: @- audience@
audience :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
audience asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "audience") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The audience for validation and requests.
--
-- ObjC selector: @- setAudience:@
setAudience :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAudience asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setAudience:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Token Endpoint URL for login request.
--
-- ObjC selector: @- tokenEndpointURL@
tokenEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
tokenEndpointURL asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "tokenEndpointURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Token Endpoint URL for login request.
--
-- ObjC selector: @- setTokenEndpointURL:@
setTokenEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setTokenEndpointURL asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setTokenEndpointURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | JWKS Endpoint URL for keys.
--
-- ObjC selector: @- jwksEndpointURL@
jwksEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
jwksEndpointURL asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "jwksEndpointURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | JWKS Endpoint URL for keys.
--
-- ObjC selector: @- setJwksEndpointURL:@
setJwksEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setJwksEndpointURL asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setJwksEndpointURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The biometric policy for User Secure Enclave Key authentication.
--
-- ObjC selector: @- userSecureEnclaveKeyBiometricPolicy@
userSecureEnclaveKeyBiometricPolicy :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy
userSecureEnclaveKeyBiometricPolicy asAuthorizationProviderExtensionLoginConfiguration  =
  fmap (coerce :: CULong -> ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "userSecureEnclaveKeyBiometricPolicy") retCULong []

-- | The biometric policy for User Secure Enclave Key authentication.
--
-- ObjC selector: @- setUserSecureEnclaveKeyBiometricPolicy:@
setUserSecureEnclaveKeyBiometricPolicy :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy -> IO ()
setUserSecureEnclaveKeyBiometricPolicy asAuthorizationProviderExtensionLoginConfiguration  value =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setUserSecureEnclaveKeyBiometricPolicy:") retVoid [argCULong (coerce value)]

-- | Nonce Endpoint URL, defaults to token tokenEndpointURL.
--
-- ObjC selector: @- nonceEndpointURL@
nonceEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
nonceEndpointURL asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "nonceEndpointURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Nonce Endpoint URL, defaults to token tokenEndpointURL.
--
-- ObjC selector: @- setNonceEndpointURL:@
setNonceEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setNonceEndpointURL asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setNonceEndpointURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The keypath in the nonce response that contains the nonce value.
--
-- ObjC selector: @- nonceResponseKeypath@
nonceResponseKeypath :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
nonceResponseKeypath asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "nonceResponseKeypath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The keypath in the nonce response that contains the nonce value.
--
-- ObjC selector: @- setNonceResponseKeypath:@
setNonceResponseKeypath :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setNonceResponseKeypath asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setNonceResponseKeypath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The name of the server nonce claim when included in authentication requests.
--
-- ObjC selector: @- serverNonceClaimName@
serverNonceClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
serverNonceClaimName asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "serverNonceClaimName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the server nonce claim when included in authentication requests.
--
-- ObjC selector: @- setServerNonceClaimName:@
setServerNonceClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setServerNonceClaimName asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setServerNonceClaimName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Custom values added to the server nonce POST request body.
--
-- ObjC selector: @- customNonceRequestValues@
customNonceRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customNonceRequestValues asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "customNonceRequestValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Custom values added to the server nonce POST request body.
--
-- ObjC selector: @- setCustomNonceRequestValues:@
setCustomNonceRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomNonceRequestValues asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomNonceRequestValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Additional login scopes.
--
-- ObjC selector: @- additionalScopes@
additionalScopes :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
additionalScopes asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "additionalScopes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Additional login scopes.
--
-- ObjC selector: @- setAdditionalScopes:@
setAdditionalScopes :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAdditionalScopes asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setAdditionalScopes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If true and there is a refresh token for the user in the SSO tokens, it will be included in the login request.
--
-- ObjC selector: @- includePreviousRefreshTokenInLoginRequest@
includePreviousRefreshTokenInLoginRequest :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO Bool
includePreviousRefreshTokenInLoginRequest asAuthorizationProviderExtensionLoginConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "includePreviousRefreshTokenInLoginRequest") retCULong []

-- | If true and there is a refresh token for the user in the SSO tokens, it will be included in the login request.
--
-- ObjC selector: @- setIncludePreviousRefreshTokenInLoginRequest:@
setIncludePreviousRefreshTokenInLoginRequest :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> Bool -> IO ()
setIncludePreviousRefreshTokenInLoginRequest asAuthorizationProviderExtensionLoginConfiguration  value =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setIncludePreviousRefreshTokenInLoginRequest:") retVoid [argCULong (if value then 1 else 0)]

-- | The claim name for the previous SSO token value in the login request.
--
-- ObjC selector: @- previousRefreshTokenClaimName@
previousRefreshTokenClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
previousRefreshTokenClaimName asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "previousRefreshTokenClaimName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The claim name for the previous SSO token value in the login request.
--
-- ObjC selector: @- setPreviousRefreshTokenClaimName:@
setPreviousRefreshTokenClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setPreviousRefreshTokenClaimName asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setPreviousRefreshTokenClaimName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Custom values added to the login POST request body.
--
-- ObjC selector: @- customLoginRequestValues@
customLoginRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customLoginRequestValues asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "customLoginRequestValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Custom values added to the login POST request body.
--
-- ObjC selector: @- setCustomLoginRequestValues:@
setCustomLoginRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomLoginRequestValues asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setCustomLoginRequestValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Kerberos ticket mappings to use.
--
-- ObjC selector: @- kerberosTicketMappings@
kerberosTicketMappings :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
kerberosTicketMappings asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "kerberosTicketMappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Kerberos ticket mappings to use.
--
-- ObjC selector: @- setKerberosTicketMappings:@
setKerberosTicketMappings :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setKerberosTicketMappings asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setKerberosTicketMappings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The federation method to use.
--
-- ObjC selector: @- federationType@
federationType :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO ASAuthorizationProviderExtensionFederationType
federationType asAuthorizationProviderExtensionLoginConfiguration  =
  fmap (coerce :: CLong -> ASAuthorizationProviderExtensionFederationType) $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "federationType") retCLong []

-- | The federation method to use.
--
-- ObjC selector: @- setFederationType:@
setFederationType :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> ASAuthorizationProviderExtensionFederationType -> IO ()
setFederationType asAuthorizationProviderExtensionLoginConfiguration  value =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setFederationType:") retVoid [argCLong (coerce value)]

-- | The public key to use for encrypting the embedded login assertion.
--
-- Only applies to password authentication.  If set, the password will encrypted in an embedded assertion instead of the login request itself.
--
-- ObjC selector: @- loginRequestEncryptionPublicKey@
loginRequestEncryptionPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Ptr ())
loginRequestEncryptionPublicKey asAuthorizationProviderExtensionLoginConfiguration  =
  fmap castPtr $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "loginRequestEncryptionPublicKey") (retPtr retVoid) []

-- | The public key to use for encrypting the embedded login assertion.
--
-- Only applies to password authentication.  If set, the password will encrypted in an embedded assertion instead of the login request itself.
--
-- ObjC selector: @- setLoginRequestEncryptionPublicKey:@
setLoginRequestEncryptionPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> Ptr () -> IO ()
setLoginRequestEncryptionPublicKey asAuthorizationProviderExtensionLoginConfiguration  value =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setLoginRequestEncryptionPublicKey:") retVoid [argPtr value]

-- | The encryption algorithm to use for the embedded login assertion.
--
-- ObjC selector: @- loginRequestEncryptionAlgorithm@
loginRequestEncryptionAlgorithm :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSNumber)
loginRequestEncryptionAlgorithm asAuthorizationProviderExtensionLoginConfiguration  =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "loginRequestEncryptionAlgorithm") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The encryption algorithm to use for the embedded login assertion.
--
-- ObjC selector: @- setLoginRequestEncryptionAlgorithm:@
setLoginRequestEncryptionAlgorithm :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSNumber value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setLoginRequestEncryptionAlgorithm asAuthorizationProviderExtensionLoginConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setLoginRequestEncryptionAlgorithm:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Authentication public key to be used for HPKE.  Setting this value with changet the mode to Auth or AuthPSK if the hpkePreSharedKey is also set.  This public key is used to authenticate HPKE responses.
--
-- ObjC selector: @- hpkeAuthPublicKey@
hpkeAuthPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Ptr ())
hpkeAuthPublicKey asAuthorizationProviderExtensionLoginConfiguration  =
  fmap castPtr $ sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "hpkeAuthPublicKey") (retPtr retVoid) []

-- | The Authentication public key to be used for HPKE.  Setting this value with changet the mode to Auth or AuthPSK if the hpkePreSharedKey is also set.  This public key is used to authenticate HPKE responses.
--
-- ObjC selector: @- setHpkeAuthPublicKey:@
setHpkeAuthPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> Ptr () -> IO ()
setHpkeAuthPublicKey asAuthorizationProviderExtensionLoginConfiguration  value =
  sendMsg asAuthorizationProviderExtensionLoginConfiguration (mkSelector "setHpkeAuthPublicKey:") retVoid [argPtr value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithClientID:issuer:tokenEndpointURL:jwksEndpointURL:audience:@
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector :: Selector
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector = mkSelector "initWithClientID:issuer:tokenEndpointURL:jwksEndpointURL:audience:"

-- | @Selector@ for @configurationWithOpenIDConfigurationURL:clientID:issuer:completion:@
configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector :: Selector
configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector = mkSelector "configurationWithOpenIDConfigurationURL:clientID:issuer:completion:"

-- | @Selector@ for @setCustomAssertionRequestHeaderClaims:returningError:@
setCustomAssertionRequestHeaderClaims_returningErrorSelector :: Selector
setCustomAssertionRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomAssertionRequestBodyClaims:returningError:@
setCustomAssertionRequestBodyClaims_returningErrorSelector :: Selector
setCustomAssertionRequestBodyClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestHeaderClaims:returningError:@
setCustomLoginRequestHeaderClaims_returningErrorSelector :: Selector
setCustomLoginRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomLoginRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestBodyClaims:returningError:@
setCustomLoginRequestBodyClaims_returningErrorSelector :: Selector
setCustomLoginRequestBodyClaims_returningErrorSelector = mkSelector "setCustomLoginRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomRefreshRequestHeaderClaims:returningError:@
setCustomRefreshRequestHeaderClaims_returningErrorSelector :: Selector
setCustomRefreshRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomRefreshRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomRefreshRequestBodyClaims:returningError:@
setCustomRefreshRequestBodyClaims_returningErrorSelector :: Selector
setCustomRefreshRequestBodyClaims_returningErrorSelector = mkSelector "setCustomRefreshRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomKeyExchangeRequestHeaderClaims:returningError:@
setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector :: Selector
setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomKeyExchangeRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomKeyExchangeRequestBodyClaims:returningError:@
setCustomKeyExchangeRequestBodyClaims_returningErrorSelector :: Selector
setCustomKeyExchangeRequestBodyClaims_returningErrorSelector = mkSelector "setCustomKeyExchangeRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomKeyRequestHeaderClaims:returningError:@
setCustomKeyRequestHeaderClaims_returningErrorSelector :: Selector
setCustomKeyRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomKeyRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomKeyRequestBodyClaims:returningError:@
setCustomKeyRequestBodyClaims_returningErrorSelector :: Selector
setCustomKeyRequestBodyClaims_returningErrorSelector = mkSelector "setCustomKeyRequestBodyClaims:returningError:"

-- | @Selector@ for @invalidCredentialPredicate@
invalidCredentialPredicateSelector :: Selector
invalidCredentialPredicateSelector = mkSelector "invalidCredentialPredicate"

-- | @Selector@ for @setInvalidCredentialPredicate:@
setInvalidCredentialPredicateSelector :: Selector
setInvalidCredentialPredicateSelector = mkSelector "setInvalidCredentialPredicate:"

-- | @Selector@ for @accountDisplayName@
accountDisplayNameSelector :: Selector
accountDisplayNameSelector = mkSelector "accountDisplayName"

-- | @Selector@ for @setAccountDisplayName:@
setAccountDisplayNameSelector :: Selector
setAccountDisplayNameSelector = mkSelector "setAccountDisplayName:"

-- | @Selector@ for @clientID@
clientIDSelector :: Selector
clientIDSelector = mkSelector "clientID"

-- | @Selector@ for @issuer@
issuerSelector :: Selector
issuerSelector = mkSelector "issuer"

-- | @Selector@ for @audience@
audienceSelector :: Selector
audienceSelector = mkSelector "audience"

-- | @Selector@ for @setAudience:@
setAudienceSelector :: Selector
setAudienceSelector = mkSelector "setAudience:"

-- | @Selector@ for @tokenEndpointURL@
tokenEndpointURLSelector :: Selector
tokenEndpointURLSelector = mkSelector "tokenEndpointURL"

-- | @Selector@ for @setTokenEndpointURL:@
setTokenEndpointURLSelector :: Selector
setTokenEndpointURLSelector = mkSelector "setTokenEndpointURL:"

-- | @Selector@ for @jwksEndpointURL@
jwksEndpointURLSelector :: Selector
jwksEndpointURLSelector = mkSelector "jwksEndpointURL"

-- | @Selector@ for @setJwksEndpointURL:@
setJwksEndpointURLSelector :: Selector
setJwksEndpointURLSelector = mkSelector "setJwksEndpointURL:"

-- | @Selector@ for @userSecureEnclaveKeyBiometricPolicy@
userSecureEnclaveKeyBiometricPolicySelector :: Selector
userSecureEnclaveKeyBiometricPolicySelector = mkSelector "userSecureEnclaveKeyBiometricPolicy"

-- | @Selector@ for @setUserSecureEnclaveKeyBiometricPolicy:@
setUserSecureEnclaveKeyBiometricPolicySelector :: Selector
setUserSecureEnclaveKeyBiometricPolicySelector = mkSelector "setUserSecureEnclaveKeyBiometricPolicy:"

-- | @Selector@ for @nonceEndpointURL@
nonceEndpointURLSelector :: Selector
nonceEndpointURLSelector = mkSelector "nonceEndpointURL"

-- | @Selector@ for @setNonceEndpointURL:@
setNonceEndpointURLSelector :: Selector
setNonceEndpointURLSelector = mkSelector "setNonceEndpointURL:"

-- | @Selector@ for @nonceResponseKeypath@
nonceResponseKeypathSelector :: Selector
nonceResponseKeypathSelector = mkSelector "nonceResponseKeypath"

-- | @Selector@ for @setNonceResponseKeypath:@
setNonceResponseKeypathSelector :: Selector
setNonceResponseKeypathSelector = mkSelector "setNonceResponseKeypath:"

-- | @Selector@ for @serverNonceClaimName@
serverNonceClaimNameSelector :: Selector
serverNonceClaimNameSelector = mkSelector "serverNonceClaimName"

-- | @Selector@ for @setServerNonceClaimName:@
setServerNonceClaimNameSelector :: Selector
setServerNonceClaimNameSelector = mkSelector "setServerNonceClaimName:"

-- | @Selector@ for @customNonceRequestValues@
customNonceRequestValuesSelector :: Selector
customNonceRequestValuesSelector = mkSelector "customNonceRequestValues"

-- | @Selector@ for @setCustomNonceRequestValues:@
setCustomNonceRequestValuesSelector :: Selector
setCustomNonceRequestValuesSelector = mkSelector "setCustomNonceRequestValues:"

-- | @Selector@ for @additionalScopes@
additionalScopesSelector :: Selector
additionalScopesSelector = mkSelector "additionalScopes"

-- | @Selector@ for @setAdditionalScopes:@
setAdditionalScopesSelector :: Selector
setAdditionalScopesSelector = mkSelector "setAdditionalScopes:"

-- | @Selector@ for @includePreviousRefreshTokenInLoginRequest@
includePreviousRefreshTokenInLoginRequestSelector :: Selector
includePreviousRefreshTokenInLoginRequestSelector = mkSelector "includePreviousRefreshTokenInLoginRequest"

-- | @Selector@ for @setIncludePreviousRefreshTokenInLoginRequest:@
setIncludePreviousRefreshTokenInLoginRequestSelector :: Selector
setIncludePreviousRefreshTokenInLoginRequestSelector = mkSelector "setIncludePreviousRefreshTokenInLoginRequest:"

-- | @Selector@ for @previousRefreshTokenClaimName@
previousRefreshTokenClaimNameSelector :: Selector
previousRefreshTokenClaimNameSelector = mkSelector "previousRefreshTokenClaimName"

-- | @Selector@ for @setPreviousRefreshTokenClaimName:@
setPreviousRefreshTokenClaimNameSelector :: Selector
setPreviousRefreshTokenClaimNameSelector = mkSelector "setPreviousRefreshTokenClaimName:"

-- | @Selector@ for @customLoginRequestValues@
customLoginRequestValuesSelector :: Selector
customLoginRequestValuesSelector = mkSelector "customLoginRequestValues"

-- | @Selector@ for @setCustomLoginRequestValues:@
setCustomLoginRequestValuesSelector :: Selector
setCustomLoginRequestValuesSelector = mkSelector "setCustomLoginRequestValues:"

-- | @Selector@ for @kerberosTicketMappings@
kerberosTicketMappingsSelector :: Selector
kerberosTicketMappingsSelector = mkSelector "kerberosTicketMappings"

-- | @Selector@ for @setKerberosTicketMappings:@
setKerberosTicketMappingsSelector :: Selector
setKerberosTicketMappingsSelector = mkSelector "setKerberosTicketMappings:"

-- | @Selector@ for @federationType@
federationTypeSelector :: Selector
federationTypeSelector = mkSelector "federationType"

-- | @Selector@ for @setFederationType:@
setFederationTypeSelector :: Selector
setFederationTypeSelector = mkSelector "setFederationType:"

-- | @Selector@ for @loginRequestEncryptionPublicKey@
loginRequestEncryptionPublicKeySelector :: Selector
loginRequestEncryptionPublicKeySelector = mkSelector "loginRequestEncryptionPublicKey"

-- | @Selector@ for @setLoginRequestEncryptionPublicKey:@
setLoginRequestEncryptionPublicKeySelector :: Selector
setLoginRequestEncryptionPublicKeySelector = mkSelector "setLoginRequestEncryptionPublicKey:"

-- | @Selector@ for @loginRequestEncryptionAlgorithm@
loginRequestEncryptionAlgorithmSelector :: Selector
loginRequestEncryptionAlgorithmSelector = mkSelector "loginRequestEncryptionAlgorithm"

-- | @Selector@ for @setLoginRequestEncryptionAlgorithm:@
setLoginRequestEncryptionAlgorithmSelector :: Selector
setLoginRequestEncryptionAlgorithmSelector = mkSelector "setLoginRequestEncryptionAlgorithm:"

-- | @Selector@ for @hpkeAuthPublicKey@
hpkeAuthPublicKeySelector :: Selector
hpkeAuthPublicKeySelector = mkSelector "hpkeAuthPublicKey"

-- | @Selector@ for @setHpkeAuthPublicKey:@
setHpkeAuthPublicKeySelector :: Selector
setHpkeAuthPublicKeySelector = mkSelector "setHpkeAuthPublicKey:"

