{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , jwksTrustedRootCertificates
  , setJwksTrustedRootCertificates
  , deviceContext
  , setDeviceContext
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
  , additionalAuthorizationScopes
  , setAdditionalAuthorizationScopes
  , includePreviousRefreshTokenInLoginRequest
  , setIncludePreviousRefreshTokenInLoginRequest
  , previousRefreshTokenClaimName
  , setPreviousRefreshTokenClaimName
  , customRequestJWTParameterName
  , setCustomRequestJWTParameterName
  , customLoginRequestValues
  , setCustomLoginRequestValues
  , uniqueIdentifierClaimName
  , setUniqueIdentifierClaimName
  , groupRequestClaimName
  , setGroupRequestClaimName
  , groupResponseClaimName
  , setGroupResponseClaimName
  , kerberosTicketMappings
  , setKerberosTicketMappings
  , refreshEndpointURL
  , setRefreshEndpointURL
  , customRefreshRequestValues
  , setCustomRefreshRequestValues
  , federationType
  , setFederationType
  , federationRequestURN
  , setFederationRequestURN
  , federationMEXURL
  , setFederationMEXURL
  , federationUserPreauthenticationURL
  , setFederationUserPreauthenticationURL
  , federationMEXURLKeypath
  , setFederationMEXURLKeypath
  , federationPredicate
  , setFederationPredicate
  , customFederationUserPreauthenticationRequestValues
  , setCustomFederationUserPreauthenticationRequestValues
  , loginRequestEncryptionPublicKey
  , setLoginRequestEncryptionPublicKey
  , loginRequestEncryptionAPVPrefix
  , setLoginRequestEncryptionAPVPrefix
  , loginRequestEncryptionAlgorithm
  , setLoginRequestEncryptionAlgorithm
  , loginRequestHPKEPreSharedKey
  , setLoginRequestHPKEPreSharedKey
  , loginRequestHPKEPreSharedKeyID
  , setLoginRequestHPKEPreSharedKeyID
  , keyEndpointURL
  , setKeyEndpointURL
  , customKeyExchangeRequestValues
  , setCustomKeyExchangeRequestValues
  , customKeyRequestValues
  , setCustomKeyRequestValues
  , hpkePreSharedKey
  , setHpkePreSharedKey
  , hpkePreSharedKeyID
  , setHpkePreSharedKeyID
  , hpkeAuthPublicKey
  , setHpkeAuthPublicKey
  , accountDisplayNameSelector
  , additionalAuthorizationScopesSelector
  , additionalScopesSelector
  , audienceSelector
  , clientIDSelector
  , configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector
  , customFederationUserPreauthenticationRequestValuesSelector
  , customKeyExchangeRequestValuesSelector
  , customKeyRequestValuesSelector
  , customLoginRequestValuesSelector
  , customNonceRequestValuesSelector
  , customRefreshRequestValuesSelector
  , customRequestJWTParameterNameSelector
  , deviceContextSelector
  , federationMEXURLKeypathSelector
  , federationMEXURLSelector
  , federationPredicateSelector
  , federationRequestURNSelector
  , federationTypeSelector
  , federationUserPreauthenticationURLSelector
  , groupRequestClaimNameSelector
  , groupResponseClaimNameSelector
  , hpkeAuthPublicKeySelector
  , hpkePreSharedKeyIDSelector
  , hpkePreSharedKeySelector
  , includePreviousRefreshTokenInLoginRequestSelector
  , initSelector
  , initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector
  , invalidCredentialPredicateSelector
  , issuerSelector
  , jwksEndpointURLSelector
  , jwksTrustedRootCertificatesSelector
  , kerberosTicketMappingsSelector
  , keyEndpointURLSelector
  , loginRequestEncryptionAPVPrefixSelector
  , loginRequestEncryptionAlgorithmSelector
  , loginRequestEncryptionPublicKeySelector
  , loginRequestHPKEPreSharedKeyIDSelector
  , loginRequestHPKEPreSharedKeySelector
  , newSelector
  , nonceEndpointURLSelector
  , nonceResponseKeypathSelector
  , previousRefreshTokenClaimNameSelector
  , refreshEndpointURLSelector
  , serverNonceClaimNameSelector
  , setAccountDisplayNameSelector
  , setAdditionalAuthorizationScopesSelector
  , setAdditionalScopesSelector
  , setAudienceSelector
  , setCustomAssertionRequestBodyClaims_returningErrorSelector
  , setCustomAssertionRequestHeaderClaims_returningErrorSelector
  , setCustomFederationUserPreauthenticationRequestValuesSelector
  , setCustomKeyExchangeRequestBodyClaims_returningErrorSelector
  , setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector
  , setCustomKeyExchangeRequestValuesSelector
  , setCustomKeyRequestBodyClaims_returningErrorSelector
  , setCustomKeyRequestHeaderClaims_returningErrorSelector
  , setCustomKeyRequestValuesSelector
  , setCustomLoginRequestBodyClaims_returningErrorSelector
  , setCustomLoginRequestHeaderClaims_returningErrorSelector
  , setCustomLoginRequestValuesSelector
  , setCustomNonceRequestValuesSelector
  , setCustomRefreshRequestBodyClaims_returningErrorSelector
  , setCustomRefreshRequestHeaderClaims_returningErrorSelector
  , setCustomRefreshRequestValuesSelector
  , setCustomRequestJWTParameterNameSelector
  , setDeviceContextSelector
  , setFederationMEXURLKeypathSelector
  , setFederationMEXURLSelector
  , setFederationPredicateSelector
  , setFederationRequestURNSelector
  , setFederationTypeSelector
  , setFederationUserPreauthenticationURLSelector
  , setGroupRequestClaimNameSelector
  , setGroupResponseClaimNameSelector
  , setHpkeAuthPublicKeySelector
  , setHpkePreSharedKeyIDSelector
  , setHpkePreSharedKeySelector
  , setIncludePreviousRefreshTokenInLoginRequestSelector
  , setInvalidCredentialPredicateSelector
  , setJwksEndpointURLSelector
  , setJwksTrustedRootCertificatesSelector
  , setKerberosTicketMappingsSelector
  , setKeyEndpointURLSelector
  , setLoginRequestEncryptionAPVPrefixSelector
  , setLoginRequestEncryptionAlgorithmSelector
  , setLoginRequestEncryptionPublicKeySelector
  , setLoginRequestHPKEPreSharedKeyIDSelector
  , setLoginRequestHPKEPreSharedKeySelector
  , setNonceEndpointURLSelector
  , setNonceResponseKeypathSelector
  , setPreviousRefreshTokenClaimNameSelector
  , setRefreshEndpointURLSelector
  , setServerNonceClaimNameSelector
  , setTokenEndpointURLSelector
  , setUniqueIdentifierClaimNameSelector
  , setUserSecureEnclaveKeyBiometricPolicySelector
  , tokenEndpointURLSelector
  , uniqueIdentifierClaimNameSelector
  , userSecureEnclaveKeyBiometricPolicySelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id ASAuthorizationProviderExtensionLoginConfiguration)
init_ asAuthorizationProviderExtensionLoginConfiguration =
  sendOwnedMessage asAuthorizationProviderExtensionLoginConfiguration initSelector

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
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audience asAuthorizationProviderExtensionLoginConfiguration clientID issuer tokenEndpointURL jwksEndpointURL audience =
  sendOwnedMessage asAuthorizationProviderExtensionLoginConfiguration initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector (toNSString clientID) (toNSString issuer) (toNSURL tokenEndpointURL) (toNSURL jwksEndpointURL) (toNSString audience)

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
    sendClassMessage cls' configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector (toNSURL openIDConfigurationURL) (toNSString clientID) (toNSString issuer) completion

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
setCustomAssertionRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomAssertionRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomAssertionRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomAssertionRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomLoginRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomLoginRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomLoginRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomLoginRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomRefreshRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomRefreshRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomRefreshRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomRefreshRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomKeyExchangeRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomKeyExchangeRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomKeyExchangeRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomKeyRequestHeaderClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomKeyRequestHeaderClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

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
setCustomKeyRequestBodyClaims_returningError asAuthorizationProviderExtensionLoginConfiguration claims error_ =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomKeyRequestBodyClaims_returningErrorSelector (toNSDictionary claims) (toNSError error_)

-- | Predicate string used to identify invalid credential errors.
--
-- If there is an HTTP 400 or HTTP 401 error when authenticating, this predicate will be used on the response body JSON to determine if the error is due to an invalid password or something else.  If nil, then only an HTTP 401 will be used for an invalid credential.
--
-- ObjC selector: @- invalidCredentialPredicate@
invalidCredentialPredicate :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
invalidCredentialPredicate asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration invalidCredentialPredicateSelector

-- | Predicate string used to identify invalid credential errors.
--
-- If there is an HTTP 400 or HTTP 401 error when authenticating, this predicate will be used on the response body JSON to determine if the error is due to an invalid password or something else.  If nil, then only an HTTP 401 will be used for an invalid credential.
--
-- ObjC selector: @- setInvalidCredentialPredicate:@
setInvalidCredentialPredicate :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setInvalidCredentialPredicate asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setInvalidCredentialPredicateSelector (toNSString value)

-- | The display name for the account.  Used for notifications and login prompts.
--
-- ObjC selector: @- accountDisplayName@
accountDisplayName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
accountDisplayName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration accountDisplayNameSelector

-- | The display name for the account.  Used for notifications and login prompts.
--
-- ObjC selector: @- setAccountDisplayName:@
setAccountDisplayName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAccountDisplayName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setAccountDisplayNameSelector (toNSString value)

-- | The login client_id.
--
-- ObjC selector: @- clientID@
clientID :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
clientID asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration clientIDSelector

-- | The issuer for validation.
--
-- ObjC selector: @- issuer@
issuer :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
issuer asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration issuerSelector

-- | The audience for validation and requests.
--
-- ObjC selector: @- audience@
audience :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
audience asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration audienceSelector

-- | The audience for validation and requests.
--
-- ObjC selector: @- setAudience:@
setAudience :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAudience asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setAudienceSelector (toNSString value)

-- | Token Endpoint URL for login request.
--
-- ObjC selector: @- tokenEndpointURL@
tokenEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
tokenEndpointURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration tokenEndpointURLSelector

-- | Token Endpoint URL for login request.
--
-- ObjC selector: @- setTokenEndpointURL:@
setTokenEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setTokenEndpointURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setTokenEndpointURLSelector (toNSURL value)

-- | JWKS Endpoint URL for keys.
--
-- ObjC selector: @- jwksEndpointURL@
jwksEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
jwksEndpointURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration jwksEndpointURLSelector

-- | JWKS Endpoint URL for keys.
--
-- ObjC selector: @- setJwksEndpointURL:@
setJwksEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setJwksEndpointURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setJwksEndpointURLSelector (toNSURL value)

-- | The root certificates to use for trust evaluation of jwks keys.
--
-- if set, certificates will be required in jwks responses and evaluated using the supplied certificates.  If the jwks certificates are missing or fail trust evaluation the login will fail.
--
-- ObjC selector: @- jwksTrustedRootCertificates@
jwksTrustedRootCertificates :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
jwksTrustedRootCertificates asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration jwksTrustedRootCertificatesSelector

-- | The root certificates to use for trust evaluation of jwks keys.
--
-- if set, certificates will be required in jwks responses and evaluated using the supplied certificates.  If the jwks certificates are missing or fail trust evaluation the login will fail.
--
-- ObjC selector: @- setJwksTrustedRootCertificates:@
setJwksTrustedRootCertificates :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setJwksTrustedRootCertificates asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setJwksTrustedRootCertificatesSelector (toNSArray value)

-- | The device context for storing device meta data.
--
-- ObjC selector: @- deviceContext@
deviceContext :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSData)
deviceContext asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration deviceContextSelector

-- | The device context for storing device meta data.
--
-- ObjC selector: @- setDeviceContext:@
setDeviceContext :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSData value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setDeviceContext asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setDeviceContextSelector (toNSData value)

-- | The biometric policy for User Secure Enclave Key authentication.
--
-- ObjC selector: @- userSecureEnclaveKeyBiometricPolicy@
userSecureEnclaveKeyBiometricPolicy :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy
userSecureEnclaveKeyBiometricPolicy asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration userSecureEnclaveKeyBiometricPolicySelector

-- | The biometric policy for User Secure Enclave Key authentication.
--
-- ObjC selector: @- setUserSecureEnclaveKeyBiometricPolicy:@
setUserSecureEnclaveKeyBiometricPolicy :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy -> IO ()
setUserSecureEnclaveKeyBiometricPolicy asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setUserSecureEnclaveKeyBiometricPolicySelector value

-- | Nonce Endpoint URL, defaults to token tokenEndpointURL.
--
-- ObjC selector: @- nonceEndpointURL@
nonceEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
nonceEndpointURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration nonceEndpointURLSelector

-- | Nonce Endpoint URL, defaults to token tokenEndpointURL.
--
-- ObjC selector: @- setNonceEndpointURL:@
setNonceEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setNonceEndpointURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setNonceEndpointURLSelector (toNSURL value)

-- | The keypath in the nonce response that contains the nonce value.
--
-- ObjC selector: @- nonceResponseKeypath@
nonceResponseKeypath :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
nonceResponseKeypath asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration nonceResponseKeypathSelector

-- | The keypath in the nonce response that contains the nonce value.
--
-- ObjC selector: @- setNonceResponseKeypath:@
setNonceResponseKeypath :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setNonceResponseKeypath asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setNonceResponseKeypathSelector (toNSString value)

-- | The name of the server nonce claim when included in authentication requests.
--
-- ObjC selector: @- serverNonceClaimName@
serverNonceClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
serverNonceClaimName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration serverNonceClaimNameSelector

-- | The name of the server nonce claim when included in authentication requests.
--
-- ObjC selector: @- setServerNonceClaimName:@
setServerNonceClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setServerNonceClaimName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setServerNonceClaimNameSelector (toNSString value)

-- | Custom values added to the server nonce POST request body.
--
-- ObjC selector: @- customNonceRequestValues@
customNonceRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customNonceRequestValues asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customNonceRequestValuesSelector

-- | Custom values added to the server nonce POST request body.
--
-- ObjC selector: @- setCustomNonceRequestValues:@
setCustomNonceRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomNonceRequestValues asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomNonceRequestValuesSelector (toNSArray value)

-- | Additional login scopes.
--
-- ObjC selector: @- additionalScopes@
additionalScopes :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
additionalScopes asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration additionalScopesSelector

-- | Additional login scopes.
--
-- ObjC selector: @- setAdditionalScopes:@
setAdditionalScopes :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAdditionalScopes asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setAdditionalScopesSelector (toNSString value)

-- | Additional authorization scopes.
--
-- ObjC selector: @- additionalAuthorizationScopes@
additionalAuthorizationScopes :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
additionalAuthorizationScopes asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration additionalAuthorizationScopesSelector

-- | Additional authorization scopes.
--
-- ObjC selector: @- setAdditionalAuthorizationScopes:@
setAdditionalAuthorizationScopes :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setAdditionalAuthorizationScopes asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setAdditionalAuthorizationScopesSelector (toNSString value)

-- | If true and there is a refresh token for the user in the SSO tokens, it will be included in the login request.
--
-- ObjC selector: @- includePreviousRefreshTokenInLoginRequest@
includePreviousRefreshTokenInLoginRequest :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO Bool
includePreviousRefreshTokenInLoginRequest asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration includePreviousRefreshTokenInLoginRequestSelector

-- | If true and there is a refresh token for the user in the SSO tokens, it will be included in the login request.
--
-- ObjC selector: @- setIncludePreviousRefreshTokenInLoginRequest:@
setIncludePreviousRefreshTokenInLoginRequest :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> Bool -> IO ()
setIncludePreviousRefreshTokenInLoginRequest asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setIncludePreviousRefreshTokenInLoginRequestSelector value

-- | The claim name for the previous SSO token value in the login request.
--
-- ObjC selector: @- previousRefreshTokenClaimName@
previousRefreshTokenClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
previousRefreshTokenClaimName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration previousRefreshTokenClaimNameSelector

-- | The claim name for the previous SSO token value in the login request.
--
-- ObjC selector: @- setPreviousRefreshTokenClaimName:@
setPreviousRefreshTokenClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setPreviousRefreshTokenClaimName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setPreviousRefreshTokenClaimNameSelector (toNSString value)

-- | The request parameter name for the JWT.  The default is "assertion".
--
-- ObjC selector: @- customRequestJWTParameterName@
customRequestJWTParameterName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
customRequestJWTParameterName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customRequestJWTParameterNameSelector

-- | The request parameter name for the JWT.  The default is "assertion".
--
-- ObjC selector: @- setCustomRequestJWTParameterName:@
setCustomRequestJWTParameterName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomRequestJWTParameterName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomRequestJWTParameterNameSelector (toNSString value)

-- | Custom values added to the login POST request body.
--
-- ObjC selector: @- customLoginRequestValues@
customLoginRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customLoginRequestValues asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customLoginRequestValuesSelector

-- | Custom values added to the login POST request body.
--
-- ObjC selector: @- setCustomLoginRequestValues:@
setCustomLoginRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomLoginRequestValues asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomLoginRequestValuesSelector (toNSArray value)

-- | The claim name for the user unique identifier in the id token. Defaults to "sub".
--
-- ObjC selector: @- uniqueIdentifierClaimName@
uniqueIdentifierClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
uniqueIdentifierClaimName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration uniqueIdentifierClaimNameSelector

-- | The claim name for the user unique identifier in the id token. Defaults to "sub".
--
-- ObjC selector: @- setUniqueIdentifierClaimName:@
setUniqueIdentifierClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setUniqueIdentifierClaimName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setUniqueIdentifierClaimNameSelector (toNSString value)

-- | The claim name for group membership request.
--
-- ObjC selector: @- groupRequestClaimName@
groupRequestClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
groupRequestClaimName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration groupRequestClaimNameSelector

-- | The claim name for group membership request.
--
-- ObjC selector: @- setGroupRequestClaimName:@
setGroupRequestClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setGroupRequestClaimName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setGroupRequestClaimNameSelector (toNSString value)

-- | The claim name for group responses in the id_token.
--
-- ObjC selector: @- groupResponseClaimName@
groupResponseClaimName :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
groupResponseClaimName asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration groupResponseClaimNameSelector

-- | The claim name for group responses in the id_token.
--
-- ObjC selector: @- setGroupResponseClaimName:@
setGroupResponseClaimName :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setGroupResponseClaimName asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setGroupResponseClaimNameSelector (toNSString value)

-- | The Kerberos ticket mappings to use.
--
-- ObjC selector: @- kerberosTicketMappings@
kerberosTicketMappings :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
kerberosTicketMappings asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration kerberosTicketMappingsSelector

-- | The Kerberos ticket mappings to use.
--
-- ObjC selector: @- setKerberosTicketMappings:@
setKerberosTicketMappings :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setKerberosTicketMappings asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setKerberosTicketMappingsSelector (toNSArray value)

-- | Token Refresh Endpoint URL for login request.  Defaults to the tokenEndpointURL.
--
-- ObjC selector: @- refreshEndpointURL@
refreshEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
refreshEndpointURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration refreshEndpointURLSelector

-- | Token Refresh Endpoint URL for login request.  Defaults to the tokenEndpointURL.
--
-- ObjC selector: @- setRefreshEndpointURL:@
setRefreshEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setRefreshEndpointURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setRefreshEndpointURLSelector (toNSURL value)

-- | Custom values added to the refresh POST request body.
--
-- ObjC selector: @- customRefreshRequestValues@
customRefreshRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customRefreshRequestValues asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customRefreshRequestValuesSelector

-- | Custom values added to the refresh POST request body.
--
-- ObjC selector: @- setCustomRefreshRequestValues:@
setCustomRefreshRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomRefreshRequestValues asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomRefreshRequestValuesSelector (toNSArray value)

-- | The federation method to use.
--
-- ObjC selector: @- federationType@
federationType :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO ASAuthorizationProviderExtensionFederationType
federationType asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration federationTypeSelector

-- | The federation method to use.
--
-- ObjC selector: @- setFederationType:@
setFederationType :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> ASAuthorizationProviderExtensionFederationType -> IO ()
setFederationType asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setFederationTypeSelector value

-- | The URN to request when performing a federated login.
--
-- ObjC selector: @- federationRequestURN@
federationRequestURN :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
federationRequestURN asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration federationRequestURNSelector

-- | The URN to request when performing a federated login.
--
-- ObjC selector: @- setFederationRequestURN:@
setFederationRequestURN :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setFederationRequestURN asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setFederationRequestURNSelector (toNSString value)

-- | The federation MEX URL to use.  This can be overwritten when using dynamic federation.
--
-- ObjC selector: @- federationMEXURL@
federationMEXURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
federationMEXURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration federationMEXURLSelector

-- | The federation MEX URL to use.  This can be overwritten when using dynamic federation.
--
-- ObjC selector: @- setFederationMEXURL:@
setFederationMEXURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setFederationMEXURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setFederationMEXURLSelector (toNSURL value)

-- | The URL to use when performing dynamic federation.
--
-- ObjC selector: @- federationUserPreauthenticationURL@
federationUserPreauthenticationURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
federationUserPreauthenticationURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration federationUserPreauthenticationURLSelector

-- | The URL to use when performing dynamic federation.
--
-- ObjC selector: @- setFederationUserPreauthenticationURL:@
setFederationUserPreauthenticationURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setFederationUserPreauthenticationURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setFederationUserPreauthenticationURLSelector (toNSURL value)

-- | The claim in the preauthentication response that contains the MEX URL.
--
-- ObjC selector: @- federationMEXURLKeypath@
federationMEXURLKeypath :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
federationMEXURLKeypath asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration federationMEXURLKeypathSelector

-- | The claim in the preauthentication response that contains the MEX URL.
--
-- ObjC selector: @- setFederationMEXURLKeypath:@
setFederationMEXURLKeypath :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setFederationMEXURLKeypath asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setFederationMEXURLKeypathSelector (toNSString value)

-- | The predicate to apply to the preauthentication response to perform federation or not.
--
-- ObjC selector: @- federationPredicate@
federationPredicate :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSString)
federationPredicate asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration federationPredicateSelector

-- | The predicate to apply to the preauthentication response to perform federation or not.
--
-- ObjC selector: @- setFederationPredicate:@
setFederationPredicate :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSString value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setFederationPredicate asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setFederationPredicateSelector (toNSString value)

-- | The custom query string values to add when making the preauthenticaion request.
--
-- ObjC selector: @- customFederationUserPreauthenticationRequestValues@
customFederationUserPreauthenticationRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customFederationUserPreauthenticationRequestValues asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customFederationUserPreauthenticationRequestValuesSelector

-- | The custom query string values to add when making the preauthenticaion request.
--
-- ObjC selector: @- setCustomFederationUserPreauthenticationRequestValues:@
setCustomFederationUserPreauthenticationRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomFederationUserPreauthenticationRequestValues asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomFederationUserPreauthenticationRequestValuesSelector (toNSArray value)

-- | The public key to use for encrypting the embedded login assertion.
--
-- Only applies to password authentication.  If set, the password will encrypted in an embedded assertion instead of the login request itself.
--
-- ObjC selector: @- loginRequestEncryptionPublicKey@
loginRequestEncryptionPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Ptr ())
loginRequestEncryptionPublicKey asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration loginRequestEncryptionPublicKeySelector

-- | The public key to use for encrypting the embedded login assertion.
--
-- Only applies to password authentication.  If set, the password will encrypted in an embedded assertion instead of the login request itself.
--
-- ObjC selector: @- setLoginRequestEncryptionPublicKey:@
setLoginRequestEncryptionPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> Ptr () -> IO ()
setLoginRequestEncryptionPublicKey asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setLoginRequestEncryptionPublicKeySelector value

-- | The APV prefix used for encrypted embedded login assertions.
--
-- ObjC selector: @- loginRequestEncryptionAPVPrefix@
loginRequestEncryptionAPVPrefix :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSData)
loginRequestEncryptionAPVPrefix asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration loginRequestEncryptionAPVPrefixSelector

-- | The APV prefix used for encrypted embedded login assertions.
--
-- ObjC selector: @- setLoginRequestEncryptionAPVPrefix:@
setLoginRequestEncryptionAPVPrefix :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSData value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setLoginRequestEncryptionAPVPrefix asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setLoginRequestEncryptionAPVPrefixSelector (toNSData value)

-- | The encryption algorithm to use for the embedded login assertion.
--
-- ObjC selector: @- loginRequestEncryptionAlgorithm@
loginRequestEncryptionAlgorithm :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSNumber)
loginRequestEncryptionAlgorithm asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration loginRequestEncryptionAlgorithmSelector

-- | The encryption algorithm to use for the embedded login assertion.
--
-- ObjC selector: @- setLoginRequestEncryptionAlgorithm:@
setLoginRequestEncryptionAlgorithm :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSNumber value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setLoginRequestEncryptionAlgorithm asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setLoginRequestEncryptionAlgorithmSelector (toNSNumber value)

-- | The PreSharedKey to be used for HKPE for embedded login assertions. Setting this value will change the mode to PSK if the loginRequestHPKEPreSharedKeyID is also set. Must be at least 32 bytes.
--
-- ObjC selector: @- loginRequestHPKEPreSharedKey@
loginRequestHPKEPreSharedKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSData)
loginRequestHPKEPreSharedKey asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration loginRequestHPKEPreSharedKeySelector

-- | The PreSharedKey to be used for HKPE for embedded login assertions. Setting this value will change the mode to PSK if the loginRequestHPKEPreSharedKeyID is also set. Must be at least 32 bytes.
--
-- ObjC selector: @- setLoginRequestHPKEPreSharedKey:@
setLoginRequestHPKEPreSharedKey :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSData value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setLoginRequestHPKEPreSharedKey asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setLoginRequestHPKEPreSharedKeySelector (toNSData value)

-- | The PreSharedKey Id to be used for HPKE PSK for embedded login assertions.  This is required if the loginRequestHPKEPreSharedKey is set.
--
-- ObjC selector: @- loginRequestHPKEPreSharedKeyID@
loginRequestHPKEPreSharedKeyID :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSData)
loginRequestHPKEPreSharedKeyID asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration loginRequestHPKEPreSharedKeyIDSelector

-- | The PreSharedKey Id to be used for HPKE PSK for embedded login assertions.  This is required if the loginRequestHPKEPreSharedKey is set.
--
-- ObjC selector: @- setLoginRequestHPKEPreSharedKeyID:@
setLoginRequestHPKEPreSharedKeyID :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSData value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setLoginRequestHPKEPreSharedKeyID asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setLoginRequestHPKEPreSharedKeyIDSelector (toNSData value)

-- | The url endpoint for key service, defaults to token tokenEndpointURL.
--
-- ObjC selector: @- keyEndpointURL@
keyEndpointURL :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSURL)
keyEndpointURL asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration keyEndpointURLSelector

-- | The url endpoint for key service, defaults to token tokenEndpointURL.
--
-- ObjC selector: @- setKeyEndpointURL:@
setKeyEndpointURL :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSURL value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setKeyEndpointURL asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setKeyEndpointURLSelector (toNSURL value)

-- | Custom values added to the key exchange POST request body.
--
-- ObjC selector: @- customKeyExchangeRequestValues@
customKeyExchangeRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customKeyExchangeRequestValues asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customKeyExchangeRequestValuesSelector

-- | Custom values added to the key exchange POST request body.
--
-- ObjC selector: @- setCustomKeyExchangeRequestValues:@
setCustomKeyExchangeRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomKeyExchangeRequestValues asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomKeyExchangeRequestValuesSelector (toNSArray value)

-- | Custom values added to the key request POST request body.
--
-- ObjC selector: @- customKeyRequestValues@
customKeyRequestValues :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSArray)
customKeyRequestValues asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration customKeyRequestValuesSelector

-- | Custom values added to the key request POST request body.
--
-- ObjC selector: @- setCustomKeyRequestValues:@
setCustomKeyRequestValues :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSArray value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setCustomKeyRequestValues asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setCustomKeyRequestValuesSelector (toNSArray value)

-- | The PreSharedKey to be used for HKPE. Setting this value will change the mode to PSK or AuthPSK if the hpkeAuthPublicKey is also set. Must be at least 32 bytes.
--
-- ObjC selector: @- hpkePreSharedKey@
hpkePreSharedKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSData)
hpkePreSharedKey asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration hpkePreSharedKeySelector

-- | The PreSharedKey to be used for HKPE. Setting this value will change the mode to PSK or AuthPSK if the hpkeAuthPublicKey is also set. Must be at least 32 bytes.
--
-- ObjC selector: @- setHpkePreSharedKey:@
setHpkePreSharedKey :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSData value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setHpkePreSharedKey asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setHpkePreSharedKeySelector (toNSData value)

-- | The PreSharedKey Id to be used for HPKE PSK or AuthPSK mode.  This is requred if the hpkePreSharedKey is set.
--
-- ObjC selector: @- hpkePreSharedKeyID@
hpkePreSharedKeyID :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Id NSData)
hpkePreSharedKeyID asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration hpkePreSharedKeyIDSelector

-- | The PreSharedKey Id to be used for HPKE PSK or AuthPSK mode.  This is requred if the hpkePreSharedKey is set.
--
-- ObjC selector: @- setHpkePreSharedKeyID:@
setHpkePreSharedKeyID :: (IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration, IsNSData value) => asAuthorizationProviderExtensionLoginConfiguration -> value -> IO ()
setHpkePreSharedKeyID asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setHpkePreSharedKeyIDSelector (toNSData value)

-- | The Authentication public key to be used for HPKE.  Setting this value with changet the mode to Auth or AuthPSK if the hpkePreSharedKey is also set.  This public key is used to authenticate HPKE responses.
--
-- ObjC selector: @- hpkeAuthPublicKey@
hpkeAuthPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> IO (Ptr ())
hpkeAuthPublicKey asAuthorizationProviderExtensionLoginConfiguration =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration hpkeAuthPublicKeySelector

-- | The Authentication public key to be used for HPKE.  Setting this value with changet the mode to Auth or AuthPSK if the hpkePreSharedKey is also set.  This public key is used to authenticate HPKE responses.
--
-- ObjC selector: @- setHpkeAuthPublicKey:@
setHpkeAuthPublicKey :: IsASAuthorizationProviderExtensionLoginConfiguration asAuthorizationProviderExtensionLoginConfiguration => asAuthorizationProviderExtensionLoginConfiguration -> Ptr () -> IO ()
setHpkeAuthPublicKey asAuthorizationProviderExtensionLoginConfiguration value =
  sendMessage asAuthorizationProviderExtensionLoginConfiguration setHpkeAuthPublicKeySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationProviderExtensionLoginConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationProviderExtensionLoginConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithClientID:issuer:tokenEndpointURL:jwksEndpointURL:audience:@
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector :: Selector '[Id NSString, Id NSString, Id NSURL, Id NSURL, Id NSString] (Id ASAuthorizationProviderExtensionLoginConfiguration)
initWithClientID_issuer_tokenEndpointURL_jwksEndpointURL_audienceSelector = mkSelector "initWithClientID:issuer:tokenEndpointURL:jwksEndpointURL:audience:"

-- | @Selector@ for @configurationWithOpenIDConfigurationURL:clientID:issuer:completion:@
configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector :: Selector '[Id NSURL, Id NSString, Id NSString, Ptr ()] ()
configurationWithOpenIDConfigurationURL_clientID_issuer_completionSelector = mkSelector "configurationWithOpenIDConfigurationURL:clientID:issuer:completion:"

-- | @Selector@ for @setCustomAssertionRequestHeaderClaims:returningError:@
setCustomAssertionRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomAssertionRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomAssertionRequestBodyClaims:returningError:@
setCustomAssertionRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomAssertionRequestBodyClaims_returningErrorSelector = mkSelector "setCustomAssertionRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestHeaderClaims:returningError:@
setCustomLoginRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomLoginRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomLoginRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomLoginRequestBodyClaims:returningError:@
setCustomLoginRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomLoginRequestBodyClaims_returningErrorSelector = mkSelector "setCustomLoginRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomRefreshRequestHeaderClaims:returningError:@
setCustomRefreshRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomRefreshRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomRefreshRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomRefreshRequestBodyClaims:returningError:@
setCustomRefreshRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomRefreshRequestBodyClaims_returningErrorSelector = mkSelector "setCustomRefreshRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomKeyExchangeRequestHeaderClaims:returningError:@
setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomKeyExchangeRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomKeyExchangeRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomKeyExchangeRequestBodyClaims:returningError:@
setCustomKeyExchangeRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomKeyExchangeRequestBodyClaims_returningErrorSelector = mkSelector "setCustomKeyExchangeRequestBodyClaims:returningError:"

-- | @Selector@ for @setCustomKeyRequestHeaderClaims:returningError:@
setCustomKeyRequestHeaderClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomKeyRequestHeaderClaims_returningErrorSelector = mkSelector "setCustomKeyRequestHeaderClaims:returningError:"

-- | @Selector@ for @setCustomKeyRequestBodyClaims:returningError:@
setCustomKeyRequestBodyClaims_returningErrorSelector :: Selector '[Id NSDictionary, Id NSError] Bool
setCustomKeyRequestBodyClaims_returningErrorSelector = mkSelector "setCustomKeyRequestBodyClaims:returningError:"

-- | @Selector@ for @invalidCredentialPredicate@
invalidCredentialPredicateSelector :: Selector '[] (Id NSString)
invalidCredentialPredicateSelector = mkSelector "invalidCredentialPredicate"

-- | @Selector@ for @setInvalidCredentialPredicate:@
setInvalidCredentialPredicateSelector :: Selector '[Id NSString] ()
setInvalidCredentialPredicateSelector = mkSelector "setInvalidCredentialPredicate:"

-- | @Selector@ for @accountDisplayName@
accountDisplayNameSelector :: Selector '[] (Id NSString)
accountDisplayNameSelector = mkSelector "accountDisplayName"

-- | @Selector@ for @setAccountDisplayName:@
setAccountDisplayNameSelector :: Selector '[Id NSString] ()
setAccountDisplayNameSelector = mkSelector "setAccountDisplayName:"

-- | @Selector@ for @clientID@
clientIDSelector :: Selector '[] (Id NSString)
clientIDSelector = mkSelector "clientID"

-- | @Selector@ for @issuer@
issuerSelector :: Selector '[] (Id NSString)
issuerSelector = mkSelector "issuer"

-- | @Selector@ for @audience@
audienceSelector :: Selector '[] (Id NSString)
audienceSelector = mkSelector "audience"

-- | @Selector@ for @setAudience:@
setAudienceSelector :: Selector '[Id NSString] ()
setAudienceSelector = mkSelector "setAudience:"

-- | @Selector@ for @tokenEndpointURL@
tokenEndpointURLSelector :: Selector '[] (Id NSURL)
tokenEndpointURLSelector = mkSelector "tokenEndpointURL"

-- | @Selector@ for @setTokenEndpointURL:@
setTokenEndpointURLSelector :: Selector '[Id NSURL] ()
setTokenEndpointURLSelector = mkSelector "setTokenEndpointURL:"

-- | @Selector@ for @jwksEndpointURL@
jwksEndpointURLSelector :: Selector '[] (Id NSURL)
jwksEndpointURLSelector = mkSelector "jwksEndpointURL"

-- | @Selector@ for @setJwksEndpointURL:@
setJwksEndpointURLSelector :: Selector '[Id NSURL] ()
setJwksEndpointURLSelector = mkSelector "setJwksEndpointURL:"

-- | @Selector@ for @jwksTrustedRootCertificates@
jwksTrustedRootCertificatesSelector :: Selector '[] (Id NSArray)
jwksTrustedRootCertificatesSelector = mkSelector "jwksTrustedRootCertificates"

-- | @Selector@ for @setJwksTrustedRootCertificates:@
setJwksTrustedRootCertificatesSelector :: Selector '[Id NSArray] ()
setJwksTrustedRootCertificatesSelector = mkSelector "setJwksTrustedRootCertificates:"

-- | @Selector@ for @deviceContext@
deviceContextSelector :: Selector '[] (Id NSData)
deviceContextSelector = mkSelector "deviceContext"

-- | @Selector@ for @setDeviceContext:@
setDeviceContextSelector :: Selector '[Id NSData] ()
setDeviceContextSelector = mkSelector "setDeviceContext:"

-- | @Selector@ for @userSecureEnclaveKeyBiometricPolicy@
userSecureEnclaveKeyBiometricPolicySelector :: Selector '[] ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy
userSecureEnclaveKeyBiometricPolicySelector = mkSelector "userSecureEnclaveKeyBiometricPolicy"

-- | @Selector@ for @setUserSecureEnclaveKeyBiometricPolicy:@
setUserSecureEnclaveKeyBiometricPolicySelector :: Selector '[ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy] ()
setUserSecureEnclaveKeyBiometricPolicySelector = mkSelector "setUserSecureEnclaveKeyBiometricPolicy:"

-- | @Selector@ for @nonceEndpointURL@
nonceEndpointURLSelector :: Selector '[] (Id NSURL)
nonceEndpointURLSelector = mkSelector "nonceEndpointURL"

-- | @Selector@ for @setNonceEndpointURL:@
setNonceEndpointURLSelector :: Selector '[Id NSURL] ()
setNonceEndpointURLSelector = mkSelector "setNonceEndpointURL:"

-- | @Selector@ for @nonceResponseKeypath@
nonceResponseKeypathSelector :: Selector '[] (Id NSString)
nonceResponseKeypathSelector = mkSelector "nonceResponseKeypath"

-- | @Selector@ for @setNonceResponseKeypath:@
setNonceResponseKeypathSelector :: Selector '[Id NSString] ()
setNonceResponseKeypathSelector = mkSelector "setNonceResponseKeypath:"

-- | @Selector@ for @serverNonceClaimName@
serverNonceClaimNameSelector :: Selector '[] (Id NSString)
serverNonceClaimNameSelector = mkSelector "serverNonceClaimName"

-- | @Selector@ for @setServerNonceClaimName:@
setServerNonceClaimNameSelector :: Selector '[Id NSString] ()
setServerNonceClaimNameSelector = mkSelector "setServerNonceClaimName:"

-- | @Selector@ for @customNonceRequestValues@
customNonceRequestValuesSelector :: Selector '[] (Id NSArray)
customNonceRequestValuesSelector = mkSelector "customNonceRequestValues"

-- | @Selector@ for @setCustomNonceRequestValues:@
setCustomNonceRequestValuesSelector :: Selector '[Id NSArray] ()
setCustomNonceRequestValuesSelector = mkSelector "setCustomNonceRequestValues:"

-- | @Selector@ for @additionalScopes@
additionalScopesSelector :: Selector '[] (Id NSString)
additionalScopesSelector = mkSelector "additionalScopes"

-- | @Selector@ for @setAdditionalScopes:@
setAdditionalScopesSelector :: Selector '[Id NSString] ()
setAdditionalScopesSelector = mkSelector "setAdditionalScopes:"

-- | @Selector@ for @additionalAuthorizationScopes@
additionalAuthorizationScopesSelector :: Selector '[] (Id NSString)
additionalAuthorizationScopesSelector = mkSelector "additionalAuthorizationScopes"

-- | @Selector@ for @setAdditionalAuthorizationScopes:@
setAdditionalAuthorizationScopesSelector :: Selector '[Id NSString] ()
setAdditionalAuthorizationScopesSelector = mkSelector "setAdditionalAuthorizationScopes:"

-- | @Selector@ for @includePreviousRefreshTokenInLoginRequest@
includePreviousRefreshTokenInLoginRequestSelector :: Selector '[] Bool
includePreviousRefreshTokenInLoginRequestSelector = mkSelector "includePreviousRefreshTokenInLoginRequest"

-- | @Selector@ for @setIncludePreviousRefreshTokenInLoginRequest:@
setIncludePreviousRefreshTokenInLoginRequestSelector :: Selector '[Bool] ()
setIncludePreviousRefreshTokenInLoginRequestSelector = mkSelector "setIncludePreviousRefreshTokenInLoginRequest:"

-- | @Selector@ for @previousRefreshTokenClaimName@
previousRefreshTokenClaimNameSelector :: Selector '[] (Id NSString)
previousRefreshTokenClaimNameSelector = mkSelector "previousRefreshTokenClaimName"

-- | @Selector@ for @setPreviousRefreshTokenClaimName:@
setPreviousRefreshTokenClaimNameSelector :: Selector '[Id NSString] ()
setPreviousRefreshTokenClaimNameSelector = mkSelector "setPreviousRefreshTokenClaimName:"

-- | @Selector@ for @customRequestJWTParameterName@
customRequestJWTParameterNameSelector :: Selector '[] (Id NSString)
customRequestJWTParameterNameSelector = mkSelector "customRequestJWTParameterName"

-- | @Selector@ for @setCustomRequestJWTParameterName:@
setCustomRequestJWTParameterNameSelector :: Selector '[Id NSString] ()
setCustomRequestJWTParameterNameSelector = mkSelector "setCustomRequestJWTParameterName:"

-- | @Selector@ for @customLoginRequestValues@
customLoginRequestValuesSelector :: Selector '[] (Id NSArray)
customLoginRequestValuesSelector = mkSelector "customLoginRequestValues"

-- | @Selector@ for @setCustomLoginRequestValues:@
setCustomLoginRequestValuesSelector :: Selector '[Id NSArray] ()
setCustomLoginRequestValuesSelector = mkSelector "setCustomLoginRequestValues:"

-- | @Selector@ for @uniqueIdentifierClaimName@
uniqueIdentifierClaimNameSelector :: Selector '[] (Id NSString)
uniqueIdentifierClaimNameSelector = mkSelector "uniqueIdentifierClaimName"

-- | @Selector@ for @setUniqueIdentifierClaimName:@
setUniqueIdentifierClaimNameSelector :: Selector '[Id NSString] ()
setUniqueIdentifierClaimNameSelector = mkSelector "setUniqueIdentifierClaimName:"

-- | @Selector@ for @groupRequestClaimName@
groupRequestClaimNameSelector :: Selector '[] (Id NSString)
groupRequestClaimNameSelector = mkSelector "groupRequestClaimName"

-- | @Selector@ for @setGroupRequestClaimName:@
setGroupRequestClaimNameSelector :: Selector '[Id NSString] ()
setGroupRequestClaimNameSelector = mkSelector "setGroupRequestClaimName:"

-- | @Selector@ for @groupResponseClaimName@
groupResponseClaimNameSelector :: Selector '[] (Id NSString)
groupResponseClaimNameSelector = mkSelector "groupResponseClaimName"

-- | @Selector@ for @setGroupResponseClaimName:@
setGroupResponseClaimNameSelector :: Selector '[Id NSString] ()
setGroupResponseClaimNameSelector = mkSelector "setGroupResponseClaimName:"

-- | @Selector@ for @kerberosTicketMappings@
kerberosTicketMappingsSelector :: Selector '[] (Id NSArray)
kerberosTicketMappingsSelector = mkSelector "kerberosTicketMappings"

-- | @Selector@ for @setKerberosTicketMappings:@
setKerberosTicketMappingsSelector :: Selector '[Id NSArray] ()
setKerberosTicketMappingsSelector = mkSelector "setKerberosTicketMappings:"

-- | @Selector@ for @refreshEndpointURL@
refreshEndpointURLSelector :: Selector '[] (Id NSURL)
refreshEndpointURLSelector = mkSelector "refreshEndpointURL"

-- | @Selector@ for @setRefreshEndpointURL:@
setRefreshEndpointURLSelector :: Selector '[Id NSURL] ()
setRefreshEndpointURLSelector = mkSelector "setRefreshEndpointURL:"

-- | @Selector@ for @customRefreshRequestValues@
customRefreshRequestValuesSelector :: Selector '[] (Id NSArray)
customRefreshRequestValuesSelector = mkSelector "customRefreshRequestValues"

-- | @Selector@ for @setCustomRefreshRequestValues:@
setCustomRefreshRequestValuesSelector :: Selector '[Id NSArray] ()
setCustomRefreshRequestValuesSelector = mkSelector "setCustomRefreshRequestValues:"

-- | @Selector@ for @federationType@
federationTypeSelector :: Selector '[] ASAuthorizationProviderExtensionFederationType
federationTypeSelector = mkSelector "federationType"

-- | @Selector@ for @setFederationType:@
setFederationTypeSelector :: Selector '[ASAuthorizationProviderExtensionFederationType] ()
setFederationTypeSelector = mkSelector "setFederationType:"

-- | @Selector@ for @federationRequestURN@
federationRequestURNSelector :: Selector '[] (Id NSString)
federationRequestURNSelector = mkSelector "federationRequestURN"

-- | @Selector@ for @setFederationRequestURN:@
setFederationRequestURNSelector :: Selector '[Id NSString] ()
setFederationRequestURNSelector = mkSelector "setFederationRequestURN:"

-- | @Selector@ for @federationMEXURL@
federationMEXURLSelector :: Selector '[] (Id NSURL)
federationMEXURLSelector = mkSelector "federationMEXURL"

-- | @Selector@ for @setFederationMEXURL:@
setFederationMEXURLSelector :: Selector '[Id NSURL] ()
setFederationMEXURLSelector = mkSelector "setFederationMEXURL:"

-- | @Selector@ for @federationUserPreauthenticationURL@
federationUserPreauthenticationURLSelector :: Selector '[] (Id NSURL)
federationUserPreauthenticationURLSelector = mkSelector "federationUserPreauthenticationURL"

-- | @Selector@ for @setFederationUserPreauthenticationURL:@
setFederationUserPreauthenticationURLSelector :: Selector '[Id NSURL] ()
setFederationUserPreauthenticationURLSelector = mkSelector "setFederationUserPreauthenticationURL:"

-- | @Selector@ for @federationMEXURLKeypath@
federationMEXURLKeypathSelector :: Selector '[] (Id NSString)
federationMEXURLKeypathSelector = mkSelector "federationMEXURLKeypath"

-- | @Selector@ for @setFederationMEXURLKeypath:@
setFederationMEXURLKeypathSelector :: Selector '[Id NSString] ()
setFederationMEXURLKeypathSelector = mkSelector "setFederationMEXURLKeypath:"

-- | @Selector@ for @federationPredicate@
federationPredicateSelector :: Selector '[] (Id NSString)
federationPredicateSelector = mkSelector "federationPredicate"

-- | @Selector@ for @setFederationPredicate:@
setFederationPredicateSelector :: Selector '[Id NSString] ()
setFederationPredicateSelector = mkSelector "setFederationPredicate:"

-- | @Selector@ for @customFederationUserPreauthenticationRequestValues@
customFederationUserPreauthenticationRequestValuesSelector :: Selector '[] (Id NSArray)
customFederationUserPreauthenticationRequestValuesSelector = mkSelector "customFederationUserPreauthenticationRequestValues"

-- | @Selector@ for @setCustomFederationUserPreauthenticationRequestValues:@
setCustomFederationUserPreauthenticationRequestValuesSelector :: Selector '[Id NSArray] ()
setCustomFederationUserPreauthenticationRequestValuesSelector = mkSelector "setCustomFederationUserPreauthenticationRequestValues:"

-- | @Selector@ for @loginRequestEncryptionPublicKey@
loginRequestEncryptionPublicKeySelector :: Selector '[] (Ptr ())
loginRequestEncryptionPublicKeySelector = mkSelector "loginRequestEncryptionPublicKey"

-- | @Selector@ for @setLoginRequestEncryptionPublicKey:@
setLoginRequestEncryptionPublicKeySelector :: Selector '[Ptr ()] ()
setLoginRequestEncryptionPublicKeySelector = mkSelector "setLoginRequestEncryptionPublicKey:"

-- | @Selector@ for @loginRequestEncryptionAPVPrefix@
loginRequestEncryptionAPVPrefixSelector :: Selector '[] (Id NSData)
loginRequestEncryptionAPVPrefixSelector = mkSelector "loginRequestEncryptionAPVPrefix"

-- | @Selector@ for @setLoginRequestEncryptionAPVPrefix:@
setLoginRequestEncryptionAPVPrefixSelector :: Selector '[Id NSData] ()
setLoginRequestEncryptionAPVPrefixSelector = mkSelector "setLoginRequestEncryptionAPVPrefix:"

-- | @Selector@ for @loginRequestEncryptionAlgorithm@
loginRequestEncryptionAlgorithmSelector :: Selector '[] (Id NSNumber)
loginRequestEncryptionAlgorithmSelector = mkSelector "loginRequestEncryptionAlgorithm"

-- | @Selector@ for @setLoginRequestEncryptionAlgorithm:@
setLoginRequestEncryptionAlgorithmSelector :: Selector '[Id NSNumber] ()
setLoginRequestEncryptionAlgorithmSelector = mkSelector "setLoginRequestEncryptionAlgorithm:"

-- | @Selector@ for @loginRequestHPKEPreSharedKey@
loginRequestHPKEPreSharedKeySelector :: Selector '[] (Id NSData)
loginRequestHPKEPreSharedKeySelector = mkSelector "loginRequestHPKEPreSharedKey"

-- | @Selector@ for @setLoginRequestHPKEPreSharedKey:@
setLoginRequestHPKEPreSharedKeySelector :: Selector '[Id NSData] ()
setLoginRequestHPKEPreSharedKeySelector = mkSelector "setLoginRequestHPKEPreSharedKey:"

-- | @Selector@ for @loginRequestHPKEPreSharedKeyID@
loginRequestHPKEPreSharedKeyIDSelector :: Selector '[] (Id NSData)
loginRequestHPKEPreSharedKeyIDSelector = mkSelector "loginRequestHPKEPreSharedKeyID"

-- | @Selector@ for @setLoginRequestHPKEPreSharedKeyID:@
setLoginRequestHPKEPreSharedKeyIDSelector :: Selector '[Id NSData] ()
setLoginRequestHPKEPreSharedKeyIDSelector = mkSelector "setLoginRequestHPKEPreSharedKeyID:"

-- | @Selector@ for @keyEndpointURL@
keyEndpointURLSelector :: Selector '[] (Id NSURL)
keyEndpointURLSelector = mkSelector "keyEndpointURL"

-- | @Selector@ for @setKeyEndpointURL:@
setKeyEndpointURLSelector :: Selector '[Id NSURL] ()
setKeyEndpointURLSelector = mkSelector "setKeyEndpointURL:"

-- | @Selector@ for @customKeyExchangeRequestValues@
customKeyExchangeRequestValuesSelector :: Selector '[] (Id NSArray)
customKeyExchangeRequestValuesSelector = mkSelector "customKeyExchangeRequestValues"

-- | @Selector@ for @setCustomKeyExchangeRequestValues:@
setCustomKeyExchangeRequestValuesSelector :: Selector '[Id NSArray] ()
setCustomKeyExchangeRequestValuesSelector = mkSelector "setCustomKeyExchangeRequestValues:"

-- | @Selector@ for @customKeyRequestValues@
customKeyRequestValuesSelector :: Selector '[] (Id NSArray)
customKeyRequestValuesSelector = mkSelector "customKeyRequestValues"

-- | @Selector@ for @setCustomKeyRequestValues:@
setCustomKeyRequestValuesSelector :: Selector '[Id NSArray] ()
setCustomKeyRequestValuesSelector = mkSelector "setCustomKeyRequestValues:"

-- | @Selector@ for @hpkePreSharedKey@
hpkePreSharedKeySelector :: Selector '[] (Id NSData)
hpkePreSharedKeySelector = mkSelector "hpkePreSharedKey"

-- | @Selector@ for @setHpkePreSharedKey:@
setHpkePreSharedKeySelector :: Selector '[Id NSData] ()
setHpkePreSharedKeySelector = mkSelector "setHpkePreSharedKey:"

-- | @Selector@ for @hpkePreSharedKeyID@
hpkePreSharedKeyIDSelector :: Selector '[] (Id NSData)
hpkePreSharedKeyIDSelector = mkSelector "hpkePreSharedKeyID"

-- | @Selector@ for @setHpkePreSharedKeyID:@
setHpkePreSharedKeyIDSelector :: Selector '[Id NSData] ()
setHpkePreSharedKeyIDSelector = mkSelector "setHpkePreSharedKeyID:"

-- | @Selector@ for @hpkeAuthPublicKey@
hpkeAuthPublicKeySelector :: Selector '[] (Ptr ())
hpkeAuthPublicKeySelector = mkSelector "hpkeAuthPublicKey"

-- | @Selector@ for @setHpkeAuthPublicKey:@
setHpkeAuthPublicKeySelector :: Selector '[Ptr ()] ()
setHpkeAuthPublicKeySelector = mkSelector "setHpkeAuthPublicKey:"

