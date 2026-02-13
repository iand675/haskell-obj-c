{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationAppleIDCredential@.
module ObjC.AuthenticationServices.ASAuthorizationAppleIDCredential
  ( ASAuthorizationAppleIDCredential
  , IsASAuthorizationAppleIDCredential(..)
  , new
  , init_
  , user
  , state
  , authorizedScopes
  , authorizationCode
  , identityToken
  , email
  , fullName
  , realUserStatus
  , userAgeRange
  , authorizationCodeSelector
  , authorizedScopesSelector
  , emailSelector
  , fullNameSelector
  , identityTokenSelector
  , initSelector
  , newSelector
  , realUserStatusSelector
  , stateSelector
  , userAgeRangeSelector
  , userSelector

  -- * Enum types
  , ASUserAgeRange(ASUserAgeRange)
  , pattern ASUserAgeRangeUnknown
  , pattern ASUserAgeRangeChild
  , pattern ASUserAgeRangeNotChild
  , ASUserDetectionStatus(ASUserDetectionStatus)
  , pattern ASUserDetectionStatusUnsupported
  , pattern ASUserDetectionStatusUnknown
  , pattern ASUserDetectionStatusLikelyReal

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
new :: IO (Id ASAuthorizationAppleIDCredential)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationAppleIDCredential"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id ASAuthorizationAppleIDCredential)
init_ asAuthorizationAppleIDCredential =
  sendOwnedMessage asAuthorizationAppleIDCredential initSelector

-- | An opaque user ID associated with the AppleID used for the sign in. This identifier will be stable across the 'developer team', it can later be used as an input to
--
-- See: ASAuthorizationRequest to request user contact information.
--
-- The identifier will remain stable as long as the user is connected with the requesting client.  The value may change upon user disconnecting from the identity provider.
--
-- ObjC selector: @- user@
user :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSString)
user asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential userSelector

-- | A copy of the state value that was passed to ASAuthorizationRequest.
--
-- ObjC selector: @- state@
state :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSString)
state asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential stateSelector

-- | This value will contain a list of scopes for which the user provided authorization.  These may contain a subset of the requested scopes on
--
-- See: ASAuthorizationAppleIDRequest.  The application should query this value to identify which scopes were returned as it maybe different from ones requested.
--
-- ObjC selector: @- authorizedScopes@
authorizedScopes :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSArray)
authorizedScopes asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential authorizedScopesSelector

-- | A short-lived, one-time valid token that provides proof of authorization to the server component of the app. The authorization code is bound to the specific transaction using the state attribute passed in the authorization request. The server component of the app can validate the code using Apple’s identity service endpoint provided for this purpose.
--
-- ObjC selector: @- authorizationCode@
authorizationCode :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSData)
authorizationCode asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential authorizationCodeSelector

-- | A JSON Web Token (JWT) used to communicate information about the identity of the user in a secure way to the app. The ID token will contain the following information: Issuer Identifier, Subject Identifier, Audience, Expiry Time and Issuance Time signed by Apple's identity service.
--
-- ObjC selector: @- identityToken@
identityToken :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSData)
identityToken asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential identityTokenSelector

-- | An optional email shared by the user.  This field is populated with a value that the user authorized.
--
-- ObjC selector: @- email@
email :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSString)
email asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential emailSelector

-- | An optional full name shared by the user.  This field is populated with a value that the user authorized.
--
-- ObjC selector: @- fullName@
fullName :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSPersonNameComponents)
fullName asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential fullNameSelector

-- | Check this property for a hint as to whether the current user is a "real user".
--
-- See: ASUserDetectionStatus for guidelines on handling each status
--
-- ObjC selector: @- realUserStatus@
realUserStatus :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO ASUserDetectionStatus
realUserStatus asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential realUserStatusSelector

-- | Check this property to determine whether the current user is a child.
--
-- See: ASUserAgeRange for guidelines on handling each status.
--
-- ObjC selector: @- userAgeRange@
userAgeRange :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO ASUserAgeRange
userAgeRange asAuthorizationAppleIDCredential =
  sendMessage asAuthorizationAppleIDCredential userAgeRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationAppleIDCredential)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationAppleIDCredential)
initSelector = mkSelector "init"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSString)
stateSelector = mkSelector "state"

-- | @Selector@ for @authorizedScopes@
authorizedScopesSelector :: Selector '[] (Id NSArray)
authorizedScopesSelector = mkSelector "authorizedScopes"

-- | @Selector@ for @authorizationCode@
authorizationCodeSelector :: Selector '[] (Id NSData)
authorizationCodeSelector = mkSelector "authorizationCode"

-- | @Selector@ for @identityToken@
identityTokenSelector :: Selector '[] (Id NSData)
identityTokenSelector = mkSelector "identityToken"

-- | @Selector@ for @email@
emailSelector :: Selector '[] (Id NSString)
emailSelector = mkSelector "email"

-- | @Selector@ for @fullName@
fullNameSelector :: Selector '[] (Id NSPersonNameComponents)
fullNameSelector = mkSelector "fullName"

-- | @Selector@ for @realUserStatus@
realUserStatusSelector :: Selector '[] ASUserDetectionStatus
realUserStatusSelector = mkSelector "realUserStatus"

-- | @Selector@ for @userAgeRange@
userAgeRangeSelector :: Selector '[] ASUserAgeRange
userAgeRangeSelector = mkSelector "userAgeRange"

