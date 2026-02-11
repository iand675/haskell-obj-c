{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , userSelector
  , stateSelector
  , authorizedScopesSelector
  , authorizationCodeSelector
  , identityTokenSelector
  , emailSelector
  , fullNameSelector
  , realUserStatusSelector
  , userAgeRangeSelector

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
new :: IO (Id ASAuthorizationAppleIDCredential)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationAppleIDCredential"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id ASAuthorizationAppleIDCredential)
init_ asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An opaque user ID associated with the AppleID used for the sign in. This identifier will be stable across the 'developer team', it can later be used as an input to
--
-- See: ASAuthorizationRequest to request user contact information.
--
-- The identifier will remain stable as long as the user is connected with the requesting client.  The value may change upon user disconnecting from the identity provider.
--
-- ObjC selector: @- user@
user :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSString)
user asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A copy of the state value that was passed to ASAuthorizationRequest.
--
-- ObjC selector: @- state@
state :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSString)
state asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This value will contain a list of scopes for which the user provided authorization.  These may contain a subset of the requested scopes on
--
-- See: ASAuthorizationAppleIDRequest.  The application should query this value to identify which scopes were returned as it maybe different from ones requested.
--
-- ObjC selector: @- authorizedScopes@
authorizedScopes :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSArray)
authorizedScopes asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "authorizedScopes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A short-lived, one-time valid token that provides proof of authorization to the server component of the app. The authorization code is bound to the specific transaction using the state attribute passed in the authorization request. The server component of the app can validate the code using Apple’s identity service endpoint provided for this purpose.
--
-- ObjC selector: @- authorizationCode@
authorizationCode :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSData)
authorizationCode asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "authorizationCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A JSON Web Token (JWT) used to communicate information about the identity of the user in a secure way to the app. The ID token will contain the following information: Issuer Identifier, Subject Identifier, Audience, Expiry Time and Issuance Time signed by Apple's identity service.
--
-- ObjC selector: @- identityToken@
identityToken :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSData)
identityToken asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "identityToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional email shared by the user.  This field is populated with a value that the user authorized.
--
-- ObjC selector: @- email@
email :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSString)
email asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "email") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional full name shared by the user.  This field is populated with a value that the user authorized.
--
-- ObjC selector: @- fullName@
fullName :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO (Id NSPersonNameComponents)
fullName asAuthorizationAppleIDCredential  =
  sendMsg asAuthorizationAppleIDCredential (mkSelector "fullName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Check this property for a hint as to whether the current user is a "real user".
--
-- See: ASUserDetectionStatus for guidelines on handling each status
--
-- ObjC selector: @- realUserStatus@
realUserStatus :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO ASUserDetectionStatus
realUserStatus asAuthorizationAppleIDCredential  =
  fmap (coerce :: CLong -> ASUserDetectionStatus) $ sendMsg asAuthorizationAppleIDCredential (mkSelector "realUserStatus") retCLong []

-- | Check this property to determine whether the current user is a child.
--
-- See: ASUserAgeRange for guidelines on handling each status.
--
-- ObjC selector: @- userAgeRange@
userAgeRange :: IsASAuthorizationAppleIDCredential asAuthorizationAppleIDCredential => asAuthorizationAppleIDCredential -> IO ASUserAgeRange
userAgeRange asAuthorizationAppleIDCredential  =
  fmap (coerce :: CLong -> ASUserAgeRange) $ sendMsg asAuthorizationAppleIDCredential (mkSelector "userAgeRange") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @authorizedScopes@
authorizedScopesSelector :: Selector
authorizedScopesSelector = mkSelector "authorizedScopes"

-- | @Selector@ for @authorizationCode@
authorizationCodeSelector :: Selector
authorizationCodeSelector = mkSelector "authorizationCode"

-- | @Selector@ for @identityToken@
identityTokenSelector :: Selector
identityTokenSelector = mkSelector "identityToken"

-- | @Selector@ for @email@
emailSelector :: Selector
emailSelector = mkSelector "email"

-- | @Selector@ for @fullName@
fullNameSelector :: Selector
fullNameSelector = mkSelector "fullName"

-- | @Selector@ for @realUserStatus@
realUserStatusSelector :: Selector
realUserStatusSelector = mkSelector "realUserStatus"

-- | @Selector@ for @userAgeRange@
userAgeRangeSelector :: Selector
userAgeRangeSelector = mkSelector "userAgeRange"

