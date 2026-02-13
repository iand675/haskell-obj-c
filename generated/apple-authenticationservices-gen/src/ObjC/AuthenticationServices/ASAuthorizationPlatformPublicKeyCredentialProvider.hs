{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPlatformPublicKeyCredentialProvider@.
module ObjC.AuthenticationServices.ASAuthorizationPlatformPublicKeyCredentialProvider
  ( ASAuthorizationPlatformPublicKeyCredentialProvider
  , IsASAuthorizationPlatformPublicKeyCredentialProvider(..)
  , initWithRelyingPartyIdentifier
  , createCredentialRegistrationRequestWithChallenge_name_userID
  , createCredentialRegistrationRequestWithChallenge_name_userID_requestStyle
  , createCredentialAssertionRequestWithChallenge
  , new
  , init_
  , relyingPartyIdentifier
  , createCredentialAssertionRequestWithChallengeSelector
  , createCredentialRegistrationRequestWithChallenge_name_userIDSelector
  , createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector
  , initSelector
  , initWithRelyingPartyIdentifierSelector
  , newSelector
  , relyingPartyIdentifierSelector

  -- * Enum types
  , ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle(ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle)
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleStandard
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleConditional

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

-- | @- initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifier :: (IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider, IsNSString relyingPartyIdentifier) => asAuthorizationPlatformPublicKeyCredentialProvider -> relyingPartyIdentifier -> IO (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
initWithRelyingPartyIdentifier asAuthorizationPlatformPublicKeyCredentialProvider relyingPartyIdentifier =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialProvider initWithRelyingPartyIdentifierSelector (toNSString relyingPartyIdentifier)

-- | Create a request to register a new platform credential.
--
-- @challenge@ — The challenge to sign.
--
-- @name@ — The user name for the new credential.
--
-- @userID@ — An identifier to be stored alongside the credential, which will be returned with the credential when it is used to authenticate.
--
-- ObjC selector: @- createCredentialRegistrationRequestWithChallenge:name:userID:@
createCredentialRegistrationRequestWithChallenge_name_userID :: (IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider, IsNSData challenge, IsNSString name, IsNSData userID) => asAuthorizationPlatformPublicKeyCredentialProvider -> challenge -> name -> userID -> IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
createCredentialRegistrationRequestWithChallenge_name_userID asAuthorizationPlatformPublicKeyCredentialProvider challenge name userID =
  sendMessage asAuthorizationPlatformPublicKeyCredentialProvider createCredentialRegistrationRequestWithChallenge_name_userIDSelector (toNSData challenge) (toNSString name) (toNSData userID)

-- | Create a request to register a new platform credential.
--
-- @challenge@ — The challenge to sign.
--
-- @name@ — The user name for the new credential.
--
-- @userID@ — An identifier to be stored alongside the credential, which will be returned with the credential when it is used to authenticate.
--
-- @requestStyle@ — The style for this request.
--
-- ObjC selector: @- createCredentialRegistrationRequestWithChallenge:name:userID:requestStyle:@
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyle :: (IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider, IsNSData challenge, IsNSString name, IsNSData userID) => asAuthorizationPlatformPublicKeyCredentialProvider -> challenge -> name -> userID -> ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle -> IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyle asAuthorizationPlatformPublicKeyCredentialProvider challenge name userID requestStyle =
  sendMessage asAuthorizationPlatformPublicKeyCredentialProvider createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector (toNSData challenge) (toNSString name) (toNSData userID) requestStyle

-- | Create a request to authenticate using an existing credential.
--
-- @challenge@ — The challenge to sign.
--
-- ObjC selector: @- createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallenge :: (IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider, IsNSData challenge) => asAuthorizationPlatformPublicKeyCredentialProvider -> challenge -> IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
createCredentialAssertionRequestWithChallenge asAuthorizationPlatformPublicKeyCredentialProvider challenge =
  sendMessage asAuthorizationPlatformPublicKeyCredentialProvider createCredentialAssertionRequestWithChallengeSelector (toNSData challenge)

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialProvider"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider => asAuthorizationPlatformPublicKeyCredentialProvider -> IO (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
init_ asAuthorizationPlatformPublicKeyCredentialProvider =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialProvider initSelector

-- | The Relying Party identifier used for all requests created by this object.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider => asAuthorizationPlatformPublicKeyCredentialProvider -> IO (Id NSString)
relyingPartyIdentifier asAuthorizationPlatformPublicKeyCredentialProvider =
  sendMessage asAuthorizationPlatformPublicKeyCredentialProvider relyingPartyIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifierSelector :: Selector '[Id NSString] (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
initWithRelyingPartyIdentifierSelector = mkSelector "initWithRelyingPartyIdentifier:"

-- | @Selector@ for @createCredentialRegistrationRequestWithChallenge:name:userID:@
createCredentialRegistrationRequestWithChallenge_name_userIDSelector :: Selector '[Id NSData, Id NSString, Id NSData] (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
createCredentialRegistrationRequestWithChallenge_name_userIDSelector = mkSelector "createCredentialRegistrationRequestWithChallenge:name:userID:"

-- | @Selector@ for @createCredentialRegistrationRequestWithChallenge:name:userID:requestStyle:@
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector :: Selector '[Id NSData, Id NSString, Id NSData, ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle] (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector = mkSelector "createCredentialRegistrationRequestWithChallenge:name:userID:requestStyle:"

-- | @Selector@ for @createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallengeSelector :: Selector '[Id NSData] (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
createCredentialAssertionRequestWithChallengeSelector = mkSelector "createCredentialAssertionRequestWithChallenge:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector '[] (Id NSString)
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

