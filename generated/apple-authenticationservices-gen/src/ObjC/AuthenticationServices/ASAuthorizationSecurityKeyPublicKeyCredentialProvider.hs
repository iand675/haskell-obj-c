{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialProvider@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialProvider
  ( ASAuthorizationSecurityKeyPublicKeyCredentialProvider
  , IsASAuthorizationSecurityKeyPublicKeyCredentialProvider(..)
  , initWithRelyingPartyIdentifier
  , createCredentialRegistrationRequestWithChallenge_displayName_name_userID
  , createCredentialAssertionRequestWithChallenge
  , init_
  , new
  , relyingPartyIdentifier
  , createCredentialAssertionRequestWithChallengeSelector
  , createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector
  , initSelector
  , initWithRelyingPartyIdentifierSelector
  , newSelector
  , relyingPartyIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifier :: (IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider, IsNSString relyingPartyIdentifier) => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> relyingPartyIdentifier -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
initWithRelyingPartyIdentifier asAuthorizationSecurityKeyPublicKeyCredentialProvider relyingPartyIdentifier =
  sendOwnedMessage asAuthorizationSecurityKeyPublicKeyCredentialProvider initWithRelyingPartyIdentifierSelector (toNSString relyingPartyIdentifier)

-- | Create a request to register a new security key credential.
--
-- @challenge@ — The challenge to sign.
--
-- @displayName@ — The display name for the new credential.
--
-- @name@ — The name for the new credential.
--
-- @userID@ — An identifier to be stored alongside the credential, which will be returned with the credential when it is used to authenticate.
--
-- ObjC selector: @- createCredentialRegistrationRequestWithChallenge:displayName:name:userID:@
createCredentialRegistrationRequestWithChallenge_displayName_name_userID :: (IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider, IsNSData challenge, IsNSString displayName, IsNSString name, IsNSData userID) => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> challenge -> displayName -> name -> userID -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
createCredentialRegistrationRequestWithChallenge_displayName_name_userID asAuthorizationSecurityKeyPublicKeyCredentialProvider challenge displayName name userID =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialProvider createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector (toNSData challenge) (toNSString displayName) (toNSString name) (toNSData userID)

-- | Create a request to authenticate using an existing credential.
--
-- @challenge@ — The challenge to sign.
--
-- ObjC selector: @- createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallenge :: (IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider, IsNSData challenge) => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> challenge -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest)
createCredentialAssertionRequestWithChallenge asAuthorizationSecurityKeyPublicKeyCredentialProvider challenge =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialProvider createCredentialAssertionRequestWithChallengeSelector (toNSData challenge)

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
init_ asAuthorizationSecurityKeyPublicKeyCredentialProvider =
  sendOwnedMessage asAuthorizationSecurityKeyPublicKeyCredentialProvider initSelector

-- | @+ new@
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialProvider"
    sendOwnedClassMessage cls' newSelector

-- | The Relying Party identifier used for all requests created by this object.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> IO (Id NSString)
relyingPartyIdentifier asAuthorizationSecurityKeyPublicKeyCredentialProvider =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialProvider relyingPartyIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifierSelector :: Selector '[Id NSString] (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
initWithRelyingPartyIdentifierSelector = mkSelector "initWithRelyingPartyIdentifier:"

-- | @Selector@ for @createCredentialRegistrationRequestWithChallenge:displayName:name:userID:@
createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector :: Selector '[Id NSData, Id NSString, Id NSString, Id NSData] (Id ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest)
createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector = mkSelector "createCredentialRegistrationRequestWithChallenge:displayName:name:userID:"

-- | @Selector@ for @createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallengeSelector :: Selector '[Id NSData] (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest)
createCredentialAssertionRequestWithChallengeSelector = mkSelector "createCredentialAssertionRequestWithChallenge:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
newSelector = mkSelector "new"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector '[] (Id NSString)
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

