{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRelyingPartyIdentifierSelector
  , createCredentialRegistrationRequestWithChallenge_name_userIDSelector
  , createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector
  , createCredentialAssertionRequestWithChallengeSelector
  , newSelector
  , initSelector
  , relyingPartyIdentifierSelector

  -- * Enum types
  , ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle(ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle)
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleStandard
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleConditional

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

-- | @- initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifier :: (IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider, IsNSString relyingPartyIdentifier) => asAuthorizationPlatformPublicKeyCredentialProvider -> relyingPartyIdentifier -> IO (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
initWithRelyingPartyIdentifier asAuthorizationPlatformPublicKeyCredentialProvider  relyingPartyIdentifier =
withObjCPtr relyingPartyIdentifier $ \raw_relyingPartyIdentifier ->
    sendMsg asAuthorizationPlatformPublicKeyCredentialProvider (mkSelector "initWithRelyingPartyIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_relyingPartyIdentifier :: Ptr ())] >>= ownedObject . castPtr

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
createCredentialRegistrationRequestWithChallenge_name_userID asAuthorizationPlatformPublicKeyCredentialProvider  challenge name userID =
withObjCPtr challenge $ \raw_challenge ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr userID $ \raw_userID ->
        sendMsg asAuthorizationPlatformPublicKeyCredentialProvider (mkSelector "createCredentialRegistrationRequestWithChallenge:name:userID:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_userID :: Ptr ())] >>= retainedObject . castPtr

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
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyle asAuthorizationPlatformPublicKeyCredentialProvider  challenge name userID requestStyle =
withObjCPtr challenge $ \raw_challenge ->
  withObjCPtr name $ \raw_name ->
    withObjCPtr userID $ \raw_userID ->
        sendMsg asAuthorizationPlatformPublicKeyCredentialProvider (mkSelector "createCredentialRegistrationRequestWithChallenge:name:userID:requestStyle:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_userID :: Ptr ()), argCLong (coerce requestStyle)] >>= retainedObject . castPtr

-- | Create a request to authenticate using an existing credential.
--
-- @challenge@ — The challenge to sign.
--
-- ObjC selector: @- createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallenge :: (IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider, IsNSData challenge) => asAuthorizationPlatformPublicKeyCredentialProvider -> challenge -> IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
createCredentialAssertionRequestWithChallenge asAuthorizationPlatformPublicKeyCredentialProvider  challenge =
withObjCPtr challenge $ \raw_challenge ->
    sendMsg asAuthorizationPlatformPublicKeyCredentialProvider (mkSelector "createCredentialAssertionRequestWithChallenge:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ())] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider => asAuthorizationPlatformPublicKeyCredentialProvider -> IO (Id ASAuthorizationPlatformPublicKeyCredentialProvider)
init_ asAuthorizationPlatformPublicKeyCredentialProvider  =
  sendMsg asAuthorizationPlatformPublicKeyCredentialProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Relying Party identifier used for all requests created by this object.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASAuthorizationPlatformPublicKeyCredentialProvider asAuthorizationPlatformPublicKeyCredentialProvider => asAuthorizationPlatformPublicKeyCredentialProvider -> IO (Id NSString)
relyingPartyIdentifier asAuthorizationPlatformPublicKeyCredentialProvider  =
  sendMsg asAuthorizationPlatformPublicKeyCredentialProvider (mkSelector "relyingPartyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifierSelector :: Selector
initWithRelyingPartyIdentifierSelector = mkSelector "initWithRelyingPartyIdentifier:"

-- | @Selector@ for @createCredentialRegistrationRequestWithChallenge:name:userID:@
createCredentialRegistrationRequestWithChallenge_name_userIDSelector :: Selector
createCredentialRegistrationRequestWithChallenge_name_userIDSelector = mkSelector "createCredentialRegistrationRequestWithChallenge:name:userID:"

-- | @Selector@ for @createCredentialRegistrationRequestWithChallenge:name:userID:requestStyle:@
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector :: Selector
createCredentialRegistrationRequestWithChallenge_name_userID_requestStyleSelector = mkSelector "createCredentialRegistrationRequestWithChallenge:name:userID:requestStyle:"

-- | @Selector@ for @createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallengeSelector :: Selector
createCredentialAssertionRequestWithChallengeSelector = mkSelector "createCredentialAssertionRequestWithChallenge:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

