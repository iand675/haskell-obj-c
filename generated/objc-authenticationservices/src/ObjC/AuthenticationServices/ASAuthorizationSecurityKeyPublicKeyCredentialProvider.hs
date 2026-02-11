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
  , initWithRelyingPartyIdentifierSelector
  , createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector
  , createCredentialAssertionRequestWithChallengeSelector
  , initSelector
  , newSelector
  , relyingPartyIdentifierSelector


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

-- | @- initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifier :: (IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider, IsNSString relyingPartyIdentifier) => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> relyingPartyIdentifier -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
initWithRelyingPartyIdentifier asAuthorizationSecurityKeyPublicKeyCredentialProvider  relyingPartyIdentifier =
withObjCPtr relyingPartyIdentifier $ \raw_relyingPartyIdentifier ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialProvider (mkSelector "initWithRelyingPartyIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_relyingPartyIdentifier :: Ptr ())] >>= ownedObject . castPtr

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
createCredentialRegistrationRequestWithChallenge_displayName_name_userID asAuthorizationSecurityKeyPublicKeyCredentialProvider  challenge displayName name userID =
withObjCPtr challenge $ \raw_challenge ->
  withObjCPtr displayName $ \raw_displayName ->
    withObjCPtr name $ \raw_name ->
      withObjCPtr userID $ \raw_userID ->
          sendMsg asAuthorizationSecurityKeyPublicKeyCredentialProvider (mkSelector "createCredentialRegistrationRequestWithChallenge:displayName:name:userID:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_userID :: Ptr ())] >>= retainedObject . castPtr

-- | Create a request to authenticate using an existing credential.
--
-- @challenge@ — The challenge to sign.
--
-- ObjC selector: @- createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallenge :: (IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider, IsNSData challenge) => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> challenge -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest)
createCredentialAssertionRequestWithChallenge asAuthorizationSecurityKeyPublicKeyCredentialProvider  challenge =
withObjCPtr challenge $ \raw_challenge ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialProvider (mkSelector "createCredentialAssertionRequestWithChallenge:") (retPtr retVoid) [argPtr (castPtr raw_challenge :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
init_ asAuthorizationSecurityKeyPublicKeyCredentialProvider  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialProvider)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialProvider"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Relying Party identifier used for all requests created by this object.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASAuthorizationSecurityKeyPublicKeyCredentialProvider asAuthorizationSecurityKeyPublicKeyCredentialProvider => asAuthorizationSecurityKeyPublicKeyCredentialProvider -> IO (Id NSString)
relyingPartyIdentifier asAuthorizationSecurityKeyPublicKeyCredentialProvider  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialProvider (mkSelector "relyingPartyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRelyingPartyIdentifier:@
initWithRelyingPartyIdentifierSelector :: Selector
initWithRelyingPartyIdentifierSelector = mkSelector "initWithRelyingPartyIdentifier:"

-- | @Selector@ for @createCredentialRegistrationRequestWithChallenge:displayName:name:userID:@
createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector :: Selector
createCredentialRegistrationRequestWithChallenge_displayName_name_userIDSelector = mkSelector "createCredentialRegistrationRequestWithChallenge:displayName:name:userID:"

-- | @Selector@ for @createCredentialAssertionRequestWithChallenge:@
createCredentialAssertionRequestWithChallengeSelector :: Selector
createCredentialAssertionRequestWithChallengeSelector = mkSelector "createCredentialAssertionRequestWithChallenge:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

