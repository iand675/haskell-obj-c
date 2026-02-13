{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ASPasskeyCredentialIdentity
--
-- An ASPasswordCredentialIdentity is used to describe an identity that can use a service upon successful passkey based authentication. Use this class to save entries into ASCredentialIdentityStore.
--
-- Generated bindings for @ASPasskeyCredentialIdentity@.
module ObjC.AuthenticationServices.ASPasskeyCredentialIdentity
  ( ASPasskeyCredentialIdentity
  , IsASPasskeyCredentialIdentity(..)
  , init_
  , initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier
  , identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier
  , relyingPartyIdentifier
  , userName
  , credentialID
  , userHandle
  , recordIdentifier
  , rank
  , setRank
  , credentialIDSelector
  , identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector
  , initSelector
  , initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector
  , rankSelector
  , recordIdentifierSelector
  , relyingPartyIdentifierSelector
  , setRankSelector
  , userHandleSelector
  , userNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id ASPasskeyCredentialIdentity)
init_ asPasskeyCredentialIdentity =
  sendOwnedMessage asPasskeyCredentialIdentity initSelector

-- | Initialize an instance of ASPasskeyCredentialIdentity.
--
-- @relyingPartyIdentifier@ — relying party for this credential.
--
-- @userName@ — user name associated with this credential.
--
-- @credentialID@ — credential ID of this passkey credential.
--
-- @userHandle@ — user handle data of this passkey credential.
--
-- @recordIdentifier@ — identifier used by credential provider extension to identify this credential.
--
-- ObjC selector: @- initWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:@
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier :: (IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity, IsNSString relyingPartyIdentifier, IsNSString userName, IsNSData credentialID, IsNSData userHandle, IsNSString recordIdentifier) => asPasskeyCredentialIdentity -> relyingPartyIdentifier -> userName -> credentialID -> userHandle -> recordIdentifier -> IO (Id ASPasskeyCredentialIdentity)
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier asPasskeyCredentialIdentity relyingPartyIdentifier userName credentialID userHandle recordIdentifier =
  sendOwnedMessage asPasskeyCredentialIdentity initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector (toNSString relyingPartyIdentifier) (toNSString userName) (toNSData credentialID) (toNSData userHandle) (toNSString recordIdentifier)

-- | Create and initialize an instance of ASPasskeyCredentialIdentity.
--
-- @relyingPartyIdentifier@ — relying party for this credential.
--
-- @userName@ — user name associated with this credential.
--
-- @credentialID@ — credential ID of this passkey credential.
--
-- @userHandle@ — user handle data of this passkey credential.
--
-- @recordIdentifier@ — identifier used by credential provider extension to identify this credential.
--
-- ObjC selector: @+ identityWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:@
identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier :: (IsNSString relyingPartyIdentifier, IsNSString userName, IsNSData credentialID, IsNSData userHandle, IsNSString recordIdentifier) => relyingPartyIdentifier -> userName -> credentialID -> userHandle -> recordIdentifier -> IO (Id ASPasskeyCredentialIdentity)
identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier relyingPartyIdentifier userName credentialID userHandle recordIdentifier =
  do
    cls' <- getRequiredClass "ASPasskeyCredentialIdentity"
    sendClassMessage cls' identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector (toNSString relyingPartyIdentifier) (toNSString userName) (toNSData credentialID) (toNSData userHandle) (toNSString recordIdentifier)

-- | The relying party identifier of this passkey credential.
--
-- This field is reported as the serviceIdentifier property of ASCredentialIdentity.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSString)
relyingPartyIdentifier asPasskeyCredentialIdentity =
  sendMessage asPasskeyCredentialIdentity relyingPartyIdentifierSelector

-- | The user name of this passkey credential.
--
-- This field is reported as the user property of ASCredentialIdentity.
--
-- ObjC selector: @- userName@
userName :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSString)
userName asPasskeyCredentialIdentity =
  sendMessage asPasskeyCredentialIdentity userNameSelector

-- | The credential ID of this passkey credential.
--
-- This field is used to identify the correct credential to use based on relying party request parameters.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSData)
credentialID asPasskeyCredentialIdentity =
  sendMessage asPasskeyCredentialIdentity credentialIDSelector

-- | The user handle of this passkey credential.
--
-- This field is used to identify the correct credential to use based on relying party request parameters.
--
-- ObjC selector: @- userHandle@
userHandle :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSData)
userHandle asPasskeyCredentialIdentity =
  sendMessage asPasskeyCredentialIdentity userHandleSelector

-- | Get the record identifier.
--
-- Returns: The record identifier.
--
-- You can utilize the record identifier to uniquely identify the credential identity in your local database.
--
-- ObjC selector: @- recordIdentifier@
recordIdentifier :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSString)
recordIdentifier asPasskeyCredentialIdentity =
  sendMessage asPasskeyCredentialIdentity recordIdentifierSelector

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- rank@
rank :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO CLong
rank asPasskeyCredentialIdentity =
  sendMessage asPasskeyCredentialIdentity rankSelector

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- setRank:@
setRank :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> CLong -> IO ()
setRank asPasskeyCredentialIdentity value =
  sendMessage asPasskeyCredentialIdentity setRankSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasskeyCredentialIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:@
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector :: Selector '[Id NSString, Id NSString, Id NSData, Id NSData, Id NSString] (Id ASPasskeyCredentialIdentity)
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector = mkSelector "initWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:"

-- | @Selector@ for @identityWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:@
identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector :: Selector '[Id NSString, Id NSString, Id NSData, Id NSData, Id NSString] (Id ASPasskeyCredentialIdentity)
identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector = mkSelector "identityWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector '[] (Id NSString)
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

-- | @Selector@ for @userName@
userNameSelector :: Selector '[] (Id NSString)
userNameSelector = mkSelector "userName"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector '[] (Id NSData)
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @userHandle@
userHandleSelector :: Selector '[] (Id NSData)
userHandleSelector = mkSelector "userHandle"

-- | @Selector@ for @recordIdentifier@
recordIdentifierSelector :: Selector '[] (Id NSString)
recordIdentifierSelector = mkSelector "recordIdentifier"

-- | @Selector@ for @rank@
rankSelector :: Selector '[] CLong
rankSelector = mkSelector "rank"

-- | @Selector@ for @setRank:@
setRankSelector :: Selector '[CLong] ()
setRankSelector = mkSelector "setRank:"

