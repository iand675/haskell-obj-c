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
  , initSelector
  , initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector
  , identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector
  , relyingPartyIdentifierSelector
  , userNameSelector
  , credentialIDSelector
  , userHandleSelector
  , recordIdentifierSelector
  , rankSelector
  , setRankSelector


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

-- | @- init@
init_ :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id ASPasskeyCredentialIdentity)
init_ asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifier asPasskeyCredentialIdentity  relyingPartyIdentifier userName credentialID userHandle recordIdentifier =
withObjCPtr relyingPartyIdentifier $ \raw_relyingPartyIdentifier ->
  withObjCPtr userName $ \raw_userName ->
    withObjCPtr credentialID $ \raw_credentialID ->
      withObjCPtr userHandle $ \raw_userHandle ->
        withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
            sendMsg asPasskeyCredentialIdentity (mkSelector "initWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_relyingPartyIdentifier :: Ptr ()), argPtr (castPtr raw_userName :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_recordIdentifier :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr relyingPartyIdentifier $ \raw_relyingPartyIdentifier ->
      withObjCPtr userName $ \raw_userName ->
        withObjCPtr credentialID $ \raw_credentialID ->
          withObjCPtr userHandle $ \raw_userHandle ->
            withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
              sendClassMsg cls' (mkSelector "identityWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_relyingPartyIdentifier :: Ptr ()), argPtr (castPtr raw_userName :: Ptr ()), argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_userHandle :: Ptr ()), argPtr (castPtr raw_recordIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | The relying party identifier of this passkey credential.
--
-- This field is reported as the serviceIdentifier property of ASCredentialIdentity.
--
-- ObjC selector: @- relyingPartyIdentifier@
relyingPartyIdentifier :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSString)
relyingPartyIdentifier asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "relyingPartyIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user name of this passkey credential.
--
-- This field is reported as the user property of ASCredentialIdentity.
--
-- ObjC selector: @- userName@
userName :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSString)
userName asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "userName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The credential ID of this passkey credential.
--
-- This field is used to identify the correct credential to use based on relying party request parameters.
--
-- ObjC selector: @- credentialID@
credentialID :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSData)
credentialID asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "credentialID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user handle of this passkey credential.
--
-- This field is used to identify the correct credential to use based on relying party request parameters.
--
-- ObjC selector: @- userHandle@
userHandle :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSData)
userHandle asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "userHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the record identifier.
--
-- Returns: The record identifier.
--
-- You can utilize the record identifier to uniquely identify the credential identity in your local database.
--
-- ObjC selector: @- recordIdentifier@
recordIdentifier :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO (Id NSString)
recordIdentifier asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "recordIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- rank@
rank :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> IO CLong
rank asPasskeyCredentialIdentity  =
  sendMsg asPasskeyCredentialIdentity (mkSelector "rank") retCLong []

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- setRank:@
setRank :: IsASPasskeyCredentialIdentity asPasskeyCredentialIdentity => asPasskeyCredentialIdentity -> CLong -> IO ()
setRank asPasskeyCredentialIdentity  value =
  sendMsg asPasskeyCredentialIdentity (mkSelector "setRank:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:@
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector :: Selector
initWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector = mkSelector "initWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:"

-- | @Selector@ for @identityWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:@
identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector :: Selector
identityWithRelyingPartyIdentifier_userName_credentialID_userHandle_recordIdentifierSelector = mkSelector "identityWithRelyingPartyIdentifier:userName:credentialID:userHandle:recordIdentifier:"

-- | @Selector@ for @relyingPartyIdentifier@
relyingPartyIdentifierSelector :: Selector
relyingPartyIdentifierSelector = mkSelector "relyingPartyIdentifier"

-- | @Selector@ for @userName@
userNameSelector :: Selector
userNameSelector = mkSelector "userName"

-- | @Selector@ for @credentialID@
credentialIDSelector :: Selector
credentialIDSelector = mkSelector "credentialID"

-- | @Selector@ for @userHandle@
userHandleSelector :: Selector
userHandleSelector = mkSelector "userHandle"

-- | @Selector@ for @recordIdentifier@
recordIdentifierSelector :: Selector
recordIdentifierSelector = mkSelector "recordIdentifier"

-- | @Selector@ for @rank@
rankSelector :: Selector
rankSelector = mkSelector "rank"

-- | @Selector@ for @setRank:@
setRankSelector :: Selector
setRankSelector = mkSelector "setRank:"

