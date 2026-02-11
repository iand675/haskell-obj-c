{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ASPasswordCredentialIdentity
--
-- An ASPasswordCredentialIdentity is used to describe an identity that can use a service upon successful password based authentication. Use this class to save entries into ASCredentialIdentityStore.
--
-- Generated bindings for @ASPasswordCredentialIdentity@.
module ObjC.AuthenticationServices.ASPasswordCredentialIdentity
  ( ASPasswordCredentialIdentity
  , IsASPasswordCredentialIdentity(..)
  , init_
  , initWithServiceIdentifier_user_recordIdentifier
  , identityWithServiceIdentifier_user_recordIdentifier
  , serviceIdentifier
  , user
  , recordIdentifier
  , rank
  , setRank
  , initSelector
  , initWithServiceIdentifier_user_recordIdentifierSelector
  , identityWithServiceIdentifier_user_recordIdentifierSelector
  , serviceIdentifierSelector
  , userSelector
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
init_ :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id ASPasswordCredentialIdentity)
init_ asPasswordCredentialIdentity  =
  sendMsg asPasswordCredentialIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an instance of ASPasswordCredentialIdentity.
--
-- @serviceIdentifier@ — the service identifier for which this credential identity is valid.
--
-- @user@ — the user that can authenticate into the service indicated by the serviceIdentifier.
--
-- @recordIdentifier@ — an optional string to uniquely identify this record in your local database.
--
-- ObjC selector: @- initWithServiceIdentifier:user:recordIdentifier:@
initWithServiceIdentifier_user_recordIdentifier :: (IsASPasswordCredentialIdentity asPasswordCredentialIdentity, IsASCredentialServiceIdentifier serviceIdentifier, IsNSString user, IsNSString recordIdentifier) => asPasswordCredentialIdentity -> serviceIdentifier -> user -> recordIdentifier -> IO (Id ASPasswordCredentialIdentity)
initWithServiceIdentifier_user_recordIdentifier asPasswordCredentialIdentity  serviceIdentifier user recordIdentifier =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr user $ \raw_user ->
    withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
        sendMsg asPasswordCredentialIdentity (mkSelector "initWithServiceIdentifier:user:recordIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_user :: Ptr ()), argPtr (castPtr raw_recordIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | Creates and initializes an instance of ASPasswordCredentialIdentity.
--
-- @serviceIdentifier@ — the service identifier for which this credential identity is valid.
--
-- @user@ — the user that can authenticate into the service indicated by the serviceIdentifier.
--
-- @recordIdentifier@ — an optional string to uniquely identify this record in your local database.
--
-- ObjC selector: @+ identityWithServiceIdentifier:user:recordIdentifier:@
identityWithServiceIdentifier_user_recordIdentifier :: (IsASCredentialServiceIdentifier serviceIdentifier, IsNSString user, IsNSString recordIdentifier) => serviceIdentifier -> user -> recordIdentifier -> IO (Id ASPasswordCredentialIdentity)
identityWithServiceIdentifier_user_recordIdentifier serviceIdentifier user recordIdentifier =
  do
    cls' <- getRequiredClass "ASPasswordCredentialIdentity"
    withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
      withObjCPtr user $ \raw_user ->
        withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
          sendClassMsg cls' (mkSelector "identityWithServiceIdentifier:user:recordIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_user :: Ptr ()), argPtr (castPtr raw_recordIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | Get the service identifier.
--
-- Returns: The service identifier for this credential identity.
--
-- ObjC selector: @- serviceIdentifier@
serviceIdentifier :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asPasswordCredentialIdentity  =
  sendMsg asPasswordCredentialIdentity (mkSelector "serviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the user.
--
-- Returns: The user string.
--
-- ObjC selector: @- user@
user :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id NSString)
user asPasswordCredentialIdentity  =
  sendMsg asPasswordCredentialIdentity (mkSelector "user") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get the record identifier.
--
-- Returns: The record identifier.
--
-- You can utilize the record identifier to uniquely identify the credential identity in your local database.
--
-- ObjC selector: @- recordIdentifier@
recordIdentifier :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id NSString)
recordIdentifier asPasswordCredentialIdentity  =
  sendMsg asPasswordCredentialIdentity (mkSelector "recordIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- rank@
rank :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO CLong
rank asPasswordCredentialIdentity  =
  sendMsg asPasswordCredentialIdentity (mkSelector "rank") retCLong []

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- setRank:@
setRank :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> CLong -> IO ()
setRank asPasswordCredentialIdentity  value =
  sendMsg asPasswordCredentialIdentity (mkSelector "setRank:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithServiceIdentifier:user:recordIdentifier:@
initWithServiceIdentifier_user_recordIdentifierSelector :: Selector
initWithServiceIdentifier_user_recordIdentifierSelector = mkSelector "initWithServiceIdentifier:user:recordIdentifier:"

-- | @Selector@ for @identityWithServiceIdentifier:user:recordIdentifier:@
identityWithServiceIdentifier_user_recordIdentifierSelector :: Selector
identityWithServiceIdentifier_user_recordIdentifierSelector = mkSelector "identityWithServiceIdentifier:user:recordIdentifier:"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @user@
userSelector :: Selector
userSelector = mkSelector "user"

-- | @Selector@ for @recordIdentifier@
recordIdentifierSelector :: Selector
recordIdentifierSelector = mkSelector "recordIdentifier"

-- | @Selector@ for @rank@
rankSelector :: Selector
rankSelector = mkSelector "rank"

-- | @Selector@ for @setRank:@
setRankSelector :: Selector
setRankSelector = mkSelector "setRank:"

