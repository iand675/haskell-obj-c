{-# LANGUAGE DataKinds #-}
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
  , identityWithServiceIdentifier_user_recordIdentifierSelector
  , initSelector
  , initWithServiceIdentifier_user_recordIdentifierSelector
  , rankSelector
  , recordIdentifierSelector
  , serviceIdentifierSelector
  , setRankSelector
  , userSelector


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
init_ :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id ASPasswordCredentialIdentity)
init_ asPasswordCredentialIdentity =
  sendOwnedMessage asPasswordCredentialIdentity initSelector

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
initWithServiceIdentifier_user_recordIdentifier asPasswordCredentialIdentity serviceIdentifier user recordIdentifier =
  sendOwnedMessage asPasswordCredentialIdentity initWithServiceIdentifier_user_recordIdentifierSelector (toASCredentialServiceIdentifier serviceIdentifier) (toNSString user) (toNSString recordIdentifier)

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
    sendClassMessage cls' identityWithServiceIdentifier_user_recordIdentifierSelector (toASCredentialServiceIdentifier serviceIdentifier) (toNSString user) (toNSString recordIdentifier)

-- | Get the service identifier.
--
-- Returns: The service identifier for this credential identity.
--
-- ObjC selector: @- serviceIdentifier@
serviceIdentifier :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asPasswordCredentialIdentity =
  sendMessage asPasswordCredentialIdentity serviceIdentifierSelector

-- | Get the user.
--
-- Returns: The user string.
--
-- ObjC selector: @- user@
user :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id NSString)
user asPasswordCredentialIdentity =
  sendMessage asPasswordCredentialIdentity userSelector

-- | Get the record identifier.
--
-- Returns: The record identifier.
--
-- You can utilize the record identifier to uniquely identify the credential identity in your local database.
--
-- ObjC selector: @- recordIdentifier@
recordIdentifier :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO (Id NSString)
recordIdentifier asPasswordCredentialIdentity =
  sendMessage asPasswordCredentialIdentity recordIdentifierSelector

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- rank@
rank :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> IO CLong
rank asPasswordCredentialIdentity =
  sendMessage asPasswordCredentialIdentity rankSelector

-- | Get or set the rank of the credential identity object.
--
-- The system may utilize the rank to decide which credential identity precedes the other if two identities have the same service identifier. A credential identity with a larger rank value precedes one with a smaller value if both credential identities have the same service identifier. The default value of this property is 0.
--
-- ObjC selector: @- setRank:@
setRank :: IsASPasswordCredentialIdentity asPasswordCredentialIdentity => asPasswordCredentialIdentity -> CLong -> IO ()
setRank asPasswordCredentialIdentity value =
  sendMessage asPasswordCredentialIdentity setRankSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasswordCredentialIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithServiceIdentifier:user:recordIdentifier:@
initWithServiceIdentifier_user_recordIdentifierSelector :: Selector '[Id ASCredentialServiceIdentifier, Id NSString, Id NSString] (Id ASPasswordCredentialIdentity)
initWithServiceIdentifier_user_recordIdentifierSelector = mkSelector "initWithServiceIdentifier:user:recordIdentifier:"

-- | @Selector@ for @identityWithServiceIdentifier:user:recordIdentifier:@
identityWithServiceIdentifier_user_recordIdentifierSelector :: Selector '[Id ASCredentialServiceIdentifier, Id NSString, Id NSString] (Id ASPasswordCredentialIdentity)
identityWithServiceIdentifier_user_recordIdentifierSelector = mkSelector "identityWithServiceIdentifier:user:recordIdentifier:"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector '[] (Id ASCredentialServiceIdentifier)
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @user@
userSelector :: Selector '[] (Id NSString)
userSelector = mkSelector "user"

-- | @Selector@ for @recordIdentifier@
recordIdentifierSelector :: Selector '[] (Id NSString)
recordIdentifierSelector = mkSelector "recordIdentifier"

-- | @Selector@ for @rank@
rankSelector :: Selector '[] CLong
rankSelector = mkSelector "rank"

-- | @Selector@ for @setRank:@
setRankSelector :: Selector '[CLong] ()
setRankSelector = mkSelector "setRank:"

