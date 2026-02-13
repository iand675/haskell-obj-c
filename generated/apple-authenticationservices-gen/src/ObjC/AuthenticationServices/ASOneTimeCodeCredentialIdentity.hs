{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ASOneTimeCodeCredentialIdentity
--
-- An ASOneTimeCodeCredentialIdentity is used to describe an identity that can use a service upon successful one time code based authentication. Use this class to save entries into ASCredentialIdentityStore.
--
-- Generated bindings for @ASOneTimeCodeCredentialIdentity@.
module ObjC.AuthenticationServices.ASOneTimeCodeCredentialIdentity
  ( ASOneTimeCodeCredentialIdentity
  , IsASOneTimeCodeCredentialIdentity(..)
  , init_
  , initWithServiceIdentifier_label_recordIdentifier
  , label
  , initSelector
  , initWithServiceIdentifier_label_recordIdentifierSelector
  , labelSelector


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
init_ :: IsASOneTimeCodeCredentialIdentity asOneTimeCodeCredentialIdentity => asOneTimeCodeCredentialIdentity -> IO (Id ASOneTimeCodeCredentialIdentity)
init_ asOneTimeCodeCredentialIdentity =
  sendOwnedMessage asOneTimeCodeCredentialIdentity initSelector

-- | Initializes an instance of ASOneTimeCodeCredentialIdentity.
--
-- @serviceIdentifier@ — The service identifier for which this credential identity is valid.
--
-- @label@ — A user-provided label to identify the one time code.
--
-- @recordIdentifier@ — An optional string to uniquely identify this record in your local database.
--
-- ObjC selector: @- initWithServiceIdentifier:label:recordIdentifier:@
initWithServiceIdentifier_label_recordIdentifier :: (IsASOneTimeCodeCredentialIdentity asOneTimeCodeCredentialIdentity, IsASCredentialServiceIdentifier serviceIdentifier, IsNSString label, IsNSString recordIdentifier) => asOneTimeCodeCredentialIdentity -> serviceIdentifier -> label -> recordIdentifier -> IO (Id ASOneTimeCodeCredentialIdentity)
initWithServiceIdentifier_label_recordIdentifier asOneTimeCodeCredentialIdentity serviceIdentifier label recordIdentifier =
  sendOwnedMessage asOneTimeCodeCredentialIdentity initWithServiceIdentifier_label_recordIdentifierSelector (toASCredentialServiceIdentifier serviceIdentifier) (toNSString label) (toNSString recordIdentifier)

-- | A label to identify the one time code, typically supplied by the user. This string will be shown in the AutoFill suggestion for this one time code credential.
--
-- ObjC selector: @- label@
label :: IsASOneTimeCodeCredentialIdentity asOneTimeCodeCredentialIdentity => asOneTimeCodeCredentialIdentity -> IO (Id NSString)
label asOneTimeCodeCredentialIdentity =
  sendMessage asOneTimeCodeCredentialIdentity labelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASOneTimeCodeCredentialIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithServiceIdentifier:label:recordIdentifier:@
initWithServiceIdentifier_label_recordIdentifierSelector :: Selector '[Id ASCredentialServiceIdentifier, Id NSString, Id NSString] (Id ASOneTimeCodeCredentialIdentity)
initWithServiceIdentifier_label_recordIdentifierSelector = mkSelector "initWithServiceIdentifier:label:recordIdentifier:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

