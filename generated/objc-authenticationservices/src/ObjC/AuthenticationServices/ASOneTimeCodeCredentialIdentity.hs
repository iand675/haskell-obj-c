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
init_ :: IsASOneTimeCodeCredentialIdentity asOneTimeCodeCredentialIdentity => asOneTimeCodeCredentialIdentity -> IO (Id ASOneTimeCodeCredentialIdentity)
init_ asOneTimeCodeCredentialIdentity  =
  sendMsg asOneTimeCodeCredentialIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithServiceIdentifier_label_recordIdentifier asOneTimeCodeCredentialIdentity  serviceIdentifier label recordIdentifier =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr label $ \raw_label ->
    withObjCPtr recordIdentifier $ \raw_recordIdentifier ->
        sendMsg asOneTimeCodeCredentialIdentity (mkSelector "initWithServiceIdentifier:label:recordIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_recordIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | A label to identify the one time code, typically supplied by the user. This string will be shown in the AutoFill suggestion for this one time code credential.
--
-- ObjC selector: @- label@
label :: IsASOneTimeCodeCredentialIdentity asOneTimeCodeCredentialIdentity => asOneTimeCodeCredentialIdentity -> IO (Id NSString)
label asOneTimeCodeCredentialIdentity  =
  sendMsg asOneTimeCodeCredentialIdentity (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithServiceIdentifier:label:recordIdentifier:@
initWithServiceIdentifier_label_recordIdentifierSelector :: Selector
initWithServiceIdentifier_label_recordIdentifierSelector = mkSelector "initWithServiceIdentifier:label:recordIdentifier:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

