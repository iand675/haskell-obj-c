{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASSavePasswordRequest@.
module ObjC.AuthenticationServices.ASSavePasswordRequest
  ( ASSavePasswordRequest
  , IsASSavePasswordRequest(..)
  , init_
  , new
  , initWithServiceIdentifier_credential_sessionID_event
  , initWithServiceIdentifier_credential_title_sessionID_event
  , initWithServiceIdentifier_credential_sessionID_event_passwordKind
  , initWithServiceIdentifier_credential_title_sessionID_event_passwordKind
  , serviceIdentifier
  , credential
  , title
  , sessionID
  , event
  , passwordKind
  , credentialSelector
  , eventSelector
  , initSelector
  , initWithServiceIdentifier_credential_sessionID_eventSelector
  , initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector
  , initWithServiceIdentifier_credential_title_sessionID_eventSelector
  , initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector
  , newSelector
  , passwordKindSelector
  , serviceIdentifierSelector
  , sessionIDSelector
  , titleSelector

  -- * Enum types
  , ASSavePasswordRequestEvent(ASSavePasswordRequestEvent)
  , pattern ASSavePasswordRequestEventUserInitiated
  , pattern ASSavePasswordRequestEventFormDidDisappear
  , pattern ASSavePasswordRequestEventGeneratedPasswordFilled

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

-- | @- init@
init_ :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id ASSavePasswordRequest)
init_ asSavePasswordRequest =
  sendOwnedMessage asSavePasswordRequest initSelector

-- | @+ new@
new :: IO (Id ASSavePasswordRequest)
new  =
  do
    cls' <- getRequiredClass "ASSavePasswordRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithServiceIdentifier:credential:sessionID:event:@
initWithServiceIdentifier_credential_sessionID_event :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString sessionID) => asSavePasswordRequest -> serviceIdentifier -> credential -> sessionID -> ASSavePasswordRequestEvent -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_sessionID_event asSavePasswordRequest serviceIdentifier credential sessionID event =
  sendOwnedMessage asSavePasswordRequest initWithServiceIdentifier_credential_sessionID_eventSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential credential) (toNSString sessionID) event

-- | @- initWithServiceIdentifier:credential:title:sessionID:event:@
initWithServiceIdentifier_credential_title_sessionID_event :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString title, IsNSString sessionID) => asSavePasswordRequest -> serviceIdentifier -> credential -> title -> sessionID -> ASSavePasswordRequestEvent -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_title_sessionID_event asSavePasswordRequest serviceIdentifier credential title sessionID event =
  sendOwnedMessage asSavePasswordRequest initWithServiceIdentifier_credential_title_sessionID_eventSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential credential) (toNSString title) (toNSString sessionID) event

-- | @- initWithServiceIdentifier:credential:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_sessionID_event_passwordKind :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString sessionID, IsNSString passwordKind) => asSavePasswordRequest -> serviceIdentifier -> credential -> sessionID -> ASSavePasswordRequestEvent -> passwordKind -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_sessionID_event_passwordKind asSavePasswordRequest serviceIdentifier credential sessionID event passwordKind =
  sendOwnedMessage asSavePasswordRequest initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential credential) (toNSString sessionID) event (toNSString passwordKind)

-- | @- initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_title_sessionID_event_passwordKind :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString title, IsNSString sessionID, IsNSString passwordKind) => asSavePasswordRequest -> serviceIdentifier -> credential -> title -> sessionID -> ASSavePasswordRequestEvent -> passwordKind -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_title_sessionID_event_passwordKind asSavePasswordRequest serviceIdentifier credential title sessionID event passwordKind =
  sendOwnedMessage asSavePasswordRequest initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector (toASCredentialServiceIdentifier serviceIdentifier) (toASPasswordCredential credential) (toNSString title) (toNSString sessionID) event (toNSString passwordKind)

-- | The identifier of the service for which the the credential should be associated.
--
-- ObjC selector: @- serviceIdentifier@
serviceIdentifier :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asSavePasswordRequest =
  sendMessage asSavePasswordRequest serviceIdentifierSelector

-- | The credential to save.
--
-- ObjC selector: @- credential@
credential :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id ASPasswordCredential)
credential asSavePasswordRequest =
  sendMessage asSavePasswordRequest credentialSelector

-- | A user-displayable name for the password credential to be saved.
--
-- This is independent of the service identifier and should be used for identifying the individual credential.
--
-- ObjC selector: @- title@
title :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id NSString)
title asSavePasswordRequest =
  sendMessage asSavePasswordRequest titleSelector

-- | An ID that represents a form's session.
--
-- ObjC selector: @- sessionID@
sessionID :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id NSString)
sessionID asSavePasswordRequest =
  sendMessage asSavePasswordRequest sessionIDSelector

-- | The type of event that the save request represents.
--
-- ObjC selector: @- event@
event :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO ASSavePasswordRequestEvent
event asSavePasswordRequest =
  sendMessage asSavePasswordRequest eventSelector

-- | For passwordFilled events, this is the kind of password that was created.
--
-- ObjC selector: @- passwordKind@
passwordKind :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id NSString)
passwordKind asSavePasswordRequest =
  sendMessage asSavePasswordRequest passwordKindSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASSavePasswordRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASSavePasswordRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithServiceIdentifier:credential:sessionID:event:@
initWithServiceIdentifier_credential_sessionID_eventSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSString, ASSavePasswordRequestEvent] (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_sessionID_eventSelector = mkSelector "initWithServiceIdentifier:credential:sessionID:event:"

-- | @Selector@ for @initWithServiceIdentifier:credential:title:sessionID:event:@
initWithServiceIdentifier_credential_title_sessionID_eventSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSString, Id NSString, ASSavePasswordRequestEvent] (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_title_sessionID_eventSelector = mkSelector "initWithServiceIdentifier:credential:title:sessionID:event:"

-- | @Selector@ for @initWithServiceIdentifier:credential:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSString, ASSavePasswordRequestEvent, Id NSString] (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector = mkSelector "initWithServiceIdentifier:credential:sessionID:event:passwordKind:"

-- | @Selector@ for @initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector :: Selector '[Id ASCredentialServiceIdentifier, Id ASPasswordCredential, Id NSString, Id NSString, ASSavePasswordRequestEvent, Id NSString] (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector = mkSelector "initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector '[] (Id ASCredentialServiceIdentifier)
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @credential@
credentialSelector :: Selector '[] (Id ASPasswordCredential)
credentialSelector = mkSelector "credential"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector '[] (Id NSString)
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @event@
eventSelector :: Selector '[] ASSavePasswordRequestEvent
eventSelector = mkSelector "event"

-- | @Selector@ for @passwordKind@
passwordKindSelector :: Selector '[] (Id NSString)
passwordKindSelector = mkSelector "passwordKind"

