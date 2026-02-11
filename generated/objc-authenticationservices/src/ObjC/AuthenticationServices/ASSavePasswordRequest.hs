{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , initWithServiceIdentifier_credential_sessionID_eventSelector
  , initWithServiceIdentifier_credential_title_sessionID_eventSelector
  , initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector
  , initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector
  , serviceIdentifierSelector
  , credentialSelector
  , titleSelector
  , sessionIDSelector
  , eventSelector
  , passwordKindSelector

  -- * Enum types
  , ASSavePasswordRequestEvent(ASSavePasswordRequestEvent)
  , pattern ASSavePasswordRequestEventUserInitiated
  , pattern ASSavePasswordRequestEventFormDidDisappear
  , pattern ASSavePasswordRequestEventGeneratedPasswordFilled

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

-- | @- init@
init_ :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id ASSavePasswordRequest)
init_ asSavePasswordRequest  =
  sendMsg asSavePasswordRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASSavePasswordRequest)
new  =
  do
    cls' <- getRequiredClass "ASSavePasswordRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithServiceIdentifier:credential:sessionID:event:@
initWithServiceIdentifier_credential_sessionID_event :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString sessionID) => asSavePasswordRequest -> serviceIdentifier -> credential -> sessionID -> ASSavePasswordRequestEvent -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_sessionID_event asSavePasswordRequest  serviceIdentifier credential sessionID event =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr credential $ \raw_credential ->
    withObjCPtr sessionID $ \raw_sessionID ->
        sendMsg asSavePasswordRequest (mkSelector "initWithServiceIdentifier:credential:sessionID:event:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_sessionID :: Ptr ()), argCLong (coerce event)] >>= ownedObject . castPtr

-- | @- initWithServiceIdentifier:credential:title:sessionID:event:@
initWithServiceIdentifier_credential_title_sessionID_event :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString title, IsNSString sessionID) => asSavePasswordRequest -> serviceIdentifier -> credential -> title -> sessionID -> ASSavePasswordRequestEvent -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_title_sessionID_event asSavePasswordRequest  serviceIdentifier credential title sessionID event =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr credential $ \raw_credential ->
    withObjCPtr title $ \raw_title ->
      withObjCPtr sessionID $ \raw_sessionID ->
          sendMsg asSavePasswordRequest (mkSelector "initWithServiceIdentifier:credential:title:sessionID:event:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_sessionID :: Ptr ()), argCLong (coerce event)] >>= ownedObject . castPtr

-- | @- initWithServiceIdentifier:credential:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_sessionID_event_passwordKind :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString sessionID, IsNSString passwordKind) => asSavePasswordRequest -> serviceIdentifier -> credential -> sessionID -> ASSavePasswordRequestEvent -> passwordKind -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_sessionID_event_passwordKind asSavePasswordRequest  serviceIdentifier credential sessionID event passwordKind =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr credential $ \raw_credential ->
    withObjCPtr sessionID $ \raw_sessionID ->
      withObjCPtr passwordKind $ \raw_passwordKind ->
          sendMsg asSavePasswordRequest (mkSelector "initWithServiceIdentifier:credential:sessionID:event:passwordKind:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_sessionID :: Ptr ()), argCLong (coerce event), argPtr (castPtr raw_passwordKind :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_title_sessionID_event_passwordKind :: (IsASSavePasswordRequest asSavePasswordRequest, IsASCredentialServiceIdentifier serviceIdentifier, IsASPasswordCredential credential, IsNSString title, IsNSString sessionID, IsNSString passwordKind) => asSavePasswordRequest -> serviceIdentifier -> credential -> title -> sessionID -> ASSavePasswordRequestEvent -> passwordKind -> IO (Id ASSavePasswordRequest)
initWithServiceIdentifier_credential_title_sessionID_event_passwordKind asSavePasswordRequest  serviceIdentifier credential title sessionID event passwordKind =
withObjCPtr serviceIdentifier $ \raw_serviceIdentifier ->
  withObjCPtr credential $ \raw_credential ->
    withObjCPtr title $ \raw_title ->
      withObjCPtr sessionID $ \raw_sessionID ->
        withObjCPtr passwordKind $ \raw_passwordKind ->
            sendMsg asSavePasswordRequest (mkSelector "initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:") (retPtr retVoid) [argPtr (castPtr raw_serviceIdentifier :: Ptr ()), argPtr (castPtr raw_credential :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_sessionID :: Ptr ()), argCLong (coerce event), argPtr (castPtr raw_passwordKind :: Ptr ())] >>= ownedObject . castPtr

-- | The identifier of the service for which the the credential should be associated.
--
-- ObjC selector: @- serviceIdentifier@
serviceIdentifier :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id ASCredentialServiceIdentifier)
serviceIdentifier asSavePasswordRequest  =
  sendMsg asSavePasswordRequest (mkSelector "serviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The credential to save.
--
-- ObjC selector: @- credential@
credential :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id ASPasswordCredential)
credential asSavePasswordRequest  =
  sendMsg asSavePasswordRequest (mkSelector "credential") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A user-displayable name for the password credential to be saved.
--
-- This is independent of the service identifier and should be used for identifying the individual credential.
--
-- ObjC selector: @- title@
title :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id NSString)
title asSavePasswordRequest  =
  sendMsg asSavePasswordRequest (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An ID that represents a form's session.
--
-- ObjC selector: @- sessionID@
sessionID :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id NSString)
sessionID asSavePasswordRequest  =
  sendMsg asSavePasswordRequest (mkSelector "sessionID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of event that the save request represents.
--
-- ObjC selector: @- event@
event :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO ASSavePasswordRequestEvent
event asSavePasswordRequest  =
  fmap (coerce :: CLong -> ASSavePasswordRequestEvent) $ sendMsg asSavePasswordRequest (mkSelector "event") retCLong []

-- | For passwordFilled events, this is the kind of password that was created.
--
-- ObjC selector: @- passwordKind@
passwordKind :: IsASSavePasswordRequest asSavePasswordRequest => asSavePasswordRequest -> IO (Id NSString)
passwordKind asSavePasswordRequest  =
  sendMsg asSavePasswordRequest (mkSelector "passwordKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithServiceIdentifier:credential:sessionID:event:@
initWithServiceIdentifier_credential_sessionID_eventSelector :: Selector
initWithServiceIdentifier_credential_sessionID_eventSelector = mkSelector "initWithServiceIdentifier:credential:sessionID:event:"

-- | @Selector@ for @initWithServiceIdentifier:credential:title:sessionID:event:@
initWithServiceIdentifier_credential_title_sessionID_eventSelector :: Selector
initWithServiceIdentifier_credential_title_sessionID_eventSelector = mkSelector "initWithServiceIdentifier:credential:title:sessionID:event:"

-- | @Selector@ for @initWithServiceIdentifier:credential:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector :: Selector
initWithServiceIdentifier_credential_sessionID_event_passwordKindSelector = mkSelector "initWithServiceIdentifier:credential:sessionID:event:passwordKind:"

-- | @Selector@ for @initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:@
initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector :: Selector
initWithServiceIdentifier_credential_title_sessionID_event_passwordKindSelector = mkSelector "initWithServiceIdentifier:credential:title:sessionID:event:passwordKind:"

-- | @Selector@ for @serviceIdentifier@
serviceIdentifierSelector :: Selector
serviceIdentifierSelector = mkSelector "serviceIdentifier"

-- | @Selector@ for @credential@
credentialSelector :: Selector
credentialSelector = mkSelector "credential"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @sessionID@
sessionIDSelector :: Selector
sessionIDSelector = mkSelector "sessionID"

-- | @Selector@ for @event@
eventSelector :: Selector
eventSelector = mkSelector "event"

-- | @Selector@ for @passwordKind@
passwordKindSelector :: Selector
passwordKindSelector = mkSelector "passwordKind"

