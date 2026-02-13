{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class encapsulates a password request made to a credential provider extension.
--
-- Generated bindings for @ASPasswordCredentialRequest@.
module ObjC.AuthenticationServices.ASPasswordCredentialRequest
  ( ASPasswordCredentialRequest
  , IsASPasswordCredentialRequest(..)
  , init_
  , initWithCredentialIdentity
  , requestWithCredentialIdentity
  , initSelector
  , initWithCredentialIdentitySelector
  , requestWithCredentialIdentitySelector


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
init_ :: IsASPasswordCredentialRequest asPasswordCredentialRequest => asPasswordCredentialRequest -> IO (Id ASPasswordCredentialRequest)
init_ asPasswordCredentialRequest =
  sendOwnedMessage asPasswordCredentialRequest initSelector

-- | Initializes an instance of ASPasswordCredentialRequest.
--
-- @credentialIdentity@ — the credential identity to use for this request.
--
-- ObjC selector: @- initWithCredentialIdentity:@
initWithCredentialIdentity :: (IsASPasswordCredentialRequest asPasswordCredentialRequest, IsASPasswordCredentialIdentity credentialIdentity) => asPasswordCredentialRequest -> credentialIdentity -> IO (Id ASPasswordCredentialRequest)
initWithCredentialIdentity asPasswordCredentialRequest credentialIdentity =
  sendOwnedMessage asPasswordCredentialRequest initWithCredentialIdentitySelector (toASPasswordCredentialIdentity credentialIdentity)

-- | Creates and initializes an instance of ASPasswordCredentialRequest.
--
-- @credentialIdentity@ — the credential identity to use for this request.
--
-- ObjC selector: @+ requestWithCredentialIdentity:@
requestWithCredentialIdentity :: IsASPasswordCredentialIdentity credentialIdentity => credentialIdentity -> IO (Id ASPasswordCredentialRequest)
requestWithCredentialIdentity credentialIdentity =
  do
    cls' <- getRequiredClass "ASPasswordCredentialRequest"
    sendClassMessage cls' requestWithCredentialIdentitySelector (toASPasswordCredentialIdentity credentialIdentity)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASPasswordCredentialRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCredentialIdentity:@
initWithCredentialIdentitySelector :: Selector '[Id ASPasswordCredentialIdentity] (Id ASPasswordCredentialRequest)
initWithCredentialIdentitySelector = mkSelector "initWithCredentialIdentity:"

-- | @Selector@ for @requestWithCredentialIdentity:@
requestWithCredentialIdentitySelector :: Selector '[Id ASPasswordCredentialIdentity] (Id ASPasswordCredentialRequest)
requestWithCredentialIdentitySelector = mkSelector "requestWithCredentialIdentity:"

