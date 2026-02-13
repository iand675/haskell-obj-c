{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASOneTimeCodeCredentialRequest@.
module ObjC.AuthenticationServices.ASOneTimeCodeCredentialRequest
  ( ASOneTimeCodeCredentialRequest
  , IsASOneTimeCodeCredentialRequest(..)
  , init_
  , initWithCredentialIdentity
  , initSelector
  , initWithCredentialIdentitySelector


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
init_ :: IsASOneTimeCodeCredentialRequest asOneTimeCodeCredentialRequest => asOneTimeCodeCredentialRequest -> IO (Id ASOneTimeCodeCredentialRequest)
init_ asOneTimeCodeCredentialRequest =
  sendOwnedMessage asOneTimeCodeCredentialRequest initSelector

-- | Initializes an instance of ASOneTimeCodeCredentialRequest.
--
-- @credentialIdentity@ — the credential identity to use for this request.
--
-- ObjC selector: @- initWithCredentialIdentity:@
initWithCredentialIdentity :: (IsASOneTimeCodeCredentialRequest asOneTimeCodeCredentialRequest, IsASOneTimeCodeCredentialIdentity credentialIdentity) => asOneTimeCodeCredentialRequest -> credentialIdentity -> IO (Id ASOneTimeCodeCredentialRequest)
initWithCredentialIdentity asOneTimeCodeCredentialRequest credentialIdentity =
  sendOwnedMessage asOneTimeCodeCredentialRequest initWithCredentialIdentitySelector (toASOneTimeCodeCredentialIdentity credentialIdentity)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASOneTimeCodeCredentialRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCredentialIdentity:@
initWithCredentialIdentitySelector :: Selector '[Id ASOneTimeCodeCredentialIdentity] (Id ASOneTimeCodeCredentialRequest)
initWithCredentialIdentitySelector = mkSelector "initWithCredentialIdentity:"

