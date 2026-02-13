{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest@.
module ObjC.AuthenticationServices.ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest
  ( ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest
  , IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest(..)
  , new
  , init_
  , largeBlob
  , setLargeBlob
  , prf
  , setPrf
  , requestStyle
  , setRequestStyle
  , initSelector
  , largeBlobSelector
  , newSelector
  , prfSelector
  , requestStyleSelector
  , setLargeBlobSelector
  , setPrfSelector
  , setRequestStyleSelector

  -- * Enum types
  , ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle(ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle)
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleStandard
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleConditional

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

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
init_ asAuthorizationPlatformPublicKeyCredentialRegistrationRequest =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest initSelector

-- | @- largeBlob@
largeBlob :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
largeBlob asAuthorizationPlatformPublicKeyCredentialRegistrationRequest =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest largeBlobSelector

-- | @- setLargeBlob:@
setLargeBlob :: (IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest, IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput value) => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> value -> IO ()
setLargeBlob asAuthorizationPlatformPublicKeyCredentialRegistrationRequest value =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest setLargeBlobSelector (toASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput value)

-- | @- prf@
prf :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
prf asAuthorizationPlatformPublicKeyCredentialRegistrationRequest =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest prfSelector

-- | @- setPrf:@
setPrf :: (IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest, IsASAuthorizationPublicKeyCredentialPRFRegistrationInput value) => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> value -> IO ()
setPrf asAuthorizationPlatformPublicKeyCredentialRegistrationRequest value =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest setPrfSelector (toASAuthorizationPublicKeyCredentialPRFRegistrationInput value)

-- | @- requestStyle@
requestStyle :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle
requestStyle asAuthorizationPlatformPublicKeyCredentialRegistrationRequest =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest requestStyleSelector

-- | @- setRequestStyle:@
setRequestStyle :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle -> IO ()
setRequestStyle asAuthorizationPlatformPublicKeyCredentialRegistrationRequest value =
  sendMessage asAuthorizationPlatformPublicKeyCredentialRegistrationRequest setRequestStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
largeBlobSelector = mkSelector "largeBlob"

-- | @Selector@ for @setLargeBlob:@
setLargeBlobSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput] ()
setLargeBlobSelector = mkSelector "setLargeBlob:"

-- | @Selector@ for @prf@
prfSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
prfSelector = mkSelector "prf"

-- | @Selector@ for @setPrf:@
setPrfSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput] ()
setPrfSelector = mkSelector "setPrf:"

-- | @Selector@ for @requestStyle@
requestStyleSelector :: Selector '[] ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle
requestStyleSelector = mkSelector "requestStyle"

-- | @Selector@ for @setRequestStyle:@
setRequestStyleSelector :: Selector '[ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle] ()
setRequestStyleSelector = mkSelector "setRequestStyle:"

