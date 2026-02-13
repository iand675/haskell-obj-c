{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPlatformPublicKeyCredentialAssertionRequest@.
module ObjC.AuthenticationServices.ASAuthorizationPlatformPublicKeyCredentialAssertionRequest
  ( ASAuthorizationPlatformPublicKeyCredentialAssertionRequest
  , IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest(..)
  , new
  , init_
  , allowedCredentials
  , setAllowedCredentials
  , largeBlob
  , setLargeBlob
  , prf
  , setPrf
  , allowedCredentialsSelector
  , initSelector
  , largeBlobSelector
  , newSelector
  , prfSelector
  , setAllowedCredentialsSelector
  , setLargeBlobSelector
  , setPrfSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialAssertionRequest"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
init_ asAuthorizationPlatformPublicKeyCredentialAssertionRequest =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest initSelector

-- | A list of credentials to allow for this request. If this ilist is nonempty, only credentials matching the provided descriptors can be used to sign in.
--
-- ObjC selector: @- allowedCredentials@
allowedCredentials :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id NSArray)
allowedCredentials asAuthorizationPlatformPublicKeyCredentialAssertionRequest =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest allowedCredentialsSelector

-- | A list of credentials to allow for this request. If this ilist is nonempty, only credentials matching the provided descriptors can be used to sign in.
--
-- ObjC selector: @- setAllowedCredentials:@
setAllowedCredentials :: (IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest, IsNSArray value) => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> value -> IO ()
setAllowedCredentials asAuthorizationPlatformPublicKeyCredentialAssertionRequest value =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest setAllowedCredentialsSelector (toNSArray value)

-- | @- largeBlob@
largeBlob :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
largeBlob asAuthorizationPlatformPublicKeyCredentialAssertionRequest =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest largeBlobSelector

-- | @- setLargeBlob:@
setLargeBlob :: (IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest, IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput value) => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> value -> IO ()
setLargeBlob asAuthorizationPlatformPublicKeyCredentialAssertionRequest value =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest setLargeBlobSelector (toASAuthorizationPublicKeyCredentialLargeBlobAssertionInput value)

-- | @- prf@
prf :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput)
prf asAuthorizationPlatformPublicKeyCredentialAssertionRequest =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest prfSelector

-- | @- setPrf:@
setPrf :: (IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest, IsASAuthorizationPublicKeyCredentialPRFAssertionInput value) => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> value -> IO ()
setPrf asAuthorizationPlatformPublicKeyCredentialAssertionRequest value =
  sendMessage asAuthorizationPlatformPublicKeyCredentialAssertionRequest setPrfSelector (toASAuthorizationPublicKeyCredentialPRFAssertionInput value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @allowedCredentials@
allowedCredentialsSelector :: Selector '[] (Id NSArray)
allowedCredentialsSelector = mkSelector "allowedCredentials"

-- | @Selector@ for @setAllowedCredentials:@
setAllowedCredentialsSelector :: Selector '[Id NSArray] ()
setAllowedCredentialsSelector = mkSelector "setAllowedCredentials:"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
largeBlobSelector = mkSelector "largeBlob"

-- | @Selector@ for @setLargeBlob:@
setLargeBlobSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput] ()
setLargeBlobSelector = mkSelector "setLargeBlob:"

-- | @Selector@ for @prf@
prfSelector :: Selector '[] (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput)
prfSelector = mkSelector "prf"

-- | @Selector@ for @setPrf:@
setPrfSelector :: Selector '[Id ASAuthorizationPublicKeyCredentialPRFAssertionInput] ()
setPrfSelector = mkSelector "setPrf:"

