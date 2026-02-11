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
  , newSelector
  , initSelector
  , allowedCredentialsSelector
  , setAllowedCredentialsSelector
  , largeBlobSelector
  , setLargeBlobSelector
  , prfSelector
  , setPrfSelector


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

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialAssertionRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id ASAuthorizationPlatformPublicKeyCredentialAssertionRequest)
init_ asAuthorizationPlatformPublicKeyCredentialAssertionRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A list of credentials to allow for this request. If this ilist is nonempty, only credentials matching the provided descriptors can be used to sign in.
--
-- ObjC selector: @- allowedCredentials@
allowedCredentials :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id NSArray)
allowedCredentials asAuthorizationPlatformPublicKeyCredentialAssertionRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "allowedCredentials") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of credentials to allow for this request. If this ilist is nonempty, only credentials matching the provided descriptors can be used to sign in.
--
-- ObjC selector: @- setAllowedCredentials:@
setAllowedCredentials :: (IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest, IsNSArray value) => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> value -> IO ()
setAllowedCredentials asAuthorizationPlatformPublicKeyCredentialAssertionRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "setAllowedCredentials:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- largeBlob@
largeBlob :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput)
largeBlob asAuthorizationPlatformPublicKeyCredentialAssertionRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "largeBlob") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLargeBlob:@
setLargeBlob :: (IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest, IsASAuthorizationPublicKeyCredentialLargeBlobAssertionInput value) => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> value -> IO ()
setLargeBlob asAuthorizationPlatformPublicKeyCredentialAssertionRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "setLargeBlob:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- prf@
prf :: IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> IO (Id ASAuthorizationPublicKeyCredentialPRFAssertionInput)
prf asAuthorizationPlatformPublicKeyCredentialAssertionRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "prf") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrf:@
setPrf :: (IsASAuthorizationPlatformPublicKeyCredentialAssertionRequest asAuthorizationPlatformPublicKeyCredentialAssertionRequest, IsASAuthorizationPublicKeyCredentialPRFAssertionInput value) => asAuthorizationPlatformPublicKeyCredentialAssertionRequest -> value -> IO ()
setPrf asAuthorizationPlatformPublicKeyCredentialAssertionRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAuthorizationPlatformPublicKeyCredentialAssertionRequest (mkSelector "setPrf:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @allowedCredentials@
allowedCredentialsSelector :: Selector
allowedCredentialsSelector = mkSelector "allowedCredentials"

-- | @Selector@ for @setAllowedCredentials:@
setAllowedCredentialsSelector :: Selector
setAllowedCredentialsSelector = mkSelector "setAllowedCredentials:"

-- | @Selector@ for @largeBlob@
largeBlobSelector :: Selector
largeBlobSelector = mkSelector "largeBlob"

-- | @Selector@ for @setLargeBlob:@
setLargeBlobSelector :: Selector
setLargeBlobSelector = mkSelector "setLargeBlob:"

-- | @Selector@ for @prf@
prfSelector :: Selector
prfSelector = mkSelector "prf"

-- | @Selector@ for @setPrf:@
setPrfSelector :: Selector
setPrfSelector = mkSelector "setPrf:"

