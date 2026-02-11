{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , largeBlobSelector
  , setLargeBlobSelector
  , prfSelector
  , setPrfSelector
  , requestStyleSelector
  , setRequestStyleSelector

  -- * Enum types
  , ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle(ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle)
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleStandard
  , pattern ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyleConditional

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

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest)
init_ asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- largeBlob@
largeBlob :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput)
largeBlob asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "largeBlob") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLargeBlob:@
setLargeBlob :: (IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest, IsASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput value) => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> value -> IO ()
setLargeBlob asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "setLargeBlob:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- prf@
prf :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO (Id ASAuthorizationPublicKeyCredentialPRFRegistrationInput)
prf asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  =
    sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "prf") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrf:@
setPrf :: (IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest, IsASAuthorizationPublicKeyCredentialPRFRegistrationInput value) => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> value -> IO ()
setPrf asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "setPrf:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- requestStyle@
requestStyle :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> IO ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle
requestStyle asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  =
    fmap (coerce :: CLong -> ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle) $ sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "requestStyle") retCLong []

-- | @- setRequestStyle:@
setRequestStyle :: IsASAuthorizationPlatformPublicKeyCredentialRegistrationRequest asAuthorizationPlatformPublicKeyCredentialRegistrationRequest => asAuthorizationPlatformPublicKeyCredentialRegistrationRequest -> ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestStyle -> IO ()
setRequestStyle asAuthorizationPlatformPublicKeyCredentialRegistrationRequest  value =
    sendMsg asAuthorizationPlatformPublicKeyCredentialRegistrationRequest (mkSelector "setRequestStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

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

-- | @Selector@ for @requestStyle@
requestStyleSelector :: Selector
requestStyleSelector = mkSelector "requestStyle"

-- | @Selector@ for @setRequestStyle:@
setRequestStyleSelector :: Selector
setRequestStyleSelector = mkSelector "setRequestStyle:"

