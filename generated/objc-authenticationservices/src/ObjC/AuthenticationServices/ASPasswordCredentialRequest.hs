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
init_ :: IsASPasswordCredentialRequest asPasswordCredentialRequest => asPasswordCredentialRequest -> IO (Id ASPasswordCredentialRequest)
init_ asPasswordCredentialRequest  =
  sendMsg asPasswordCredentialRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an instance of ASPasswordCredentialRequest.
--
-- @credentialIdentity@ — the credential identity to use for this request.
--
-- ObjC selector: @- initWithCredentialIdentity:@
initWithCredentialIdentity :: (IsASPasswordCredentialRequest asPasswordCredentialRequest, IsASPasswordCredentialIdentity credentialIdentity) => asPasswordCredentialRequest -> credentialIdentity -> IO (Id ASPasswordCredentialRequest)
initWithCredentialIdentity asPasswordCredentialRequest  credentialIdentity =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
    sendMsg asPasswordCredentialRequest (mkSelector "initWithCredentialIdentity:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ())] >>= ownedObject . castPtr

-- | Creates and initializes an instance of ASPasswordCredentialRequest.
--
-- @credentialIdentity@ — the credential identity to use for this request.
--
-- ObjC selector: @+ requestWithCredentialIdentity:@
requestWithCredentialIdentity :: IsASPasswordCredentialIdentity credentialIdentity => credentialIdentity -> IO (Id ASPasswordCredentialRequest)
requestWithCredentialIdentity credentialIdentity =
  do
    cls' <- getRequiredClass "ASPasswordCredentialRequest"
    withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
      sendClassMsg cls' (mkSelector "requestWithCredentialIdentity:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCredentialIdentity:@
initWithCredentialIdentitySelector :: Selector
initWithCredentialIdentitySelector = mkSelector "initWithCredentialIdentity:"

-- | @Selector@ for @requestWithCredentialIdentity:@
requestWithCredentialIdentitySelector :: Selector
requestWithCredentialIdentitySelector = mkSelector "requestWithCredentialIdentity:"

