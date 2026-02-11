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
init_ :: IsASOneTimeCodeCredentialRequest asOneTimeCodeCredentialRequest => asOneTimeCodeCredentialRequest -> IO (Id ASOneTimeCodeCredentialRequest)
init_ asOneTimeCodeCredentialRequest  =
  sendMsg asOneTimeCodeCredentialRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an instance of ASOneTimeCodeCredentialRequest.
--
-- @credentialIdentity@ — the credential identity to use for this request.
--
-- ObjC selector: @- initWithCredentialIdentity:@
initWithCredentialIdentity :: (IsASOneTimeCodeCredentialRequest asOneTimeCodeCredentialRequest, IsASOneTimeCodeCredentialIdentity credentialIdentity) => asOneTimeCodeCredentialRequest -> credentialIdentity -> IO (Id ASOneTimeCodeCredentialRequest)
initWithCredentialIdentity asOneTimeCodeCredentialRequest  credentialIdentity =
withObjCPtr credentialIdentity $ \raw_credentialIdentity ->
    sendMsg asOneTimeCodeCredentialRequest (mkSelector "initWithCredentialIdentity:") (retPtr retVoid) [argPtr (castPtr raw_credentialIdentity :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCredentialIdentity:@
initWithCredentialIdentitySelector :: Selector
initWithCredentialIdentitySelector = mkSelector "initWithCredentialIdentity:"

