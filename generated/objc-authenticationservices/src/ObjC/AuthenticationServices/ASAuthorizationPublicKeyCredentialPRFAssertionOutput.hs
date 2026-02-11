{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFAssertionOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFAssertionOutput
  ( ASAuthorizationPublicKeyCredentialPRFAssertionOutput
  , IsASAuthorizationPublicKeyCredentialPRFAssertionOutput(..)
  , first
  , second
  , firstSelector
  , secondSelector


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

-- | @- first@
first :: IsASAuthorizationPublicKeyCredentialPRFAssertionOutput asAuthorizationPublicKeyCredentialPRFAssertionOutput => asAuthorizationPublicKeyCredentialPRFAssertionOutput -> IO (Id NSData)
first asAuthorizationPublicKeyCredentialPRFAssertionOutput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFAssertionOutput (mkSelector "first") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- second@
second :: IsASAuthorizationPublicKeyCredentialPRFAssertionOutput asAuthorizationPublicKeyCredentialPRFAssertionOutput => asAuthorizationPublicKeyCredentialPRFAssertionOutput -> IO (Id NSData)
second asAuthorizationPublicKeyCredentialPRFAssertionOutput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFAssertionOutput (mkSelector "second") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @first@
firstSelector :: Selector
firstSelector = mkSelector "first"

-- | @Selector@ for @second@
secondSelector :: Selector
secondSelector = mkSelector "second"

