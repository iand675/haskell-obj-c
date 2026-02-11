{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPublicKeyCredentialPRFRegistrationOutput@.
module ObjC.AuthenticationServices.ASAuthorizationPublicKeyCredentialPRFRegistrationOutput
  ( ASAuthorizationPublicKeyCredentialPRFRegistrationOutput
  , IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput(..)
  , isSupported
  , first
  , second
  , isSupportedSelector
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

-- | @- isSupported@
isSupported :: IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput asAuthorizationPublicKeyCredentialPRFRegistrationOutput => asAuthorizationPublicKeyCredentialPRFRegistrationOutput -> IO Bool
isSupported asAuthorizationPublicKeyCredentialPRFRegistrationOutput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationPublicKeyCredentialPRFRegistrationOutput (mkSelector "isSupported") retCULong []

-- | @- first@
first :: IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput asAuthorizationPublicKeyCredentialPRFRegistrationOutput => asAuthorizationPublicKeyCredentialPRFRegistrationOutput -> IO (Id NSData)
first asAuthorizationPublicKeyCredentialPRFRegistrationOutput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFRegistrationOutput (mkSelector "first") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- second@
second :: IsASAuthorizationPublicKeyCredentialPRFRegistrationOutput asAuthorizationPublicKeyCredentialPRFRegistrationOutput => asAuthorizationPublicKeyCredentialPRFRegistrationOutput -> IO (Id NSData)
second asAuthorizationPublicKeyCredentialPRFRegistrationOutput  =
  sendMsg asAuthorizationPublicKeyCredentialPRFRegistrationOutput (mkSelector "second") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isSupported@
isSupportedSelector :: Selector
isSupportedSelector = mkSelector "isSupported"

-- | @Selector@ for @first@
firstSelector :: Selector
firstSelector = mkSelector "first"

-- | @Selector@ for @second@
secondSelector :: Selector
secondSelector = mkSelector "second"

