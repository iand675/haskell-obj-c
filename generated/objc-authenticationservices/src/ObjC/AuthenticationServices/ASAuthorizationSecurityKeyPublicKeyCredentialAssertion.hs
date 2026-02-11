{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialAssertion@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialAssertion
  ( ASAuthorizationSecurityKeyPublicKeyCredentialAssertion
  , IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion(..)
  , new
  , init_
  , appID
  , newSelector
  , initSelector
  , appIDSelector


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
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialAssertion"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion asAuthorizationSecurityKeyPublicKeyCredentialAssertion => asAuthorizationSecurityKeyPublicKeyCredentialAssertion -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialAssertion)
init_ asAuthorizationSecurityKeyPublicKeyCredentialAssertion  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialAssertion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates that this assertion used the appid WebAuthn extension. This can only happen if the requesting app is a web browser and requested to use this extension.
--
-- ObjC selector: @- appID@
appID :: IsASAuthorizationSecurityKeyPublicKeyCredentialAssertion asAuthorizationSecurityKeyPublicKeyCredentialAssertion => asAuthorizationSecurityKeyPublicKeyCredentialAssertion -> IO Bool
appID asAuthorizationSecurityKeyPublicKeyCredentialAssertion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asAuthorizationSecurityKeyPublicKeyCredentialAssertion (mkSelector "appID") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @appID@
appIDSelector :: Selector
appIDSelector = mkSelector "appID"

