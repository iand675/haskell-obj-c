{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAuthorizationPlatformPublicKeyCredentialDescriptor@.
module ObjC.AuthenticationServices.ASAuthorizationPlatformPublicKeyCredentialDescriptor
  ( ASAuthorizationPlatformPublicKeyCredentialDescriptor
  , IsASAuthorizationPlatformPublicKeyCredentialDescriptor(..)
  , initWithCredentialID
  , new
  , init_
  , initWithCredentialIDSelector
  , newSelector
  , initSelector


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

-- | @- initWithCredentialID:@
initWithCredentialID :: (IsASAuthorizationPlatformPublicKeyCredentialDescriptor asAuthorizationPlatformPublicKeyCredentialDescriptor, IsNSData credentialID) => asAuthorizationPlatformPublicKeyCredentialDescriptor -> credentialID -> IO (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
initWithCredentialID asAuthorizationPlatformPublicKeyCredentialDescriptor  credentialID =
withObjCPtr credentialID $ \raw_credentialID ->
    sendMsg asAuthorizationPlatformPublicKeyCredentialDescriptor (mkSelector "initWithCredentialID:") (retPtr retVoid) [argPtr (castPtr raw_credentialID :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialDescriptor asAuthorizationPlatformPublicKeyCredentialDescriptor => asAuthorizationPlatformPublicKeyCredentialDescriptor -> IO (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
init_ asAuthorizationPlatformPublicKeyCredentialDescriptor  =
  sendMsg asAuthorizationPlatformPublicKeyCredentialDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCredentialID:@
initWithCredentialIDSelector :: Selector
initWithCredentialIDSelector = mkSelector "initWithCredentialID:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

