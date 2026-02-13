{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithCredentialIDSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCredentialID:@
initWithCredentialID :: (IsASAuthorizationPlatformPublicKeyCredentialDescriptor asAuthorizationPlatformPublicKeyCredentialDescriptor, IsNSData credentialID) => asAuthorizationPlatformPublicKeyCredentialDescriptor -> credentialID -> IO (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
initWithCredentialID asAuthorizationPlatformPublicKeyCredentialDescriptor credentialID =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialDescriptor initWithCredentialIDSelector (toNSData credentialID)

-- | @+ new@
new :: IO (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationPlatformPublicKeyCredentialDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationPlatformPublicKeyCredentialDescriptor asAuthorizationPlatformPublicKeyCredentialDescriptor => asAuthorizationPlatformPublicKeyCredentialDescriptor -> IO (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
init_ asAuthorizationPlatformPublicKeyCredentialDescriptor =
  sendOwnedMessage asAuthorizationPlatformPublicKeyCredentialDescriptor initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCredentialID:@
initWithCredentialIDSelector :: Selector '[Id NSData] (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
initWithCredentialIDSelector = mkSelector "initWithCredentialID:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationPlatformPublicKeyCredentialDescriptor)
initSelector = mkSelector "init"

