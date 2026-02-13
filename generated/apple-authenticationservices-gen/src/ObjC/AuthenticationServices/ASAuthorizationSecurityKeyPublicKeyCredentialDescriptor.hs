{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object to describe a credential on a security key.
--
-- Generated bindings for @ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor@.
module ObjC.AuthenticationServices.ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor
  ( ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor
  , IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor(..)
  , initWithCredentialID_transports
  , new
  , init_
  , transports
  , setTransports
  , initSelector
  , initWithCredentialID_transportsSelector
  , newSelector
  , setTransportsSelector
  , transportsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCredentialID:transports:@
initWithCredentialID_transports :: (IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor, IsNSData credentialID, IsNSArray allowedTransports) => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> credentialID -> allowedTransports -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
initWithCredentialID_transports asAuthorizationSecurityKeyPublicKeyCredentialDescriptor credentialID allowedTransports =
  sendOwnedMessage asAuthorizationSecurityKeyPublicKeyCredentialDescriptor initWithCredentialID_transportsSelector (toNSData credentialID) (toNSArray allowedTransports)

-- | @+ new@
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
init_ asAuthorizationSecurityKeyPublicKeyCredentialDescriptor =
  sendOwnedMessage asAuthorizationSecurityKeyPublicKeyCredentialDescriptor initSelector

-- | An array indicating transports for the credential indicated by credentialID.
--
-- ObjC selector: @- transports@
transports :: IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> IO (Id NSArray)
transports asAuthorizationSecurityKeyPublicKeyCredentialDescriptor =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialDescriptor transportsSelector

-- | An array indicating transports for the credential indicated by credentialID.
--
-- ObjC selector: @- setTransports:@
setTransports :: (IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> value -> IO ()
setTransports asAuthorizationSecurityKeyPublicKeyCredentialDescriptor value =
  sendMessage asAuthorizationSecurityKeyPublicKeyCredentialDescriptor setTransportsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCredentialID:transports:@
initWithCredentialID_transportsSelector :: Selector '[Id NSData, Id NSArray] (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
initWithCredentialID_transportsSelector = mkSelector "initWithCredentialID:transports:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @transports@
transportsSelector :: Selector '[] (Id NSArray)
transportsSelector = mkSelector "transports"

-- | @Selector@ for @setTransports:@
setTransportsSelector :: Selector '[Id NSArray] ()
setTransportsSelector = mkSelector "setTransports:"

