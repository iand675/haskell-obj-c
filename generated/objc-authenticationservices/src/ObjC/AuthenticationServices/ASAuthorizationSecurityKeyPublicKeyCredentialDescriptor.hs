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
  , initWithCredentialID_transportsSelector
  , newSelector
  , initSelector
  , transportsSelector
  , setTransportsSelector


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

-- | @- initWithCredentialID:transports:@
initWithCredentialID_transports :: (IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor, IsNSData credentialID, IsNSArray allowedTransports) => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> credentialID -> allowedTransports -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
initWithCredentialID_transports asAuthorizationSecurityKeyPublicKeyCredentialDescriptor  credentialID allowedTransports =
withObjCPtr credentialID $ \raw_credentialID ->
  withObjCPtr allowedTransports $ \raw_allowedTransports ->
      sendMsg asAuthorizationSecurityKeyPublicKeyCredentialDescriptor (mkSelector "initWithCredentialID:transports:") (retPtr retVoid) [argPtr (castPtr raw_credentialID :: Ptr ()), argPtr (castPtr raw_allowedTransports :: Ptr ())] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
new  =
  do
    cls' <- getRequiredClass "ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> IO (Id ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor)
init_ asAuthorizationSecurityKeyPublicKeyCredentialDescriptor  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An array indicating transports for the credential indicated by credentialID.
--
-- ObjC selector: @- transports@
transports :: IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> IO (Id NSArray)
transports asAuthorizationSecurityKeyPublicKeyCredentialDescriptor  =
  sendMsg asAuthorizationSecurityKeyPublicKeyCredentialDescriptor (mkSelector "transports") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array indicating transports for the credential indicated by credentialID.
--
-- ObjC selector: @- setTransports:@
setTransports :: (IsASAuthorizationSecurityKeyPublicKeyCredentialDescriptor asAuthorizationSecurityKeyPublicKeyCredentialDescriptor, IsNSArray value) => asAuthorizationSecurityKeyPublicKeyCredentialDescriptor -> value -> IO ()
setTransports asAuthorizationSecurityKeyPublicKeyCredentialDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg asAuthorizationSecurityKeyPublicKeyCredentialDescriptor (mkSelector "setTransports:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCredentialID:transports:@
initWithCredentialID_transportsSelector :: Selector
initWithCredentialID_transportsSelector = mkSelector "initWithCredentialID:transports:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @transports@
transportsSelector :: Selector
transportsSelector = mkSelector "transports"

-- | @Selector@ for @setTransports:@
setTransportsSelector :: Selector
setTransportsSelector = mkSelector "setTransports:"

