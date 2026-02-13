{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKTokenKeychainCertificate
--
-- Interface for propagation token's certificates into the keychain.
--
-- Generated bindings for @TKTokenKeychainCertificate@.
module ObjC.CryptoTokenKit.TKTokenKeychainCertificate
  ( TKTokenKeychainCertificate
  , IsTKTokenKeychainCertificate(..)
  , initWithCertificate_objectID
  , initWithObjectID
  , data_
  , dataSelector
  , initWithCertificate_objectIDSelector
  , initWithObjectIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initialize TKTokenKeychainCertificate with data from SecCertificateRef.  Use SecCertificateCreateWithData to obtain SecCertificateRef.  @constraints@ property is initialized indicating that reading of certificate is always allowed, all other operations are disallowed.
--
-- ObjC selector: @- initWithCertificate:objectID:@
initWithCertificate_objectID :: IsTKTokenKeychainCertificate tkTokenKeychainCertificate => tkTokenKeychainCertificate -> Ptr () -> RawId -> IO (Id TKTokenKeychainCertificate)
initWithCertificate_objectID tkTokenKeychainCertificate certificateRef objectID =
  sendOwnedMessage tkTokenKeychainCertificate initWithCertificate_objectIDSelector certificateRef objectID

-- | @- initWithObjectID:@
initWithObjectID :: IsTKTokenKeychainCertificate tkTokenKeychainCertificate => tkTokenKeychainCertificate -> RawId -> IO (Id TKTokenKeychainCertificate)
initWithObjectID tkTokenKeychainCertificate objectID =
  sendOwnedMessage tkTokenKeychainCertificate initWithObjectIDSelector objectID

-- | Contains DER-encoded representation of an X.509 certificate.
--
-- ObjC selector: @- data@
data_ :: IsTKTokenKeychainCertificate tkTokenKeychainCertificate => tkTokenKeychainCertificate -> IO (Id NSData)
data_ tkTokenKeychainCertificate =
  sendMessage tkTokenKeychainCertificate dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCertificate:objectID:@
initWithCertificate_objectIDSelector :: Selector '[Ptr (), RawId] (Id TKTokenKeychainCertificate)
initWithCertificate_objectIDSelector = mkSelector "initWithCertificate:objectID:"

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector '[RawId] (Id TKTokenKeychainCertificate)
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

