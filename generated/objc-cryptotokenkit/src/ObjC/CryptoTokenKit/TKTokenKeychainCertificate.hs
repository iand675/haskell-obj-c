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
  , initWithCertificate_objectIDSelector
  , initWithObjectIDSelector
  , dataSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initialize TKTokenKeychainCertificate with data from SecCertificateRef.  Use SecCertificateCreateWithData to obtain SecCertificateRef.  @constraints@ property is initialized indicating that reading of certificate is always allowed, all other operations are disallowed.
--
-- ObjC selector: @- initWithCertificate:objectID:@
initWithCertificate_objectID :: IsTKTokenKeychainCertificate tkTokenKeychainCertificate => tkTokenKeychainCertificate -> Ptr () -> RawId -> IO (Id TKTokenKeychainCertificate)
initWithCertificate_objectID tkTokenKeychainCertificate  certificateRef objectID =
  sendMsg tkTokenKeychainCertificate (mkSelector "initWithCertificate:objectID:") (retPtr retVoid) [argPtr certificateRef, argPtr (castPtr (unRawId objectID) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithObjectID:@
initWithObjectID :: IsTKTokenKeychainCertificate tkTokenKeychainCertificate => tkTokenKeychainCertificate -> RawId -> IO (Id TKTokenKeychainCertificate)
initWithObjectID tkTokenKeychainCertificate  objectID =
  sendMsg tkTokenKeychainCertificate (mkSelector "initWithObjectID:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ())] >>= ownedObject . castPtr

-- | Contains DER-encoded representation of an X.509 certificate.
--
-- ObjC selector: @- data@
data_ :: IsTKTokenKeychainCertificate tkTokenKeychainCertificate => tkTokenKeychainCertificate -> IO (Id NSData)
data_ tkTokenKeychainCertificate  =
  sendMsg tkTokenKeychainCertificate (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCertificate:objectID:@
initWithCertificate_objectIDSelector :: Selector
initWithCertificate_objectIDSelector = mkSelector "initWithCertificate:objectID:"

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

