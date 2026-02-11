{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | TKTokenKeychainKey
--
-- Interface for propagation token's keys into the keychain.
--
-- Generated bindings for @TKTokenKeychainKey@.
module ObjC.CryptoTokenKit.TKTokenKeychainKey
  ( TKTokenKeychainKey
  , IsTKTokenKeychainKey(..)
  , initWithCertificate_objectID
  , initWithObjectID
  , keyType
  , setKeyType
  , applicationTag
  , setApplicationTag
  , keySizeInBits
  , setKeySizeInBits
  , publicKeyData
  , setPublicKeyData
  , publicKeyHash
  , setPublicKeyHash
  , canDecrypt
  , setCanDecrypt
  , canSign
  , setCanSign
  , canPerformKeyExchange
  , setCanPerformKeyExchange
  , suitableForLogin
  , setSuitableForLogin
  , initWithCertificate_objectIDSelector
  , initWithObjectIDSelector
  , keyTypeSelector
  , setKeyTypeSelector
  , applicationTagSelector
  , setApplicationTagSelector
  , keySizeInBitsSelector
  , setKeySizeInBitsSelector
  , publicKeyDataSelector
  , setPublicKeyDataSelector
  , publicKeyHashSelector
  , setPublicKeyHashSelector
  , canDecryptSelector
  , setCanDecryptSelector
  , canSignSelector
  , setCanSignSelector
  , canPerformKeyExchangeSelector
  , setCanPerformKeyExchangeSelector
  , suitableForLoginSelector
  , setSuitableForLoginSelector


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

-- | Initialize TKTokenKeychainKey with informations from SecCertificateRef associated with the key.  Use SecCertificateCreateWithData to obtain SecCertificateRef.  If NULL is passed instead of certificate, all properties of created instance must be initialized manually.
--
-- ObjC selector: @- initWithCertificate:objectID:@
initWithCertificate_objectID :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Ptr () -> RawId -> IO (Id TKTokenKeychainKey)
initWithCertificate_objectID tkTokenKeychainKey  certificateRef objectID =
  sendMsg tkTokenKeychainKey (mkSelector "initWithCertificate:objectID:") (retPtr retVoid) [argPtr certificateRef, argPtr (castPtr (unRawId objectID) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithObjectID:@
initWithObjectID :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> RawId -> IO (Id TKTokenKeychainKey)
initWithObjectID tkTokenKeychainKey  objectID =
  sendMsg tkTokenKeychainKey (mkSelector "initWithObjectID:") (retPtr retVoid) [argPtr (castPtr (unRawId objectID) :: Ptr ())] >>= ownedObject . castPtr

-- | Type of the key, currently kSecAttrKeyTypeRSA and kSecAttrKeyTypeECSECPrimeRandom is supported).  The property is an equivalent to kSecAttrKeyType in SecItem.h
--
-- ObjC selector: @- keyType@
keyType :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSString)
keyType tkTokenKeychainKey  =
  sendMsg tkTokenKeychainKey (mkSelector "keyType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Type of the key, currently kSecAttrKeyTypeRSA and kSecAttrKeyTypeECSECPrimeRandom is supported).  The property is an equivalent to kSecAttrKeyType in SecItem.h
--
-- ObjC selector: @- setKeyType:@
setKeyType :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSString value) => tkTokenKeychainKey -> value -> IO ()
setKeyType tkTokenKeychainKey  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenKeychainKey (mkSelector "setKeyType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Represents private tag data.  The property is an equivalent to kSecAttrApplicationTag in SecItem.h
--
-- ObjC selector: @- applicationTag@
applicationTag :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSData)
applicationTag tkTokenKeychainKey  =
  sendMsg tkTokenKeychainKey (mkSelector "applicationTag") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Represents private tag data.  The property is an equivalent to kSecAttrApplicationTag in SecItem.h
--
-- ObjC selector: @- setApplicationTag:@
setApplicationTag :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSData value) => tkTokenKeychainKey -> value -> IO ()
setApplicationTag tkTokenKeychainKey  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenKeychainKey (mkSelector "setApplicationTag:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates the number of bits in this key.  The property is an equivalent to kSecAttrKeySizeInBits in SecItem.h
--
-- ObjC selector: @- keySizeInBits@
keySizeInBits :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO CLong
keySizeInBits tkTokenKeychainKey  =
  sendMsg tkTokenKeychainKey (mkSelector "keySizeInBits") retCLong []

-- | Indicates the number of bits in this key.  The property is an equivalent to kSecAttrKeySizeInBits in SecItem.h
--
-- ObjC selector: @- setKeySizeInBits:@
setKeySizeInBits :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> CLong -> IO ()
setKeySizeInBits tkTokenKeychainKey  value =
  sendMsg tkTokenKeychainKey (mkSelector "setKeySizeInBits:") retVoid [argCLong (fromIntegral value)]

-- | Contains raw public key data for this private key.
--
-- ObjC selector: @- publicKeyData@
publicKeyData :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSData)
publicKeyData tkTokenKeychainKey  =
  sendMsg tkTokenKeychainKey (mkSelector "publicKeyData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Contains raw public key data for this private key.
--
-- ObjC selector: @- setPublicKeyData:@
setPublicKeyData :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSData value) => tkTokenKeychainKey -> value -> IO ()
setPublicKeyData tkTokenKeychainKey  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenKeychainKey (mkSelector "setPublicKeyData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | SHA1 hash of the raw public key.  The property is an equivalent to kSecAttrApplicationLabel in SecItem.h
--
-- ObjC selector: @- publicKeyHash@
publicKeyHash :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSData)
publicKeyHash tkTokenKeychainKey  =
  sendMsg tkTokenKeychainKey (mkSelector "publicKeyHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SHA1 hash of the raw public key.  The property is an equivalent to kSecAttrApplicationLabel in SecItem.h
--
-- ObjC selector: @- setPublicKeyHash:@
setPublicKeyHash :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSData value) => tkTokenKeychainKey -> value -> IO ()
setPublicKeyHash tkTokenKeychainKey  value =
withObjCPtr value $ \raw_value ->
    sendMsg tkTokenKeychainKey (mkSelector "setPublicKeyHash:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether this key can be used to decrypt data.  The property is an equivalent to kSecAttrCanDecrypt in SecItem.h
--
-- ObjC selector: @- canDecrypt@
canDecrypt :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
canDecrypt tkTokenKeychainKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenKeychainKey (mkSelector "canDecrypt") retCULong []

-- | Indicates whether this key can be used to decrypt data.  The property is an equivalent to kSecAttrCanDecrypt in SecItem.h
--
-- ObjC selector: @- setCanDecrypt:@
setCanDecrypt :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setCanDecrypt tkTokenKeychainKey  value =
  sendMsg tkTokenKeychainKey (mkSelector "setCanDecrypt:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether this key can be used to create a digital signature.  The property is an equivalent to kSecAttrCanSign in SecItem.h
--
-- ObjC selector: @- canSign@
canSign :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
canSign tkTokenKeychainKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenKeychainKey (mkSelector "canSign") retCULong []

-- | Indicates whether this key can be used to create a digital signature.  The property is an equivalent to kSecAttrCanSign in SecItem.h
--
-- ObjC selector: @- setCanSign:@
setCanSign :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setCanSign tkTokenKeychainKey  value =
  sendMsg tkTokenKeychainKey (mkSelector "setCanSign:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether this key can be used to perform Diffie-Hellman style cryptographic key exchange.
--
-- ObjC selector: @- canPerformKeyExchange@
canPerformKeyExchange :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
canPerformKeyExchange tkTokenKeychainKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenKeychainKey (mkSelector "canPerformKeyExchange") retCULong []

-- | Indicates whether this key can be used to perform Diffie-Hellman style cryptographic key exchange.
--
-- ObjC selector: @- setCanPerformKeyExchange:@
setCanPerformKeyExchange :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setCanPerformKeyExchange tkTokenKeychainKey  value =
  sendMsg tkTokenKeychainKey (mkSelector "setCanPerformKeyExchange:") retVoid [argCULong (if value then 1 else 0)]

-- | Indicates whether this key can be used for login in to the system.
--
-- ObjC selector: @- suitableForLogin@
suitableForLogin :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
suitableForLogin tkTokenKeychainKey  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkTokenKeychainKey (mkSelector "suitableForLogin") retCULong []

-- | Indicates whether this key can be used for login in to the system.
--
-- ObjC selector: @- setSuitableForLogin:@
setSuitableForLogin :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setSuitableForLogin tkTokenKeychainKey  value =
  sendMsg tkTokenKeychainKey (mkSelector "setSuitableForLogin:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCertificate:objectID:@
initWithCertificate_objectIDSelector :: Selector
initWithCertificate_objectIDSelector = mkSelector "initWithCertificate:objectID:"

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @keyType@
keyTypeSelector :: Selector
keyTypeSelector = mkSelector "keyType"

-- | @Selector@ for @setKeyType:@
setKeyTypeSelector :: Selector
setKeyTypeSelector = mkSelector "setKeyType:"

-- | @Selector@ for @applicationTag@
applicationTagSelector :: Selector
applicationTagSelector = mkSelector "applicationTag"

-- | @Selector@ for @setApplicationTag:@
setApplicationTagSelector :: Selector
setApplicationTagSelector = mkSelector "setApplicationTag:"

-- | @Selector@ for @keySizeInBits@
keySizeInBitsSelector :: Selector
keySizeInBitsSelector = mkSelector "keySizeInBits"

-- | @Selector@ for @setKeySizeInBits:@
setKeySizeInBitsSelector :: Selector
setKeySizeInBitsSelector = mkSelector "setKeySizeInBits:"

-- | @Selector@ for @publicKeyData@
publicKeyDataSelector :: Selector
publicKeyDataSelector = mkSelector "publicKeyData"

-- | @Selector@ for @setPublicKeyData:@
setPublicKeyDataSelector :: Selector
setPublicKeyDataSelector = mkSelector "setPublicKeyData:"

-- | @Selector@ for @publicKeyHash@
publicKeyHashSelector :: Selector
publicKeyHashSelector = mkSelector "publicKeyHash"

-- | @Selector@ for @setPublicKeyHash:@
setPublicKeyHashSelector :: Selector
setPublicKeyHashSelector = mkSelector "setPublicKeyHash:"

-- | @Selector@ for @canDecrypt@
canDecryptSelector :: Selector
canDecryptSelector = mkSelector "canDecrypt"

-- | @Selector@ for @setCanDecrypt:@
setCanDecryptSelector :: Selector
setCanDecryptSelector = mkSelector "setCanDecrypt:"

-- | @Selector@ for @canSign@
canSignSelector :: Selector
canSignSelector = mkSelector "canSign"

-- | @Selector@ for @setCanSign:@
setCanSignSelector :: Selector
setCanSignSelector = mkSelector "setCanSign:"

-- | @Selector@ for @canPerformKeyExchange@
canPerformKeyExchangeSelector :: Selector
canPerformKeyExchangeSelector = mkSelector "canPerformKeyExchange"

-- | @Selector@ for @setCanPerformKeyExchange:@
setCanPerformKeyExchangeSelector :: Selector
setCanPerformKeyExchangeSelector = mkSelector "setCanPerformKeyExchange:"

-- | @Selector@ for @suitableForLogin@
suitableForLoginSelector :: Selector
suitableForLoginSelector = mkSelector "suitableForLogin"

-- | @Selector@ for @setSuitableForLogin:@
setSuitableForLoginSelector :: Selector
setSuitableForLoginSelector = mkSelector "setSuitableForLogin:"

