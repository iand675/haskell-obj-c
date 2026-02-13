{-# LANGUAGE DataKinds #-}
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
  , applicationTagSelector
  , canDecryptSelector
  , canPerformKeyExchangeSelector
  , canSignSelector
  , initWithCertificate_objectIDSelector
  , initWithObjectIDSelector
  , keySizeInBitsSelector
  , keyTypeSelector
  , publicKeyDataSelector
  , publicKeyHashSelector
  , setApplicationTagSelector
  , setCanDecryptSelector
  , setCanPerformKeyExchangeSelector
  , setCanSignSelector
  , setKeySizeInBitsSelector
  , setKeyTypeSelector
  , setPublicKeyDataSelector
  , setPublicKeyHashSelector
  , setSuitableForLoginSelector
  , suitableForLoginSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize TKTokenKeychainKey with informations from SecCertificateRef associated with the key.  Use SecCertificateCreateWithData to obtain SecCertificateRef.  If NULL is passed instead of certificate, all properties of created instance must be initialized manually.
--
-- ObjC selector: @- initWithCertificate:objectID:@
initWithCertificate_objectID :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Ptr () -> RawId -> IO (Id TKTokenKeychainKey)
initWithCertificate_objectID tkTokenKeychainKey certificateRef objectID =
  sendOwnedMessage tkTokenKeychainKey initWithCertificate_objectIDSelector certificateRef objectID

-- | @- initWithObjectID:@
initWithObjectID :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> RawId -> IO (Id TKTokenKeychainKey)
initWithObjectID tkTokenKeychainKey objectID =
  sendOwnedMessage tkTokenKeychainKey initWithObjectIDSelector objectID

-- | Type of the key, currently kSecAttrKeyTypeRSA and kSecAttrKeyTypeECSECPrimeRandom is supported).  The property is an equivalent to kSecAttrKeyType in SecItem.h
--
-- ObjC selector: @- keyType@
keyType :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSString)
keyType tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey keyTypeSelector

-- | Type of the key, currently kSecAttrKeyTypeRSA and kSecAttrKeyTypeECSECPrimeRandom is supported).  The property is an equivalent to kSecAttrKeyType in SecItem.h
--
-- ObjC selector: @- setKeyType:@
setKeyType :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSString value) => tkTokenKeychainKey -> value -> IO ()
setKeyType tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setKeyTypeSelector (toNSString value)

-- | Represents private tag data.  The property is an equivalent to kSecAttrApplicationTag in SecItem.h
--
-- ObjC selector: @- applicationTag@
applicationTag :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSData)
applicationTag tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey applicationTagSelector

-- | Represents private tag data.  The property is an equivalent to kSecAttrApplicationTag in SecItem.h
--
-- ObjC selector: @- setApplicationTag:@
setApplicationTag :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSData value) => tkTokenKeychainKey -> value -> IO ()
setApplicationTag tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setApplicationTagSelector (toNSData value)

-- | Indicates the number of bits in this key.  The property is an equivalent to kSecAttrKeySizeInBits in SecItem.h
--
-- ObjC selector: @- keySizeInBits@
keySizeInBits :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO CLong
keySizeInBits tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey keySizeInBitsSelector

-- | Indicates the number of bits in this key.  The property is an equivalent to kSecAttrKeySizeInBits in SecItem.h
--
-- ObjC selector: @- setKeySizeInBits:@
setKeySizeInBits :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> CLong -> IO ()
setKeySizeInBits tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setKeySizeInBitsSelector value

-- | Contains raw public key data for this private key.
--
-- ObjC selector: @- publicKeyData@
publicKeyData :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSData)
publicKeyData tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey publicKeyDataSelector

-- | Contains raw public key data for this private key.
--
-- ObjC selector: @- setPublicKeyData:@
setPublicKeyData :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSData value) => tkTokenKeychainKey -> value -> IO ()
setPublicKeyData tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setPublicKeyDataSelector (toNSData value)

-- | SHA1 hash of the raw public key.  The property is an equivalent to kSecAttrApplicationLabel in SecItem.h
--
-- ObjC selector: @- publicKeyHash@
publicKeyHash :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO (Id NSData)
publicKeyHash tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey publicKeyHashSelector

-- | SHA1 hash of the raw public key.  The property is an equivalent to kSecAttrApplicationLabel in SecItem.h
--
-- ObjC selector: @- setPublicKeyHash:@
setPublicKeyHash :: (IsTKTokenKeychainKey tkTokenKeychainKey, IsNSData value) => tkTokenKeychainKey -> value -> IO ()
setPublicKeyHash tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setPublicKeyHashSelector (toNSData value)

-- | Indicates whether this key can be used to decrypt data.  The property is an equivalent to kSecAttrCanDecrypt in SecItem.h
--
-- ObjC selector: @- canDecrypt@
canDecrypt :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
canDecrypt tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey canDecryptSelector

-- | Indicates whether this key can be used to decrypt data.  The property is an equivalent to kSecAttrCanDecrypt in SecItem.h
--
-- ObjC selector: @- setCanDecrypt:@
setCanDecrypt :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setCanDecrypt tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setCanDecryptSelector value

-- | Indicates whether this key can be used to create a digital signature.  The property is an equivalent to kSecAttrCanSign in SecItem.h
--
-- ObjC selector: @- canSign@
canSign :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
canSign tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey canSignSelector

-- | Indicates whether this key can be used to create a digital signature.  The property is an equivalent to kSecAttrCanSign in SecItem.h
--
-- ObjC selector: @- setCanSign:@
setCanSign :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setCanSign tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setCanSignSelector value

-- | Indicates whether this key can be used to perform Diffie-Hellman style cryptographic key exchange.
--
-- ObjC selector: @- canPerformKeyExchange@
canPerformKeyExchange :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
canPerformKeyExchange tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey canPerformKeyExchangeSelector

-- | Indicates whether this key can be used to perform Diffie-Hellman style cryptographic key exchange.
--
-- ObjC selector: @- setCanPerformKeyExchange:@
setCanPerformKeyExchange :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setCanPerformKeyExchange tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setCanPerformKeyExchangeSelector value

-- | Indicates whether this key can be used for login in to the system.
--
-- ObjC selector: @- suitableForLogin@
suitableForLogin :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> IO Bool
suitableForLogin tkTokenKeychainKey =
  sendMessage tkTokenKeychainKey suitableForLoginSelector

-- | Indicates whether this key can be used for login in to the system.
--
-- ObjC selector: @- setSuitableForLogin:@
setSuitableForLogin :: IsTKTokenKeychainKey tkTokenKeychainKey => tkTokenKeychainKey -> Bool -> IO ()
setSuitableForLogin tkTokenKeychainKey value =
  sendMessage tkTokenKeychainKey setSuitableForLoginSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCertificate:objectID:@
initWithCertificate_objectIDSelector :: Selector '[Ptr (), RawId] (Id TKTokenKeychainKey)
initWithCertificate_objectIDSelector = mkSelector "initWithCertificate:objectID:"

-- | @Selector@ for @initWithObjectID:@
initWithObjectIDSelector :: Selector '[RawId] (Id TKTokenKeychainKey)
initWithObjectIDSelector = mkSelector "initWithObjectID:"

-- | @Selector@ for @keyType@
keyTypeSelector :: Selector '[] (Id NSString)
keyTypeSelector = mkSelector "keyType"

-- | @Selector@ for @setKeyType:@
setKeyTypeSelector :: Selector '[Id NSString] ()
setKeyTypeSelector = mkSelector "setKeyType:"

-- | @Selector@ for @applicationTag@
applicationTagSelector :: Selector '[] (Id NSData)
applicationTagSelector = mkSelector "applicationTag"

-- | @Selector@ for @setApplicationTag:@
setApplicationTagSelector :: Selector '[Id NSData] ()
setApplicationTagSelector = mkSelector "setApplicationTag:"

-- | @Selector@ for @keySizeInBits@
keySizeInBitsSelector :: Selector '[] CLong
keySizeInBitsSelector = mkSelector "keySizeInBits"

-- | @Selector@ for @setKeySizeInBits:@
setKeySizeInBitsSelector :: Selector '[CLong] ()
setKeySizeInBitsSelector = mkSelector "setKeySizeInBits:"

-- | @Selector@ for @publicKeyData@
publicKeyDataSelector :: Selector '[] (Id NSData)
publicKeyDataSelector = mkSelector "publicKeyData"

-- | @Selector@ for @setPublicKeyData:@
setPublicKeyDataSelector :: Selector '[Id NSData] ()
setPublicKeyDataSelector = mkSelector "setPublicKeyData:"

-- | @Selector@ for @publicKeyHash@
publicKeyHashSelector :: Selector '[] (Id NSData)
publicKeyHashSelector = mkSelector "publicKeyHash"

-- | @Selector@ for @setPublicKeyHash:@
setPublicKeyHashSelector :: Selector '[Id NSData] ()
setPublicKeyHashSelector = mkSelector "setPublicKeyHash:"

-- | @Selector@ for @canDecrypt@
canDecryptSelector :: Selector '[] Bool
canDecryptSelector = mkSelector "canDecrypt"

-- | @Selector@ for @setCanDecrypt:@
setCanDecryptSelector :: Selector '[Bool] ()
setCanDecryptSelector = mkSelector "setCanDecrypt:"

-- | @Selector@ for @canSign@
canSignSelector :: Selector '[] Bool
canSignSelector = mkSelector "canSign"

-- | @Selector@ for @setCanSign:@
setCanSignSelector :: Selector '[Bool] ()
setCanSignSelector = mkSelector "setCanSign:"

-- | @Selector@ for @canPerformKeyExchange@
canPerformKeyExchangeSelector :: Selector '[] Bool
canPerformKeyExchangeSelector = mkSelector "canPerformKeyExchange"

-- | @Selector@ for @setCanPerformKeyExchange:@
setCanPerformKeyExchangeSelector :: Selector '[Bool] ()
setCanPerformKeyExchangeSelector = mkSelector "setCanPerformKeyExchange:"

-- | @Selector@ for @suitableForLogin@
suitableForLoginSelector :: Selector '[] Bool
suitableForLoginSelector = mkSelector "suitableForLogin"

-- | @Selector@ for @setSuitableForLogin:@
setSuitableForLoginSelector :: Selector '[Bool] ()
setSuitableForLoginSelector = mkSelector "setSuitableForLogin:"

