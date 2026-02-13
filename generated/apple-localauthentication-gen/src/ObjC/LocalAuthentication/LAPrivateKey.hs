{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Managed Private Key.
--
-- Generated bindings for @LAPrivateKey@.
module ObjC.LocalAuthentication.LAPrivateKey
  ( LAPrivateKey
  , IsLAPrivateKey(..)
  , signData_secKeyAlgorithm_completion
  , canSignUsingSecKeyAlgorithm
  , decryptData_secKeyAlgorithm_completion
  , canDecryptUsingSecKeyAlgorithm
  , exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completion
  , canExchangeKeysUsingSecKeyAlgorithm
  , new
  , init_
  , publicKey
  , canDecryptUsingSecKeyAlgorithmSelector
  , canExchangeKeysUsingSecKeyAlgorithmSelector
  , canSignUsingSecKeyAlgorithmSelector
  , decryptData_secKeyAlgorithm_completionSelector
  , exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completionSelector
  , initSelector
  , newSelector
  , publicKeySelector
  , signData_secKeyAlgorithm_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Generates a digital signature for the given data.
--
-- @data@ — The data to be signed, typically the digest of the actual data.
--
-- @algorithm@ — A  @SecKeyAlgorithm@ suitable for generating signatures with this key – e.g: @kSecKeyAlgorithmECDSASignatureMessageX962SHA256@
--
-- @handler@ — Completion handler with the signature of given data or an error on failure.
--
-- ObjC selector: @- signData:secKeyAlgorithm:completion:@
signData_secKeyAlgorithm_completion :: (IsLAPrivateKey laPrivateKey, IsNSData data_) => laPrivateKey -> data_ -> RawId -> Ptr () -> IO ()
signData_secKeyAlgorithm_completion laPrivateKey data_ algorithm handler =
  sendMessage laPrivateKey signData_secKeyAlgorithm_completionSelector (toNSData data_) algorithm handler

-- | Checks if the the provided algorithm can be used for signing data
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canSignUsingSecKeyAlgorithm:@
canSignUsingSecKeyAlgorithm :: IsLAPrivateKey laPrivateKey => laPrivateKey -> RawId -> IO Bool
canSignUsingSecKeyAlgorithm laPrivateKey algorithm =
  sendMessage laPrivateKey canSignUsingSecKeyAlgorithmSelector algorithm

-- | Decrypts the given ciphertext
--
-- @data@ — The data to decrypt. The length and format of the data must conform to chosen algorithm, typically be less or equal to the value returned by SecKeyGetBlockSize().
--
-- @algorithm@ — A @SecKeyAlgorithm@ suitable for decrypting data with this key –e.g: @kSecKeyAlgorithmECIESEncryptionStandardVariableIVX963SHA256AESGCM@
--
-- @handler@ — Completion handler with plaintext or an error on failure.
--
-- ObjC selector: @- decryptData:secKeyAlgorithm:completion:@
decryptData_secKeyAlgorithm_completion :: (IsLAPrivateKey laPrivateKey, IsNSData data_) => laPrivateKey -> data_ -> RawId -> Ptr () -> IO ()
decryptData_secKeyAlgorithm_completion laPrivateKey data_ algorithm handler =
  sendMessage laPrivateKey decryptData_secKeyAlgorithm_completionSelector (toNSData data_) algorithm handler

-- | Checks if the the provided algorithm can be used for decryption
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canDecryptUsingSecKeyAlgorithm:@
canDecryptUsingSecKeyAlgorithm :: IsLAPrivateKey laPrivateKey => laPrivateKey -> RawId -> IO Bool
canDecryptUsingSecKeyAlgorithm laPrivateKey algorithm =
  sendMessage laPrivateKey canDecryptUsingSecKeyAlgorithmSelector algorithm

-- | Performs a Diffie-Hellman style key exchange operation
--
-- @publicKey@ — Remote party's public key.
--
-- @algorithm@ — A @SecKeyAlgorithm@ suitable for performing a key exchange with this key –e.g: @kSecKeyAlgorithmECDHKeyExchangeCofactorX963SHA256@
--
-- @parameters@ — Dictionary with parameters, see @SecKeyKeyExchangeParameter@ constants.  Used algorithm determines the set of required and optional parameters to be used.
--
-- @handler@ — Completion handler with the result of the key exchange or an error on failure.
--
-- ObjC selector: @- exchangeKeysWithPublicKey:secKeyAlgorithm:secKeyParameters:completion:@
exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completion :: (IsLAPrivateKey laPrivateKey, IsNSData publicKey, IsNSDictionary parameters) => laPrivateKey -> publicKey -> RawId -> parameters -> Ptr () -> IO ()
exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completion laPrivateKey publicKey algorithm parameters handler =
  sendMessage laPrivateKey exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completionSelector (toNSData publicKey) algorithm (toNSDictionary parameters) handler

-- | Checks if the the provided algorithm can be used for performing key exchanges
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canExchangeKeysUsingSecKeyAlgorithm:@
canExchangeKeysUsingSecKeyAlgorithm :: IsLAPrivateKey laPrivateKey => laPrivateKey -> RawId -> IO Bool
canExchangeKeysUsingSecKeyAlgorithm laPrivateKey algorithm =
  sendMessage laPrivateKey canExchangeKeysUsingSecKeyAlgorithmSelector algorithm

-- | Clients cannot create @LAPrivateKey@ instances directly. They typically obtain them from a @LAPersistedRight@ instance.
--
-- ObjC selector: @+ new@
new :: IO (Id LAPrivateKey)
new  =
  do
    cls' <- getRequiredClass "LAPrivateKey"
    sendOwnedClassMessage cls' newSelector

-- | Clients cannot create @LAPrivateKey@ instances directly. They typically obtain them from a @LAPersistedRight@ instance.
--
-- ObjC selector: @- init@
init_ :: IsLAPrivateKey laPrivateKey => laPrivateKey -> IO (Id LAPrivateKey)
init_ laPrivateKey =
  sendOwnedMessage laPrivateKey initSelector

-- | Offers the public key counterpart of a @LAPrivateKey@ instance
--
-- ObjC selector: @- publicKey@
publicKey :: IsLAPrivateKey laPrivateKey => laPrivateKey -> IO (Id LAPublicKey)
publicKey laPrivateKey =
  sendMessage laPrivateKey publicKeySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @signData:secKeyAlgorithm:completion:@
signData_secKeyAlgorithm_completionSelector :: Selector '[Id NSData, RawId, Ptr ()] ()
signData_secKeyAlgorithm_completionSelector = mkSelector "signData:secKeyAlgorithm:completion:"

-- | @Selector@ for @canSignUsingSecKeyAlgorithm:@
canSignUsingSecKeyAlgorithmSelector :: Selector '[RawId] Bool
canSignUsingSecKeyAlgorithmSelector = mkSelector "canSignUsingSecKeyAlgorithm:"

-- | @Selector@ for @decryptData:secKeyAlgorithm:completion:@
decryptData_secKeyAlgorithm_completionSelector :: Selector '[Id NSData, RawId, Ptr ()] ()
decryptData_secKeyAlgorithm_completionSelector = mkSelector "decryptData:secKeyAlgorithm:completion:"

-- | @Selector@ for @canDecryptUsingSecKeyAlgorithm:@
canDecryptUsingSecKeyAlgorithmSelector :: Selector '[RawId] Bool
canDecryptUsingSecKeyAlgorithmSelector = mkSelector "canDecryptUsingSecKeyAlgorithm:"

-- | @Selector@ for @exchangeKeysWithPublicKey:secKeyAlgorithm:secKeyParameters:completion:@
exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completionSelector :: Selector '[Id NSData, RawId, Id NSDictionary, Ptr ()] ()
exchangeKeysWithPublicKey_secKeyAlgorithm_secKeyParameters_completionSelector = mkSelector "exchangeKeysWithPublicKey:secKeyAlgorithm:secKeyParameters:completion:"

-- | @Selector@ for @canExchangeKeysUsingSecKeyAlgorithm:@
canExchangeKeysUsingSecKeyAlgorithmSelector :: Selector '[RawId] Bool
canExchangeKeysUsingSecKeyAlgorithmSelector = mkSelector "canExchangeKeysUsingSecKeyAlgorithm:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LAPrivateKey)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LAPrivateKey)
initSelector = mkSelector "init"

-- | @Selector@ for @publicKey@
publicKeySelector :: Selector '[] (Id LAPublicKey)
publicKeySelector = mkSelector "publicKey"

