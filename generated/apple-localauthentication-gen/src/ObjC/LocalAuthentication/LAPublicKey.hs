{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The public part of an asymmetric key pair
--
-- Generated bindings for @LAPublicKey@.
module ObjC.LocalAuthentication.LAPublicKey
  ( LAPublicKey
  , IsLAPublicKey(..)
  , exportBytesWithCompletion
  , encryptData_secKeyAlgorithm_completion
  , canEncryptUsingSecKeyAlgorithm
  , verifyData_signature_secKeyAlgorithm_completion
  , canVerifyUsingSecKeyAlgorithm
  , new
  , init_
  , canEncryptUsingSecKeyAlgorithmSelector
  , canVerifyUsingSecKeyAlgorithmSelector
  , encryptData_secKeyAlgorithm_completionSelector
  , exportBytesWithCompletionSelector
  , initSelector
  , newSelector
  , verifyData_signature_secKeyAlgorithm_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Exports public key bytes.
--
-- @handler@ — Completion handler with the raw bytes of the public key or an error on failure
--
-- ObjC selector: @- exportBytesWithCompletion:@
exportBytesWithCompletion :: IsLAPublicKey laPublicKey => laPublicKey -> Ptr () -> IO ()
exportBytesWithCompletion laPublicKey handler =
  sendMessage laPublicKey exportBytesWithCompletionSelector handler

-- | Encrypts the given data
--
-- @data@ — The data to encrypt.
--
-- @algorithm@ — A @SecKeyAlgorithm@ suitable for encrypting with this key –e.g: @kSecKeyAlgorithmECIESEncryptionStandardVariableIVX963SHA256AESGCM@ .
--
-- @handler@ — Completion handler with the cipher text or an error on failure.
--
-- ObjC selector: @- encryptData:secKeyAlgorithm:completion:@
encryptData_secKeyAlgorithm_completion :: (IsLAPublicKey laPublicKey, IsNSData data_) => laPublicKey -> data_ -> RawId -> Ptr () -> IO ()
encryptData_secKeyAlgorithm_completion laPublicKey data_ algorithm handler =
  sendMessage laPublicKey encryptData_secKeyAlgorithm_completionSelector (toNSData data_) algorithm handler

-- | Checks if the the provided algorithm can be used for encryption with the key.
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canEncryptUsingSecKeyAlgorithm:@
canEncryptUsingSecKeyAlgorithm :: IsLAPublicKey laPublicKey => laPublicKey -> RawId -> IO Bool
canEncryptUsingSecKeyAlgorithm laPublicKey algorithm =
  sendMessage laPublicKey canEncryptUsingSecKeyAlgorithmSelector algorithm

-- | Verifies a digital signature for the given data.
--
-- @signedData@ — The signed data.
--
-- @signature@ — The signature of the given data.
--
-- @algorithm@ — One of @SecKeyAlgorithm@ suitable for verifying signatures with this key –e.g: @kSecKeyAlgorithmECDSASignatureMessageX962SHA256@
--
-- @handler@ — Completion handler with the signature of given data or an error on failure.
--
-- ObjC selector: @- verifyData:signature:secKeyAlgorithm:completion:@
verifyData_signature_secKeyAlgorithm_completion :: (IsLAPublicKey laPublicKey, IsNSData signedData, IsNSData signature) => laPublicKey -> signedData -> signature -> RawId -> Ptr () -> IO ()
verifyData_signature_secKeyAlgorithm_completion laPublicKey signedData signature algorithm handler =
  sendMessage laPublicKey verifyData_signature_secKeyAlgorithm_completionSelector (toNSData signedData) (toNSData signature) algorithm handler

-- | Checks if the the provided algorithm can be used for verifying signatures with the key.
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canVerifyUsingSecKeyAlgorithm:@
canVerifyUsingSecKeyAlgorithm :: IsLAPublicKey laPublicKey => laPublicKey -> RawId -> IO Bool
canVerifyUsingSecKeyAlgorithm laPublicKey algorithm =
  sendMessage laPublicKey canVerifyUsingSecKeyAlgorithmSelector algorithm

-- | Clients cannot create @LAPublicKey@ instances directly. They can only obtain them from a related @LAPrivateKey@ instance
--
-- ObjC selector: @+ new@
new :: IO (Id LAPublicKey)
new  =
  do
    cls' <- getRequiredClass "LAPublicKey"
    sendOwnedClassMessage cls' newSelector

-- | Clients cannot create @LAPublicKey@ instances directly. They can only obtain them from a related @LAPrivateKey@ instance
--
-- ObjC selector: @- init@
init_ :: IsLAPublicKey laPublicKey => laPublicKey -> IO (Id LAPublicKey)
init_ laPublicKey =
  sendOwnedMessage laPublicKey initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @exportBytesWithCompletion:@
exportBytesWithCompletionSelector :: Selector '[Ptr ()] ()
exportBytesWithCompletionSelector = mkSelector "exportBytesWithCompletion:"

-- | @Selector@ for @encryptData:secKeyAlgorithm:completion:@
encryptData_secKeyAlgorithm_completionSelector :: Selector '[Id NSData, RawId, Ptr ()] ()
encryptData_secKeyAlgorithm_completionSelector = mkSelector "encryptData:secKeyAlgorithm:completion:"

-- | @Selector@ for @canEncryptUsingSecKeyAlgorithm:@
canEncryptUsingSecKeyAlgorithmSelector :: Selector '[RawId] Bool
canEncryptUsingSecKeyAlgorithmSelector = mkSelector "canEncryptUsingSecKeyAlgorithm:"

-- | @Selector@ for @verifyData:signature:secKeyAlgorithm:completion:@
verifyData_signature_secKeyAlgorithm_completionSelector :: Selector '[Id NSData, Id NSData, RawId, Ptr ()] ()
verifyData_signature_secKeyAlgorithm_completionSelector = mkSelector "verifyData:signature:secKeyAlgorithm:completion:"

-- | @Selector@ for @canVerifyUsingSecKeyAlgorithm:@
canVerifyUsingSecKeyAlgorithmSelector :: Selector '[RawId] Bool
canVerifyUsingSecKeyAlgorithmSelector = mkSelector "canVerifyUsingSecKeyAlgorithm:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LAPublicKey)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LAPublicKey)
initSelector = mkSelector "init"

