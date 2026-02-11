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
  , exportBytesWithCompletionSelector
  , encryptData_secKeyAlgorithm_completionSelector
  , canEncryptUsingSecKeyAlgorithmSelector
  , verifyData_signature_secKeyAlgorithm_completionSelector
  , canVerifyUsingSecKeyAlgorithmSelector
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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Exports public key bytes.
--
-- @handler@ — Completion handler with the raw bytes of the public key or an error on failure
--
-- ObjC selector: @- exportBytesWithCompletion:@
exportBytesWithCompletion :: IsLAPublicKey laPublicKey => laPublicKey -> Ptr () -> IO ()
exportBytesWithCompletion laPublicKey  handler =
  sendMsg laPublicKey (mkSelector "exportBytesWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

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
encryptData_secKeyAlgorithm_completion laPublicKey  data_ algorithm handler =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg laPublicKey (mkSelector "encryptData:secKeyAlgorithm:completion:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr (unRawId algorithm) :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Checks if the the provided algorithm can be used for encryption with the key.
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canEncryptUsingSecKeyAlgorithm:@
canEncryptUsingSecKeyAlgorithm :: IsLAPublicKey laPublicKey => laPublicKey -> RawId -> IO Bool
canEncryptUsingSecKeyAlgorithm laPublicKey  algorithm =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laPublicKey (mkSelector "canEncryptUsingSecKeyAlgorithm:") retCULong [argPtr (castPtr (unRawId algorithm) :: Ptr ())]

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
verifyData_signature_secKeyAlgorithm_completion laPublicKey  signedData signature algorithm handler =
withObjCPtr signedData $ \raw_signedData ->
  withObjCPtr signature $ \raw_signature ->
      sendMsg laPublicKey (mkSelector "verifyData:signature:secKeyAlgorithm:completion:") retVoid [argPtr (castPtr raw_signedData :: Ptr ()), argPtr (castPtr raw_signature :: Ptr ()), argPtr (castPtr (unRawId algorithm) :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Checks if the the provided algorithm can be used for verifying signatures with the key.
--
-- @algorithm@ — Cryptographic algorithm
--
-- Returns: @YES@ in case the key supports the provided algorithm with the specified operation.
--
-- ObjC selector: @- canVerifyUsingSecKeyAlgorithm:@
canVerifyUsingSecKeyAlgorithm :: IsLAPublicKey laPublicKey => laPublicKey -> RawId -> IO Bool
canVerifyUsingSecKeyAlgorithm laPublicKey  algorithm =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg laPublicKey (mkSelector "canVerifyUsingSecKeyAlgorithm:") retCULong [argPtr (castPtr (unRawId algorithm) :: Ptr ())]

-- | Clients cannot create @LAPublicKey@ instances directly. They can only obtain them from a related @LAPrivateKey@ instance
--
-- ObjC selector: @+ new@
new :: IO (Id LAPublicKey)
new  =
  do
    cls' <- getRequiredClass "LAPublicKey"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Clients cannot create @LAPublicKey@ instances directly. They can only obtain them from a related @LAPrivateKey@ instance
--
-- ObjC selector: @- init@
init_ :: IsLAPublicKey laPublicKey => laPublicKey -> IO (Id LAPublicKey)
init_ laPublicKey  =
  sendMsg laPublicKey (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @exportBytesWithCompletion:@
exportBytesWithCompletionSelector :: Selector
exportBytesWithCompletionSelector = mkSelector "exportBytesWithCompletion:"

-- | @Selector@ for @encryptData:secKeyAlgorithm:completion:@
encryptData_secKeyAlgorithm_completionSelector :: Selector
encryptData_secKeyAlgorithm_completionSelector = mkSelector "encryptData:secKeyAlgorithm:completion:"

-- | @Selector@ for @canEncryptUsingSecKeyAlgorithm:@
canEncryptUsingSecKeyAlgorithmSelector :: Selector
canEncryptUsingSecKeyAlgorithmSelector = mkSelector "canEncryptUsingSecKeyAlgorithm:"

-- | @Selector@ for @verifyData:signature:secKeyAlgorithm:completion:@
verifyData_signature_secKeyAlgorithm_completionSelector :: Selector
verifyData_signature_secKeyAlgorithm_completionSelector = mkSelector "verifyData:signature:secKeyAlgorithm:completion:"

-- | @Selector@ for @canVerifyUsingSecKeyAlgorithm:@
canVerifyUsingSecKeyAlgorithmSelector :: Selector
canVerifyUsingSecKeyAlgorithmSelector = mkSelector "canVerifyUsingSecKeyAlgorithm:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

