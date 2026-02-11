{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIssuerProvisioningExtensionHandler@.
module ObjC.PassKit.PKIssuerProvisioningExtensionHandler
  ( PKIssuerProvisioningExtensionHandler
  , IsPKIssuerProvisioningExtensionHandler(..)
  , statusWithCompletion
  , generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandler
  , statusWithCompletionSelector
  , generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- statusWithCompletion:@
statusWithCompletion :: IsPKIssuerProvisioningExtensionHandler pkIssuerProvisioningExtensionHandler => pkIssuerProvisioningExtensionHandler -> Ptr () -> IO ()
statusWithCompletion pkIssuerProvisioningExtensionHandler  completion =
  sendMsg pkIssuerProvisioningExtensionHandler (mkSelector "statusWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | @- generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:@
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandler :: (IsPKIssuerProvisioningExtensionHandler pkIssuerProvisioningExtensionHandler, IsNSString identifier, IsPKAddPaymentPassRequestConfiguration configuration, IsNSArray certificates, IsNSData nonce, IsNSData nonceSignature) => pkIssuerProvisioningExtensionHandler -> identifier -> configuration -> certificates -> nonce -> nonceSignature -> Ptr () -> IO ()
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandler pkIssuerProvisioningExtensionHandler  identifier configuration certificates nonce nonceSignature completion =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr configuration $ \raw_configuration ->
    withObjCPtr certificates $ \raw_certificates ->
      withObjCPtr nonce $ \raw_nonce ->
        withObjCPtr nonceSignature $ \raw_nonceSignature ->
            sendMsg pkIssuerProvisioningExtensionHandler (mkSelector "generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_certificates :: Ptr ()), argPtr (castPtr raw_nonce :: Ptr ()), argPtr (castPtr raw_nonceSignature :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @statusWithCompletion:@
statusWithCompletionSelector :: Selector
statusWithCompletionSelector = mkSelector "statusWithCompletion:"

-- | @Selector@ for @generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:@
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector :: Selector
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector = mkSelector "generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:"

