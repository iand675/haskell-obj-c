{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIssuerProvisioningExtensionHandler@.
module ObjC.PassKit.PKIssuerProvisioningExtensionHandler
  ( PKIssuerProvisioningExtensionHandler
  , IsPKIssuerProvisioningExtensionHandler(..)
  , statusWithCompletion
  , generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandler
  , generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector
  , statusWithCompletionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- statusWithCompletion:@
statusWithCompletion :: IsPKIssuerProvisioningExtensionHandler pkIssuerProvisioningExtensionHandler => pkIssuerProvisioningExtensionHandler -> Ptr () -> IO ()
statusWithCompletion pkIssuerProvisioningExtensionHandler completion =
  sendMessage pkIssuerProvisioningExtensionHandler statusWithCompletionSelector completion

-- | @- generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:@
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandler :: (IsPKIssuerProvisioningExtensionHandler pkIssuerProvisioningExtensionHandler, IsNSString identifier, IsPKAddPaymentPassRequestConfiguration configuration, IsNSArray certificates, IsNSData nonce, IsNSData nonceSignature) => pkIssuerProvisioningExtensionHandler -> identifier -> configuration -> certificates -> nonce -> nonceSignature -> Ptr () -> IO ()
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandler pkIssuerProvisioningExtensionHandler identifier configuration certificates nonce nonceSignature completion =
  sendMessage pkIssuerProvisioningExtensionHandler generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector (toNSString identifier) (toPKAddPaymentPassRequestConfiguration configuration) (toNSArray certificates) (toNSData nonce) (toNSData nonceSignature) completion

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @statusWithCompletion:@
statusWithCompletionSelector :: Selector '[Ptr ()] ()
statusWithCompletionSelector = mkSelector "statusWithCompletion:"

-- | @Selector@ for @generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:@
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector :: Selector '[Id NSString, Id PKAddPaymentPassRequestConfiguration, Id NSArray, Id NSData, Id NSData, Ptr ()] ()
generateAddPaymentPassRequestForPassEntryWithIdentifier_configuration_certificateChain_nonce_nonceSignature_completionHandlerSelector = mkSelector "generateAddPaymentPassRequestForPassEntryWithIdentifier:configuration:certificateChain:nonce:nonceSignature:completionHandler:"

