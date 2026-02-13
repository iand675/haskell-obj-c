{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIssuerProvisioningExtensionPaymentPassEntry@.
module ObjC.PassKit.PKIssuerProvisioningExtensionPaymentPassEntry
  ( PKIssuerProvisioningExtensionPaymentPassEntry
  , IsPKIssuerProvisioningExtensionPaymentPassEntry(..)
  , initWithIdentifier_title_art_addRequestConfiguration
  , addRequestConfiguration
  , addRequestConfigurationSelector
  , initWithIdentifier_title_art_addRequestConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithIdentifier:title:art:addRequestConfiguration:@
initWithIdentifier_title_art_addRequestConfiguration :: (IsPKIssuerProvisioningExtensionPaymentPassEntry pkIssuerProvisioningExtensionPaymentPassEntry, IsNSString identifier, IsNSString title, IsPKAddPaymentPassRequestConfiguration configuration) => pkIssuerProvisioningExtensionPaymentPassEntry -> identifier -> title -> Ptr () -> configuration -> IO (Id PKIssuerProvisioningExtensionPaymentPassEntry)
initWithIdentifier_title_art_addRequestConfiguration pkIssuerProvisioningExtensionPaymentPassEntry identifier title art configuration =
  sendOwnedMessage pkIssuerProvisioningExtensionPaymentPassEntry initWithIdentifier_title_art_addRequestConfigurationSelector (toNSString identifier) (toNSString title) art (toPKAddPaymentPassRequestConfiguration configuration)

-- | @- addRequestConfiguration@
addRequestConfiguration :: IsPKIssuerProvisioningExtensionPaymentPassEntry pkIssuerProvisioningExtensionPaymentPassEntry => pkIssuerProvisioningExtensionPaymentPassEntry -> IO (Id PKAddPaymentPassRequestConfiguration)
addRequestConfiguration pkIssuerProvisioningExtensionPaymentPassEntry =
  sendMessage pkIssuerProvisioningExtensionPaymentPassEntry addRequestConfigurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:art:addRequestConfiguration:@
initWithIdentifier_title_art_addRequestConfigurationSelector :: Selector '[Id NSString, Id NSString, Ptr (), Id PKAddPaymentPassRequestConfiguration] (Id PKIssuerProvisioningExtensionPaymentPassEntry)
initWithIdentifier_title_art_addRequestConfigurationSelector = mkSelector "initWithIdentifier:title:art:addRequestConfiguration:"

-- | @Selector@ for @addRequestConfiguration@
addRequestConfigurationSelector :: Selector '[] (Id PKAddPaymentPassRequestConfiguration)
addRequestConfigurationSelector = mkSelector "addRequestConfiguration"

