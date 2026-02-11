{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKIssuerProvisioningExtensionPaymentPassEntry@.
module ObjC.PassKit.PKIssuerProvisioningExtensionPaymentPassEntry
  ( PKIssuerProvisioningExtensionPaymentPassEntry
  , IsPKIssuerProvisioningExtensionPaymentPassEntry(..)
  , initWithIdentifier_title_art_addRequestConfiguration
  , addRequestConfiguration
  , initWithIdentifier_title_art_addRequestConfigurationSelector
  , addRequestConfigurationSelector


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

-- | @- initWithIdentifier:title:art:addRequestConfiguration:@
initWithIdentifier_title_art_addRequestConfiguration :: (IsPKIssuerProvisioningExtensionPaymentPassEntry pkIssuerProvisioningExtensionPaymentPassEntry, IsNSString identifier, IsNSString title, IsPKAddPaymentPassRequestConfiguration configuration) => pkIssuerProvisioningExtensionPaymentPassEntry -> identifier -> title -> Ptr () -> configuration -> IO (Id PKIssuerProvisioningExtensionPaymentPassEntry)
initWithIdentifier_title_art_addRequestConfiguration pkIssuerProvisioningExtensionPaymentPassEntry  identifier title art configuration =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
    withObjCPtr configuration $ \raw_configuration ->
        sendMsg pkIssuerProvisioningExtensionPaymentPassEntry (mkSelector "initWithIdentifier:title:art:addRequestConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr art, argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | @- addRequestConfiguration@
addRequestConfiguration :: IsPKIssuerProvisioningExtensionPaymentPassEntry pkIssuerProvisioningExtensionPaymentPassEntry => pkIssuerProvisioningExtensionPaymentPassEntry -> IO (Id PKAddPaymentPassRequestConfiguration)
addRequestConfiguration pkIssuerProvisioningExtensionPaymentPassEntry  =
  sendMsg pkIssuerProvisioningExtensionPaymentPassEntry (mkSelector "addRequestConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:art:addRequestConfiguration:@
initWithIdentifier_title_art_addRequestConfigurationSelector :: Selector
initWithIdentifier_title_art_addRequestConfigurationSelector = mkSelector "initWithIdentifier:title:art:addRequestConfiguration:"

-- | @Selector@ for @addRequestConfiguration@
addRequestConfigurationSelector :: Selector
addRequestConfigurationSelector = mkSelector "addRequestConfiguration"

