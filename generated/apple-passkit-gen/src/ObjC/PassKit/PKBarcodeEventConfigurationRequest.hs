{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKBarcodeEventConfigurationRequest@.
module ObjC.PassKit.PKBarcodeEventConfigurationRequest
  ( PKBarcodeEventConfigurationRequest
  , IsPKBarcodeEventConfigurationRequest(..)
  , deviceAccountIdentifier
  , configurationData
  , configurationDataType
  , configurationDataSelector
  , configurationDataTypeSelector
  , deviceAccountIdentifierSelector

  -- * Enum types
  , PKBarcodeEventConfigurationDataType(PKBarcodeEventConfigurationDataType)
  , pattern PKBarcodeEventConfigurationDataTypeUnknown
  , pattern PKBarcodeEventConfigurationDataTypeSigningKeyMaterial
  , pattern PKBarcodeEventConfigurationDataTypeSigningCertificate

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKBarcodeEventConfigurationRequest pkBarcodeEventConfigurationRequest => pkBarcodeEventConfigurationRequest -> IO (Id NSString)
deviceAccountIdentifier pkBarcodeEventConfigurationRequest =
  sendMessage pkBarcodeEventConfigurationRequest deviceAccountIdentifierSelector

-- | @- configurationData@
configurationData :: IsPKBarcodeEventConfigurationRequest pkBarcodeEventConfigurationRequest => pkBarcodeEventConfigurationRequest -> IO (Id NSData)
configurationData pkBarcodeEventConfigurationRequest =
  sendMessage pkBarcodeEventConfigurationRequest configurationDataSelector

-- | @- configurationDataType@
configurationDataType :: IsPKBarcodeEventConfigurationRequest pkBarcodeEventConfigurationRequest => pkBarcodeEventConfigurationRequest -> IO PKBarcodeEventConfigurationDataType
configurationDataType pkBarcodeEventConfigurationRequest =
  sendMessage pkBarcodeEventConfigurationRequest configurationDataTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector '[] (Id NSString)
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @configurationData@
configurationDataSelector :: Selector '[] (Id NSData)
configurationDataSelector = mkSelector "configurationData"

-- | @Selector@ for @configurationDataType@
configurationDataTypeSelector :: Selector '[] PKBarcodeEventConfigurationDataType
configurationDataTypeSelector = mkSelector "configurationDataType"

