{-# LANGUAGE PatternSynonyms #-}
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
  , deviceAccountIdentifierSelector
  , configurationDataSelector
  , configurationDataTypeSelector

  -- * Enum types
  , PKBarcodeEventConfigurationDataType(PKBarcodeEventConfigurationDataType)
  , pattern PKBarcodeEventConfigurationDataTypeUnknown
  , pattern PKBarcodeEventConfigurationDataTypeSigningKeyMaterial
  , pattern PKBarcodeEventConfigurationDataTypeSigningCertificate

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
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- deviceAccountIdentifier@
deviceAccountIdentifier :: IsPKBarcodeEventConfigurationRequest pkBarcodeEventConfigurationRequest => pkBarcodeEventConfigurationRequest -> IO (Id NSString)
deviceAccountIdentifier pkBarcodeEventConfigurationRequest  =
  sendMsg pkBarcodeEventConfigurationRequest (mkSelector "deviceAccountIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- configurationData@
configurationData :: IsPKBarcodeEventConfigurationRequest pkBarcodeEventConfigurationRequest => pkBarcodeEventConfigurationRequest -> IO (Id NSData)
configurationData pkBarcodeEventConfigurationRequest  =
  sendMsg pkBarcodeEventConfigurationRequest (mkSelector "configurationData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- configurationDataType@
configurationDataType :: IsPKBarcodeEventConfigurationRequest pkBarcodeEventConfigurationRequest => pkBarcodeEventConfigurationRequest -> IO PKBarcodeEventConfigurationDataType
configurationDataType pkBarcodeEventConfigurationRequest  =
  fmap (coerce :: CLong -> PKBarcodeEventConfigurationDataType) $ sendMsg pkBarcodeEventConfigurationRequest (mkSelector "configurationDataType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceAccountIdentifier@
deviceAccountIdentifierSelector :: Selector
deviceAccountIdentifierSelector = mkSelector "deviceAccountIdentifier"

-- | @Selector@ for @configurationData@
configurationDataSelector :: Selector
configurationDataSelector = mkSelector "configurationData"

-- | @Selector@ for @configurationDataType@
configurationDataTypeSelector :: Selector
configurationDataTypeSelector = mkSelector "configurationDataType"

