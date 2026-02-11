{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAddCarKeyPassConfiguration@.
module ObjC.PassKit.PKAddCarKeyPassConfiguration
  ( PKAddCarKeyPassConfiguration
  , IsPKAddCarKeyPassConfiguration(..)
  , init_
  , password
  , setPassword
  , supportedRadioTechnologies
  , setSupportedRadioTechnologies
  , manufacturerIdentifier
  , setManufacturerIdentifier
  , provisioningTemplateIdentifier
  , setProvisioningTemplateIdentifier
  , initSelector
  , passwordSelector
  , setPasswordSelector
  , supportedRadioTechnologiesSelector
  , setSupportedRadioTechnologiesSelector
  , manufacturerIdentifierSelector
  , setManufacturerIdentifierSelector
  , provisioningTemplateIdentifierSelector
  , setProvisioningTemplateIdentifierSelector

  -- * Enum types
  , PKRadioTechnology(PKRadioTechnology)
  , pattern PKRadioTechnologyNone
  , pattern PKRadioTechnologyNFC
  , pattern PKRadioTechnologyBluetooth

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

-- | @- init@
init_ :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id PKAddCarKeyPassConfiguration)
init_ pkAddCarKeyPassConfiguration  =
  sendMsg pkAddCarKeyPassConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- password@
password :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id NSString)
password pkAddCarKeyPassConfiguration  =
  sendMsg pkAddCarKeyPassConfiguration (mkSelector "password") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPassword:@
setPassword :: (IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration, IsNSString value) => pkAddCarKeyPassConfiguration -> value -> IO ()
setPassword pkAddCarKeyPassConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddCarKeyPassConfiguration (mkSelector "setPassword:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supportedRadioTechnologies@
supportedRadioTechnologies :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO PKRadioTechnology
supportedRadioTechnologies pkAddCarKeyPassConfiguration  =
  fmap (coerce :: CULong -> PKRadioTechnology) $ sendMsg pkAddCarKeyPassConfiguration (mkSelector "supportedRadioTechnologies") retCULong []

-- | @- setSupportedRadioTechnologies:@
setSupportedRadioTechnologies :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> PKRadioTechnology -> IO ()
setSupportedRadioTechnologies pkAddCarKeyPassConfiguration  value =
  sendMsg pkAddCarKeyPassConfiguration (mkSelector "setSupportedRadioTechnologies:") retVoid [argCULong (coerce value)]

-- | @- manufacturerIdentifier@
manufacturerIdentifier :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id NSString)
manufacturerIdentifier pkAddCarKeyPassConfiguration  =
  sendMsg pkAddCarKeyPassConfiguration (mkSelector "manufacturerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManufacturerIdentifier:@
setManufacturerIdentifier :: (IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration, IsNSString value) => pkAddCarKeyPassConfiguration -> value -> IO ()
setManufacturerIdentifier pkAddCarKeyPassConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddCarKeyPassConfiguration (mkSelector "setManufacturerIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- provisioningTemplateIdentifier@
provisioningTemplateIdentifier :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id NSString)
provisioningTemplateIdentifier pkAddCarKeyPassConfiguration  =
  sendMsg pkAddCarKeyPassConfiguration (mkSelector "provisioningTemplateIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProvisioningTemplateIdentifier:@
setProvisioningTemplateIdentifier :: (IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration, IsNSString value) => pkAddCarKeyPassConfiguration -> value -> IO ()
setProvisioningTemplateIdentifier pkAddCarKeyPassConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAddCarKeyPassConfiguration (mkSelector "setProvisioningTemplateIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @password@
passwordSelector :: Selector
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @supportedRadioTechnologies@
supportedRadioTechnologiesSelector :: Selector
supportedRadioTechnologiesSelector = mkSelector "supportedRadioTechnologies"

-- | @Selector@ for @setSupportedRadioTechnologies:@
setSupportedRadioTechnologiesSelector :: Selector
setSupportedRadioTechnologiesSelector = mkSelector "setSupportedRadioTechnologies:"

-- | @Selector@ for @manufacturerIdentifier@
manufacturerIdentifierSelector :: Selector
manufacturerIdentifierSelector = mkSelector "manufacturerIdentifier"

-- | @Selector@ for @setManufacturerIdentifier:@
setManufacturerIdentifierSelector :: Selector
setManufacturerIdentifierSelector = mkSelector "setManufacturerIdentifier:"

-- | @Selector@ for @provisioningTemplateIdentifier@
provisioningTemplateIdentifierSelector :: Selector
provisioningTemplateIdentifierSelector = mkSelector "provisioningTemplateIdentifier"

-- | @Selector@ for @setProvisioningTemplateIdentifier:@
setProvisioningTemplateIdentifierSelector :: Selector
setProvisioningTemplateIdentifierSelector = mkSelector "setProvisioningTemplateIdentifier:"

