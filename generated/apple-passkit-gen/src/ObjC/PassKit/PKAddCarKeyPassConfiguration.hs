{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , manufacturerIdentifierSelector
  , passwordSelector
  , provisioningTemplateIdentifierSelector
  , setManufacturerIdentifierSelector
  , setPasswordSelector
  , setProvisioningTemplateIdentifierSelector
  , setSupportedRadioTechnologiesSelector
  , supportedRadioTechnologiesSelector

  -- * Enum types
  , PKRadioTechnology(PKRadioTechnology)
  , pattern PKRadioTechnologyNone
  , pattern PKRadioTechnologyNFC
  , pattern PKRadioTechnologyBluetooth

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

-- | @- init@
init_ :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id PKAddCarKeyPassConfiguration)
init_ pkAddCarKeyPassConfiguration =
  sendOwnedMessage pkAddCarKeyPassConfiguration initSelector

-- | @- password@
password :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id NSString)
password pkAddCarKeyPassConfiguration =
  sendMessage pkAddCarKeyPassConfiguration passwordSelector

-- | @- setPassword:@
setPassword :: (IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration, IsNSString value) => pkAddCarKeyPassConfiguration -> value -> IO ()
setPassword pkAddCarKeyPassConfiguration value =
  sendMessage pkAddCarKeyPassConfiguration setPasswordSelector (toNSString value)

-- | @- supportedRadioTechnologies@
supportedRadioTechnologies :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO PKRadioTechnology
supportedRadioTechnologies pkAddCarKeyPassConfiguration =
  sendMessage pkAddCarKeyPassConfiguration supportedRadioTechnologiesSelector

-- | @- setSupportedRadioTechnologies:@
setSupportedRadioTechnologies :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> PKRadioTechnology -> IO ()
setSupportedRadioTechnologies pkAddCarKeyPassConfiguration value =
  sendMessage pkAddCarKeyPassConfiguration setSupportedRadioTechnologiesSelector value

-- | @- manufacturerIdentifier@
manufacturerIdentifier :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id NSString)
manufacturerIdentifier pkAddCarKeyPassConfiguration =
  sendMessage pkAddCarKeyPassConfiguration manufacturerIdentifierSelector

-- | @- setManufacturerIdentifier:@
setManufacturerIdentifier :: (IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration, IsNSString value) => pkAddCarKeyPassConfiguration -> value -> IO ()
setManufacturerIdentifier pkAddCarKeyPassConfiguration value =
  sendMessage pkAddCarKeyPassConfiguration setManufacturerIdentifierSelector (toNSString value)

-- | @- provisioningTemplateIdentifier@
provisioningTemplateIdentifier :: IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration => pkAddCarKeyPassConfiguration -> IO (Id NSString)
provisioningTemplateIdentifier pkAddCarKeyPassConfiguration =
  sendMessage pkAddCarKeyPassConfiguration provisioningTemplateIdentifierSelector

-- | @- setProvisioningTemplateIdentifier:@
setProvisioningTemplateIdentifier :: (IsPKAddCarKeyPassConfiguration pkAddCarKeyPassConfiguration, IsNSString value) => pkAddCarKeyPassConfiguration -> value -> IO ()
setProvisioningTemplateIdentifier pkAddCarKeyPassConfiguration value =
  sendMessage pkAddCarKeyPassConfiguration setProvisioningTemplateIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKAddCarKeyPassConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @password@
passwordSelector :: Selector '[] (Id NSString)
passwordSelector = mkSelector "password"

-- | @Selector@ for @setPassword:@
setPasswordSelector :: Selector '[Id NSString] ()
setPasswordSelector = mkSelector "setPassword:"

-- | @Selector@ for @supportedRadioTechnologies@
supportedRadioTechnologiesSelector :: Selector '[] PKRadioTechnology
supportedRadioTechnologiesSelector = mkSelector "supportedRadioTechnologies"

-- | @Selector@ for @setSupportedRadioTechnologies:@
setSupportedRadioTechnologiesSelector :: Selector '[PKRadioTechnology] ()
setSupportedRadioTechnologiesSelector = mkSelector "setSupportedRadioTechnologies:"

-- | @Selector@ for @manufacturerIdentifier@
manufacturerIdentifierSelector :: Selector '[] (Id NSString)
manufacturerIdentifierSelector = mkSelector "manufacturerIdentifier"

-- | @Selector@ for @setManufacturerIdentifier:@
setManufacturerIdentifierSelector :: Selector '[Id NSString] ()
setManufacturerIdentifierSelector = mkSelector "setManufacturerIdentifier:"

-- | @Selector@ for @provisioningTemplateIdentifier@
provisioningTemplateIdentifierSelector :: Selector '[] (Id NSString)
provisioningTemplateIdentifierSelector = mkSelector "provisioningTemplateIdentifier"

-- | @Selector@ for @setProvisioningTemplateIdentifier:@
setProvisioningTemplateIdentifierSelector :: Selector '[Id NSString] ()
setProvisioningTemplateIdentifierSelector = mkSelector "setProvisioningTemplateIdentifier:"

