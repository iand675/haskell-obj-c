{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionProviderProperties
--
-- A CMIOExtensionProviderProperties describes CoreMediaIO extension provider properties.
--
-- Generated bindings for @CMIOExtensionProviderProperties@.
module ObjC.CoreMediaIO.CMIOExtensionProviderProperties
  ( CMIOExtensionProviderProperties
  , IsCMIOExtensionProviderProperties(..)
  , init_
  , new
  , providerPropertiesWithDictionary
  , initWithDictionary
  , setPropertyState_forProperty
  , name
  , setName
  , manufacturer
  , setManufacturer
  , propertiesDictionary
  , setPropertiesDictionary
  , initSelector
  , initWithDictionarySelector
  , manufacturerSelector
  , nameSelector
  , newSelector
  , propertiesDictionarySelector
  , providerPropertiesWithDictionarySelector
  , setManufacturerSelector
  , setNameSelector
  , setPropertiesDictionarySelector
  , setPropertyState_forPropertySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id CMIOExtensionProviderProperties)
init_ cmioExtensionProviderProperties =
  sendOwnedMessage cmioExtensionProviderProperties initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionProviderProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionProviderProperties"
    sendOwnedClassMessage cls' newSelector

-- | providerPropertiesWithDictionary:
--
-- Return a provider properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionProviderProperties instance.
--
-- ObjC selector: @+ providerPropertiesWithDictionary:@
providerPropertiesWithDictionary :: IsNSDictionary propertiesDictionary => propertiesDictionary -> IO (Id CMIOExtensionProviderProperties)
providerPropertiesWithDictionary propertiesDictionary =
  do
    cls' <- getRequiredClass "CMIOExtensionProviderProperties"
    sendClassMessage cls' providerPropertiesWithDictionarySelector (toNSDictionary propertiesDictionary)

-- | initWithDictionary:
--
-- Initialize a provider properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionProviderProperties instance.
--
-- ObjC selector: @- initWithDictionary:@
initWithDictionary :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSDictionary propertiesDictionary) => cmioExtensionProviderProperties -> propertiesDictionary -> IO (Id CMIOExtensionProviderProperties)
initWithDictionary cmioExtensionProviderProperties propertiesDictionary =
  sendOwnedMessage cmioExtensionProviderProperties initWithDictionarySelector (toNSDictionary propertiesDictionary)

-- | setPropertyState:forProperty:
--
-- Set the property value.
--
-- @propertyState@ — The property state.
--
-- @property@ — The property key.
--
-- Setting nil to propertyState does remove the property.
--
-- ObjC selector: @- setPropertyState:forProperty:@
setPropertyState_forProperty :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsCMIOExtensionPropertyState propertyState, IsNSString property) => cmioExtensionProviderProperties -> propertyState -> property -> IO ()
setPropertyState_forProperty cmioExtensionProviderProperties propertyState property =
  sendMessage cmioExtensionProviderProperties setPropertyState_forPropertySelector (toCMIOExtensionPropertyState propertyState) (toNSString property)

-- | name
--
-- The provider name.
--
-- The property key is CMIOExtensionPropertyProviderName.
--
-- ObjC selector: @- name@
name :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id NSString)
name cmioExtensionProviderProperties =
  sendMessage cmioExtensionProviderProperties nameSelector

-- | name
--
-- The provider name.
--
-- The property key is CMIOExtensionPropertyProviderName.
--
-- ObjC selector: @- setName:@
setName :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSString value) => cmioExtensionProviderProperties -> value -> IO ()
setName cmioExtensionProviderProperties value =
  sendMessage cmioExtensionProviderProperties setNameSelector (toNSString value)

-- | manufacturer
--
-- The provider manufacturer.
--
-- The property key is CMIOExtensionPropertyProviderManufacturer.
--
-- ObjC selector: @- manufacturer@
manufacturer :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id NSString)
manufacturer cmioExtensionProviderProperties =
  sendMessage cmioExtensionProviderProperties manufacturerSelector

-- | manufacturer
--
-- The provider manufacturer.
--
-- The property key is CMIOExtensionPropertyProviderManufacturer.
--
-- ObjC selector: @- setManufacturer:@
setManufacturer :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSString value) => cmioExtensionProviderProperties -> value -> IO ()
setManufacturer cmioExtensionProviderProperties value =
  sendMessage cmioExtensionProviderProperties setManufacturerSelector (toNSString value)

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionProviderProperties =
  sendMessage cmioExtensionProviderProperties propertiesDictionarySelector

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSDictionary value) => cmioExtensionProviderProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionProviderProperties value =
  sendMessage cmioExtensionProviderProperties setPropertiesDictionarySelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionProviderProperties)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionProviderProperties)
newSelector = mkSelector "new"

-- | @Selector@ for @providerPropertiesWithDictionary:@
providerPropertiesWithDictionarySelector :: Selector '[Id NSDictionary] (Id CMIOExtensionProviderProperties)
providerPropertiesWithDictionarySelector = mkSelector "providerPropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id CMIOExtensionProviderProperties)
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector '[Id CMIOExtensionPropertyState, Id NSString] ()
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector '[] (Id NSString)
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @setManufacturer:@
setManufacturerSelector :: Selector '[Id NSString] ()
setManufacturerSelector = mkSelector "setManufacturer:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector '[] (Id NSDictionary)
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector '[Id NSDictionary] ()
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

