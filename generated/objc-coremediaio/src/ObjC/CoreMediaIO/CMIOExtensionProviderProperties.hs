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
  , newSelector
  , providerPropertiesWithDictionarySelector
  , initWithDictionarySelector
  , setPropertyState_forPropertySelector
  , nameSelector
  , setNameSelector
  , manufacturerSelector
  , setManufacturerSelector
  , propertiesDictionarySelector
  , setPropertiesDictionarySelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id CMIOExtensionProviderProperties)
init_ cmioExtensionProviderProperties  =
  sendMsg cmioExtensionProviderProperties (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionProviderProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionProviderProperties"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
      sendClassMsg cls' (mkSelector "providerPropertiesWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= retainedObject . castPtr

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
initWithDictionary cmioExtensionProviderProperties  propertiesDictionary =
withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
    sendMsg cmioExtensionProviderProperties (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= ownedObject . castPtr

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
setPropertyState_forProperty cmioExtensionProviderProperties  propertyState property =
withObjCPtr propertyState $ \raw_propertyState ->
  withObjCPtr property $ \raw_property ->
      sendMsg cmioExtensionProviderProperties (mkSelector "setPropertyState:forProperty:") retVoid [argPtr (castPtr raw_propertyState :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())]

-- | name
--
-- The provider name.
--
-- The property key is CMIOExtensionPropertyProviderName.
--
-- ObjC selector: @- name@
name :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id NSString)
name cmioExtensionProviderProperties  =
  sendMsg cmioExtensionProviderProperties (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- The provider name.
--
-- The property key is CMIOExtensionPropertyProviderName.
--
-- ObjC selector: @- setName:@
setName :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSString value) => cmioExtensionProviderProperties -> value -> IO ()
setName cmioExtensionProviderProperties  value =
withObjCPtr value $ \raw_value ->
    sendMsg cmioExtensionProviderProperties (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | manufacturer
--
-- The provider manufacturer.
--
-- The property key is CMIOExtensionPropertyProviderManufacturer.
--
-- ObjC selector: @- manufacturer@
manufacturer :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id NSString)
manufacturer cmioExtensionProviderProperties  =
  sendMsg cmioExtensionProviderProperties (mkSelector "manufacturer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | manufacturer
--
-- The provider manufacturer.
--
-- The property key is CMIOExtensionPropertyProviderManufacturer.
--
-- ObjC selector: @- setManufacturer:@
setManufacturer :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSString value) => cmioExtensionProviderProperties -> value -> IO ()
setManufacturer cmioExtensionProviderProperties  value =
withObjCPtr value $ \raw_value ->
    sendMsg cmioExtensionProviderProperties (mkSelector "setManufacturer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionProviderProperties cmioExtensionProviderProperties => cmioExtensionProviderProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionProviderProperties  =
  sendMsg cmioExtensionProviderProperties (mkSelector "propertiesDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionProviderProperties cmioExtensionProviderProperties, IsNSDictionary value) => cmioExtensionProviderProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionProviderProperties  value =
withObjCPtr value $ \raw_value ->
    sendMsg cmioExtensionProviderProperties (mkSelector "setPropertiesDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @providerPropertiesWithDictionary:@
providerPropertiesWithDictionarySelector :: Selector
providerPropertiesWithDictionarySelector = mkSelector "providerPropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @setManufacturer:@
setManufacturerSelector :: Selector
setManufacturerSelector = mkSelector "setManufacturer:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

