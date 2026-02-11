{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionStreamProperties
--
-- A CMIOExtensionStreamProperties describes a CoreMediaIO extension stream properties.
--
-- Generated bindings for @CMIOExtensionStreamProperties@.
module ObjC.CoreMediaIO.CMIOExtensionStreamProperties
  ( CMIOExtensionStreamProperties
  , IsCMIOExtensionStreamProperties(..)
  , init_
  , new
  , streamPropertiesWithDictionary
  , initWithDictionary
  , setPropertyState_forProperty
  , propertiesDictionary
  , setPropertiesDictionary
  , initSelector
  , newSelector
  , streamPropertiesWithDictionarySelector
  , initWithDictionarySelector
  , setPropertyState_forPropertySelector
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
init_ :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id CMIOExtensionStreamProperties)
init_ cmioExtensionStreamProperties  =
  sendMsg cmioExtensionStreamProperties (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionStreamProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamProperties"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | streamPropertiesWithDictionary:
--
-- Return a stream properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionStreamProperties instance.
--
-- ObjC selector: @+ streamPropertiesWithDictionary:@
streamPropertiesWithDictionary :: IsNSDictionary propertiesDictionary => propertiesDictionary -> IO (Id CMIOExtensionStreamProperties)
streamPropertiesWithDictionary propertiesDictionary =
  do
    cls' <- getRequiredClass "CMIOExtensionStreamProperties"
    withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
      sendClassMsg cls' (mkSelector "streamPropertiesWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= retainedObject . castPtr

-- | initWithDictionary:
--
-- Initialize a stream properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionStreamProperties instance.
--
-- ObjC selector: @- initWithDictionary:@
initWithDictionary :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary propertiesDictionary) => cmioExtensionStreamProperties -> propertiesDictionary -> IO (Id CMIOExtensionStreamProperties)
initWithDictionary cmioExtensionStreamProperties  propertiesDictionary =
withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
    sendMsg cmioExtensionStreamProperties (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= ownedObject . castPtr

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
setPropertyState_forProperty :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsCMIOExtensionPropertyState propertyState, IsNSString property) => cmioExtensionStreamProperties -> propertyState -> property -> IO ()
setPropertyState_forProperty cmioExtensionStreamProperties  propertyState property =
withObjCPtr propertyState $ \raw_propertyState ->
  withObjCPtr property $ \raw_property ->
      sendMsg cmioExtensionStreamProperties (mkSelector "setPropertyState:forProperty:") retVoid [argPtr (castPtr raw_propertyState :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())]

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionStreamProperties cmioExtensionStreamProperties => cmioExtensionStreamProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionStreamProperties  =
  sendMsg cmioExtensionStreamProperties (mkSelector "propertiesDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionStreamProperties cmioExtensionStreamProperties, IsNSDictionary value) => cmioExtensionStreamProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionStreamProperties  value =
withObjCPtr value $ \raw_value ->
    sendMsg cmioExtensionStreamProperties (mkSelector "setPropertiesDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @streamPropertiesWithDictionary:@
streamPropertiesWithDictionarySelector :: Selector
streamPropertiesWithDictionarySelector = mkSelector "streamPropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

