{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionDeviceProperties
--
-- A CMIOExtensionDeviceProperties describes a CoreMediaIO extension device properties.
--
-- Generated bindings for @CMIOExtensionDeviceProperties@.
module ObjC.CoreMediaIO.CMIOExtensionDeviceProperties
  ( CMIOExtensionDeviceProperties
  , IsCMIOExtensionDeviceProperties(..)
  , init_
  , new
  , devicePropertiesWithDictionary
  , initWithDictionary
  , setPropertyState_forProperty
  , model
  , setModel
  , suspended
  , setSuspended
  , transportType
  , setTransportType
  , linkedCoreAudioDeviceUID
  , setLinkedCoreAudioDeviceUID
  , propertiesDictionary
  , setPropertiesDictionary
  , initSelector
  , newSelector
  , devicePropertiesWithDictionarySelector
  , initWithDictionarySelector
  , setPropertyState_forPropertySelector
  , modelSelector
  , setModelSelector
  , suspendedSelector
  , setSuspendedSelector
  , transportTypeSelector
  , setTransportTypeSelector
  , linkedCoreAudioDeviceUIDSelector
  , setLinkedCoreAudioDeviceUIDSelector
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
init_ :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id CMIOExtensionDeviceProperties)
init_ cmioExtensionDeviceProperties  =
    sendMsg cmioExtensionDeviceProperties (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionDeviceProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionDeviceProperties"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | devicePropertiesWithDictionary:
--
-- Return a device properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionDeviceProperties instance.
--
-- ObjC selector: @+ devicePropertiesWithDictionary:@
devicePropertiesWithDictionary :: IsNSDictionary propertiesDictionary => propertiesDictionary -> IO (Id CMIOExtensionDeviceProperties)
devicePropertiesWithDictionary propertiesDictionary =
  do
    cls' <- getRequiredClass "CMIOExtensionDeviceProperties"
    withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
      sendClassMsg cls' (mkSelector "devicePropertiesWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= retainedObject . castPtr

-- | initWithDictionary:
--
-- Initialize a device properties instance.
--
-- @propertiesDictionary@ — The dictionary of properties.
--
-- Returns: A CMIOExtensionDeviceProperties instance.
--
-- ObjC selector: @- initWithDictionary:@
initWithDictionary :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSDictionary propertiesDictionary) => cmioExtensionDeviceProperties -> propertiesDictionary -> IO (Id CMIOExtensionDeviceProperties)
initWithDictionary cmioExtensionDeviceProperties  propertiesDictionary =
  withObjCPtr propertiesDictionary $ \raw_propertiesDictionary ->
      sendMsg cmioExtensionDeviceProperties (mkSelector "initWithDictionary:") (retPtr retVoid) [argPtr (castPtr raw_propertiesDictionary :: Ptr ())] >>= ownedObject . castPtr

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
setPropertyState_forProperty :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsCMIOExtensionPropertyState propertyState, IsNSString property) => cmioExtensionDeviceProperties -> propertyState -> property -> IO ()
setPropertyState_forProperty cmioExtensionDeviceProperties  propertyState property =
  withObjCPtr propertyState $ \raw_propertyState ->
    withObjCPtr property $ \raw_property ->
        sendMsg cmioExtensionDeviceProperties (mkSelector "setPropertyState:forProperty:") retVoid [argPtr (castPtr raw_propertyState :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())]

-- | model
--
-- The device model.
--
-- The property key is CMIOExtensionPropertyDeviceModel.
--
-- ObjC selector: @- model@
model :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSString)
model cmioExtensionDeviceProperties  =
    sendMsg cmioExtensionDeviceProperties (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | model
--
-- The device model.
--
-- The property key is CMIOExtensionPropertyDeviceModel.
--
-- ObjC selector: @- setModel:@
setModel :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSString value) => cmioExtensionDeviceProperties -> value -> IO ()
setModel cmioExtensionDeviceProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionDeviceProperties (mkSelector "setModel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | suspended
--
-- Indicates whether the device is suspended.
--
-- The property key is CMIOExtensionPropertyDeviceIsSuspended.
--
-- ObjC selector: @- suspended@
suspended :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSNumber)
suspended cmioExtensionDeviceProperties  =
    sendMsg cmioExtensionDeviceProperties (mkSelector "suspended") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | suspended
--
-- Indicates whether the device is suspended.
--
-- The property key is CMIOExtensionPropertyDeviceIsSuspended.
--
-- ObjC selector: @- setSuspended:@
setSuspended :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSNumber value) => cmioExtensionDeviceProperties -> value -> IO ()
setSuspended cmioExtensionDeviceProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionDeviceProperties (mkSelector "setSuspended:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | transportType
--
-- The transport type of the receiver (e.g. USB, PCI, etc) whose value correspond to the audio transport type ( kIOAudioDeviceTransportType... ) defined in <IOKit/audio/IOAudioTypes.h>.
--
-- The property key is CMIOExtensionPropertyDeviceTransportType.
--
-- ObjC selector: @- transportType@
transportType :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSNumber)
transportType cmioExtensionDeviceProperties  =
    sendMsg cmioExtensionDeviceProperties (mkSelector "transportType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | transportType
--
-- The transport type of the receiver (e.g. USB, PCI, etc) whose value correspond to the audio transport type ( kIOAudioDeviceTransportType... ) defined in <IOKit/audio/IOAudioTypes.h>.
--
-- The property key is CMIOExtensionPropertyDeviceTransportType.
--
-- ObjC selector: @- setTransportType:@
setTransportType :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSNumber value) => cmioExtensionDeviceProperties -> value -> IO ()
setTransportType cmioExtensionDeviceProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionDeviceProperties (mkSelector "setTransportType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | linkedCoreAudioDeviceUID
--
-- The device linked CoreAudio device UID.
--
-- The property key is CMIOExtensionPropertyDeviceLinkedCoreAudioDeviceUID.
--
-- ObjC selector: @- linkedCoreAudioDeviceUID@
linkedCoreAudioDeviceUID :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSString)
linkedCoreAudioDeviceUID cmioExtensionDeviceProperties  =
    sendMsg cmioExtensionDeviceProperties (mkSelector "linkedCoreAudioDeviceUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | linkedCoreAudioDeviceUID
--
-- The device linked CoreAudio device UID.
--
-- The property key is CMIOExtensionPropertyDeviceLinkedCoreAudioDeviceUID.
--
-- ObjC selector: @- setLinkedCoreAudioDeviceUID:@
setLinkedCoreAudioDeviceUID :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSString value) => cmioExtensionDeviceProperties -> value -> IO ()
setLinkedCoreAudioDeviceUID cmioExtensionDeviceProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionDeviceProperties (mkSelector "setLinkedCoreAudioDeviceUID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionDeviceProperties  =
    sendMsg cmioExtensionDeviceProperties (mkSelector "propertiesDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSDictionary value) => cmioExtensionDeviceProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionDeviceProperties  value =
  withObjCPtr value $ \raw_value ->
      sendMsg cmioExtensionDeviceProperties (mkSelector "setPropertiesDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @devicePropertiesWithDictionary:@
devicePropertiesWithDictionarySelector :: Selector
devicePropertiesWithDictionarySelector = mkSelector "devicePropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @setModel:@
setModelSelector :: Selector
setModelSelector = mkSelector "setModel:"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @setSuspended:@
setSuspendedSelector :: Selector
setSuspendedSelector = mkSelector "setSuspended:"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @setTransportType:@
setTransportTypeSelector :: Selector
setTransportTypeSelector = mkSelector "setTransportType:"

-- | @Selector@ for @linkedCoreAudioDeviceUID@
linkedCoreAudioDeviceUIDSelector :: Selector
linkedCoreAudioDeviceUIDSelector = mkSelector "linkedCoreAudioDeviceUID"

-- | @Selector@ for @setLinkedCoreAudioDeviceUID:@
setLinkedCoreAudioDeviceUIDSelector :: Selector
setLinkedCoreAudioDeviceUIDSelector = mkSelector "setLinkedCoreAudioDeviceUID:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

