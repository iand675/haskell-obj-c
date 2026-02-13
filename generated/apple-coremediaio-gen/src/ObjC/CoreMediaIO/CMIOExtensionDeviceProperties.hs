{-# LANGUAGE DataKinds #-}
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
  , devicePropertiesWithDictionarySelector
  , initSelector
  , initWithDictionarySelector
  , linkedCoreAudioDeviceUIDSelector
  , modelSelector
  , newSelector
  , propertiesDictionarySelector
  , setLinkedCoreAudioDeviceUIDSelector
  , setModelSelector
  , setPropertiesDictionarySelector
  , setPropertyState_forPropertySelector
  , setSuspendedSelector
  , setTransportTypeSelector
  , suspendedSelector
  , transportTypeSelector


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
init_ :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id CMIOExtensionDeviceProperties)
init_ cmioExtensionDeviceProperties =
  sendOwnedMessage cmioExtensionDeviceProperties initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionDeviceProperties)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionDeviceProperties"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' devicePropertiesWithDictionarySelector (toNSDictionary propertiesDictionary)

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
initWithDictionary cmioExtensionDeviceProperties propertiesDictionary =
  sendOwnedMessage cmioExtensionDeviceProperties initWithDictionarySelector (toNSDictionary propertiesDictionary)

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
setPropertyState_forProperty cmioExtensionDeviceProperties propertyState property =
  sendMessage cmioExtensionDeviceProperties setPropertyState_forPropertySelector (toCMIOExtensionPropertyState propertyState) (toNSString property)

-- | model
--
-- The device model.
--
-- The property key is CMIOExtensionPropertyDeviceModel.
--
-- ObjC selector: @- model@
model :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSString)
model cmioExtensionDeviceProperties =
  sendMessage cmioExtensionDeviceProperties modelSelector

-- | model
--
-- The device model.
--
-- The property key is CMIOExtensionPropertyDeviceModel.
--
-- ObjC selector: @- setModel:@
setModel :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSString value) => cmioExtensionDeviceProperties -> value -> IO ()
setModel cmioExtensionDeviceProperties value =
  sendMessage cmioExtensionDeviceProperties setModelSelector (toNSString value)

-- | suspended
--
-- Indicates whether the device is suspended.
--
-- The property key is CMIOExtensionPropertyDeviceIsSuspended.
--
-- ObjC selector: @- suspended@
suspended :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSNumber)
suspended cmioExtensionDeviceProperties =
  sendMessage cmioExtensionDeviceProperties suspendedSelector

-- | suspended
--
-- Indicates whether the device is suspended.
--
-- The property key is CMIOExtensionPropertyDeviceIsSuspended.
--
-- ObjC selector: @- setSuspended:@
setSuspended :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSNumber value) => cmioExtensionDeviceProperties -> value -> IO ()
setSuspended cmioExtensionDeviceProperties value =
  sendMessage cmioExtensionDeviceProperties setSuspendedSelector (toNSNumber value)

-- | transportType
--
-- The transport type of the receiver (e.g. USB, PCI, etc) whose value correspond to the audio transport type ( kIOAudioDeviceTransportType... ) defined in <IOKit/audio/IOAudioTypes.h>.
--
-- The property key is CMIOExtensionPropertyDeviceTransportType.
--
-- ObjC selector: @- transportType@
transportType :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSNumber)
transportType cmioExtensionDeviceProperties =
  sendMessage cmioExtensionDeviceProperties transportTypeSelector

-- | transportType
--
-- The transport type of the receiver (e.g. USB, PCI, etc) whose value correspond to the audio transport type ( kIOAudioDeviceTransportType... ) defined in <IOKit/audio/IOAudioTypes.h>.
--
-- The property key is CMIOExtensionPropertyDeviceTransportType.
--
-- ObjC selector: @- setTransportType:@
setTransportType :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSNumber value) => cmioExtensionDeviceProperties -> value -> IO ()
setTransportType cmioExtensionDeviceProperties value =
  sendMessage cmioExtensionDeviceProperties setTransportTypeSelector (toNSNumber value)

-- | linkedCoreAudioDeviceUID
--
-- The device linked CoreAudio device UID.
--
-- The property key is CMIOExtensionPropertyDeviceLinkedCoreAudioDeviceUID.
--
-- ObjC selector: @- linkedCoreAudioDeviceUID@
linkedCoreAudioDeviceUID :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSString)
linkedCoreAudioDeviceUID cmioExtensionDeviceProperties =
  sendMessage cmioExtensionDeviceProperties linkedCoreAudioDeviceUIDSelector

-- | linkedCoreAudioDeviceUID
--
-- The device linked CoreAudio device UID.
--
-- The property key is CMIOExtensionPropertyDeviceLinkedCoreAudioDeviceUID.
--
-- ObjC selector: @- setLinkedCoreAudioDeviceUID:@
setLinkedCoreAudioDeviceUID :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSString value) => cmioExtensionDeviceProperties -> value -> IO ()
setLinkedCoreAudioDeviceUID cmioExtensionDeviceProperties value =
  sendMessage cmioExtensionDeviceProperties setLinkedCoreAudioDeviceUIDSelector (toNSString value)

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- propertiesDictionary@
propertiesDictionary :: IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties => cmioExtensionDeviceProperties -> IO (Id NSDictionary)
propertiesDictionary cmioExtensionDeviceProperties =
  sendMessage cmioExtensionDeviceProperties propertiesDictionarySelector

-- | propertiesDictionary
--
-- The dictionary of properties.
--
-- The dictionary containing all keys and values.
--
-- ObjC selector: @- setPropertiesDictionary:@
setPropertiesDictionary :: (IsCMIOExtensionDeviceProperties cmioExtensionDeviceProperties, IsNSDictionary value) => cmioExtensionDeviceProperties -> value -> IO ()
setPropertiesDictionary cmioExtensionDeviceProperties value =
  sendMessage cmioExtensionDeviceProperties setPropertiesDictionarySelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionDeviceProperties)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionDeviceProperties)
newSelector = mkSelector "new"

-- | @Selector@ for @devicePropertiesWithDictionary:@
devicePropertiesWithDictionarySelector :: Selector '[Id NSDictionary] (Id CMIOExtensionDeviceProperties)
devicePropertiesWithDictionarySelector = mkSelector "devicePropertiesWithDictionary:"

-- | @Selector@ for @initWithDictionary:@
initWithDictionarySelector :: Selector '[Id NSDictionary] (Id CMIOExtensionDeviceProperties)
initWithDictionarySelector = mkSelector "initWithDictionary:"

-- | @Selector@ for @setPropertyState:forProperty:@
setPropertyState_forPropertySelector :: Selector '[Id CMIOExtensionPropertyState, Id NSString] ()
setPropertyState_forPropertySelector = mkSelector "setPropertyState:forProperty:"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id NSString)
modelSelector = mkSelector "model"

-- | @Selector@ for @setModel:@
setModelSelector :: Selector '[Id NSString] ()
setModelSelector = mkSelector "setModel:"

-- | @Selector@ for @suspended@
suspendedSelector :: Selector '[] (Id NSNumber)
suspendedSelector = mkSelector "suspended"

-- | @Selector@ for @setSuspended:@
setSuspendedSelector :: Selector '[Id NSNumber] ()
setSuspendedSelector = mkSelector "setSuspended:"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] (Id NSNumber)
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @setTransportType:@
setTransportTypeSelector :: Selector '[Id NSNumber] ()
setTransportTypeSelector = mkSelector "setTransportType:"

-- | @Selector@ for @linkedCoreAudioDeviceUID@
linkedCoreAudioDeviceUIDSelector :: Selector '[] (Id NSString)
linkedCoreAudioDeviceUIDSelector = mkSelector "linkedCoreAudioDeviceUID"

-- | @Selector@ for @setLinkedCoreAudioDeviceUID:@
setLinkedCoreAudioDeviceUIDSelector :: Selector '[Id NSString] ()
setLinkedCoreAudioDeviceUIDSelector = mkSelector "setLinkedCoreAudioDeviceUID:"

-- | @Selector@ for @propertiesDictionary@
propertiesDictionarySelector :: Selector '[] (Id NSDictionary)
propertiesDictionarySelector = mkSelector "propertiesDictionary"

-- | @Selector@ for @setPropertiesDictionary:@
setPropertiesDictionarySelector :: Selector '[Id NSDictionary] ()
setPropertiesDictionarySelector = mkSelector "setPropertiesDictionary:"

